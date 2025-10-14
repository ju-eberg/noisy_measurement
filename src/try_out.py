# ds_bayes_marginalized.py
import numpy as np
import pandas as pd
import pymc as pm
import arviz as az
#import aesara.tensor as at
import pytensor.tensor as pt

# ---------------------------
# 1) Daten laden & vorbereiten
# ---------------------------
CSV = "../data_basis_mit_1st_stelle.csv"  # <-- Pfad zu deiner CSV
df = pd.read_csv(CSV)

# Sicherstellen, dass die erwarteten Spalten vorhanden sind
required_cols = ["item_id", "res1_isco_1st", "res2_isco_1st"]
missing = [c for c in required_cols if c not in df.columns]
if missing:
    raise ValueError(f"Fehlende Spalten in {CSV}: {missing}")

# Rater-Spalten festlegen (hier 2 Rater; weitere z.B. 'res_auto_isco_1st' kannst du ergänzen)
rater_cols = ["res1_isco_1st", "res2_isco_1st"]

# Alle vorkommenden Klassenwerte einsammeln (über alle Rater-Spalten)
all_labels = pd.unique(pd.concat([df[c] for c in rater_cols], ignore_index=True).dropna())
# stabile Sortierung
all_labels = np.sort(all_labels.astype(str))

# Mapping: Original-Label -> Integer-Klasse [0..J-1]
label_to_int = {lab: i for i, lab in enumerate(all_labels)}
int_to_label = {i: lab for lab, i in label_to_int.items()}
J = len(label_to_int)
print(f"Anzahl Klassen J = {J}")

# Beobachtungsmatrix Y (N x K) mit -1 für Missing
N = df.shape[0]
K = len(rater_cols)
Y = np.full((N, K), -1, dtype=int)

for k, col in enumerate(rater_cols):
    col_vals = df[col].astype(str)
    mask = ~df[col].isna()
    # Nur nicht-missing in Integer codieren
    Y[mask.values, k] = col_vals[mask].map(label_to_int).astype(int).values

# ---------------------------
# 2) Bayesianisches DS-Modell (marginalisiert)
# ---------------------------
# Priors: Dirichlet auf pi (Prävalenzen) und theta (Konfusionsmatrizen)
# theta[k,j,:] = Simplex über vorhergesagte Rater-Labels l für wahre Klasse j

coords = {
    "class_true": np.arange(J),
    "class_reported": np.arange(J),
    "rater": np.arange(K)
}

with pm.Model(coords=coords) as model:
    pi = pm.Dirichlet("pi", a=np.ones(J), dims=("class_true",))

    # Dirichlet-Priors für jede Rater- & true-class-Zeile
    # a kann diagonal-betont werden, z.B. a = eye*alpha_diag + (1-eye)*alpha_off
    a_base = np.ones((K, J, J))
    # Option: leicht diagonal-dominant für Stabilisierung (anpassbar)
    a_base *= 1.0
    for k in range(K):
        for j in range(J):
            a_base[k, j, j] = 2.0  # Diagonale etwas begünstigen

    theta = pm.Dirichlet(
        "theta",
        a=a_base,
        shape=(K, J, J),
        dims=("rater", "class_true", "class_reported")
    )

    # Likelihood: marginalisiere über z (wahre Klasse)
    # Für jeden Fall n: log p(y_n | pi, theta) = log_sum_j [ log pi_j + sum_k log p(y_nk | z=j) ]
    # Missing Y[n,k] = -1 wird ignoriert
    y_shared = pm.Data("y", Y, mutable=False)

    lp_terms = []
    for n in range(N):
        lp_j = pt.log(pi)  # shape (J,)
        for k in range(K):
            ynk = y_shared[n, k]
            # Wenn missing -> nichts addieren
            lp_j = pt.switch(
                pt.eq(ynk, -1),
                lp_j,
                lp_j + pt.log(theta[k, :, ynk])
            )
        # log-sum-exp über Klassen
        lp_terms.append(pt.logsumexp(lp_j))

    total_loglike = pt.sum(pt.stack(lp_terms))
    pm.Potential("likelihood", total_loglike)

    # Sampling
    idata = pm.sample(
        draws=1000,
        tune=1000,
        chains=4,
        target_accept=0.9,
        random_seed=42
    )

# ---------------------------
# 3) Posterior-Auswertung
# ---------------------------
# Posterior summaries für pi
pi_summary = az.summary(idata, var_names=["pi"], hdi_prob=0.95)
pi_df = pi_summary.reset_index().rename(columns={"index": "param"})
# Mapping zurück zu Original-Labels
pi_df["class_true"] = pi_df["param"].str.extract(r"\[(\d+)\]").astype(int)
pi_df["class_label"] = pi_df["class_true"].map(int_to_label)
pi_df = pi_df[["class_true", "class_label", "mean", "hdi_2.5%", "hdi_97.5%"]]
pi_df.to_csv("pi_posterior.csv", index=False)

# Posterior summaries für theta (je Rater Konfusionsmatrix)
for k in range(K):
    # theta[k,:,:] → (class_true, class_reported)
    th = idata.posterior["theta"].sel(rater=k).stack(draws=("chain", "draw"))
    mean_mat = th.mean(dim="draws").to_dataframe().reset_index()
    # mean für jede (true, reported)
    mean_pivot = mean_mat.pivot(index="class_true", columns="class_reported", values="theta")
    # Labelachsen benennen
    mean_pivot.index = [int_to_label[i] for i in mean_pivot.index]
    mean_pivot.columns = [int_to_label[i] for i in mean_pivot.columns]
    mean_pivot.to_csv(f"theta_rater{k+1}_posterior.csv")

# p(z_i = j | Daten): Posterior-Mittel (über Draws) per Item
# Formel: proportional zu exp( log pi_j + sum_k log theta[k, j, y_nk] ), dann normalisieren
# Wir benutzen Posterior-Mittelwerte von pi/theta als schnelle Näherung; alternativ über alle Draws mitteln.
pi_mean = idata.posterior["pi"].stack(draws=("chain", "draw")).mean(dim="draws").values  # (J,)
theta_mean = idata.posterior["theta"].stack(draws=("chain", "draw")).mean(dim="draws").values  # (K,J,J)

post_item = np.zeros((N, J))
for n in range(N):
    logp = np.log(pi_mean.copy())
    for k in range(K):
        ynk = Y[n, k]
        if ynk != -1:
            logp += np.log(theta_mean[k, :, ynk])
    # softmax
    m = logp.max()
    p = np.exp(logp - m)
    post_item[n, :] = p / p.sum()

post_df = pd.DataFrame(post_item, columns=[f"p_true_{int_to_label[j]}" for j in range(J)])
post_df.insert(0, "item_id", df["item_id"])
post_df.to_csv("item_posteriors.csv", index=False)

print("Fertig. Dateien geschrieben:")
print(" - pi_posterior.csv")
print(" - theta_rater1_posterior.csv, theta_rater2_posterior.csv")
print(" - item_posteriors.csv")