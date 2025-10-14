import pandas as pd
import numpy as np
import pymc as pm
import arviz as az

# 1. Daten laden und bereinigen
# ==============================================================================
CSV = "../data_basis_mit_1st_stelle.csv"  # <-- Pfad zu deiner CSV

try:
    df = pd.read_csv(CSV)
except FileNotFoundError:
    print(f"Fehler: Die Datei {CSV} wurde nicht gefunden. Bitte Pfad prüfen.")
    # Exit, da die echten Daten benötigt werden
    exit()

# Erforderliche Spalten
RES_COLS = ["res1_isco_1st", "res2_isco_1st"]

# Fehlende Werte (NaN) entfernen, da das Dawid-Skene-Modell dies nicht direkt verarbeiten kann
df_clean = df.dropna(subset=RES_COLS).copy()

# Sicherstellen, dass die Klassifizierungen Ganzzahlen sind
try:
    df_clean[RES_COLS] = df_clean[RES_COLS].astype(int)
except ValueError:
    print("Fehler: Die Klassifizierungen enthalten Nicht-Zahlen, die nicht entfernt wurden. Breche ab.")
    exit()

# 2. Metriken definieren und in Triplet-Format umwandeln
# ==============================================================================
I = len(df_clean)  # Anzahl der Items (bereinigt)
J = 2  # Anzahl der Annotatoren
K = 10  # Anzahl der Klassen (0 bis 9)
N = I * J

print(f"I (Items, bereinigt): {I}, J (Annotatoren): {J}, K (Klassen): {K}")

# Item-IDs (ii): 0-basierte IDs für jedes Item, für jeden Annotator wiederholt
item_indices = df_clean["item_id"].factorize()[0]
ii = np.hstack([item_indices, item_indices])

# Annotator-IDs (jj): 0 für Annotator 1, 1 für Annotator 2
jj = np.hstack([np.zeros(I), np.ones(I)]).astype(int)

# Antworten (y): Beobachtete Labels, gestapelt
y = np.hstack([df_clean["res1_isco_1st"].values, df_clean["res2_isco_1st"].values]).astype(int)

# Initialisierungswert für die latenten wahren Labels (z_init)
# Hier wird einfach die Klassifizierung von Annotator 1 verwendet
z_init = df_clean["res1_isco_1st"].values.astype(int)
# ... (Ihr Code von Punkt 1 und 2 bleibt unverändert) ...

# 3. Das PyMC-Modell definieren
# ==============================================================================
# A-priori-Einstellungen
alpha = np.ones(K)
beta = np.ones((K, K)) + np.diag(np.ones(K))

if __name__ == '__main__':

    with pm.Model() as dawid_skene_model:
        # 1. Priors für kontinuierliche Variablen
        pi = pm.Dirichlet('pi', a=alpha, shape=K)
        theta = pm.Dirichlet('theta', a=beta, shape=(J, K, K))

        # 2. Latente Variable (wahre Labels)
        z = pm.Categorical('z', p=pi, shape=I, initval=z_init)

        # 3. Beobachtete Daten (Likelihood)
        y_obs = pm.Categorical(
            'y_obs',
            p=theta[jj, z[ii]],
            observed=y
        )

    # 4. MCMC-Inferenz durchführen
    # ==============================================================================
    N_SAMPLES = 4000
    N_TUNE = 2000

    print(f"\nStarte Sampling mit {N_TUNE} Tuning- und {N_SAMPLES} Produktions-Samples...")

    with dawid_skene_model:
        # Sampler für kontinuierliche Variablen
        step1 = pm.Metropolis(vars=[pi, theta])

        # Sampler für diskrete Variable (z)
        step2 = pm.CategoricalGibbsMetropolis(vars=[z])

        # Wenn BLAS-Warnung (wie oben) das Sampling verlangsamt, kann chains = 1 helfen.
        trace = pm.sample(
            N_SAMPLES,
            tune=N_TUNE,
            step=[step1, step2],
            progressbar=True,
            random_seed=42
        )

    # 5. Ergebnisse extrahieren
    # ==============================================================================
    print("\nSampling abgeschlossen. Extrahiere Ergebnisse...")

    z_samples = trace.posterior['z'].stack(sample=("chain", "draw")).values.T
    z_hat = np.zeros(I)

    for i in range(I):
        z_hat[i] = np.bincount(z_samples[i, :].astype(int)).argmax()

    # Fügen das geschätzte wahre Label zum DataFrame hinzu
    df_clean['z_hat_dawid_skene'] = z_hat.astype(int)

    # 6. Ergebnis-Überblick
    # ==============================================================================
    print("\n--- Geschätzte Wahre Labels (Erste 10 Items) ---")
    print(df_clean[["item_id", "res1_isco_1st", "res2_isco_1st", "z_hat_dawid_skene"]].head(10))

    print("\n--- Geschätzte Konfusionsmatrix für Annotator 0 (Auszug) ---")
    theta_0_posterior_mean = trace.posterior['theta'].mean(dim=("chain", "draw")).sel(theta_dim_0=0).values
    print(np.round(theta_0_posterior_mean, 2))
