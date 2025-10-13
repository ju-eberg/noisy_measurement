library(dplyr)
sourcw("load_data_main.R")

# neue Datenbasis mit den rot markierten Spalten
data_basis <- resultOverviewCompact %>%
  select(
    kldb_tool_mit_anfniveau,
    isco_tool_mit_anfniveau,
    kldb_tool_vorlaufig,
    isco_tool_vorlaufig,
    kldb_infas_num,
    isco_infas_num,
    kldb_infas_str,
    isco_infas_str,
    res1_kldb,
    res2_kldb,
    res3_kldb,
    res1_isco,
    res2_isco,
    res3_isco
  )

head(data_basis)
typeof(data_basis)

library(data.table)
setDT(data_basis)

flag <- "Anforderungsniveau nicht erforderlich / Default Code übernehmen"

# robust gegen führende/nachgestellte Leerzeichen:
data_basis[trimws(kldb_tool_mit_anfniveau) == flag, 
           kldb_tool_mit_anfniveau := kldb_tool_vorlaufig]

data_basis[trimws(isco_tool_mit_anfniveau) == flag, 
           isco_tool_mit_anfniveau := isco_tool_vorlaufig]

data_basis[, c("kldb_tool_1st", "res1_kldb_1st", "res2_kldb_1st") :=
             lapply(.SD, function(x) as.numeric(substr(x, 1, 1))),
           .SDcols = c("kldb_tool_mit_anfniveau", "res1_kldb", "res2_kldb")]

data_basis[, c("isco_tool_1st", "res1_isco_1st", "res2_isco_1st") :=
             lapply(.SD, function(x) as.numeric(substr(x, 1, 1))),
           .SDcols = c("isco_tool_mit_anfniveau", "res1_isco", "res2_isco")]

# TODO: NAs
# TODO: Anforderungsniveaus nicht mögliche
# TODO: Doppelte Werte


library(dplyr)
library(ggplot2)
library(tidyr)

# Datensatz in lange Form bringen
plot_data <- data_basis %>%
  select(kldb_tool_1st, res1_kldb_1st, res2_kldb_1st) %>%
  pivot_longer(cols = everything(),
               names_to = "Quelle",
               values_to = "Hauptgruppe") %>%
  mutate(Hauptgruppe = ifelse(is.na(Hauptgruppe), "NA", Hauptgruppe))

# Balkendiagramm
ggplot(plot_data, aes(x = Hauptgruppe, fill = Quelle)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                    labels = c("KldB Tool", "Manuell 1", "Manuell 2")) +
  labs(title = "Verteilung der KldB-Hauptgruppen (1. Ziffer)",
       x = "KldB-Hauptgruppe (1. Stelle des Codes)",
       y = "Anzahl Fälle",
       fill = "Quelle") +
  theme_minimal(base_size = 13)

# Datensatz in lange Form bringen
plot_data <- data_basis %>%
  select(isco_tool_1st, res1_isco_1st, res2_isco_1st) %>%
  pivot_longer(cols = everything(),
               names_to = "Quelle_isco",
               values_to = "Hauptgruppe_isco") %>%
  mutate(Hauptgruppe = ifelse(is.na(Hauptgruppe_isco), "NA", Hauptgruppe_isco))

# Balkendiagramm isco
ggplot(plot_data, aes(x = Hauptgruppe_isco, fill = Quelle_isco)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("#1b9e77", "#d95f02", "#7570b3"),
                    labels = c("ISCO Tool", "Manuell 1", "Manuell 2")) +
  labs(title = "Verteilung der ISCO-Hauptgruppen (1. Ziffer)",
       x = "ISCO-Hauptgruppe (1. Stelle des Codes)",
       y = "Anzahl Fälle",
       fill = "Quelle") +
  theme_minimal(base_size = 13)



library(dplyr)
library(tidyr)
library(ggplot2)

# ===============================
# 1) Paarweise Vergleiche
# ===============================
pair_counts <- bind_rows(
  data_basis %>%
    filter(!is.na(kldb_tool_1st), !is.na(res1_kldb_1st)) %>%
    summarise(gleich = sum(kldb_tool_1st == res1_kldb_1st),
              ungleich = sum(kldb_tool_1st != res1_kldb_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Tool vs Manuell 1"),
  data_basis %>%
    filter(!is.na(kldb_tool_1st), !is.na(res2_kldb_1st)) %>%
    summarise(gleich = sum(kldb_tool_1st == res2_kldb_1st),
              ungleich = sum(kldb_tool_1st != res2_kldb_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Tool vs Manuell 2"),
  data_basis %>%
    filter(!is.na(res1_kldb_1st), !is.na(res2_kldb_1st)) %>%
    summarise(gleich = sum(res1_kldb_1st == res2_kldb_1st),
              ungleich = sum(res1_kldb_1st != res2_kldb_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Manuell 1 vs Manuell 2")
)

ggplot(pair_counts, aes(x = Paar, y = Anzahl, fill = Status)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("gleich" = "#4CAF50", "ungleich" = "#F44336")) +
  labs(title = "Paarweise Übereinstimmung der 1. Ziffer",
       x = NULL, y = "Anzahl Fälle", fill = NULL) +
  theme_minimal(base_size = 13)

# Tipp: Falls du NA als 'Ungleich' zählen willst, entferne die filter() oben
# und definiere Status über case_when(is.na(a)|is.na(b) ~ "ungleich", a==b ~ "gleich", TRUE ~ "ungleich").

# ===============================
# 2) Alle drei zusammen
# ===============================
tri_counts <- data_basis %>%
  filter(!is.na(kldb_tool_1st), !is.na(res1_kldb_1st), !is.na(res2_kldb_1st)) %>%
  transmute(alle_drei_gleich = kldb_tool_1st == res1_kldb_1st & res1_kldb_1st == res2_kldb_1st) %>%
  summarise(gleich = sum(alle_drei_gleich),
            ungleich = sum(!alle_drei_gleich)) %>%
  pivot_longer(everything(), names_to = "Status", values_to = "Anzahl")

ggplot(tri_counts, aes(x = Status, y = Anzahl, fill = Status)) +
  geom_col() +
  scale_fill_manual(values = c("gleich" = "#4CAF50", "ungleich" = "#F44336")) +
  labs(title = "Übereinstimmung der 1. Ziffer über alle drei Quellen",
       x = NULL, y = "Anzahl Fälle", fill = NULL) +
  theme_minimal(base_size = 13)


library(dplyr)
library(tidyr)
library(ggplot2)

## ===============================
## 1) Paarweise Vergleiche (ISCO)
## ===============================
pair_counts_isco <- bind_rows(
  data_basis %>%
    filter(!is.na(isco_tool_1st), !is.na(res1_isco_1st)) %>%
    summarise(gleich = sum(isco_tool_1st == res1_isco_1st),
              ungleich = sum(isco_tool_1st != res1_isco_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Tool vs Manuell 1"),
  data_basis %>%
    filter(!is.na(isco_tool_1st), !is.na(res2_isco_1st)) %>%
    summarise(gleich = sum(isco_tool_1st == res2_isco_1st),
              ungleich = sum(isco_tool_1st != res2_isco_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Tool vs Manuell 2"),
  data_basis %>%
    filter(!is.na(res1_isco_1st), !is.na(res2_isco_1st)) %>%
    summarise(gleich = sum(res1_isco_1st == res2_isco_1st),
              ungleich = sum(res1_isco_1st != res2_isco_1st)) %>%
    pivot_longer(everything(), names_to = "Status", values_to = "Anzahl") %>%
    mutate(Paar = "Manuell 1 vs Manuell 2")
)

ggplot(pair_counts_isco, aes(x = Paar, y = Anzahl, fill = Status)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("gleich" = "#4CAF50", "ungleich" = "#F44336")) +
  labs(title = "Paarweise Übereinstimmung der 1. Ziffer (ISCO)",
       x = NULL, y = "Anzahl Fälle", fill = NULL) +
  theme_minimal(base_size = 13)

## ===============================
## 2) Alle drei zusammen (ISCO)
## ===============================
tri_counts_isco <- data_basis %>%
  filter(!is.na(isco_tool_1st), !is.na(res1_isco_1st), !is.na(res2_isco_1st)) %>%
  transmute(alle_drei_gleich = isco_tool_1st == res1_isco_1st & res1_isco_1st == res2_isco_1st) %>%
  summarise(gleich = sum(alle_drei_gleich),
            ungleich = sum(!alle_drei_gleich)) %>%
  pivot_longer(everything(), names_to = "Status", values_to = "Anzahl")

ggplot(tri_counts_isco, aes(x = Status, y = Anzahl, fill = Status)) +
  geom_col() +
  scale_fill_manual(values = c("gleich" = "#4CAF50", "ungleich" = "#F44336")) +
  labs(title = "Übereinstimmung der 1. Ziffer über alle drei Quellen (ISCO)",
       x = NULL, y = "Anzahl Fälle", fill = NULL) +
  theme_minimal(base_size = 13)

