## ----echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------
# Load project configuration
source(here::here("config.R"), local = TRUE)

setwd(PROJECT_ROOT)

library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cowplot)

knitr::opts_knit$set(root.dir = PROJECT_ROOT)
dir.create(file.path(PROJECT_ROOT, "img"), recursive = TRUE, showWarnings = FALSE)
knitr::opts_chunk$set(fig.path = "img/")

source(file.path(PROJECT_ROOT, "load_data_main.R"), local = TRUE)

out_dir <- file.path(PROJECT_ROOT, "img/data_descriptive_plots")

# Keep only variables needed for coding comparisons (tool, two raters, and Infas references)
data_basis <- resultOverviewCompact %>%
  dplyr::select(
    kldb_tool_vorlaufig,
    kldb_tool_mit_anfniveau,
    res1_kldb,
    res2_kldb,
    isco_tool_vorlaufig,
    isco_tool_mit_anfniveau,
    res1_isco,
    res2_isco,
    kldb_infas_num,
    kldb_infas_str,
    isco_infas_num,
    isco_infas_str
  ) %>%
  as.data.table()

# Treat code fields as text (flags and multiple codes can occur)
code_cols <- c(
  "kldb_tool_mit_anfniveau","isco_tool_mit_anfniveau",
  "kldb_tool_vorlaufig","isco_tool_vorlaufig",
  "res1_kldb","res2_kldb",
  "res1_isco","res2_isco"
)
data_basis[, (code_cols) := lapply(.SD, as.character), .SDcols = code_cols]

# Replace tool flags related to requirement level with the preliminary tool code
flag_anfn_not_nec <- "Anforderungsniveau nicht erforderlich / Default Code übernehmen"
flag_anfn_not_avl <- "Code liegt für gewähltes AnfNiveau nicht vor / manuell prüfen"

# Tool requirement-level flags:
# For KldB we fall back to the preliminary tool code for both flag types.
data_basis[trimws(kldb_tool_mit_anfniveau) %in% c(flag_anfn_not_nec, flag_anfn_not_avl),
           kldb_tool_mit_anfniveau := kldb_tool_vorlaufig]

# For ISCO we only fall back when the requirement level is not needed.
# If no ISCO code exists for the chosen requirement level, keep the flag (and it becomes NA later),
# because ISCO is more tightly linked to qualification/skill level.
data_basis[trimws(isco_tool_mit_anfniveau) == flag_anfn_not_nec,
           isco_tool_mit_anfniveau := isco_tool_vorlaufig]


# Extract a single major group (1st digit) per field
# If multiple candidate codes exist, pick one at random among valid codes (seeded for reproducibility)
random_1st_digit <- function(x, target_length) {
  sapply(as.character(x), function(s) {
    if (is.na(s) || trimws(s) == "") return(NA_character_)

    parts <- unlist(strsplit(s, "\\s*,\\s*|\\s*;\\s*"))
    parts <- trimws(parts)
    parts <- parts[parts != ""]

    parts <- grep("^-?[0-9]+$", parts, value = TRUE)
    parts <- parts[!grepl("^-", parts)]

    if (length(parts) == 0) return(NA_character_)

    parts <- vapply(parts, function(p) {
      if (nchar(p) == target_length - 1) paste0("0", p) else p
    }, FUN.VALUE = character(1))

    valid_codes <- parts[nchar(parts) == target_length]
    if (length(valid_codes) == 0) return(NA_character_)

    chosen_code <- sample(valid_codes, 1)
    substr(chosen_code, 1, 1)
  }, USE.NAMES = FALSE)
}

# Major groups for KldB (5-digit codes)
set.seed(123)
data_basis[, c("kldb_tool_1st", "res1_kldb_1st", "res2_kldb_1st") :=
             lapply(.SD, random_1st_digit, target_length = 5),
           .SDcols = c("kldb_tool_mit_anfniveau", "res1_kldb", "res2_kldb")]

# Major groups for ISCO (4-digit codes)
set.seed(456)
data_basis[, c("isco_tool_1st", "res1_isco_1st", "res2_isco_1st") :=
             lapply(.SD, random_1st_digit, target_length = 4),
           .SDcols = c("isco_tool_mit_anfniveau", "res1_isco", "res2_isco")]

# Standardise missing values and drop rows with no information in any selected field
all_cols <- names(data_basis)
data_basis[, (all_cols) := lapply(.SD, as.character), .SDcols = all_cols]

for (col in names(data_basis)) {
  data_basis[get(col) == "" | get(col) == "NA", (col) := NA_character_]
}

char_cols <- names(data_basis)[sapply(data_basis, is.character)]
data_basis[, (char_cols) := lapply(.SD, function(x) {
  x <- trimws(x)
  ifelse(x == "" | x == "NA", NA_character_, x)
}), .SDcols = char_cols]

data_basis <- data_basis[rowSums(!is.na(data_basis)) > 0]

# Add stable identifier for downstream linking
data_basis$item_id <- seq_len(nrow(data_basis))

# Color scheme: dark blue for matches, dark red for mismatches
agreement_palette <- c("match" = "darkblue", "mismatch" = "darkred")

# Plot distributions of major groups by source (KldB)
kldb_long <- data_basis %>%
  dplyr::select(kldb_tool_1st, res1_kldb_1st, res2_kldb_1st) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "major_group") %>%
  mutate(
    major_group = ifelse(is.na(major_group), "NA", major_group),
    source = factor(source,
                    levels = c("res1_kldb_1st", "res2_kldb_1st", "kldb_tool_1st"),
                    labels = c("Human rater 1", "Human rater 2", "Tool"))
  )

p_kldb_dist <- ggplot(kldb_long, aes(x = major_group, fill = source)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Human rater 1" = "#d95f02", "Human rater 2" = "#7570b3", "Tool" = "#1b9e77")) +
  labs(
    title = "",
    x = "KldB major group (first digit)",
    y = "Number of observations",
    fill = "Rater"
  ) +
  theme_light(base_size = 13, base_family = "Times")

ggsave(file.path(out_dir, "kldb_major_group_distribution.png"), p_kldb_dist, width = 9, height = 5, dpi = 600)
print(p_kldb_dist)

# Plot distributions of major groups by source (ISCO)
isco_long <- data_basis %>%
  dplyr::select(isco_tool_1st, res1_isco_1st, res2_isco_1st) %>%
  pivot_longer(cols = everything(), names_to = "source", values_to = "major_group") %>%
  mutate(
    major_group = ifelse(is.na(major_group), "NA", major_group),
    source = factor(source,
                    levels = c("res1_isco_1st", "res2_isco_1st", "isco_tool_1st"),
                    labels = c("Human rater 1", "Human rater 2", "Tool"))
  )

p_isco_dist <- ggplot(isco_long, aes(x = major_group, fill = source)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("Human rater 1" = "#d95f02", "Human rater 2" = "#7570b3", "Tool" = "#1b9e77")) +
  labs(
    title = "",
    x = "ISCO major group (first digit)",
    y = "Number of observations",
    fill = "Rater" 
  ) +
  theme_light(base_size = 13, base_family = "Times") 
ggsave(file.path(out_dir, "isco_major_group_distribution.png"), p_isco_dist, width = 9, height = 5, dpi = 600)
print(p_isco_dist)
# Helper to compute pairwise agreement (counts + mismatch percentage), using complete cases for that pair
pair_agreement <- function(df, a, b, label) {
  df %>%
    filter(!is.na(.data[[a]]), !is.na(.data[[b]])) %>%
    summarise(
      match = sum(.data[[a]] == .data[[b]]),
      mismatch = sum(.data[[a]] != .data[[b]]),
      total = n()
    ) %>%
    mutate(
      mismatch_pct = ifelse(total > 0, 100 * mismatch / total, NA_real_),
      pair = label
    )
}

# Pairwise agreement for KldB
kldb_pairs <- bind_rows(
  pair_agreement(data_basis, "kldb_tool_1st", "res1_kldb_1st", "Tool vs Human 1"),
  pair_agreement(data_basis, "kldb_tool_1st", "res2_kldb_1st", "Tool vs Human 2"),
  pair_agreement(data_basis, "res1_kldb_1st", "res2_kldb_1st", "Human 1 vs Human 2")
)


kldb_pair_labels <- kldb_pairs %>%
  mutate(
    match_pct = 100 - mismatch_pct,
    label_match = paste0(round(match_pct, 1), "% match"),
    label_mismatch = paste0(round(mismatch_pct, 1), "% mismatch"),
    y_match = mismatch + match / 2,
    y_mismatch = dplyr::case_when(
      mismatch == 0 ~ NA_real_,
      mismatch / total < 0.05 ~ 0.9 * mismatch,  # tiny red: place near top of red
      TRUE ~ mismatch / 2                        # otherwise: center of red
    )
  ) %>%
  dplyr::select(pair, y_match, y_mismatch, label_match, label_mismatch) %>%
  tidyr::pivot_longer(
    cols = c(y_match, y_mismatch, label_match, label_mismatch),
    names_to = c(".value", "status"),
    names_pattern = "(y|label)_(match|mismatch)"
  ) %>%
  mutate(status = factor(status, levels = c("mismatch", "match"))) %>%
  filter(!is.na(y))

# offset for one label to avoid overlap
kldb_pair_labels <- kldb_pair_labels %>%
  mutate(
    y = ifelse(pair == "Human 1 vs Human 2" & status == "mismatch", y + 40, y),
    pair_label = case_when(
      pair == "Tool vs Human 1" ~ "Tool vs Human 1",
      pair == "Tool vs Human 2" ~ "Tool vs Human 2",
      pair == "Human 1 vs Human 2" ~ "Human 1 vs Human 2",
      TRUE ~ pair
    )
  )


kldb_pairs_long <- kldb_pairs %>%
  dplyr::select(pair, match, mismatch) %>%
  pivot_longer(cols = c(match, mismatch), names_to = "status", values_to = "count") %>%
  mutate(
    status = factor(status, levels = c("mismatch", "match")),
    pair_label = case_when(
      pair == "Tool vs Human 1" ~ "Tool vs Human 1",
      pair == "Tool vs Human 2" ~ "Tool vs Human 2",
      pair == "Human 1 vs Human 2" ~ "Human 1 vs Human 2",
      TRUE ~ pair
    )
  )

# Prepare percentage data for pairwise
kldb_pairs_pct <- kldb_pairs_long %>%
  left_join(kldb_pairs %>% select(pair, total), by = "pair") %>%
  group_by(pair) %>%
  mutate(
    pct = 100 * count / total,
    pair_label_with_n = case_when(
      pair == "Tool vs Human 1" ~ paste0("Tool vs Human 1\n(n=", total[1], ")"),
      pair == "Tool vs Human 2" ~ paste0("Tool vs Human 2\n(n=", total[1], ")"),
      pair == "Human 1 vs Human 2" ~ paste0("Human 1 vs Human 2\n(n=", total[1], ")"),
      TRUE ~ paste0(pair, "\n(n=", total[1], ")")
    ),
    pair_label = case_when(
      pair == "Tool vs Human 1" ~ "Tool vs Human 1",
      pair == "Tool vs Human 2" ~ "Tool vs Human 2",
      pair == "Human 1 vs Human 2" ~ "Human 1 vs Human 2",
      TRUE ~ pair
    )
  ) %>%
  ungroup()

p_kldb_pairs <- ggplot(kldb_pairs_long, aes(x = pair_label, y = count, fill = status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  geom_text(
    data = kldb_pair_labels,
    aes(x = pair_label, y = y, label = label),
    inherit.aes = FALSE,
    size = 3,
    color = "white"
  ) +
  scale_fill_manual(values = agreement_palette, breaks = c("match", "mismatch")) +
  labs(
    title = "KldB: Pairwise agreement on the first digit",
    x = NULL,
    y = "Number of observations",
    fill = NULL
  ) +
  theme_light(base_size = 13, base_family = "Times")

# Percentage version for combined plot
p_kldb_pairs_pct <- ggplot(kldb_pairs_pct, aes(x = pair_label_with_n, y = pct, fill = status)) +
  geom_col(position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = agreement_palette, breaks = c("match", "mismatch")) +
  labs(
    title = "KldB: Pairwise agreement",
    x = NULL,
    y = "Percentage",
    fill = NULL
  ) +
  theme_light(base_size = 11, base_family = "Times") +
  theme(legend.position = "none", axis.text.x = element_text(size = 12)) +
  ylim(0, 100)

ggsave(file.path(out_dir, "kldb_pairwise_agreement.png"), p_kldb_pairs, width = 9, height = 5, dpi = 600)
print(p_kldb_pairs)
# Three-way agreement for KldB (complete cases across all three sources)
kldb_tri <- data_basis %>%
  filter(!is.na(kldb_tool_1st), !is.na(res1_kldb_1st), !is.na(res2_kldb_1st)) %>%
  transmute(all_three_match = (kldb_tool_1st == res1_kldb_1st) & (res1_kldb_1st == res2_kldb_1st)) %>%
  summarise(
    match = sum(all_three_match),
    mismatch = sum(!all_three_match),
    total = n()
  ) %>%
  mutate(mismatch_pct = ifelse(total > 0, 100 * mismatch / total, NA_real_))

kldb_tri_long <- kldb_tri %>%
  pivot_longer(cols = c(match, mismatch), names_to = "status", values_to = "count") %>%
  mutate(
  status = factor(status, levels = c("match", "mismatch")),
  status_label = factor(ifelse(status == "match", "Match", "Mismatch"), levels = c("Match", "Mismatch")),
  match_pct = 100 - mismatch_pct,
  label = ifelse(
    status == "match",
    paste0(round(match_pct, 1), "% match"),
    paste0(round(mismatch_pct, 1), "% mismatch")
  ),
  pct = ifelse(status == "match", match_pct, mismatch_pct)
)

p_kldb_tri <- ggplot(kldb_tri_long, aes(x = status_label, y = count, fill = status)) +
  geom_col() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = agreement_palette) +
  labs(
    title = "KldB: Agreement across all three sources on the first digit",
    x = NULL,
    y = "Number of observations",
    fill = NULL
  ) +
  theme_light(base_size = 13, base_family = "Times")

# Percentage version for combined plot
kldb_tri_n <- kldb_tri$total[1]
p_kldb_tri_pct <- ggplot(kldb_tri_long, aes(x = status_label, y = pct, fill = status)) +
  geom_col() +
  scale_fill_manual(values = agreement_palette) +
  labs(
    title = paste0("KldB: Three-way agreement (n=", kldb_tri_n, ")"),
    x = NULL,
    y = "Percentage",
    fill = NULL
  ) +
  theme_light(base_size = 11, base_family = "Times") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 12)) +
  ylim(0, 100)

ggsave(file.path(out_dir, "kldb_three_way_agreement.png"), p_kldb_tri, width = 7, height = 5, dpi = 600)
print(p_kldb_tri)
# Pairwise agreement for ISCO
isco_pairs <- bind_rows(
  pair_agreement(data_basis, "isco_tool_1st", "res1_isco_1st", "Tool vs Human 1"),
  pair_agreement(data_basis, "isco_tool_1st", "res2_isco_1st", "Tool vs Human 2"),
  pair_agreement(data_basis, "res1_isco_1st", "res2_isco_1st", "Human 1 vs Human 2")
)

isco_pairs_long <- isco_pairs %>%
  dplyr::select(pair, match, mismatch, total, mismatch_pct) %>%
  pivot_longer(cols = c(match, mismatch), names_to = "status", values_to = "count") %>%
  mutate(
  status = factor(status, levels = c("match", "mismatch")),
  match_pct = 100 - mismatch_pct,
  label = ifelse(
    status == "match",
    paste0(round(match_pct, 1), "% match"),
    paste0(round(mismatch_pct, 1), "% mismatch")
  ),
  pair_label = case_when(
    pair == "Tool vs Human 1" ~ "Tool vs Human 1",
    pair == "Tool vs Human 2" ~ "Tool vs Human 2",
    pair == "Human 1 vs Human 2" ~ "Human 1 vs Human 2",
    TRUE ~ pair
  )
)

# Prepare percentage data for pairwise
isco_pairs_pct <- isco_pairs_long %>%
  group_by(pair) %>%
  mutate(
    pct = 100 * count / total,
    pair_label_with_n = case_when(
      pair == "Tool vs Human 1" ~ paste0("Tool vs Human 1\n(n=", total[1], ")"),
      pair == "Tool vs Human 2" ~ paste0("Tool vs Human 2\n(n=", total[1], ")"),
      pair == "Human 1 vs Human 2" ~ paste0("Human 1 vs Human 2\n(n=", total[1], ")"),
      TRUE ~ paste0(pair, "\n(n=", total[1], ")")
    ),
    pair_label = case_when(
      pair == "Tool vs Human 1" ~ "Tool vs Human 1",
      pair == "Tool vs Human 2" ~ "Tool vs Human 2",
      pair == "Human 1 vs Human 2" ~ "Human 1 vs Human 2",
      TRUE ~ pair
    )
  ) %>%
  ungroup()

p_isco_pairs <- ggplot(isco_pairs_long, aes(x = pair_label, y = count, fill = status)) +
  geom_col(position = "stack") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white") +
  scale_fill_manual(values = agreement_palette) +
  labs(
    title = "ISCO: Pairwise agreement on the first digit",
    x = NULL,
    y = "Number of observations",
    fill = NULL
  ) +
  theme_light(base_size = 13, base_family = "Times")

# Percentage version for combined plot
p_isco_pairs_pct <- ggplot(isco_pairs_pct, aes(x = pair_label_with_n, y = pct, fill = status)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = agreement_palette) +
  labs(
    title = "ISCO: Pairwise agreement",
    x = NULL,
    y = "Percentage",
    fill = NULL
  ) +
  theme_light(base_size = 11, base_family = "Times") +
  theme(legend.position = "none", axis.text.x = element_text(size = 12)) +
  ylim(0, 100)

ggsave(file.path(out_dir, "isco_pairwise_agreement.png"), p_isco_pairs, width = 9, height = 5, dpi = 600)
print(p_isco_pairs)
# Three-way agreement for ISCO (complete cases across all three sources)
isco_tri <- data_basis %>%
  filter(!is.na(isco_tool_1st), !is.na(res1_isco_1st), !is.na(res2_isco_1st)) %>%
  transmute(all_three_match = (isco_tool_1st == res1_isco_1st) & (res1_isco_1st == res2_isco_1st)) %>%
  summarise(
    match = sum(all_three_match),
    mismatch = sum(!all_three_match),
    total = n()
  ) %>%
  mutate(mismatch_pct = ifelse(total > 0, 100 * mismatch / total, NA_real_))

isco_tri_long <- isco_tri %>%
  pivot_longer(cols = c(match, mismatch), names_to = "status", values_to = "count") %>%
  mutate(
    status = factor(status, levels = c("match", "mismatch")),
    status_label = factor(ifelse(status == "match", "Match", "Mismatch"), levels = c("Match", "Mismatch")),
    match_pct = 100 - mismatch_pct,
    label = ifelse(
      status == "match",
      paste0(round(match_pct, 1), "% match"),
      paste0(round(mismatch_pct, 1), "% mismatch")
    ),
    pct = ifelse(status == "match", match_pct, mismatch_pct)
  )

p_isco_tri <- ggplot(isco_tri_long, aes(x = status_label, y = count, fill = status)) +
  geom_col() +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white") +
  scale_fill_manual(values = agreement_palette, breaks = c("match", "mismatch")) +
  labs(
    title = "ISCO: Agreement across all three sources on the first digit",
    x = NULL, y = "Number of observations", fill = NULL
  ) +
  theme_light(base_size = 13, base_family = "Times")

# Percentage version for combined plot
isco_tri_n <- isco_tri$total[1]
p_isco_tri_pct <- ggplot(isco_tri_long, aes(x = status_label, y = pct, fill = status)) +
  geom_col() +
  scale_fill_manual(values = agreement_palette, breaks = c("match", "mismatch")) +
  labs(
    title = paste0("ISCO: Three-way agreement (n=", isco_tri_n, ")"),
    x = NULL, 
    y = "Percentage", 
    fill = NULL
  ) +
  theme_light(base_size = 11, base_family = "Times") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 14),
        axis.text.x = element_text(size = 12)) +
  ylim(0, 100)

ggsave(file.path(out_dir, "isco_three_way_agreement.png"), p_isco_tri, width = 7, height = 5, dpi = 600)
print(p_isco_tri)
# Combined plot with all four plots
# Create legend plot (only once, positioned on the right)
legend_data <- data.frame(
  x = 1:2, 
  y = 1:2, 
  status = factor(c("match", "mismatch"), levels = c("match", "mismatch"))
)

legend_plot <- ggplot(legend_data, aes(x = x, y = y, fill = status)) +
  geom_col() +
  scale_fill_manual(values = agreement_palette, breaks = c("match", "mismatch"),
                    labels = c("Match", "Mismatch"), name = NULL) +
  theme_void() +
  theme(legend.position = "right",
        legend.text = element_text(size = 11, family = "Times"),
        legend.key.size = unit(0.8, "cm"))

# Extract legend
legend_grob <- cowplot::get_legend(legend_plot)

# Create combined plot
# Top row: ISCO (left: pairwise, right: three-way)
# Bottom row: KLDB (left: pairwise, right: three-way)
p_combined <- cowplot::plot_grid(
  p_isco_pairs_pct,
  p_isco_tri_pct,
  p_kldb_pairs_pct,
  p_kldb_tri_pct,
  ncol = 2,
  nrow = 2,
  align = "hv",
  axis = "tblr"
)

# Add legend on the right side
p_combined_with_legend <- cowplot::plot_grid(
  p_combined,
  legend_grob,
  ncol = 2,
  rel_widths = c(1, 0.15)
)

ggsave(file.path(out_dir, "combined_agreement_plots.png"), p_combined_with_legend, 
       width = 14, height = 10, dpi = 600)
print(p_combined_with_legend)

kldb_pairs
isco_pairs
kldb_tri
isco_tri
head(data_basis)



