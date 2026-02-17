# Analysis Scripts

This directory contains all R Markdown analysis scripts and their compiled PDF outputs. Each `.Rmd` file has a corresponding `.pdf` file that contains the complete results and outputs from that analysis.

## Data Preparation

### [`data_preparation.Rmd`](data_preparation.Rmd) / [`data_preparation.pdf`](data_preparation.pdf)

Prepares and processes the raw data for analysis. This script:
- Loads data from various sources
- Filters and cleans the data
- Extracts major groups (first digit) from ISCO and KldB codes
- Creates descriptive plots showing distributions and agreement between raters
- Generates the `data_basis` dataset used by all other analysis scripts

**Note**: This script is automatically sourced by all other analysis scripts via `knitr::purl()`.

## ISCO Analyses

### [`isco_alpha_beta_parameters.Rmd`](isco_alpha_beta_parameters.Rmd) / [`isco_alpha_beta_parameters.pdf`](isco_alpha_beta_parameters.pdf)

Compares different prior parameter configurations (alpha and beta) for the Dawid-Skene model using ISCO classification. Analyzes four model configurations:
- `alpha11_beta1`: Weak priors (alpha = [1,1], beta = 1)
- `alpha11_beta5diag`: Weak alpha, stronger diagonal beta
- `alpha1510_beta1`: Stronger alpha priors, weak beta
- `alpha1510_beta5diag`: Stronger priors on both alpha and beta diagonal

### [`isco_alpha_beta_parameters_200300.Rmd`](isco_alpha_beta_parameters_200300.Rmd) / [`isco_alpha_beta_parameters_200300.pdf`](isco_alpha_beta_parameters_200300.pdf)

Similar to [`isco_alpha_beta_parameters.Rmd`](isco_alpha_beta_parameters.Rmd), but uses different alpha prior values (300 and 200 instead of 15 and 10) to test sensitivity to very strong priors.

### [`isco_w_priors.Rmd`](isco_w_priors.Rmd) / [`isco_w_priors.pdf`](isco_w_priors.pdf)

Analyzes ISCO classification, optionally with informative priors. Focuses on two-category analysis (categories 2 and 3) and includes detailed posterior analysis and convergence diagnostics.

### [`isco_three_categories.Rmd`](isco_three_categories.Rmd) / [`isco_three_categories.pdf`](isco_three_categories.pdf)

Extends the analysis to three ISCO categories instead of two. Includes comprehensive convergence diagnostics, label switching checks, and posterior summaries.

### [`model_basis_isco_all.Rmd`](model_basis_isco_all.Rmd) / [`model_basis_isco_all.pdf`](model_basis_isco_all.pdf)

Baseline model analysis for ISCO with convergence tests under different HMC (Hamiltonian Monte Carlo) hyperparameters. Tests various adapt_delta values and other sampling parameters to ensure robust inference.

## KldB Analyses

### [`kldb_alpha_beta_parameters.Rmd`](kldb_alpha_beta_parameters.Rmd) / [`kldb_alpha_beta_parameters.pdf`](kldb_alpha_beta_parameters.pdf)

Compares different prior parameter configurations for the Dawid-Skene model using KldB classification. Similar structure to the ISCO version but adapted for KldB's 5-digit coding system.

### [`kldb_w_priors.Rmd`](kldb_w_priors.Rmd) / [`kldb_w_priors.pdf`](kldb_w_priors.pdf)

Analyzes KldB classification, optionally with informative priors. Focuses on two-category analysis (categories 7 and 8) with detailed posterior analysis.

### [`model_basis_kldb_all.Rmd`](model_basis_kldb_all.Rmd) / [`model_basis_kldb_all.pdf`](model_basis_kldb_all.pdf)

Baseline model analysis for KldB with convergence tests under different HMC hyperparameters. Tests various adapt_delta values and sampling parameters.

## Stan Model Files

### [`dawid_skene_isco.stan`](dawid_skene_isco.stan)

Stan model definition for the Dawid-Skene model used in ISCO analyses.

## Utility Scripts

### [`install_cmdstan.R`](install_cmdstan.R)

Optional setup script for installing and configuring CmdStan. Generally not needed as CmdStan is automatically detected by `cmdstanr`, but can be useful if CmdStan is installed in a non-standard location.

## Running Analyses

To run any analysis:

1. Ensure [`config.R`](../config.R) is set correctly (see [main README](../README.md))
2. Open the desired `.Rmd` file in RStudio
3. Click "Knit" to generate the PDF, or run chunks interactively

All scripts automatically:
- Load the project configuration
- Source [`data_preparation.Rmd`](data_preparation.Rmd) to get the prepared data
- Set up necessary directories for output

**Note**: The Bayesian analyses using Stan can take considerable time to complete. Pre-compiled PDF outputs are provided for all analyses. 

Output files (plots, results) are saved in the [`img/`](../img/) directory with subdirectories organized by analysis type.
