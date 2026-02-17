# Noisy Measurement

This project implements a Bayesian noisy measurement model (based on Dawid & Skene, 1979) to estimate latent true classes and rater accuracy from conflicting occupational codes. The framework utilizes Stan to quantify classification uncertainty. It compares automated coding tools and human raters for both ISCO (International Standard Classification of Occupations) and KldB (Klassifikation der Berufe) classification systems.

## Getting Started

### Configuration

Before running any analysis, you need to configure the project path. Edit the file [`config.R`](config.R) in the root directory and set your project root path:

```r
PROJECT_ROOT <- "C:/Users/yourname/path/to/noisy_measurement"
```

On Mac/Linux, use:
```r
PROJECT_ROOT <- "/Users/yourname/path/to/noisy_measurement"
```

This path will be used throughout all R scripts and R Markdown files. The configuration file is automatically loaded by all analysis scripts.

**Note**: Data loading happens automatically when you run any analysis script. The scripts in [`src/`](src/) automatically load [`data_preparation.Rmd`](src/data_preparation.Rmd), which in turn loads all necessary data via [`load_data_main.R`](load_data_main.R). You do not need to manually load data before running analyses.

-----------------------------------

## Project Structure

### Folders

#### Core Directories

- **[`doc/`](doc/)**: Contains documentation, codebooks, and reference materials. See [`doc/README.md`](doc/README.md) for details.

- **[`img/`](img/)**: Contains generated plots and figures from the analyses.

- **[`src/`](src/)**: **Contains all Bayesian model analyses, Stan implementations of the Dawid-Skene model, and complete analysis workflows.** Includes R Markdown scripts for ISCO and KldB classification analyses, prior sensitivity tests, convergence diagnostics, and posterior summaries. Each analysis script has a corresponding PDF with full results. See [`src/README.md`](src/README.md) for detailed descriptions of all analyses.


#### Data and Supporting Directories

- **[`behavior-coding-data/`](behavior-coding-data/)**, **[`generated-data/`](generated-data/)**, **[`orig-data/`](orig-data/)**: Contain source data files (behavioral coding data, generated RData files, and original survey data in DTA/CSV format). These are source data that the project builds upon.

- **[`prog/`](prog/)**: Contains R scripts for loading data. These scripts are sourced by [`load_data_main.R`](load_data_main.R) to load various data sources.

-----------------------------------

## Computational Considerations

**Note**: The Bayesian analyses using Stan can take considerable time to complete. Pre-compiled PDF outputs are provided for all analyses to allow immediate access to results without running the computationally intensive models. See [`src/README.md`](src/README.md) for a detailed overview of all analysis scripts.

## Interactive Analysis with Shinystan

Some analysis scripts include code to launch Shinystan for interactive exploration of MCMC diagnostics. **Important**: Shinystan is intentionally restricted to interactive R sessions only. It will not launch when knitting R Markdown documents, as this would block the document compilation process. This behavior is by design to ensure smooth PDF generation.

## Dependencies

The project requires:
- R (with packages: data.table, dplyr, ggplot2, tidyr, here, cmdstanr, posterior, etc.)
- CmdStan (installed via `cmdstanr::install_cmdstan()`). See [`src/install_cmdstan.R`](src/install_cmdstan.R) in case of issues
- Stan models for Bayesian inference
- Shinystan (optional, for interactive MCMC diagnostics)

The `here` package is used for robust path handling across different working directories.

See individual R Markdown files for specific package requirements.
