# Project Configuration
# =====================
# Set your project root directory here.
# This path will be used throughout all R scripts and R Markdown files.
#
# Example for Windows:
# PROJECT_ROOT <- "C:/Users/yourname/path/to/noisy_measurement"
#
# Example for Mac/Linux:
# PROJECT_ROOT <- "/Users/yourname/path/to/noisy_measurement"

PROJECT_ROOT <- "C:/Users/jeroe/Uni_C/noisy_measurement"

# Verify that the path exists
if (!dir.exists(PROJECT_ROOT)) {
  warning(paste("PROJECT_ROOT does not exist:", PROJECT_ROOT))
}
