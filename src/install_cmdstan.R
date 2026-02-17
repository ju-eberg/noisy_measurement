install.packages("pak")
pak::pak("stan-dev/cmdstanr")
library(cmdstanr)
check_cmdstan_toolchain()
#install_cmdstan()

set_cmdstan_path("C:/Users/jeroe/.cmdstan/cmdstan-2.37.0") # adjust path # 2.36 also possible
cmdstan_path()   
cmdstan_version() 
