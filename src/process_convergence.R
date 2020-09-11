# Convergence compilations

# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer

# Disinfection:  0
# Prevalence: Medium


# setup -------------------------------------------------------------------
# external libraries
 library(tidyverse) 

# read and process data  --------------------------------------------------

infile1 = "data/processed/compilation_risk.csv" # Compilation of all risks for 10,000 simulations
infile2 = "data/processed/convergence_risk.csv" # Risk for baseline scenario for 100,000 simulations
infile3 = "data/processed/convergence_risk_20.csv" # Risk for baseline scenario for 20,000 simulations
infile4 = "data/processed/convergence_risk_50.csv" # Risk for baseline scenario for 50,000 simulations

# outfile = "data/processed/TEhandtosaliva.csv"

# read data 

df_convergence_1 <- read.csv (file=infile1, header = TRUE, sep=",")
df_convergence_2 <- read.csv (file=infile2, header = TRUE, sep=",")
df_convergence_3 <- read.csv (file=infile3, header = TRUE, sep=",")
df_convergence_4 <- read.csv (file=infile4, header = TRUE, sep=",")




