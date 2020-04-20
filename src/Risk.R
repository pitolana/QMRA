
require(tidyverse)

## Parameters Used

# Reading the data frames 
source ("src/process_concentrationsaliva.R") # Wölfel 2020 and Pan 2020
source ("src/process_TEhandtosaliva.R") # Pitol 2017
source ("src/process_TEsurfacetohand.R") # Lopez 2013
source ("src/process_ATM_observation.R") # Observation ATM
source ("src/process_decay.R") # van Doremalen 2020

# Simulation parameters
simNum = 100 #number of simulations

# Constant parameters
GC_TCID50 <- 0.01 # Proportion of infective gene copies [GC:TCID50]
cough_surf_ang <- pi/2 
t_k <- 2
x_k <- 4

# Functions
f_Cs0 <- function (Ncough_min, Csp, Vs) {
    Cs_0 <- (Ncough_min * Csp * Vs)
    return(Cs_0)
}

# Parameters from distributions and datasets
Vs <- runif(simNum, 0.0396, 0.0484) # Volume of saliva per cough, Nicas and Jones (2009)
Ncough_min <- rnorm(simNum, 0.57, 1) # Number of coughs per minute, 
Csp <- sample(df_Csp$C_sp, size=simNum, replace = TRUE) # Concentration of virus in sputum [GC/ml] 
TEsh_steel <- rnorm(simNum, df_TE_sh$pt_mean[14], df_TE_sh$pt_sd[14]) # stainless steel RH=[40-65%]


df <- as.data.frame(cbind(Vs, Ncough_min, Csp, TEsh_steel))
df$Cs_0 <- NA
df$Cs_1 <- NA


for (i in 1:simNum )
{
    df$Cs_0[i] <- f_Cs0(Ncough_min[i], Csp[i], Vs[i])
    df$Cs_1[i] <- df$Cs_0[i] + 1
}




