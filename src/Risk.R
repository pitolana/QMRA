## Set-up
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
cough_ang <- pi/2 # 
t <- 2 # Time 
x <- 4

# Functions (relationships between variables)
f_Cs0 <- function (Ncough_min, Csp, Vs) {
    Cs_0 <- (Ncough_min * Csp * Vs)
    return(Cs_0)
}

f_Cs1 <- function(Cs_0, n, t) {
    Cs_1 <- (Cs_0 * exp(-nt))
    return(Cs_1)
}

f_Nf <- function(Cs_1, SAf, TEsh) {
    Cs_1 <- (Cs_0 * exp(-nt))
    return(Cs_1)
}

f_TE_hm <- function(t, TE_hm_d, TE_hm_w, sd_TE_hm_d, sd_TE_hm_w) {
    m <- t/dryingtime 
    TE_hm_mean <- m * TE_hm_d + (1-m) * TE_hm_w
    TE_hm_sd <-  m * sd_TE_hm_d + (1-m) * sd_TE_hm_w # Check how to compute mean of sd
    return(TE_hm_mean, TE_hm_sd)
}


# Parameters from distributions and datasets
Vs <- runif(simNum, 0.0396, 0.0484) # Volume of saliva per cough, Nicas and Jones (2009)
Ncough_min <- rnorm(simNum, 0.57, 1) # Number of coughs per minute, 
Csp <- sample(df_Csp$C_sp, size=simNum, replace = TRUE) # Concentration of virus in sputum [GC/ml] 
TEsh_steel <- rnorm(simNum, df_TE_sh$pt_mean[14], df_TE_sh$pt_sd[14]) # stainless steel RH=[40-65%]
t <- runif(simNum, 0, 30) # Decay time in the surface
t_0.5_steel <- runif(simNum, df_decay$lowCI[4], df_decay$highCI[4]) # Must correct this
n <- 

df <- as.data.frame(cbind(Vs, Ncough_min, Csp, TEsh_steel, t_0.5_steel))
df$n <- log(2)/df$t_0.5_steel
df$Cs_0 <- NA
df$Cs_1 <- NA


for (i in 1:simNum )
{
    df$Cs_0[i] <- f_Cs0(Ncough_min[i], Csp[i], Vs[i])
    df$Cs_1[i] <- df$Cs_0[i] + 1
}




