# Conditions
# Risk of infection given surface concentration

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

# read and process data  -------------------------------------------------------
source ("src/process_TEhandtosaliva.R") # Pitol 2017 (dry transfer from hand to saliva)
source ("src/process_TEsurfacetohand.R") # Lopez 2013 (high himidity)
source ("src/process_doseresponse.R") # QMRAwiki, using the from 0.5th, 50th, and 99.5 th percentiles as min, mode, and max
source ("src/process_Asf.R") # Fractional surface area, @EPA2011 and @AuYeung2008 (adult front partial finger)
source ("src/process_SurfaceConcentration.R")



# Simulation parameters:
simNum = 100000 # Number of simulations
# ----- #


# General parameters
C_empirical <- sample(Surface_Concentrations_Brazil, size=simNum, replace = TRUE) # [copies/cm2] 
GC_Inf <- runif(simNum, 100, 1000) # [copies/TCID50] Proportion of infective gene copies [GC:TCID50], Ip (2015) 
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # [unitless] Transfer Efficiency from hand to saliva, Pitol 2017
k <-  rtri(simNum, min = Q_0.5, max = Q_99.5 , mode = Q_50) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
FSA <- runif(simNum, Asf_min, Asf_max) # [cm^2] Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]

# -----------------------------------------------------------------------------
#                                 Risk Analysis

# Create DataSet for the sensitivity analysis
df_empirical <- as.data.frame(cbind(C_empirical, GC_Inf, TEhm, k, FSA, TEsh_stl ))
df_empirical$Csurface <- df_empirical$C_empirical / df_empirical$GC_Inf
df_empirical$Cfng <- df_empirical$Csurface * df_empirical$TEsh_stl 
df_empirical$dose <- df_empirical$Cfng * df_empirical$FSA * df_empirical$TEhm
df_empirical$P_inf <- (1 - exp(- (df_empirical$dose * df_empirical$k)))

risk_0.25 <- quantile(df_empirical$P_inf, 0.25)
risk_0.50 <- quantile(df_empirical$P_inf, 0.50)
risk_0.75 <- quantile(df_empirical$P_inf, 0.75)
risk_0.95 <- quantile(df_empirical$P_inf, 0.95)




    

