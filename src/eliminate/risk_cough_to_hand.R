# setup -------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

# read and process data  --------------------------------------------------
source ("src/process_concentrationsaliva.R") # Wölfel 2020, Pan 2020, Kim 2020, To 2020 (first 2 weeks after symptom onset)
source ("src/process_TEhandtosaliva.R") # Pitol 2017 (dry transfer from hand to saliva)
source ("src/process_TEsurfacetohand.R") # Lopez 2013 (high himidity)
source ("src/process_ATM_observation.R") # 20 hours of observation ATM, this study
source ("src/process_decay.R") # van Doremalen 2020

# Simulation parameters
simNum = 100 #number of simulations

# Parameters from distributions and datasets commun to all the scenarios
GC_TCID50 <- runif(simNum, 100, 1000) # (copies/TCID50)Proportion of infective gene copies [GC:TCID50], Ip (2015) 
Vs <- runif(simNum, 0.0396, 0.0484) # (ml) Volume of saliva per cough, Nicas and Jones (2009)
Ncough_min <- rtruncnorm(simNum, 0, Inf, 0.57, 1) # (cough/min) Number of coughs per minute, Leung (2020)
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # Transfer Efficiency from hand to saliva, Pitol 2017
k <-  rtri(simNum, min = 0.00135, max = 0.00459 , mode = 0.00246) # Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
Csp <- sample(df_Csp$Csp, size=simNum, replace = TRUE) # (copies/ml) Concentration of saliva in the first 2 weeks after symptom onsent 
Asf <- runif(simNum, Asf_min, Asf_max) # (cm^2) @Chabrelie2018 @Murai1996 @Peters2009a @Sahmel 2015
FSAfm <- runif(simNum, 0.5, 0.8) # Assumed

##- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Activity specific parameters

#---------------------------------------------------------------------------------------------------
#----------------------------- Cough on hand -------------------------------------------------------

#                           ---- ATM------   Material : steel

# The TEsh and inactivation on surface were steel specific.
t_ATM <-  rtruncnorm(simNum, 0, Inf, t_ATM_mean, t_ATM_sd)  # Time at ATM, 20 hours of observational studies
t_btw_ATM <- rtruncnorm(simNum, 0, Inf, 60/visits_mean, 60/visits_sd) # Time between ATM visits, observational studies
x <- 5 # Inoculation distance (cm), mouth to hand
k_stl <- rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) # Half life of CoV-2 in steel
n_stl <- log(2) / k_stl # Decay rate, function of the halflife of the virus in the surface
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]
angle <- 0.9 # assumed
TEhs_stl <- rtruncnorm(simNum, 0, 1, TE_hs_stl_mean, TE_hs_stl_sd) # TE from hand to surface for stainless steel RH=[40-65%]

# Creating data fram with all the variables and numbers simulated from distributions ATM   
df_risk_ATM <- as.data.frame(cbind(GC_TCID50, Vs, Ncough_min, Csp, Asf, FSAfm, TEhm, t_ATM, t_btw_ATM, n_stl, TEsh_stl))

# Calcuated parameters
for (i in 1:simNum)
{
    df_risk_ATM$Ch_0[i]  <- ((((df_risk_ATM$Csp [i]/df_risk_ATM$GC_TCID50[i])) * df_risk_ATM$Vs[i])) / ((4 *pi* x^2)/angle)
    df_risk_ATM$Cs_0[i]  <-  
    df_risk_ATM$Cs_1[i]  <- df_risk_ATM$Cs_0[i] * exp(- df_risk_ATM$n_stl[i] * df_risk_ATM$t_btw_ATM[i])
    df_risk_ATM$Nf[i]    <- df_risk_ATM$Cs_1[i] * df_risk_ATM$Asf [i] * df_risk_ATM$TEsh_stl[i]
    df_risk_ATM$Dose[i]  <- df_risk_ATM$Nf[i] * df_risk_ATM$FSAfm [i] * df_risk_ATM$TEhm [i]
    df_risk_ATM$P_inf[i] <- 1 - exp(- k[i] * df_risk_ATM$Dose[i])
}

# - - - - - - - - - - - - -
# Sensitivity analysis 
# calculate correlation coefficients

c_TEhm <- cor.test(df$P_inf, df$TEhm, method = "spearman", exact=F)
c_Csp <- cor.test(df$P_inf, df$Csp, method = "spearman", exact=F)
c_Vs <- cor.test(df$P_inf, df$Vs, method = "spearman", exact=F)   
c_t_btw_ATM <- cor.test(df$P_inf, df$t_btw_ATM, method = "spearman", exact=F)  
c_n <- cor.test(df$P_inf, df$n_stl, method = "spearman", exact=F)  
c_TEsh <- cor.test(df$P_inf, df$TEsh_stl, method = "spearman", exact=F)  
c_GC_inf <- cor.test(df$P_inf, df$GC_TCID50, method = "spearman", exact=F)  



corRes <- data.frame(type = c("TEhm", "Csp", "Vs", "t_btw_ATM","n", "TEsh", "GC_inf"),
                     rho = c(c_TEhm$estimate, c_Csp$estimate, c_Vs$estimate, c_t_btw_ATM$estimate, c_n$estimate, 
                             c_TEsh$estimate, c_GC_inf$estimate))

ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") 
+ theme_minimal() + xlab("") + theme(axis.text.x = element_text(angle = 10))


