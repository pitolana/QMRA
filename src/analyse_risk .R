# setup -------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)

# read and process data  --------------------------------------------------
source ("src/process_concentrationsaliva.R") # Wölfel 2020, Pan 2020, Kim 2020
source ("src/process_TEhandtosaliva.R") # Pitol 2017 (dry transfer from hand to saliva)
source ("src/process_TEsurfacetohand.R") # Lopez 2013 (high himidity?)
source ("src/process_ATM_observation.R") # Observation ATM 
source ("src/process_decay.R") # van Doremalen 2020

##    Scenario 1 : Single touch, ATM machine 

# Simulation parameters
simNum = 100 #number of simulations

# Parameters from distributions and datasets
GC_TCID50 <- runif(simNum, 100, 1000) # Proportion of infective gene copies [GC:TCID50],  
Vs <- runif(simNum, 0.0396, 0.0484) # Volume of saliva per cough, Nicas and Jones (2009)
Ncough_min <- rtruncnorm(simNum, 0, Inf, 0.57, 1) # Number of coughs per minute, Leung (2020)
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # Transfer Efficiency from hand to saliva, Pitol 2017
FSAsf <- runif(simNum, 0.5, 1) # Fraction of the finger in contact with the ATM --> correct this
FSAfm <- runif(simNum, 0.5, 1) # Fraction of the finger in contact with the mouth --> correct this
Csp <- 10^sample(df_Csp$C_sp, size=simNum, replace = TRUE) # Needs correction! Replace 10^0 with 0? Change distribution? Wölfel 2020, Pan 2020, Kim 2020

# Activity specific parameters

#                           ---- ATM------

# The TEsh and inactivation on surface were steel specific, assuming the ATM is made of steel. 
t_ATM <-  rtruncnorm(simNum, 0, Inf, t_ATM_mean, t_ATM_sd)  # Time at ATM, 20 hours of observational studies
t_btw_ATM <- rtruncnorm(simNum, 0, Inf, 60/visits_mean, 60/visits_sd) # (Change distribution at the end) Time between ATM visits, observational studies
x <- 0.4 # Inoculation distance (m), assumed based on observations
k_stl <- rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) # Half life of CoV-2 in steel
n_stl <- log(2) / k_stl # Decay rate, function of the halflife of the virus in the surface
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]
k <- rnorm(simNum, k_mean, k_sd)

# Creating data fram with all the variables and numbers simulated from distributions ATM   
df <- as.data.frame(cbind(GC_TCID50, Vs, Ncough_min, Csp, FSAsf, FSAfm, TEhm, t_ATM, t_btw_ATM, x, n_stl, TEsh_stl))

# Calcuated parameters
for (i in 1:simNum)
{
    df$Cs_0[i] <- (df$Csp [i] / df$Vs[i]) / ((4 *pi* x^2)/0.1) #correct with 10% of the area of sphere 
    df$Cs_1[i] <- df$Cs_0[i] * exp(- n_stl[i] * t_btw_ATM[i])
    df$Nf[i]   <- df$Cs_1[i] * df$FSAsf [i] * df$TEsh_stl[i]
    df$Dose[i] <- df$Nf[i] * df$FSAfm [i] * df$TEhm [i]
    df$P_inf[i] <- 1 - exp(- (df$Dose [i]) * k[i])
}

# - - - - - - - - - - - - -
# Sensitivity analysis 
# calculate correlation coefficients

c_TEhm <- cor.test(df$P_inf, df$TEhm, method = "spearman", exact=F)
c_Csp <- cor.test(df$P_inf, df$Csp, method = "spearman", exact=F)
c_Vs <- cor.test(df$P_inf, df$Vs, method = "spearman", exact=F)   
c_t_btw_ATM <- cor.test(df$P_inf, df$t_btw_ATM, method = "spearman", exact=F)  
c_n <- cor.test(df$P_inf, df$n_stl, method = "spearman", exact=F)  

corRes <- data.frame(type = c("TEhm", "Csp", "Vs", "t_btw_ATM","n" ),
                     rho = c(c_TEhm$estimate, c_Csp$estimate, c_Vs$estimate, c_t_btw_ATM$estimate, c_n$estimate))

ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") 
+ theme_minimal() + xlab("") + theme(axis.text.x = element_text(angle = 10))


# - - - - - - - - - - -



