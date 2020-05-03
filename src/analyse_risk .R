

# setup -------------------------------------------------------------------
# external libraries
library(tidyverse) 

# read and process data  --------------------------------------------------
source ("src/process_concentrationsaliva.R") # Wölfel 2020 and Pan 2020
#source ("src/process_TEhandtosaliva.R") # Pitol 2017
source ("src/process_TEsurfacetohand.R") # Lopez 2013
source ("src/process_ATM_observation.R") # Observation ATM
source ("src/process_decay.R") # van Doremalen 2020

##    Scenario 1 : Single touch, ATM machine 

# Simulation parameters
simNum = 100 #number of simulations

# Constant parameters
GC_TCID50 <- 0.01 # Proportion of infective gene copies [GC:TCID50]
cough_ang <- pi/2 # 
t <- 2 # Time since inoculation 
t_k <- 5 # Time doing the activity
x <- 4 # Inoculation distance

# Dose-Response Parameter (exponential): 
# (0.00246 (0.00135; 0.00459)),5th, 50th, and 95th percentile values, Huang (2013), wikiQMRA, 

# Parameters from distributions and datasets
Vs <- runif(simNum, 0.0396, 0.0484) # Volume of saliva per cough, Nicas and Jones (2009)
Ncough_min <- rnorm(simNum, 0.57, 1) # Number of coughs per minute, Leung (2020)
Csp <- sample(df_Csp$C_sp, size=simNum, replace = TRUE) # Concentration of virus in sputum [GC/ml] 
TEsh_stl <- rnorm(simNum, df_TE_sh$pt_mean[14], df_TE_sh$pt_sd[14]) # TE from surface to hand for stainless steel RH=[40-65%]
t_0.5_stl <- runif(simNum, df_decay$lowCI[4], df_decay$highCI[4]) # Must correct this
FSAsf <- runif(simNum, 0.5, 1) # Fraction of the finger in contact with the ATM --> correct this
k <- rnorm(simNum, 2.46*10^-3, 2.46*10^-3) # correct, use the median and quartiles to feed a normal

# Parameters from two distributions 
#TEhm <- f_TEhm(t)
TEhm <- rnorm(simNum, 1, 0.5) # provisional value, to be changed by the function

# Creating data fram with all the variables and numbers simulated from distributions    
df <- as.data.frame(cbind(Vs, Ncough_min, Csp, TEsh_stl, t_0.5_stl, TEhm, FSAsf))

# Calcuated parameters
for (i in 1:simNum)
{
    df$n[i] <- log(2) / t_0.5_stl [i]
    df$Cs_0[i] <- df$Csp [i] / cough_ang / x * t_k * Ncough_min [i]
    df$Cs_1[i] <- df$Cs_0[i] ^ 2
    df$Nf [i] <- df$Cs_1[i] * df$FSAsf [i] * df$TEsh_stl[i]
    df$P_inf[i] <- 1 - exp(- (df$Nf [i]) * k[i])
}

# - - - - - - - - - - - - -
# Sensitivity analysis 
# calculate correlation coefficients

c_TEhm <- cor.test(df$P_inf, df$TEhm, method = "spearman", exact=F)
c_Csp <- cor.test(df$P_inf, df$Csp, method = "spearman", exact=F)
c_Vs <- cor.test(df$P_inf, df$Vs, method = "spearman", exact=F)    


corRes <- data.frame(type = c("a", "b", "c"),
                     rho = c(c_TEhm$estimate, c_Csp$estimate, c_Vs$estimate))

ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") 
+ theme_minimal() + xlab("") + theme(axis.text.x = element_text(angle = 10))


# - - - - - - - - - - -



