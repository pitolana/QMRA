# Conditions
    # Material:  Metal
    # Humidity:  High
    # Surface:   Buttons

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

# read and process data  -------------------------------------------------------
source ("src/process_concentrationsaliva.R") # Wölfel 2020, Pan 2020, Kim 2020, To 2020 (first 2 weeks after symptom onset)
source ("src/process_TEhandtosaliva.R") # Pitol 2017 (dry transfer from hand to saliva)
source ("src/process_TEsurfacetohand.R") # Lopez 2013 (high himidity)
source ("src/process_decay.R") # van Doremalen 2020
source ("src/process_doseresponse.R") # QMRAwiki, using the from 0.5th, 50th, and 99.5 th percentiles as min, mode, and max
source ("src/process_Asf.R") # Fractional surface area, @EPA2011 and @AuYeung2008 (adult front partial finger)

# Simulation parameters:
simNum = 10 # Number of simulations
# ----- #

GC_TCID50 <- runif(simNum, 100, 1000) # [copies/TCID50] Proportion of infective gene copies [GC:TCID50], Ip (2015) 
Vs <- runif(simNum, 0.0396, 0.0484) # [mL] Volume of saliva per cough, Nicas and Jones (2009), mean 0.044ml /pm 10%
Ncough_min <- rtruncnorm(simNum, 0, Inf, 0.57, 1) # [cough/min] Number of coughs per minute, Leung (2020)
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # [unitless] Transfer Efficiency from hand to saliva, Pitol 2017
k <-  rtri(simNum, min = Q_0.5, max = Q_99.5 , mode = Q_50) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
Csp <- sample(df_Csp$Csp, size=simNum, replace = TRUE) # [copies/ml] Concentration of saliva in the first 2 weeks after symptom onsent 
FSAfm <- runif(simNum, Asf_min, Asf_max) # [cm^2]Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011

# Metal specific parameters
k_stl <- rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) # Half life of CoV-2 in steel
n_stl <- log(2) / k_stl # Decay rate, function of the halflife of the virus in the surface
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]

# Scenario specific parameters
angle <- 0.1 # Fixed, based in the images of @2Bourouiba2014
prev <- runif(simNum, 0, 0.01) # Assume either low, meddium and high prevalence
t_btw_push <- runif(simNum, .5, 10) # Time between pushing the buttons (assumed)
x <- 40 # Inoculation distance (cm), assumed

# -----------------------------------------------------------------------------
#                                 Risk Analysis

# Create DataSet for the sensitivity analysis
df_stl_btn <- as.data.frame(cbind(GC_TCID50, Vs, Ncough_min, Csp, Af, FSAfm, TEhm, t_ATM, t_btw_ATM, n_stl, TEsh_stl))

# To test
prev[1] <- 0.1
Csp[1] <- 1000

# Generating times, starting from 7 am, plus the time between ATM (uniform for buttons)
a <- 7*60 # Starting time = 7 am, in minutes. Ending time = 11 pm. 
c <- 1
time = c()
while(a< 60*11) {
    a= a + t_btw_push[1]
    time [c] <- a 
    c  <- c + 1
}

# Generaring the vector of if infected
prevalence <- rbinom (length(time), 1, prev[1])

# Create the risk matrix for the conditions
daily_risk <- cbind(time, prevalence)
daily_risk <- as.data.frame(daily_risk)

# Loading the virus on the surface (yes or no) Dependent on prevalence
daily_risk$Loading <- ifelse(daily_risk$prevalence, Csp[1], 0)

# Calculating surface concentration
daily_risk$Csurface <- c()
daily_risk$Cfng <- c()
daily_risk$Csurface2 <- c()

daily_risk$Csurface [1] <- daily_risk$Loading[1] # Initialize with the first loading at 7 am. 
daily_risk$Cfng [1] <- daily_risk$Csurface [1] * TEsh_stl[1] 
daily_risk$Csurface2 [1] <- daily_risk$Csurface [1] - daily_risk$Cfng [1]

for (i in 2:length(daily_risk$Csurface)) {
    daily_risk$Csurface[i] <- daily_risk$Loading[i] + (daily_risk$Csurface2[i-1] * exp(-n_stl * t_btw_push[1]))
    daily_risk$Cfng[i] <- daily_risk$Csurface[i] * TEsh_stl[1] 
    daily_risk$Csurface2[i] <- daily_risk$Csurface[i] - daily_risk$Cfng[i]
}

# Dose assuming fractional surface area of finger
daily_risk$dose <- daily_risk$Cfng * FSAfm [1] * TEhm [1]

# Dose Response
daily_risk$P_inf <- (1 - exp(- (daily_risk$dose * k[1])))

# Daily risk
risk_0.25 <- quantile(daily_risk$P_inf, 0.25)
risk_0.50 <- quantile(daily_risk$P_inf, 0.25)
risk_0.75 <- quantile(daily_risk$P_inf, 0.25)

# Save the risk 

