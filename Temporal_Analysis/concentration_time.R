         #   Simulating Concentrations Through Time  #

# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer
# Disinfection:   YES - Surface


# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

# read and process data  -------------------------------------------------------
source ("src/process_concentrationsaliva.R") # Wölfel 2020, Pan 2020, Kim 2020, To 2020 (first 2 weeks after symptom onset)
source ("src/process_TEsurfacetohand.R") # Lopez 2013 (high himidity)
source ("src/process_decay.R") # van Doremalen 2020
source ("src/process_prevalence.R") # Data obtained from antibody surveys in different countries.

# Simulation parameters:
simNum = 1000 # Number of simulations

# General parameters
Vs <- runif(simNum, 0.0396, 0.0484) # [mL] Volume of saliva per cough, Nicas and Jones (2009), mean 0.044ml /pm 10%
Csp <- sample(df_Csp$Csp, size=simNum, replace = TRUE) # [copies/ml] Concentration of saliva in the first 2 weeks after symptom onsent 

# Metal specific parameters
k_stl <- rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) # [min] Half life of CoV-2 in steel
n_stl <- log(2) / k_stl # [min-1] Decay rate, function of the halflife of the virus in the surface
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]
TEhs_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from hand to surface for stainless steel RH=[40-65%]

# Scenario specific parameters
angle <- 0.8 # Fixed, based in the images of @2Bourouiba2014
# prev <- runif(simNum, 0.002, 0.05) # For sensitivity analysis from 0.02 to 5% prevalence.
prev <- med_prev #  low_prev = 0.002, med_prev = 0.01, high_prev = 0.05, data justified in "process_prevalence.R"
dt <- runif(simNum, 1, 20) # Time between touching surfaces (Transport, Trafic lingths), data justified in "process_PublicTransport.R"
x <- 5 # Inoculation distance (cm), Assumed. Hand covering the mouth. Assuming the hand to be 5cm away

t_dis <- 0 # Disinfection strategy ([0= No dis], [1=7am], [2=12pm], [3=7am and 12pm], [4=12 and 6pm])
days <- 7 # Days simulated 
surf_dis <- runif(simNum, 10^3, 10^4)
# -----------------------------------------------------------------------------
#                           Modelling Concentration Trough Time

## Create DataSet for the sensitivity analysis
df_stl_btn <- as.data.frame(cbind(Csp, Vs, n_stl, TEsh_stl, TEhs_stl, dt, prev, surf_dis))
df_stl_btn$load_hand  <- (df_stl_btn$Csp) * (df_stl_btn$Vs / ((4 *pi*x^2)/angle))
df_stl_btn$load_surf <- df_stl_btn$load_hand * df_stl_btn$TEhs_stl

# --------------
for (p in 1:simNum) {
    
    
    # Generating times, starting from 7 am, adding delta time, ultil 11 pm, then add 8 hours and start again
    start <- 7 #start-time
    end <- 23 #end-time
    time <- c() #empty-vector
    for (i in 1:days) {
        temp <- seq(60*start,60*end,dt[p]) + (i-1)*1440
        time <- append(time,temp,length(time))
    }
    
    # Generating disinfection times  (dis at 0, 7, 12, 12 and 6)
    chunk <- length(time)/days
    if (t_dis==0) { # No disinfection
        time_dis <- rep(0, length(time)) 
    } else if (t_dis==1) { # Morning (7am) disinfection
        time_dis <- rep(c(1,rep(0,(chunk-1))),days) 
    } else if (t_dis==2) { # Middle of day (12pm) disinfection
        time_dis <- rep(c(rep(0,round(((chunk-1)/3),0)), 1, rep(0,(chunk-(round(((chunk-1)/3),0))-1))),days)
    } else if (t_dis==3) { # Middle of day (7am and 12pm) disinfection
        time_dis <- rep(c(1,rep(0,round(((chunk-2)/3),0)), 1, rep(0,(chunk-(round(((chunk-2)/3),0))-2))),days)
    } else {               # Twice a day ~12 and ~6pm
        time_dis <- rep(c(rep(0,round(((chunk-2)/3),0)), 1, rep(0,round(((chunk-2)/3),0)),1, rep(0,(chunk-(round(((chunk-2)/3),0))*2)-2)),days)
    }
    
    # Generaring the vector of if infected
    prevalence <- as.numeric(rbinom (length(time), 1, df_stl_btn$prev[p]))
    
    
    # Create the risk matrix for the conditions
    daily_risk <- cbind(time, time_dis, prevalence)
    daily_risk <- as.data.frame(daily_risk)
    
    # Loading the virus on the surface (yes or no) Dependent on prevalence
    daily_risk$Loading <- ifelse(daily_risk$prevalence, df_stl_btn$load_surf[p], 0)
    
    # Calculating surface concentration
    daily_risk$Csurface <- c()
    daily_risk$Cfng <- c()
    daily_risk$Csurface2 <- c()
    
    daily_risk$Csurface [1] <- daily_risk$Loading[1]  # Initialize with the first loading at 7 am. 
    daily_risk$Cfng [1] <- daily_risk$Csurface [1] *  df_stl_btn$TEsh_stl[p] 
    daily_risk$Csurface2 [1] <- if (daily_risk$time_dis[1] ==0) {daily_risk$Csurface [1] - daily_risk$Cfng [1]
    } else { (daily_risk$Csurface [1] - daily_risk$Cfng [1]) / surf_dis[p]}
    
    
    for (i in 2:length(daily_risk$Csurface)) {
        daily_risk$Csurface[i] <- daily_risk$Loading[i] + (daily_risk$Csurface2[i-1] * exp(-df_stl_btn$n_stl[p] * (daily_risk$time [i]-daily_risk$time[i-1])))
        daily_risk$Cfng[i] <- daily_risk$Csurface[i] * df_stl_btn$TEsh_stl[p] 
        daily_risk$Csurface2[i] <- if (daily_risk$time_dis[i]==0) {daily_risk$Csurface [i] - daily_risk$Cfng [i]
        } else { (daily_risk$Csurface [i] - daily_risk$Cfng [i]) / surf_dis[p]}
    }
}



ggplot(data = daily_risk, aes(x = daily_risk$time, y= daily_risk$Csurface))+ geom_point()

