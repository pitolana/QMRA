# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)
library(fitdistrplus)

# read and process data  -------------------------------------------------------
source ("src/process_concentrationsaliva.R") 
source ("src/process_TEhandtosaliva.R") 
source ("src/process_TEsurfacetohand.R") 
source ("src/process_decay.R") 
source ("src/process_Asf.R") 
source ("src/process_prevalence.R") 
source ("src/process_SurfaceConcentration.R")


# Simulation parameters:
# simNum = 50 # Number of simulations
Csp <-  1  # sample(df_Csp$Csp, size=simNum, replace = TRUE) # Concentration in saliva and sputum
# ----- #

# General parameters
Vs <-   0.044  # runif(simNum, 0.0396, 0.0484) 
TEhm <-  TE_hm_d  # rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) 
FSA <- (Asf_min + Asf_max)/2 # runif(simNum, Asf_min, Asf_max) 

# Metal specific parameters
k_stl <- k_stl_mean # rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) 
n_stl <- log(2) / k_stl 
TEsh_stl <- TE_sh_stl_mean # rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) 
TEhs_stl <- TE_sh_stl_mean # rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) 

# Hand inoculation
x <- 7.5 # runif(simNum, 5, 10) 
angle <- 31.25 # runif(simNum, 27.5, 35) 
angle <- angle* pi/180 
ratio <- x * tan (angle)  
area_inoculation <- pi * ratio^2

# Scenario specific parameters
prev <- med_prev #  low_prev = 0.002, med_prev = 0.01, high_prev = 0.05
dt <-  10.5 # runif(simNum, 1, 20) 
days <- 7 # Days simulated 

# -----------------------------------------------------------------------------
#                                 Risk Analysis

# Create DataSet for the sensitivity analysis
df_stl_btn <- as.data.frame(cbind(Csp, Vs, FSA,  n_stl, TEsh_stl, TEhs_stl, dt, TEhm, prev))
df_stl_btn$load_hand  <- (df_stl_btn$Csp) * (df_stl_btn$Vs / area_inoculation)
df_stl_btn$load_surf <- df_stl_btn$load_hand * df_stl_btn$TEhs_stl 

# --------------

    # Generating times, starting from 7 am, adding delta time, ultil 11 pm, then add 8 hours and start again
    start <- 7 #start-time
    end <- 23 #end-time
    time <- c() #empty-vector
    for (i in 1:days) {
        temp <- seq(60*start,60*end,dt) + (i-1)*1440
        time <- append(time,temp,length(time))
    }
    
    # Generating disinfection times  (dis at 0, 7, 12, 12 and 6)
    chunk <- length(time)/days

    # Generaring the vector of if infected
    prevalence <- as.numeric(rbinom (length(time), 1, df_stl_btn$prev))
    
    # Create the risk matrix for the conditions
    daily_risk <- cbind(time, prevalence)
    daily_risk <- as.data.frame(daily_risk)
    
    # Loading the virus on the surface (yes or no) Dependent on prevalence
    daily_risk$Loading <- ifelse(daily_risk$prevalence, df_stl_btn$load_surf, 0)
    
    # Calculating surface concentration
    daily_risk$Csurface <- c()
    daily_risk$Cfng <- c()
    daily_risk$Csurface2 <- c()
    
    daily_risk$Csurface [1] <- daily_risk$Loading[1]  # Initialize with the first loading at 7 am. 
    daily_risk$Cfng [1] <- daily_risk$Csurface [1] *  TEsh_stl 
    daily_risk$Csurface2 [1] <- daily_risk$Csurface [1] - daily_risk$Cfng [1]
   
    
    for (i in 2:length(daily_risk$Csurface)) {
        daily_risk$Csurface[i] <- daily_risk$Loading[i] + (daily_risk$Csurface2[i-1] * 
                                  exp(-n_stl * (daily_risk$time [i]-daily_risk$time[i-1])))
        daily_risk$Cfng[i] <- daily_risk$Csurface[i] * TEsh_stl 
        daily_risk$Csurface2[i] <- daily_risk$Csurface [i] - daily_risk$Cfng [i]
    }
    

# - - - - - - - - - - - - -


    
    
    
    
    
    
    
    
    
    
# If concentration is < than 0.1 virus, change to 0.1
daily_risk$Csurface2<- if_else(daily_risk$Csurface2 < 0.1, 0, daily_risk$Csurface2)   

#Analysing data to decide distribution
plotdist(daily_risk$Csurface2, histo = TRUE, demp = TRUE)
descdist(daily_risk$Csurface2, boot = 1000)


# Histogram 
p <- ggplot(daily_risk, aes(x = Csurface2)) + geom_histogram() 
p


ggplot(daily_risk, aes(x = Csurface2)) + 
    geom_histogram(aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666")
   # + geom_point(x=Surface_Concentrations_Brazil_m2)


####   Log-transforming concentration values 
log10conc<- log10(daily_risk$Csurface2)

# Histogram   - - - -  Log-transforming concentration
log10conc <- as.data.frame(log10conc)
p <- ggplot(log10conc, aes(x = log10conc)) + geom_histogram() + theme_bw() 
p



