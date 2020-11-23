# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer
# Intervention:   Surface Disinfection
# Frequency:      60-240 min

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
source ("src/process_prevalence.R") # Data obtained from antibody surveys in different countries.
source ("src/process_disinfection.R") # Log reduction by disinfection, @Hulkower2011, @Sattar1989 for Chlorine and Ethanol
# source ("src/process_mask") # Reduction in inoculation on hands when mask is used


# Simulation parameters:
simNum = 50000 # Number of simulations
# ----- #

# Paramaters (non used in this version of the model)
# adj_DoseResp <- runif(simNum, 0.001, 0.01) # Adjustments for intranasal administration, @Jones2020
# Ncough_min <- rtruncnorm(simNum, 0, Inf, 0.57, 1) # [cough/min] Number of coughs per minute, Leung (2020)

# General parameters
GC_Inf <- runif(simNum, 100, 1000) # [copies/TCID50] Proportion of infective gene copies [GC:TCID50], Ip (2015) 
Vs <- runif(simNum, 0.0396, 0.0484) # [mL] Volume of saliva per cough, Nicas and Jones (2009), mean 0.044ml /pm 10%
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # [unitless] Transfer Efficiency from hand to saliva, Pitol 2017
k <-  rtri(simNum, min = Q_0.5, max = Q_99.5 , mode = Q_50) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
Csp <- sample(df_Csp$Csp, size=simNum, replace = TRUE) # [copies/ml] Concentration of saliva in the first 2 weeks after symptom onsent 
FSA <- runif(simNum, Asf_min, Asf_max) # [cm^2] Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011

# Metal specific parameters
k_stl <- rtruncnorm(simNum, 0, Inf, k_stl_mean, k_stl_sd) # [min] Half life of CoV-2 in steel
n_stl <- log(2) / k_stl # [min-1] Decay rate, function of the halflife of the virus in the surface
TEsh_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from surface to hand for stainless steel RH=[40-65%]
TEhs_stl <- rtruncnorm(simNum, 0, 1, TE_sh_stl_mean, TE_sh_stl_sd) # TE from hand to surface for stainless steel RH=[40-65%]

# Hand inoculation
x <- runif(simNum, 5, 10) # [cm] Inoculation distance (cm), Assumed. Hand covering the mouth. Assuming the hand to be 5-10cm away
angle <- runif(simNum, 27.5, 35) # [degrees] Based in the images of @Bourouiba2014 and @Bandiera2020. 
angle <- angle* pi/180 # [radians] Converting to radians
ratio <- x * tan (angle) # [cm] Ratio of circular plane on a cone at a distance x, with an opening angle 
area_inoculation <- pi * ratio^2 # [cm2] circular area from a conical distribution at a distance x

# Scenario specific parameters
prev <- low_prev #  low_prev = 0.002, med_prev = 0.01, high_prev = 0.05, data justified in "process_prevalence.R"
t_dis <- 0 # Disinfection strategy ([0= No dis], [1=7am], [2=12pm], [3=7am and 12pm], [4=12 and 6pm])

dt <- runif(simNum, 60, 240) # Time between touching surfaces
days <- 7 # Days simulated 
surf_dis <- runif(simNum, 10^3, 10^4) # @Hulkower2011 and @Sattar1989   for ethanol and chlorine
# -----------------------------------------------------------------------------
#                                 Risk Analysis

# Create DataSet for the sensitivity analysis
df_stl_btn <- as.data.frame(cbind(Csp, GC_Inf, Vs, FSA,  n_stl, TEsh_stl, TEhs_stl, dt, TEhm, k, prev, surf_dis))
df_stl_btn$load_hand  <- (df_stl_btn$Csp/df_stl_btn$GC_Inf) * (df_stl_btn$Vs / area_inoculation)
df_stl_btn$load_surf <- df_stl_btn$load_hand * df_stl_btn$TEhs_stl # Assuming 100% overlap between the hand and the surface


# --------------

df_stl_btn$risk_0.25 <- c()
df_stl_btn$risk_0.50 <- c()
df_stl_btn$risk_0.75 <- c()

# --------------

# Calculating the 25, 50 and 75 percentile risk based on a delta time for all the rows in the data frame

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
    
    # Dose assuming fractional surface area of finger
    daily_risk$dose <- daily_risk$Cfng * df_stl_btn$FSA [p] * df_stl_btn$TEhm [p]
    
    # Dose Response
    daily_risk$P_inf <- (1 - exp(- (daily_risk$dose * df_stl_btn$k[p])))
    
    # Daily risk
    df_stl_btn$risk_0.05[p] <- quantile(daily_risk$P_inf, 0.05)
    df_stl_btn$risk_0.25[p] <- quantile(daily_risk$P_inf, 0.25)
    df_stl_btn$risk_0.50[p] <- quantile(daily_risk$P_inf, 0.50)
    df_stl_btn$risk_0.75[p] <- quantile(daily_risk$P_inf, 0.75)
    df_stl_btn$risk_0.95[p] <- quantile(daily_risk$P_inf, 0.95)
    df_stl_btn$risk_mean[p] <- mean(daily_risk$P_inf)
    df_stl_btn$risk_avobe1000000[p] <- sum(daily_risk$P_inf > 10^-6) 
    df_stl_btn$risk_below1000000[p]  <- sum(daily_risk$P_inf < 10^-6)
    df_stl_btn$risk_avobe100000[p] <- sum(daily_risk$P_inf > 10^-5) 
    df_stl_btn$risk_below100000[p]  <- sum(daily_risk$P_inf < 10^-5)
    df_stl_btn$risk_avobe10000[p] <- sum(daily_risk$P_inf > 10^-4) 
    df_stl_btn$risk_below10000[p]  <- sum(daily_risk$P_inf < 10^-4)
    df_stl_btn$risk_avobe1000[p] <- sum(daily_risk$P_inf > 10^-3) 
    df_stl_btn$risk_below1000[p]  <- sum(daily_risk$P_inf < 10^-3)
    df_stl_btn$risk_avobe100[p] <- sum(daily_risk$P_inf > 10^-2) 
    df_stl_btn$risk_below100[p]  <- sum(daily_risk$P_inf < 10^-2)
    
}












# - - - - - - - - - - - - -
# Sensitivity analysis 

# Calculate correlation coefficients
c_TEhm     <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$TEhm,      method = "spearman", exact=F)
c_Csp      <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$Csp,       method = "spearman", exact=F)
c_Vs       <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$Vs,        method = "spearman", exact=F)   
c_dt       <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$dt,        method = "spearman", exact=F)  
c_n        <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$n_stl,     method = "spearman", exact=F)  
c_TEsh     <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$TEsh_stl,  method = "spearman", exact=F) 
c_TEhs     <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$TEhs_stl,  method = "spearman", exact=F)  
c_GC_inf   <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$GC_Inf,    method = "spearman", exact=F) 
c_k        <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$k,         method = "spearman", exact=F)  
c_surf_dis <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$surf_dis,  method = "spearman", exact=F)  

# Create a correlation data frame
corRes <- data.frame(type = c("TEhm", "Csp", "Vs", "dt","n", "TEsh", "TEhs", "GC_inf", "k", "surf_dis"),
                     rho = c(c_TEhm$estimate, c_Csp$estimate, c_Vs$estimate, c_dt$estimate, c_n$estimate, 
                             c_TEsh$estimate, c_TEhs$estimate, c_GC_inf$estimate, c_k$estimate, c_surf_dis$estimate))

# Plot the correlation coefficients 
ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") 


