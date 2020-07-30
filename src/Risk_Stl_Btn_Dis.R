# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer
# Disinfection:   YES


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
source ("src/process_prevalence.R") # Fractional surface area, @EPA2011 and @AuYeung2008 (adult front partial finger)
source ("src/process_disinfection.R") # Log reduction by disinfection, @Hulkower2011, @Sattar1989 for Chlorine and Ethanol
 
# Simulation parameters:
simNum = 5000 # Number of simulations
# ----- #

# General parameters
GC_Inf <- runif(simNum, 100, 1000) # [copies/TCID50] Proportion of infective gene copies [GC:TCID50], Ip (2015) 
adj_DoseResp <- runif(simNum, 0.001, 0.01) # Adjustments for intranasal administration, @Jones2020
Vs <- runif(simNum, 0.0396, 0.0484) # [mL] Volume of saliva per cough, Nicas and Jones (2009), mean 0.044ml /pm 10%
# Ncough_min <- rtruncnorm(simNum, 0, Inf, 0.57, 1) # [cough/min] Number of coughs per minute, Leung (2020)
TEhm <- rtruncnorm(simNum, 0, 1, TE_hm_d, sd_TE_hm_d) # [unitless] Transfer Efficiency from hand to saliva, Pitol 2017
k <-  rtri(simNum, min = Q_0.5, max = Q_99.5 , mode = Q_50) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
Csp <- sample(df_Csp$Csp, size=simNum, replace = TRUE) # [copies/ml] Concentration of saliva in the first 2 weeks after symptom onsent 
FSA <- runif(simNum, Asf_min, Asf_max) # [cm^2] Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011

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

t_dis <- 0 # Disinfection strategy ([0= No dis], [1=7am], [2=3pm], [3=11pm], [4=12 and 6pm])
days <- 7 # Days simulated 
hand_dis <- 10^4.25
surf_dis <- runif(simNum, 10^3, 10^4)
# -----------------------------------------------------------------------------
#                                 Risk Analysis

# Create DataSet for the sensitivity analysis
df_stl_btn <- as.data.frame(cbind(Csp, GC_Inf, Vs, FSA,  n_stl, TEsh_stl, TEhs_stl, dt, TEhm, k, prev, surf_dis))
df_stl_btn$load_hand  <- (df_stl_btn$Csp/df_stl_btn$GC_Inf) * (df_stl_btn$Vs / ((4 *pi*x^2)/angle))
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
    
    # Generating disinfection times
    chunk <- length(time)/days
           if (t_dis==0) { # No disinfection
        time_dis <- rep(0, length(time)) 
    } else if (t_dis==1) { # Morning (7am) disinfection
        time_dis <- rep(c(1,rep(0,(chunk-1))),days) 
    } else if (t_dis==2) { # Middle of day (3pm) disinfection
        time_dis <- rep(c(rep(0,ceiling(chunk/2)-1),1,rep(0,floor(chunk/2))),days) 
    } else if (t_dis==3) { # Nigth (11pm) disinfection
        time_dis <- rep(c(rep(0,(chunk-1)),1),days) 
    } else {               # Twice a day ~12 and ~6pm
        time_dis <- c(rep(0,round(((chunk-2)/3),0)), 1, rep(0,round(((chunk-2)/3),0)),1, rep(0,(chunk-(round(((chunk-2)/3),0))*2)-2))
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
    df_stl_btn$risk_0.25[p] <- quantile(daily_risk$P_inf, 0.25)
    df_stl_btn$risk_0.50[p] <- quantile(daily_risk$P_inf, 0.50)
    df_stl_btn$risk_0.75[p] <- quantile(daily_risk$P_inf, 0.75)
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
#c_prev     <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$prev,      method = "spearman", exact=F)
c_k        <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$k,         method = "spearman", exact=F)  
c_surf_dis <- cor.test(df_stl_btn$risk_0.50, df_stl_btn$surf_dis,  method = "spearman", exact=F)  

# Create a correlation data frame
corRes <- data.frame(type = c("TEhm", "Csp", "Vs", "dt","n", "TEsh", "TEhs", "GC_inf", "k", "surf_dis"),
                 rho = c(c_TEhm$estimate, c_Csp$estimate, c_Vs$estimate, c_dt$estimate, c_n$estimate, 
                 c_TEsh$estimate, c_TEhs$estimate, c_GC_inf$estimate, c_k$estimate, c_surf_dis$estimate))

# Plot the correlation coefficients 
ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") 

ggplot(data = df_stl_btn, aes(x = df_stl_btn$TEsh_stl, y=df_stl_btn$risk_0.75))+ geom_point()
ggplot(data = df_stl_btn, aes(x = df_stl_btn$TEhs_stl, y=df_stl_btn$risk_0.75))+ geom_point()
ggplot(data = df_stl_btn, aes(x = df_stl_btn$TEhm, y=df_stl_btn$risk_0.75))+ geom_point()
ggplot(data = df_stl_btn, aes(x = df_stl_btn$Csp, y=df_stl_btn$risk_0.75))+ geom_point()
ggplot(data = df_stl_btn, aes(x = df_stl_btn$dt, y=df_stl_btn$risk_0.75))+ geom_point()


#chart.Correlation(df_stl_btn, histogram=TRUE, pch=19)

# - - - - - - - - - - -


# Example of probability of infection
library(scales)

ggplot(data = daily_risk, aes(x = time/60, y = P_inf)) + geom_point() + 
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) 




# - - - - - - - - - - -  Collecting data from simulations - - - - - - - - - - - 

# Create a risk vector after running all the conditions

pMed        <- c(pMed_d0_risk25, pMed_d0_risk50, pMed_d0_risk75, 
                 pMed_d1_risk25, pMed_d1_risk50, pMed_d1_risk75, 
                 pMed_d2_risk25, pMed_d2_risk50, pMed_d2_risk75, 
                 pMed_d4_risk25, pMed_d4_risk50, pMed_d4_risk75)

pLow        <- c(pLow_d0_risk25, pLow_d0_risk50, pLow_d0_risk75, 
                 pLow_d1_risk25, pLow_d1_risk50, pLow_d1_risk75, 
                 pLow_d2_risk25, pLow_d2_risk50, pLow_d2_risk75, 
                 pLow_d4_risk25, pLow_d4_risk50, pLow_d4_risk75)

pHigh       <- c(pHigh_d0_risk25, pHigh_d0_risk50, pHigh_d0_risk75, 
                 pHigh_d1_risk25, pHigh_d1_risk50, pHigh_d1_risk75, 
                 pHigh_d2_risk25, pHigh_d2_risk50, pHigh_d2_risk75, 
                 pHigh_d4_risk25, pHigh_d4_risk50, pHigh_d4_risk75)

Disinfection <- c(0,0,0,1,1,1,2,2,2,4,4,4)
Percentile   <- c(0.25, 0.50, 0.75, 0.25, 0.50, 0.75, 
                  0.25, 0.50, 0.75, 0.25, 0.50, 0.75)
compilation_risk <- as.data.frame( cbind(pMed, pLow, pHigh, Disinfection, Percentile))

write.csv (compilation_risk, file= "data/processed/compilation_risk.csv")



#convergence_test <- c(pMed_d0_risk25_100000, pMed_d0_risk50_100000, pMed_d0_risk75_100000)
#write.csv (convergence_test, file= "data/processed/convergence_risk.csv")
#write.csv (corRes, file= "data/processed/convergence_correlations.csv")

