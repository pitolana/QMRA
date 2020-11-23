####  Calculating risk for multimple surface concentrations

# Conditions
# Material:       Metal or plastic
# Humidity:       High

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

# Creating vector of concentrations
df_conc <- c(seq(0.001, 0.01, by=0.0001), seq(0.01, 0.1, by=0.001), seq(0.1, 1, by=0.01), seq(1, 10, by=0.1), seq(10, 100, by=1), seq(101, 1000, by=5), seq(1000, 10000, by=50), seq(10000, 100000, by=1000), 
             seq(100000, 1000000, by=10000), seq(1000000, 10000000, by=100000), seq(10000000, 100000000, by=1000000))     #  ((2:10 %o% 10^(3:5))
df_conc <- as.data.frame(df_conc)

# Simulation parameters:
simNum = 50000 # Number of simulations
# ----- #


for (p in 1:length(df_conc$df_conc)) {
    
    # General parameters
    GC_Inf <- runif(simNum, 100, 1000) # [copies/TCID50]  (Ip2015) 
    TEhm <- rtruncnorm(simNum, 0, 1, 0.2, 0.06) # [unitless] TE hand to saliva (Pitol2017)
    k <-  rtri(simNum, min = 0.00107, max = 0.0068 , mode = 0.00246) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
    FSA <- runif(simNum, 3.92, 5.88) # [cm^2] Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011
    TEsh_stl <- rtruncnorm(simNum, 0, 1, 0.374, 0.16) # TE from surface_hand, stainless steel RH=[40-65%] (Lopez2013)
   
    
    risk <- cbind(GC_Inf, TEhm, k, FSA, TEsh_stl)
    risk <- as.data.frame(risk)    
    risk$Csurf <- c()
    risk$Cfing <- c()
    risk$dose  <- c()
    risk$risk  <- c()
    
    for (i in 1:simNum) {
        risk$Cfing[i] <- df_conc$df_conc[p]/risk$GC_Inf[i] * risk$TEsh_stl[i] 
        risk$dose[i]  <- risk$FSA[i] * risk$Cfing[i] * risk$TEhm[i]
        risk$risk[i]  <- (1-exp(-(risk$dose[i]*risk$k[i])))
    }
    
    df_conc$Q_05 [p]  <- quantile(risk$risk, 0.05)
    df_conc$Q_25 [p]  <- quantile(risk$risk, 0.25)
    df_conc$Q_50 [p]  <- quantile(risk$risk, 0.50)
    df_conc$Q_75 [p]  <- quantile(risk$risk, 0.75)
    df_conc$Q_95 [p]  <- quantile(risk$risk, 0.95)
}

# output -------------------------------------------------------------------
# save file
write.csv (df_conc, file= "data/processed/risk.csv")





# Plot

# read and process data  --------------------------------------------------
infile = "data/processed/risk.csv"   # Dose response simulation
df_conc <- read.csv (file=infile, header = TRUE, sep=",")


p <-ggplot( data=df_conc) +
    geom_point(aes(x = df_conc, y = Q_05), color='gray') + 
    geom_point(aes(x = df_conc, y = Q_95), color='gray') + 
    geom_point(aes(x = df_conc, y = Q_50)) + 
    scale_x_log10() +
    #scale_y_log10() +
    ylab("Risk of Infection") +
    xlab("Concentration (gc/cm2)") +
    theme_bw()
p

##  Reading the data from positive samples
infile = "data/raw/PositiveSamplesRisk.csv"   # Positive samples
df_positive <- read.csv (file=infile, header = TRUE, sep=",")

plot_all <- ggplot(df_positive, aes(x=gc, y=Q_50)) + 
    geom_point()+
    scale_y_log10() +
    scale_x_log10() +
    theme_bw() +
    ylab("Risk of Infection") +
    geom_errorbar(aes(ymin=Q_25, ymax=Q_75), width=.5,
                  position=position_dodge(0.05)) 
print(plot_all)




##  Reading the data from positive samples from Brazil
infile = "data/raw/risk_Brazil.csv"   # Positive samples
df_positive_Brazil <- read.csv (file=infile, header = TRUE, sep=",")

plot_all <- ggplot(df_positive_Brazil, aes(x=gc/.6, y=Q_50)) + 
    geom_point()+
    scale_y_log10() +
    scale_x_log10() +
    theme_bw() +
    ylab("Risk of Infection") +
    geom_errorbar(aes(ymin=Q_25, ymax=Q_75), width=.5,
                  position=position_dodge(0.05)) 
print(plot_all)







### All data toguether

ggplot() + 
    geom_point(data=df_conc, aes(x = df_conc, y = Q_95), color="gray",size=1,alpha=.5) + 
    geom_point(data=df_conc, aes(x = df_conc, y = Q_05), color="gray",size=1,alpha=.5) + 
    geom_point(data=df_conc, aes(x = df_conc, y = Q_50), color="black",size=1,alpha=.5) + 
    geom_errorbar(data=df_positive, aes(x=gc/.6, ymin=Q_05, ymax=Q_95), width=.3, position=position_dodge(0.05), color="#009E73") +
    geom_point(data=df_positive, aes(x = gc/.6, y = Q_50), shape=23, fill="#009E73", color="black", size=3, alpha=1) +
    geom_errorbar(data=df_positive_Brazil, aes(x=gc/0.6, ymin=Q_05, ymax=Q_95), width=.3, position=position_dodge(0.05), color="#D55E00") +
    geom_point(data=df_positive_Brazil, aes(x = gc/0.6, y = Q_50), shape=21, fill="#D55E00", color="black", size=3, alpha=1) +
    geom_hline(yintercept=1.638598e-04, linetype="dashed", color = "black", size=0.6) +
    geom_hline(yintercept=6.849966e-07, linetype="dashed", color = "black", size=0.6) +
    geom_hline(yintercept=1.050096e-08, linetype="dashed", color = "black", size=0.6) +
    scale_x_log10(limits = c(1e-2,1e8), breaks = c(1e-2, 1, 1e2, 1e4, 1e6, 1e8)) +
    scale_y_log10(limits = c(1e-10,1), breaks = c(1, 1e-2, 1e-4, 1e-6, 1e-8, 1e-10)) +
    ylab("Risk of Infection") +
    xlab("SARS-CoV-2 concentration (gene copies/cm2)") +
    theme_bw() 





