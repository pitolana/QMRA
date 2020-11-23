# Conditions
# materialerial:       Metal or plastic
# Humidity:       High

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(truncnorm)
library(EnvStats)

infile    <- "data/raw/PositiveSamples_All.csv"
df_All <- read.csv (file=infile, header = TRUE, sep=",")
# Simulation parameters:
simNum = 50000 # Number of simulations
# ----- #

for (p in 1:length(df_All$gc)) {
    
    # General parameters
    GC_Inf <- runif(simNum, 100, 1000) # [copies/TCID50]  (Ip2015) 
    TEhm <- rtruncnorm(simNum, 0, 1, 0.2, 0.06) # [unitless] TE hand to saliva (Pitol2017)
    k <-  rtri(simNum, min = 0.00107, max = 0.0068 , mode = 0.00246) # [unitless] Dose-response parameter, taking median as mode, and 2.5, 97.5% CI as min and max
    FSA <- runif(simNum, 3.92, 5.88) # [cm^2] Fractional surface area @AuYeung2008 (adult front partial finger),hand area (male and female) @EPA2011
    TEsh_stl <- rtruncnorm(simNum, 0, 1, 0.374, 0.16) # TE from surface_hand, stainless steel RH=[40-65%] (Lopez2013)
    TEsh_pl  <- rtruncnorm(simNum, 0, 1, 0.795, 0.212) # TE from surface_hand, plastic, RH=[40-65%] (Lopez2013)
    TEsh_grn  <- rtruncnorm(simNum, 0, 1, 0.3, 0.243) # TE from surface_hand, granite, RH=[40-65%] (Lopez2013)
    eff   <- rtruncnorm(simNum, 0, 1, 0.6, 0.266) 
    
    
    risk <- cbind(GC_Inf, TEhm, k, FSA, TEsh_stl,TEsh_pl, TEsh_grn)
    risk <- as.data.frame(risk)    
    risk$Csurf <- c()
    risk$Cfing <- c()
    risk$dose  <- c()
    risk$risk  <- c()
    
    for (i in 1:simNum) {
        risk$Csurf[i] <- df_All$gc[p] / GC_Inf[i]
        risk$Cfing[i] <- if (df_All$mat[p]=="metal")   {risk$Csurf[i]/ eff[i] * risk$TEsh_stl[i] 
        } else           if (df_All$mat[p]=="plastic") {risk$Csurf[i]/ eff[i] * risk$TEsh_pl[i] 
        } else                                            {risk$Csurf[i]/ eff[i] * risk$TEsh_grn[i]
        }
        risk$dose[i]  <- risk$FSA[i] * risk$Cfing[i] * risk$TEhm[i]
        risk$risk[i]  <- (1-exp(-(risk$dose[i]*risk$k[i])))
    }
    
    df_All$Q_05 [p]  <- quantile(risk$risk, 0.05)
    df_All$Q_25 [p]  <- quantile(risk$risk, 0.25)
    df_All$Q_50 [p]  <- quantile(risk$risk, 0.50)
    df_All$Q_75 [p]  <- quantile(risk$risk, 0.75)
    df_All$Q_95 [p]  <- quantile(risk$risk, 0.95)
}

# output -------------------------------------------------------------------

# save file
write.csv (df_All, file= "data/processed/risk_All.csv")

# Calculate correlation coefficients
c_TEhm         <- cor.test(risk$risk, risk$TEhm,      method = "spearman", exact=F)
c_TEsh         <- cor.test(risk$risk, risk$TEsh_stl,  method = "spearman", exact=F) 
c_GC_inf       <- cor.test(risk$risk, risk$GC_Inf,    method = "spearman", exact=F) 
c_k            <- cor.test(risk$risk, risk$k,         method = "spearman", exact=F)  
c_FSA          <- cor.test(risk$risk, risk$FSA,       method = "spearman", exact=F)  
c_eff          <- cor.test(risk$risk, risk$FSA,       method = "spearman", exact=F) 

# Create a correlation data frame
corRes <- data.frame(type = c("TEhm","TEsh", "GC_inf", "k", "FSA", "eff"),
                     rho = c(c_TEhm$estimate, c_TEsh$estimate, c_GC_inf$estimate, c_k$estimate, 
                             c_FSA$estimate, c_eff$estimate))

# Plot the correlation coefficients 
ggplot(data = corRes, aes(x = type, y = rho)) + geom_bar(stat = "identity") + theme_bw()



# save file
write.csv (df_All, file= "data/processed/risk_All.csv")




# Risks for all  the samples:
median_risks_All <- df_All$Q_50
median_risks_All <- append(median_risks_All, rep(0,1202), after = length(median_risks_All))    #Adding the risk (0) of samples with no RNA

