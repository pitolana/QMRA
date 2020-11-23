# Processing data of concentration of SARS-CoV-2 samples in sputum, and saliva samples
# taken from the uper respiratory track (urt). 

# Data obtained from: 
    #"Viral load of SARS-CoV-2 in clinical samples" (Pan, 2020) 
    #"Virological assessment of hospitalized cases of coronavirus disease 2019" (Wölfel,2020); Figure 2_ Data
    #"Viral load kinetics of SARS-CoV-2 infection in first two patients in Korea" (Kim, 2020)
    #"Temporal profiles of viral load in posterior oropharyngeal saliva samples and serum antibody responses during infection by SARS-CoV-2: an observational cohort study" (To, 2020)


# Assumptions: Since concentrations on sputum, uper respiratiry track (urt), and saliva were similar, we used 
             # from sputum, urt and saliva in the concentration dataset.
             # From the study @To2020, initial and peak concentrations were both used 

# setup -------------------------------------------------------------------
# external libraries


# read and process data  --------------------------------------------------
infile = "data/raw/concentrationsaliva.csv"  # NOTE: The concentrations were taken from plots, therefore are not 
                                             # meant to be exact but an order of magnitud estimate


# read data 
df_Csp <- read.csv (file=infile, header = TRUE, sep=",") # C_sp is viral load in log10 copies/ml
df_Csp <- subset(df_Csp, day_corr<15)  # Taking only the data for the first 2 weeks of symptom onset
df_Csp <- na.omit(df_Csp)

df_Csp <- mutate(df_Csp, Csp = 10^ log10_C) 
df_Csp$Csp[df_Csp$Csp == 1] <- 0 # replacing 10^0 by 0


# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)