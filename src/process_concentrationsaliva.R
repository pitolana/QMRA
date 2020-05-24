# Processing data of concentration of SARS-CoV-2 samples in sputum (taken as saliva), and samples
# taken from the uper respiratory track (urt). 

# Data obtained from: 
    #"Viral load of SARS-CoV-2 in clinical samples" (Pan, 2020) 
    #"Virological assessment of hospitalized cases of coronavirus disease 2019" (Wölfel,2020)
    #"Viral load kinetics of SARS-CoV-2 infection in first two patients in Korea" (Kim, 2020)

# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/concentrationsaliva.csv"
# outfile = "data/processed/decay.csv"

# read data 
df_Csp <- read.csv (file=infile, header = TRUE, sep=",") # C_sp is viral load in log10 copies/ml


# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)