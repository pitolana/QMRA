# Processing data of concentration of SARS-CoV-2 samples in sputum (taken as saliva)
# Data obtained from: XXX


# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/concentrationsaliva.csv"
#outfile = "data/processed/decay.csv"

# read data 
df_C_sp <- read.csv (file=infile, header = TRUE, sep=",")


# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)