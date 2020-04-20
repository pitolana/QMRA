# Processing data obtained from the ATM observation study

# read and process data  --------------------------------------------------
infile = "data/raw/ATM_observation.csv"
#outfile = "data/processed/XX.csv"

# read data 
df_ATM <- read.csv (file=infile, header = TRUE, sep=",") # C_sp is viral load in log10 copies/ml


# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)