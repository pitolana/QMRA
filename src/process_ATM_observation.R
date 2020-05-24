# Processing data obtained from the ATM observation study

# read and process data  --------------------------------------------------
infile = "data/raw/ATM_observation.csv"
#outfile = "data/processed/XX.csv"

# Library
library(MASS) 

# read data 
df_ATM <- read.csv (file=infile, header = TRUE, sep=",") # C_sp is viral load in log10 copies/ml
t_ATM_mean <- mean(df_ATM$t_ATM)
t_ATM_sd <-  sd(df_ATM$t_ATM)    

# ATM visits per hour
visits <- df_ATM$visits_hr
visits <- visits[!is.na(visits)]
visits_mean <- mean(visits)
visits_sd <- sd(visits)

# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)