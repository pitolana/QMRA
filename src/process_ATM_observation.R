# Processing data obtained from the ATM observation study
        # 20 hours of observation divided in morning, afternoon and evening 
        # 2 different ATMs

# read and process data  --------------------------------------------------
infile = "data/raw/ATM_observation.csv"
#outfile = "data/processed/XX.csv"

# Library
library(MASS) 

# read data 
df_ATM <- read.csv (file=infile, header = TRUE, sep=",") 

# Time spent in the ATM (normally distributed)
t_ATM_mean <- mean(df_ATM$t_ATM)
t_ATM_sd <-  sd(df_ATM$t_ATM)    

# Number of ATM visits per hour (not normally distributed, Re-Sample)
visits <- df_ATM$visits_hr
visits <- visits[!is.na(visits)]

# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)