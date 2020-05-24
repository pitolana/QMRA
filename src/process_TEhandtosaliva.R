# Processing data of virus transfer from hands to saliva, based on MS2
# Data taken from Pitol, 2017. Available at <https://zenodo.org/record/1454505#.Xo3wMZNKjBI>

# setup -------------------------------------------------------------------
# external libraries
# library(tidyverse) 

# read and process data  --------------------------------------------------
infile = "data/raw/TEhandtosaliva.csv"
# outfile = "data/processed/TEhandtosaliva.csv"

# read data 
df_TE_hm <- read.csv (file=infile, header = TRUE, sep=",")

#-----------------

# dt [min] = drying time for average droplet size, assumtion. 
# TE_hm_d [%] = transfer efficiency from hand to mucous membrane (dry)
# TE_hm_w [%] = transfer efficiency from hand to mucous membrane (wet)
# TE_hm [%]= transfer efficiency from hand to mucous membrane (dry and wet)

TE_hm_d <- mean(df_TE_hm$pt_dry)/100 # mean TEhm when inoculum is dry
sd_TE_hm_d <- sd(df_TE_hm$pt_dry)/100 # sd TEhm when inoculim is dry

TE_hm_w <- mean(df_TE_hm$pt_wet)/100  # mean TEhm when inoculum is wet
sd_TE_hm_w <- sd(df_TE_hm$pt_wet)/100  # sd TEhm when inoculum is wet

# Creating a function to estimate the TEhm as a function of time after inoculation (t)
# Function normal with limits


# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)
    
    
