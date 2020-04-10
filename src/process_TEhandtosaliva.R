
# Processing data of virus transfer from hands to saliva
# Data taken from Pitol, 2017. Available at <https://zenodo.org/record/1454505#.Xo3wMZNKjBI>

# setup -------------------------------------------------------------------
# external libraries
# library(tidyverse) 

# read and process data  --------------------------------------------------
infile = "data/raw/TEhandtosaliva.csv"
#outfile = "data/processed/TEhandtosaliva.csv"

# read data 
df_TE_hm <- read.csv (file=infile, header = TRUE, sep=",")

#-----------------

# dryingtime [min] = drying time for average droplet size, assumtion. 
# TE_hm_d [%] = transfer efficiency from hand to mucous membrane (dry)
# TE_hm_w [%] = transfer efficiency from hand to mucous membrane (wet)
# TE_hm [%]= transfer efficiency from hand to mucous membrane (dry and wet)

TE_hm_d <- mean(df_TE_hm$pt_dry)
sd_TE_hm_d <- sd(df_TE_hm$pt_dry)

TE_hm_w <- mean(df_TE_hm$pt_wet)
sd_TE_hm_d <- sd(df_TE_hm$pt_wet)

# Calculating the TE_hm as a function of time 
dryingtime= 10

if t <= dryingtime, m(t) = t/dryingtime 
if t > dryingtime, m(t)=1

TE_hm(t) = m(t) * TE_hm_d + (1-m (t)) * TE_hm_w
sd_TE_hm(t)=

    
# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)

    
    
