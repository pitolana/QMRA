
# Processing data of virus transfer from hands to saliva
# Data taken from Pitol, 2017. Available at <https://zenodo.org/record/1454505#.Xo3wMZNKjBI>

# setup -------------------------------------------------------------------
# external libraries
# library(tidyverse) 

# read and process data  --------------------------------------------------
infile = "data/raw/TEhandtosaliva.csv"
#outfile = "data/processed/TEhandtosaliva.csv"
#function TEhm

# read data 
df_TE_hm <- read.csv (file=infile, header = TRUE, sep=",")

#-----------------

# dt [min] = drying time for average droplet size, assumtion. 
# TE_hm_d [%] = transfer efficiency from hand to mucous membrane (dry)
# TE_hm_w [%] = transfer efficiency from hand to mucous membrane (wet)
# TE_hm [%]= transfer efficiency from hand to mucous membrane (dry and wet)

TE_hm_d <- mean(df_TE_hm$pt_dry) # mean TEhm when dry
sd_TE_hm_d <- sd(df_TE_hm$pt_dry) # sd TEhm when dry

TE_hm_w <- mean(df_TE_hm$pt_wet)  # mean TEhm when wet
sd_TE_hm_d <- sd(df_TE_hm$pt_wet)  # sd TEhm when wet

# Creating a function to estimate the TEhm as a function of time after inoculation (t)
dt = 10
sinNum = 100
t = 2

## Question, can t be a variable with uncertainty? 

f_TEhm <- function (t, simNum, dt) {
    TEhm <- rnorm(simNum, , )
}


 if t <= dryingtime, m(t) = t/dryingtime 
 if t > dryingtime, m(t)=1

 TE_hm(t) = m(t) * TE_hm_d + (1-m (t)) * TE_hm_w
 sd_TE_hm(t)=

    
# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)

    
    
