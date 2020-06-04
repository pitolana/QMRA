# Processing data decay rate of SARS-CoV-2 on different surfaces

# Data obtained from the Suplementary Appendix of:
# van Doremalen 2020 "Aerosol and Surface Stability of SARS-CoV-2 as Compared with SARS-CoV-1"

# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/decay.csv"
# outfile = "data/processed/decay.csv"

# read data 
df_decay <- read.csv (file=infile, header = TRUE, sep=",")

# Converting hrs to min
df_decay$halflife <- df_decay$halflife * 60
df_decay$lowCI <- df_decay$lowCI * 60
df_decay$highCI <- df_decay$highCI * 60

# Parameters for steel, using median as mean, and assuming sd based on CI
steel <- subset(df_decay, material == "steel", select=c(halflife, lowCI, highCI))
k_stl_mean <- steel$halflife
k_stl_97.5 <- steel$highCI
k_stl_2.5  <- steel$lowCI   
k_stl_sd  <- round((k_stl_97.5 - k_stl_2.5)/(2 *qnorm(0.975)), 5)

# Parameters for plastic, using median as mean, and assuming sd based on CI
plastic <- subset(df_decay, material == "plastic", select=c(halflife, lowCI, highCI))
k_pl_mean <- plastic$halflife
k_pl_97.5 <- plastic$highCI
k_pl_2.5  <- plastic$lowCI   
k_pl_sd  <- round((k_pl_97.5 - k_pl_2.5)/(2 *qnorm(0.975)), 5)

# Parameters for cooper, using median as mean, and assuming sd based on CI
copper <- subset(df_decay, material == "copper", select=c(halflife, lowCI, highCI))
k_cu_mean <- copper$halflife
k_cu_97.5 <- copper$highCI
k_cu_2.5  <- copper$lowCI   
k_cu_sd  <- round((k_cu_97.5 - k_cu_2.5)/(2 *qnorm(0.975)), 5)

# Parameters for cardboard, using median as mean, and assuming sd based on CI
cardboard <- subset(df_decay, material == "cardboard", select=c(halflife, lowCI, highCI))
k_crdb_mean <- cardboard$halflife
k_crdb_97.5 <- cardboard$highCI
k_crdb_2.5  <- cardboard$lowCI   
k_crdb_sd  <- round((k_crdb_97.5 - k_crdb_2.5)/(2 *qnorm(0.975)), 5)

# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)