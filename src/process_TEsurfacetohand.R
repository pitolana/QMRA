
# Processing data of virus transfer from surface to hand 
# Data obtained from the article (Lopez, 2013, doi: 10.1128/AEM.01030-13)

# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/TEsurfacetohand.csv"
outfile = "data/processed/TEsurfacetohand.csv"

# read data 
df_TE_sh <- read.csv (file=infile, header = TRUE, sep=",")



# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)
