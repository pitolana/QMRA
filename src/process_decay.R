
# Processing data decay rate of SARS-CoV-2 on different surfaces

# Data obtained from the Suplementary Appendix of the article 


# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/decay.csv"
#outfile = "data/processed/decay.csv"

# read data 
df_decay <- read.csv (file=infile, header = TRUE, sep=",")

# Equations for first order decay
# C(t) = C(0) e^ (-kt)
# halflife = ln2 / k
# k = ln2 / halflife

# output -------------------------------------------------------------------

# save file
# write.csv (df_XXX, file= outfile)