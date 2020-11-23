# Processing data of concentration of SARS-CoV-2 on surfaces

# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 

# read and process data  --------------------------------------------------
infile = "data/raw/PositiveSamples.csv"
outfile = "data/processed/PositiveSamples.csv"

df_gc <- read.csv (file=infile, header = TRUE, sep=",")

#  Select the columns needed
df_gc <- df_gc[complete.cases(df_gc), ] 
df_gc <- unite(df_gc, id, code, date, method, source, sep = ",")
names(df_gc)[names(df_gc) == "material"] <- "mat"

# output -------------------------------------------------------------------

# save file
write.csv (df_gc, file= outfile)
