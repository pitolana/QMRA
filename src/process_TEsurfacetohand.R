
# Processing data of virus transfer from surface to hand 
# Data obtained from the article (Lopez, 2013, doi: 10.1128/AEM.01030-13)

# setup -------------------------------------------------------------------
# external libraries

# read and process data  --------------------------------------------------
infile = "data/raw/TEsurfacetohand.csv"
outfile = "data/processed/TEsurfacetohand.csv"

# read data 
df_TE_sh <- read.csv (file=infile, header = TRUE, sep=",")
df_TE_sh$pt_mean <- df_TE_sh$pt_mean / 100
df_TE_sh$pt_sd <- df_TE_sh$pt_sd / 100
df_TE_sh$pt_min <- df_TE_sh$pt_min / 100
df_TE_sh$pt_max <- df_TE_sh$pt_max / 100

# Extracting data for steel at high humidity (40-65)
TE_sh_stl_mean <- df_TE_sh[14,3]
TE_sh_stl_sd <- df_TE_sh[14,4]

# Extracting data for plastic at high humidity (40-65)
TE_sh_pl_mean <- df_TE_sh[10,3]
TE_sh_pl_sd <- df_TE_sh[10,4]

#  -------------------------------------------------------------------

# read and process data  --------------------------------------------------
#infile = "data/raw/TEsurhandtosurface.csv"

# read data 
#df_TE_hs <- read.csv (file=infile, header = TRUE, sep=",")
#df_TE_hs$pt_mean <- df_TE_hs$pt_mean / 100
#df_TE_hs$pt_sd <- df_TE_hs$pt_sd / 100
#df_TE_hs$pt_min <- df_TE_hs$pt_min / 100
#df_TE_hs$pt_max <- df_TE_hs$pt_max / 100

# Extracting data for steel at high humidity (40-65)
#TE_hs_stl_mean <- df_TE_hs[X,X]
#TE_hs_stl_sd <- df_TE_hs[X,X]

# Extracting data for plastic at high humidity (40-65)
#TE_hs_pl_mean <- df_TE_hs[X,X]
#TE_hs_pl_sd <- df_TE_hs[X,X]



