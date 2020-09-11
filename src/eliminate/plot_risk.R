## plot _ Risk with and without disinfection ##

# Conditions
# Material:       Metal
# Humidity:       High
# Surface:        Buttons
# Inoculation:    Cough on hands followed by hand to surface transfer
# Disinfection:   YES


# setup ------------------------------------------------------------------------
# external libraries
library(tidyverse) 
library(plyr)
library(scales)

# input -------------------------------------------------------------------
# data file
infile = "data/processed/compilation_risk.csv"
outfile = "figures/plot_risk.pdf"

# process -----------------------------------------------------------------

# read data 
df_risk_dis <- read.csv (file=infile, header = TRUE, sep=",")

# Preparing the data
df_risk_dis <- gather(df_risk_dis, "prevalence", "risk", 2:4)
df_risk_dis$Disinfection <- as.character(df_risk_dis$Disinfection)

# Selecting only the median values
median_risk <- filter(df_risk_dis,Percentile ==0.50)

# Plot risk as a function of prevalence and disinfection

#    ----------version_1 --------- 
median_risk$prevalence <- revalue(median_risk$prevalence , c("pHigh"="5%", "pLow"="0.2%", "pMed"="1%"))
median_risk$Disinfection <- revalue(median_risk$Disinfection , c("0"="None","1"="Once: 7am", "2"="Once: 3pm", "4"="Twice: 12pm and 6pm"))

# x and y axis are transformed and formatted
p2 <- ggplot(median_risk, aes(prevalence, risk)) + geom_point(alpha = 1, aes(shape = Disinfection, color = Disinfection)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
                  scale_size_manual(values=c(2,7,7,7))+
    theme_bw()
p2



# --------- TEST PLOTS
temporal <- read.csv (file="data/processed/temporal.csv", header = TRUE, sep=",")

p3 <- ggplot(temporal, aes(Disinfection, pMed)) + geom_point(alpha = 1, size=8, aes(color = intervention)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw()
p3

p4 <- ggplot(temporal, aes(Disinfection, pLow)) + geom_point(alpha = 1, size=8, aes(color = intervention)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw()
p4

p5 <- ggplot(temporal, aes(Disinfection, pHigh)) + geom_point(alpha = 1, size=8, aes(color = intervention)) +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme_bw()
p5



# save
ggsave (plt_risk , filename = outfile, units = "in", dpi = 600)

                
                