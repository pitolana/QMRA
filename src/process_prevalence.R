# Processing data on prevalence

# Data obtained from antibody surveys in different countries. The data is meant to be
# an estimate of the order of magnitude of the prevalence expected in the population. 

#   ---   Antibody surveys   ---  

         # Study           Place               % Seroprevalence  Date(latest test)   Start(estimate,Worldometers)
    #  @Bendavid2020   Santa Clara, Cal, USA         2.8             April 4          ~ March 1
    #  @Perez2020      Geneva, Switzerland           9.2             May 6            ~ March 1
    #  @Pollan2020     Spain                         5               May 11           ~ February 15
    #  @Erikstrup2020  Netherlands                   1.9             May 3            ~ March 1
    #  @Filho2020      Rio de Janeiro, Brazil        3.8             April 27         ~ March 1     

# Estimating prevalence at any given time
# Assuming each person is able to infect for a period of 14 days
#    Prev [%] =  Seroconversion [%] / (outbreak length [t] /duration of infection [t])

Prev_Cal <- 2.8 / (35/14) 
Prev_Swt <- 9.2 / (67/14)
Prev_Spn <- 5   / (85/14)
Prev_Nth <- 1.9 / (64/14)
Prev_Brz <- 3.8 / (58/14)

# Prevalence is between 0.4 (Netherlands) and 1.9 (Geneva). 
# Based on this data, three prevalence scenarios were suggested (0.2,1 and 5%)

low_prev  <- 0.002
med_prev  <- 0.01
high_prev <- 0.05
