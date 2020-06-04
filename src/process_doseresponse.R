
# Processing the data from the Dose Response paramaters

# Data obtained from QMRAwiki http://qmrawiki.org/experiments/sars
    # Parameter developed from 2 different studies: 
        # @
        # @

# Assuming estimate = median = mean (normal distribution)
Q_50 <- 0.00246 
Q_5 <-  0.00135   
Q_95 <- 0.00459

# Estimating SD form percentiles 
k_sd <- round((Q_95 - Q_5)/(2 *qnorm(0.95)), 5)
k_mean <- Q_5

