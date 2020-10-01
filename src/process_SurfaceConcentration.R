# Processing data on concentrations of SARS-CoV-2 on public surfaces

# ------------------ Surface contamination data ---------------------------------

#                                                                concentration
#   Article             method      place          surface       gene copies/m2   
# _________________  __________  _____________  _____________    ______________
# @Abrahao2020          qPCR     Hospital bus   Bench (metal)         620  
#  (Brazil)                      Hospital bus   Bench (metal)         80 
#                                Hospital bus   Bench (metal)         70 
#                                Hospital bus   Bench (metal)         150 
#                                Hospital bus   Ground(concrete)      20
#                                Hospital bus   Ground(concrete)      60

#                                Hospital front Ground(concrete)      2990

#                                Bus terminal   Handrail (metal)      200
#                                Bus terminal   Handrail (metal)      50   
#                                Bus terminal   Handrail (metal)      2110
#                                Bus terminal   Handrail (metal)      520
#                                Bus terminal   Handrail (metal)      70
#                                Bus terminal   Handrail (metal)      2080
#
#                                Public square  Table (concrete)      200
#                                Public square  Bench (concrete)      20
#                                Public square  Bench (concrete)      70
#                                Public square  Bench (concrete)      70


# Seventeen of the 101 samples tested positive (16.8%) for SARS-CoV-2 RNA


Surface_Concentrations_Brazil_m2 <- c(620,80,70,150,20,60,2990,200,50,2110,520,70,2080,200,20,70,70, rep(0, 84))
Surface_Concentrations_Brazil <- Surface_Concentrations_Brazil_m2/10000 #conversing to cm2
Surface_Concentrations_Brazil_LOD <- 10   # it doesn't say, so I just made it up to see how it looks like

    
    