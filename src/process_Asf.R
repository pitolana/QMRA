# Processing finger area taken from different papers 

#  ------------------------------Method 1 ----------------------------------
# Method used by Julian et. all 2018. @Julian2018
# Fractional surface area taken from @AuYeung2008 (adult front partial finger)
# Average hand surface area taken from EPA 2011 @EPA2011

FA_min <- 0.04
FA_max <- 0.06    

# Hand surface area (average of the area of 2 hands, EPA 2011 page xiv), 1000 <- m2 to cm2
HSA_women <- (0.089 * 10000)/2
HSA_men <-  (0.107  * 10000) /2

HSA <- mean(HSA_women:HSA_men) # average men/women area (cm2) 

# Area of finger (uniform using min and max of front partial finger). 
#It's divided by 5 because the FA is mean to be for 5 fingers
Asf_min <- (HSA * FA_min)/5
Asf_max <- (HSA * FA_max)/5

#  ------------------------------Method 2 ----------------------------------
#   Data from the paper @Chabrelie2018, cm2. He used a combination of data from elsewhere
Asf_median <- 4.58 #@Chabrelie2018

Asf_murai <- 6.66 # volar area of the index finger, 7 fingers from cadavers, @Murai1996
Asf_peters <- (3.55 + 4.25)/2  # Index finger distal phalanx female and male, @Peters2009a
Asf_sahmel <- 3.8 # Six participants, 3 fingers each, @Sahmel2015

#Asf_min <- Asf_sahmel
#Asd_max <- Asf_murai

# output ---------------------------------------------------------------------
# Both methods lead to similar results, min of 3.8 and 3.9, 
# and max of 6.6 and 5.8 cm2. Method 1 was selected. 

# save file
# write.csv (df_XXX, file= outfile)