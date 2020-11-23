# Processing data on log reduction based on disinfection

# ------------------ Hand Disinfection data -----------------------------------

#   Article             virus (enveloped)     log10 Red     Sanitizer
# __________________  _____________________  __________  ________________________
# @Steinmann2012       vaccinia virus           4        3 Ethanol-based (45->90%)
# @Steinmann2010       BVDV and HCV             4        2 Alcohol-based (>75%)
# @Larson2012          Influenza                3.35     1 Alcohol Gel (62%)
# @Rabenau2005         SARS-CoV               > 4.25     4 Alcohol-based (>75%)

hand_dis <- 4.25 # Conservative assumption of alcohol based sanitizer on SARS

# Regarding Hand Hygene. It is recomended by the WHO that: 
  #  @WHO2010_HandHygene
  #  An effective alcohol-based hand rub product should contain between 60% and 80% of alcohol 
  #  its efficacy should be proven according to the European Norm 1500. These products can be purchased on the market,
  #  but can also be produced locally in pharmacies using the formula and instructions provided by WHO.





# ----------------- Surface Disinfection data ---------------------------------

#   Article             virus (enveloped)     log10 Red     Sanitizer
# __________________  _____________________  __________  ________________________
# @Hulkower2011         TGEV (coronavirus)     3.2        Ethanol (70%)
#                       MHV (coronavirus)      3.9        Ethanol (70%)
#                       TGEV (coronavirus)     0.6        Chlorine bleach (0.06%)  *Concentration too low*
#                       MHV (coronavirus)      0.4        Chlorine bleach (0.06%)  *Concentration too low*
# @Sattar1989           HCV-229E(human corona)  >3        Chlorine (0.1%)
#                       HCV-229E(human corona)  >3        Chlorine (0.5%) 

# surf_dis <- runif(simNum, 10^3, 10^4)