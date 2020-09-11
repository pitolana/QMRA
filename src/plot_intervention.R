#   Set up

library(tidyverse) 
library(ggpubr)


# -----------------
# - - - - - 
# CONTROL     LOW

surf_Low_0 <-  read.csv (file="data/processed/surf_pLow_d0.csv", header = TRUE, sep=",")
cnt <- surf_Low_0$x


# SURFACE
surf_Low_1 <-  read.csv (file="data/processed/surf_pLow_d1.csv", header = TRUE, sep=",")
surf_Low_2 <-  read.csv (file="data/processed/surf_pLow_d2.csv", header = TRUE, sep=",")
surf_Low_3 <-  read.csv (file="data/processed/surf_pLow_d3.csv", header = TRUE, sep=",")

s_a <- surf_Low_1$x
s_b <- surf_Low_2$x
s_c <- surf_Low_3$x

surf_Low <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_Low$intervention <- "surface"
surf_Low$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_Low$prevalence   <- "Low Prevalence"

# HAND
hand_Low_1 <-  read.csv (file="data/processed/hand_pLow_d1.csv", header = TRUE, sep=",")
hand_Low_2 <-  read.csv (file="data/processed/hand_pLow_d2.csv", header = TRUE, sep=",")
hand_Low_3 <-  read.csv (file="data/processed/hand_pLow_d3.csv", header = TRUE, sep=",")

h_a <- hand_Low_1$x
h_b <- hand_Low_2$x
h_c <- hand_Low_3$x

hand_Low <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_Low$intervention <- "hand"
hand_Low$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_Low$prevalence   <- "Low Prevalence"

# MASK
mask_Low_1 <-  read.csv (file="data/processed/mask_pLow_d1.csv", header = TRUE, sep=",")
mask_Low_2 <-  read.csv (file="data/processed/mask_pLow_d2.csv", header = TRUE, sep=",")
mask_Low_3 <-  read.csv (file="data/processed/mask_pLow_d3.csv", header = TRUE, sep=",")

m_a <- mask_Low_1$x
m_b <- mask_Low_2$x
m_c <- mask_Low_3$x

mask_Low <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_Low$intervention <- "mask"
mask_Low$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_Low$prevalence   <- "Low Prevalence"


# ------------
Low_prev <- rbind(cnt, hand_Low, surf_Low, mask_Low)
Low_prev[1, 4] <- "-None"
Low_prev[1, 5] <- "-None"
Low_prev[1, 6] <- "Low Prevalence"

# - - - - - -
#plot_l <- ggplot(Low_prev, aes(x=condition, y=V2, color=intervention)) + 
#    geom_point()+
#    scale_y_continuous(trans='log10') +
#    ylab("Low prevalence") +
#    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),legend.position="top") +
#    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
#                  position=position_dodge(0.05))
#print(plot_l)





















#### MEDIUM

# -----------------
# - - - - - 
# CONTROL     

surf_Med_0 <-  read.csv (file="data/processed/surf_pMed_d0.csv", header = TRUE, sep=",")
cnt <- surf_Med_0$x


# SURFACE
surf_Med_1 <-  read.csv (file="data/processed/surf_pMed_d1.csv", header = TRUE, sep=",")
surf_Med_2 <-  read.csv (file="data/processed/surf_pMed_d2.csv", header = TRUE, sep=",")
surf_Med_3 <-  read.csv (file="data/processed/surf_pMed_d3.csv", header = TRUE, sep=",")

s_a <- surf_Med_1$x
s_b <- surf_Med_2$x
s_c <- surf_Med_3$x

surf_Med <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_Med$intervention <- "surface"
surf_Med$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_Med$prevalence   <- "Medium Prevalence"

# HAND
hand_Med_1 <-  read.csv (file="data/processed/hand_pMed_d1.csv", header = TRUE, sep=",")
hand_Med_2 <-  read.csv (file="data/processed/hand_pMed_d2.csv", header = TRUE, sep=",")
hand_Med_3 <-  read.csv (file="data/processed/hand_pMed_d3.csv", header = TRUE, sep=",")

h_a <- hand_Med_1$x
h_b <- hand_Med_2$x
h_c <- hand_Med_3$x

hand_Med <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_Med$intervention <- "hand"
hand_Med$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_Med$prevalence   <- "Medium Prevalence"

# MASK
mask_Med_1 <-  read.csv (file="data/processed/mask_pMed_d1.csv", header = TRUE, sep=",")
mask_Med_2 <-  read.csv (file="data/processed/mask_pMed_d2.csv", header = TRUE, sep=",")
mask_Med_3 <-  read.csv (file="data/processed/mask_pMed_d3.csv", header = TRUE, sep=",")

m_a <- mask_Med_1$x
m_b <- mask_Med_2$x
m_c <- mask_Med_3$x

mask_Med <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_Med$intervention <- "mask"
mask_Med$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_Med$prevalence   <- "Medium Prevalence"


# ------------
Med_prev <- rbind(cnt, hand_Med, surf_Med, mask_Med)
Med_prev[1, 4] <- "-None"
Med_prev[1, 5] <- "-None"
Med_prev[1, 6]<- "Medium Prevalence"

# - - - - - -
#plot_m <- ggplot(Med_prev, aes(x=condition, y=V2, color=intervention)) + 
#    geom_point()+
#    scale_y_continuous(trans='log10') +
#    ylab("Medium prevalence") +
#    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),legend.position="top") +
#    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
                 # position=position_dodge(0.05))
#print(plot_m)










#   ------------
#### High

# -----------------
# - - - - - 
# CONTROL     

surf_High_0 <-  read.csv (file="data/processed/surf_pHigh_d0.csv", header = TRUE, sep=",")
cnt <- surf_High_0$x


# SURFACE
surf_High_1 <-  read.csv (file="data/processed/surf_pHigh_d1.csv", header = TRUE, sep=",")
surf_High_2 <-  read.csv (file="data/processed/surf_pHigh_d2.csv", header = TRUE, sep=",")
surf_High_3 <-  read.csv (file="data/processed/surf_pHigh_d3.csv", header = TRUE, sep=",")

s_a <- surf_High_1$x
s_b <- surf_High_2$x
s_c <- surf_High_3$x

surf_High <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_High$intervention <- "surface"
surf_High$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_High$prevalence   <- "High Prevalence"

# HAND
hand_High_1 <-  read.csv (file="data/processed/hand_pHigh_d1.csv", header = TRUE, sep=",")
hand_High_2 <-  read.csv (file="data/processed/hand_pHigh_d2.csv", header = TRUE, sep=",")
hand_High_3 <-  read.csv (file="data/processed/hand_pHigh_d3.csv", header = TRUE, sep=",")

h_a <- hand_High_1$x
h_b <- hand_High_2$x
h_c <- hand_High_3$x

hand_High <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_High$intervention <- "hand"
hand_High$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_High$prevalence   <- "High Prevalence"

# MASK
mask_High_1 <-  read.csv (file="data/processed/mask_pHigh_d1.csv", header = TRUE, sep=",")
mask_High_2 <-  read.csv (file="data/processed/mask_pHigh_d2.csv", header = TRUE, sep=",")
mask_High_3 <-  read.csv (file="data/processed/mask_pHigh_d3.csv", header = TRUE, sep=",")

m_a <- mask_High_1$x
m_b <- mask_High_2$x
m_c <- mask_High_3$x

mask_High <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_High$intervention <- "mask"
mask_High$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_High$prevalence   <- "High Prevalence"


# ------------
High_prev <- rbind(cnt, hand_High, surf_High, mask_High)
High_prev[1, 4] <- "-None"
High_prev[1, 5] <- "-None"
High_prev[1, 6] <- "High Prevalence"

# - - - - - -
#plot_h <- ggplot(High_prev, aes(x=condition, y=V2, color=intervention)) + 
 #   geom_point()+
 #   scale_y_continuous(trans='log10') +
 #   ylab("High prevalence") +
 #   theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), legend.position = "top") +
 #   geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
               #   position=position_dodge(0.05))
#print(plot_h)







#####










#### All in a plot
All_prev <- rbind(Low_prev, Med_prev, High_prev)
All_prev$prevalence_f = factor(All_prev$prevalence, levels=c('Low Prevalence','Medium Prevalence','High Prevalence'))

plot_all <- ggplot(All_prev, aes(x=condition, y=V2, color=intervention)) + 
    geom_point()+
    scale_y_continuous(trans='log10') +
    facet_grid(. ~ prevalence_f) +
    ylab("Risk of Infection") +
    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), legend.position = "top") +
    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
                  position=position_dodge(0.05))
print(plot_all)




