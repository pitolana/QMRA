#   Set up

library(tidyverse) 
library(ggpubr)

# -----------------

# CONTROL     LOW
surf_freq_Low_0 <-  read.csv (file="data/processed/mask_freq_pLow_d0.csv", header = TRUE, sep=",")
cnt <- surf_freq_Low_0$x

# surfACE
surf_freq_Low_1 <-  read.csv (file="data/processed/surf_freq_pLow_d1.csv", header = TRUE, sep=",")
surf_freq_Low_2 <-  read.csv (file="data/processed/surf_freq_pLow_d2.csv", header = TRUE, sep=",")
surf_freq_Low_3 <-  read.csv (file="data/processed/surf_freq_pLow_d3.csv", header = TRUE, sep=",")

s_a <- surf_freq_Low_1$x
s_b <- surf_freq_Low_2$x
s_c <- surf_freq_Low_3$x

surf_freq_Low <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_freq_Low$intervention <- "surf_freqace"
surf_freq_Low$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_freq_Low$prevalence   <- "Low Prevalence (0.2%)"

# HAND
hand_freq_Low_1 <-  read.csv (file="data/processed/hand_freq_pLow_d1.csv", header = TRUE, sep=",")
hand_freq_Low_2 <-  read.csv (file="data/processed/hand_freq_pLow_d2.csv", header = TRUE, sep=",")
hand_freq_Low_3 <-  read.csv (file="data/processed/hand_freq_pLow_d3.csv", header = TRUE, sep=",")

h_a <- hand_freq_Low_1$x
h_b <- hand_freq_Low_2$x
h_c <- hand_freq_Low_3$x

hand_freq_Low <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_freq_Low$intervention <- "hand"
hand_freq_Low$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_freq_Low$prevalence   <- "Low Prevalence (0.2%)"

# mask
mask_freq_Low_1 <-  read.csv (file="data/processed/mask_freq_pLow_d1.csv", header = TRUE, sep=",")
mask_freq_Low_2 <-  read.csv (file="data/processed/mask_freq_pLow_d2.csv", header = TRUE, sep=",")
mask_freq_Low_3 <-  read.csv (file="data/processed/mask_freq_pLow_d3.csv", header = TRUE, sep=",")

m_a <- mask_freq_Low_1$x
m_b <- mask_freq_Low_2$x
m_c <- mask_freq_Low_3$x

mask_freq_Low <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_freq_Low$intervention <- "mask_freq"
mask_freq_Low$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_freq_Low$prevalence   <- "Low Prevalence (0.2%)"


# ------------
Low_prev <- rbind(cnt, hand_freq_Low, surf_freq_Low, mask_freq_Low)
Low_prev[1, 4] <- "-None"
Low_prev[1, 5] <- "-None"
Low_prev[1, 6] <- "Low Prevalence (0.2%)"

# - - - - - -
#plot_l <- ggplot(Low_prev, aes(x=condition, y=V2, color=intervention)) + 
#    geom_point()+
#    scale_y_continuous(trans='log10') +
#    ylab("Low Prevalence (0.2%)") +
#    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),legend.position="top") +
#    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
#                  position=position_dodge(0.05))
#print(plot_l)





















#### MEDIUM

# -----------------
# CONTROL     
surf_freq_Med_0 <-  read.csv (file="data/processed/surf_freq_pMed_d0.csv", header = TRUE, sep=",")
cnt <- surf_freq_Med_0$x

# surf_freqACE
surf_freq_Med_1 <-  read.csv (file="data/processed/surf_freq_pMed_d1.csv", header = TRUE, sep=",")
surf_freq_Med_2 <-  read.csv (file="data/processed/surf_freq_pMed_d2.csv", header = TRUE, sep=",")
surf_freq_Med_3 <-  read.csv (file="data/processed/surf_freq_pMed_d3.csv", header = TRUE, sep=",")

s_a <- surf_freq_Med_1$x
s_b <- surf_freq_Med_2$x
s_c <- surf_freq_Med_3$x

surf_freq_Med <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_freq_Med$intervention <- "surf_freqace"
surf_freq_Med$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_freq_Med$prevalence   <- "Medium Prevalence (1%)"

# HAND
hand_freq_Med_1 <-  read.csv (file="data/processed/hand_freq_pMed_d1.csv", header = TRUE, sep=",")
hand_freq_Med_2 <-  read.csv (file="data/processed/hand_freq_pMed_d2.csv", header = TRUE, sep=",")
hand_freq_Med_3 <-  read.csv (file="data/processed/hand_freq_pMed_d3.csv", header = TRUE, sep=",")

h_a <- hand_freq_Med_1$x
h_b <- hand_freq_Med_2$x
h_c <- hand_freq_Med_3$x

hand_freq_Med <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_freq_Med$intervention <- "hand"
hand_freq_Med$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_freq_Med$prevalence   <- "Medium Prevalence (1%)"

# mask_freq
mask_freq_Med_1 <-  read.csv (file="data/processed/mask_freq_pMed_d1.csv", header = TRUE, sep=",")
mask_freq_Med_2 <-  read.csv (file="data/processed/mask_freq_pMed_d2.csv", header = TRUE, sep=",")
mask_freq_Med_3 <-  read.csv (file="data/processed/mask_freq_pMed_d3.csv", header = TRUE, sep=",")

m_a <- mask_freq_Med_1$x
m_b <- mask_freq_Med_2$x
m_c <- mask_freq_Med_3$x

mask_freq_Med <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_freq_Med$intervention <- "mask_freq"
mask_freq_Med$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_freq_Med$prevalence   <- "Medium Prevalence (1%)"


# ------------
Med_prev <- rbind(cnt, hand_freq_Med, surf_freq_Med, mask_freq_Med)
Med_prev[1, 4] <- "-None"
Med_prev[1, 5] <- "-None"
Med_prev[1, 6]<- "Medium Prevalence (1%)"

# - - - - - -
# plot_m <- ggplot(Med_prev, aes(x=condition, y=V2, color=intervention)) + 
#    geom_point()+
#    theme_bw() +
#    scale_y_continuous(trans='log10') +
#    ylab("Medium Prevalence (1%)") +
#    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(),legend.position="top") +
#    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
# position=position_dodge(0.05))
# print(plot_m)



#   ------------
#### High

# -----------------
# - - - - - 
# CONTROL     

surf_freq_High_0 <-  read.csv (file="data/processed/surf_freq_pHigh_d0.csv", header = TRUE, sep=",")
cnt <- surf_freq_High_0$x


# surf_freqACE
surf_freq_High_1 <-  read.csv (file="data/processed/surf_freq_pHigh_d1.csv", header = TRUE, sep=",")
surf_freq_High_2 <-  read.csv (file="data/processed/surf_freq_pHigh_d2.csv", header = TRUE, sep=",")
surf_freq_High_3 <-  read.csv (file="data/processed/surf_freq_pHigh_d3.csv", header = TRUE, sep=",")

s_a <- surf_freq_High_1$x
s_b <- surf_freq_High_2$x
s_c <- surf_freq_High_3$x

surf_freq_High <- as.data.frame(rbind(s_a,s_b,s_c)) 
surf_freq_High$intervention <- "surf_freqace"
surf_freq_High$condition    <- c("Once (.7am)", "Once (12pm)", "Twice (7am,12pm)")
surf_freq_High$prevalence   <- "High Prevalence (5%)"

# HAND
hand_freq_High_1 <-  read.csv (file="data/processed/hand_freq_pHigh_d1.csv", header = TRUE, sep=",")
hand_freq_High_2 <-  read.csv (file="data/processed/hand_freq_pHigh_d2.csv", header = TRUE, sep=",")
hand_freq_High_3 <-  read.csv (file="data/processed/hand_freq_pHigh_d3.csv", header = TRUE, sep=",")

h_a <- hand_freq_High_1$x
h_b <- hand_freq_High_2$x
h_c <- hand_freq_High_3$x

hand_freq_High <- as.data.frame(rbind(h_a,h_b,h_c)) 
hand_freq_High$intervention <- "hand"
hand_freq_High$condition    <- c("Comp 25%", "Comp 50%", "Comp 75%")
hand_freq_High$prevalence   <- "High Prevalence (5%)"

# mask_freq
mask_freq_High_1 <-  read.csv (file="data/processed/mask_freq_pHigh_d1.csv", header = TRUE, sep=",")
mask_freq_High_2 <-  read.csv (file="data/processed/mask_freq_pHigh_d2.csv", header = TRUE, sep=",")
mask_freq_High_3 <-  read.csv (file="data/processed/mask_freq_pHigh_d3.csv", header = TRUE, sep=",")

m_a <- mask_freq_High_1$x
m_b <- mask_freq_High_2$x
m_c <- mask_freq_High_3$x

mask_freq_High <- as.data.frame(rbind(m_a,m_b,m_c)) 
mask_freq_High$intervention <- "mask_freq"
mask_freq_High$condition    <- c(".Comp 25%", ".Comp 50%", ".Comp 75%")
mask_freq_High$prevalence   <- "High Prevalence (5%)"


# ------------
High_prev <- rbind(cnt, hand_freq_High, surf_freq_High, mask_freq_High)
High_prev[1, 4] <- "-None"
High_prev[1, 5] <- "-None"
High_prev[1, 6] <- "High Prevalence (5%)"

# - - - - - -
#plot_h <- ggplot(High_prev, aes(x=condition, y=V2, color=intervention)) + 
#   geom_point()+
#   scale_y_continuous(trans='log10') +
#   ylab("High Prevalence (5%)") +
#   theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), legend.position = "top") +
#   geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
#   position=position_dodge(0.05))
#print(plot_h)







#####










#### All in a plot
All_prev <- rbind(Low_prev, Med_prev, High_prev)
All_prev$prevalence_f = factor(All_prev$prevalence, levels=c('Low Prevalence (0.2%)','Medium Prevalence (1%)','High Prevalence (5%)'))

plot_all <- ggplot(All_prev, aes(x=condition, y=V2, color=intervention)) + 
    geom_point()+
    scale_color_manual(values=c("red", "#D55E00","#0072B2", "#009E73"))+
    scale_y_log10(limits = c(1e-22,1e-2)) +
    theme_bw() +
    facet_grid(. ~ prevalence_f) +
    ylab("Risk of Infection") +
    theme(axis.text.x = element_text(angle = 90), axis.title.x = element_blank(), legend.position = "top") +
    geom_errorbar(aes(ymin=V1, ymax=V3), width=.5,
                  position=position_dodge(0.05))
print(plot_all)




