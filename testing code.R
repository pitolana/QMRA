# Generating disinfection times, if t_dis=0 it means no disinfection, then the vector is full of 0
if (t_dis==0) {
    time_dis <- rep(0, length(time))
} else {
    dis_times <- 960 /t_dis  # 960 is the number of minutes in a working day (16 h times 60 min)
    time_dis <- rep(c(1,rep(0,ceiling(length(time)/dis_times)-1)), ceiling(length(time)/ceiling(length(time)/dis_times)))
    time_dis <- time_dis[1:length(time)]   
}





#------------------

# Generating times, starting from 7 am, adding delta time, ultil 11 pm, then add 8 hours and start again
a <- 7*60 # Starting time = 7 am, in minutes
c <- 1
time = c()
day_number = 0 # initializing the couting of days
days <- 7  # Simulation days
while(a < (60*24*days)) {
    if( (a - (day_number*24*60 + 60*23)) >= 0 ) {
        day_number = day_number + 1
        a = a + 8 * 60 
    } else { 
        a = a + 30}
    time[c] <- a
    c = c + 1 
}


t_dis <- 1
# Generating disinfection times
chunk <- length(time)/days
if (t_dis==0) {
    time_dis <- rep(0, length(time)) # No disinfection
} else if (t_dis==1) {
    time_dis <- rep(c(1,rep(0,(chunk-1))),days) # Morning (7am) disinfection
} else if (t_dis==2) {
    time_dis <- rep(c(rep(0,ceiling(chunk/2)-1),1,rep(0,floor(chunk/2))),days) # Middle of day (3pm) disinfection
} else if (t_dis==3) {
    time_dis <- rep(c(rep(0,(chunk-1)),1),days) # Nigth (11pm) disinfection
} else {
    time_dis <- rep(c(rep(0,ceiling(chunk/3)-1),1,rep(0,floor(chunk/3)),1,rep(0,ceiling(chunk/3)-1)),days) # Twice a day ~12 and ~6pm
}


# Generaring the vector of if infected
prevalence <- rbinom (length(time), 1, df_stl_btn$prev[p])


# Create the risk matrix for the conditions
daily_risk <- cbind(time, time_dis, prevalence)



-----------------------------------
    
    
    
# Generating times, starting from 7 am, adding delta time, ultil 11 pm, then add 8 hours and start again
a <- 7*60 # Starting time = 7 am, in minutes
c <- 1
time <- c()
#days <- 7  # Simulation days
for (i in 0:6) {
  if (a<= 24*60*i + 23*60) {
      a = a + 30
      time[c] <- a
      c = c+ 1
  }  else {
      i = i + 1
      a = 24*60*i + 7*60
      time[c] <- a
      c = c + 1
      }
}




a <- 7*60 # Starting time = 7 am, in minutes
c <- 1
time_chunk <- c()
while (a< 23*60) {
  a = a + 40
  time_chunk[c] <- a
  c = c+ 1  
}

  time_rep <-rep(time_chunk, times = 7)
  days_time <- c(rep(0,length(time_chunk)), rep(1,length(time_chunk)),
                 rep(2,length(time_chunk)), rep(3,length(time_chunk)),
                 rep(4,length(time_chunk)), rep(5,length(time_chunk)),
                 rep(6,length(time_chunk)))
  days_time <- days_time * 60 * 24
  time <- time_rep + days_time
  
  
  start <- 7 #start-time
  end <- 23 #end-time
  dt <- 30 #time-interval
  t <- c() #empty-vector
  days <- 7 #number-of-days
  for (i in 1:days) {
    temp <- seq(60*start,60*end,dt) + (i-1)*1440
    t <- append(t,temp,length(t))
  }
  print(t)

  
  
  
  t_dis <-1
  days <-7
  # Generating times, starting from 7 am, adding delta time, ultil 11 pm, then add 8 hours and start again
  start <- 7 #start-time
  end <- 23 #end-time
  time <- c() #empty-vector
  for (i in 1:days) {
    temp <- seq(60*start,60*end,120) + (i-1)*1440
    time <- append(time,temp,length(time))
  }
  
  # Generating disinfection times
  chunk <- length(time)/days
  if (t_dis==0) { # No disinfection
    time_dis <- rep(0, length(time)) 
  } else if (t_dis==1) { # Morning (7am) disinfection
    time_dis <- rep(c(1,rep(0,(chunk-1))),days) 
  } else if (t_dis==2) { # Middle of day (3pm) disinfection
    time_dis <- rep(c(rep(0,ceiling(chunk/2)-1),1,rep(0,floor(chunk/2))),days) 
  } else if (t_dis==3) { # Nigth (11pm) disinfection
    time_dis <- rep(c(rep(0,(chunk-1)),1),days) 
  } else {               # Twice a day ~12 and ~6pm
    time_dis <- rep(c(rep(0,ceiling(chunk/3)-1),1,rep(0,floor(chunk/3)),1,rep(0,ceiling(chunk/3)-1)),days)
  }
  
  daily_risk <- cbind(time, time_dis)
  daily_risk <- as.data.frame(daily_risk)
  
  
  
  
  
  daily_risk$Csurface <- c()
  daily_risk$Cfng <- c()
  daily_risk$Csurface2 <- c()
  
  daily_risk$Csurface [1] <- daily_risk$Loading[1]  # Initialize with the first loading at 7 am. 
  daily_risk$Cfng [1] <- daily_risk$Csurface [1] * 0.1
  daily_risk$Csurface2 [1] <- if (daily_risk$time_dis==0) {daily_risk$Csurface [1] - daily_risk$Cfng [1]
  } else { (daily_risk$Csurface [1] - daily_risk$Cfng [1]) / surf_dis[p]}
  
  
  for (i in 2:length(daily_risk$Csurface)) {
    daily_risk$Csurface[i] <- daily_risk$Loading[i] + (daily_risk$Csurface2[i-1] * exp(-df_stl_btn$n_stl[p] * (daily_risk$time [i]-daily_risk$time[i-1])))
    daily_risk$Cfng[i] <- daily_risk$Csurface[i] * df_stl_btn$TEsh_stl[p] 
    daily_risk$Csurface2[i] <- if (daily_risk$time_dis[i]==0) {daily_risk$Csurface [i] - daily_risk$Cfng [i]
    } else { (daily_risk$Csurface [i] - daily_risk$Cfng [i]) / surf_dis[p]}
  }