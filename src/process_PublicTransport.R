# Processing data on frequency of touching buttons
# Train schedules Aprox delta time

# Zurich
S9_Zh      <- 5  #(train) https://moovitapp.com/z%C3%BCrich-3524/lines/S9/350243/4574131/en?customerId=4908&ref=2&poiType=line
T3_Zh      <- 1  #(tram)  https://moovitapp.com/z%C3%BCrich-3524/lines/3/499595/1853852/en?customerId=4908&ref=2&poiType=line
Bus_280_Zh <- 2  #(bus)   https://moovitapp.com/z%C3%BCrich-3524/lines/280/499696/1854180/en?customerId=4908&ref=2&poiType=line
Zh_Gn      <- 20 #(train) https://www.sbb.ch/en/buying/pages/fahrplan/fahrplan.xhtml  (164 min 8 stops, Zurich-Geneva)

# London
Bus_201_Ln <- 1.5 #(bus)  https://tfl.gov.uk/bus/timetable/201?fromId=490010021E&direction=outbound&intervalId=0&selectedDate=mondaytofriday
Pad_Rd_Ln  <- 3.3 #(train)  23 min, 7 stations (23/7) Paddington-> Reading 
Ln_Br     <-  12   #(train) London to Birstol- 8 stops in 98 min

# Paris
T1_Par     <- 2   #(tram) (33 stops in 79 min) https://wikiroutes.info/es/paris#ab=48.86453-02.44266-48.94209-02.24506&var=1
M3_Par     <- 1   #(metro) https://wikiroutes.info/es/paris#ab=48.86453-02.44266-48.90015-02.30646&var=1
Bus_91     <- 2   #(bus) https://wikiroutes.info/es/paris#ab=48.83945-02.31911-48.85125-02.36992&var=1


dt_PublicTranspot <- runif(simNum, 1, 20)


# Trafic light button for pedestrians in busy London street
# Assumptions, every time is red for pedestrians people presses it. 
# Time based on the Traffic Signal Timing Manual (USA) https://ops.fhwa.dot.gov/publications/fhwahop08024/chapter6.htm#6.6
# Assumtopn based on traffic lights, people touches the light once every 2 min
dt_TrafficLight <- 2

