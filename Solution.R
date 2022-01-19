library(data.table)
options(stringsAsFactors=FALSE)

flights_us_2005 <- read.csv("./hw2/DataExpo2009/2005.csv")
flights_us_2006 <- read.csv("./hw2/DataExpo2009/2006.csv")
flights_us_2007 <- read.csv("./hw2/DataExpo2009/2007.csv")

# airports data downloaded from https://ourairports.com/data/
airports <- read.csv("./hw2/us-airports.csv")
flights_us <- rbind(flights_us_2005,flights_us_2006)
flights_us <- rbind(flights_us,flights_us_2007)

flights.dt <- data.table(flights_us)
flights.dt <- flights.dt[Cancelled == 0][, .(Origin,DepDelay,ArrDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay)]

airports.dt <- data.table(airports)
airports.dt <- airports.dt[, .(iata_code, region_name, local_region)]
setkey(flights.dt,Origin)
setkey(airports.dt,iata_code)
flights.states.dt <- flights.dt[airports.dt, nomatch=0][, .(DepDelay,ArrDelay,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay, region_name, local_region)]
flights.states.dt <- flights.states.dt[, .(TotalDepDelay=sum(DepDelay ), TotalArrDelay=sum(ArrDelay, na.rm=TRUE), TotalCarrierDelay=sum(CarrierDelay), TotalWeatherDelay=sum(WeatherDelay),TotalNASDelay=sum(NASDelay), TotalSecurityDelay=sum(SecurityDelay), TotalLateAircraftDelay=sum(LateAircraftDelay)), by=local_region]
flights.states.dt[,MostTimeDelayReason := names(.SD)[max.col(.SD)], .SDcols = 4:8]
flights.states.dt[order(local_region)]
setnames(flights.states.dt, "local_region", "state")



library(usmap)
library(ggplot2)




# 1.Weather contribution to flights delays per state of origin in the years 2005-2007, %
# Weather contribution to flight delays map
flights.states.weather.dt <- flights.states.dt[, .(state, TotalCarrierDelay,TotalWeatherDelay,TotalNASDelay,TotalSecurityDelay,TotalLateAircraftDelay)]
flights.states.weather.dt[, WeatherDelayPercent := round((TotalWeatherDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2)]
flights.states.weather.df <- data.frame(flights.states.weather.dt)

plot_usmap(data = flights.states.weather.df, values = "WeatherDelayPercent", color = "firebrick", labels=TRUE) + 
  scale_fill_continuous(
    low = "white", high = "firebrick", name = "Weather contribution to flights delay in 2005-2007, %", label = scales::comma
  ) + theme(legend.position = "right")






# 2.Which reasons contributed to delays the most per state of origin in the years 2005-2007 ?
# Main reason of delay map
flights.df <- data.frame(flights.states.dt)
cols <- c("TotalCarrierDelay" = "coral", "TotalNASDelay" = "royalblue", "TotalLateAircraftDelay" = "seagreen", "TotalWeatherDelay" = "orange","TotalSecurityDelay" = "brown")

plot_usmap(data = flights.df, values = "MostTimeDelayReason", color = "black", labels=TRUE) + 
  scale_fill_manual(
    values=cols, 
    name = "Reasons that contributed the most to flight delays in 2005-2007",
    labels=c("Carrier delay", "NAS delay", "Late aircraft", "Weather", "Security")
  ) + theme(legend.position = "right")






# 3.Delay cause by year, % of total minutes, 2005-2007
# Contribution to delays per year
delays.dt <- data.table(flights_us)
delays.dt <- delays.dt[Cancelled == 0][, .(Year,CarrierDelay,WeatherDelay,NASDelay,SecurityDelay,LateAircraftDelay)]
delays.dt <- delays.dt[, .( TotalCarrierDelay=sum(CarrierDelay), TotalWeatherDelay=sum(WeatherDelay),TotalNASDelay=sum(NASDelay), TotalSecurityDelay=sum(SecurityDelay), TotalLateAircraftDelay=sum(LateAircraftDelay)), by=Year]
delays.dt <- delays.dt[, .(Year, Carrier = round((TotalCarrierDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2), Weather = round((TotalWeatherDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2), NAS = round((TotalNASDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2), Security= round((TotalSecurityDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2), LateAircraft = round((TotalLateAircraftDelay/(TotalCarrierDelay + TotalWeatherDelay + TotalNASDelay + TotalSecurityDelay + TotalLateAircraftDelay)) * 100, 2))]
delays.dt


