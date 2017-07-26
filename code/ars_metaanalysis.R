# setup ----
knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE,
  include=FALSE
)

library(tidyverse)
library(lubridate)
library(stringr)
library(aqp)
library(soilDB)
----

# read and select ars flux data ----
ars_fluxes<-read_csv("../data/ars/ars_fluxes.csv", skip = 2)

fluxes<-ars_fluxes%>%
  select(`Experimental Unit ID`, Date, N2O, Air_Temp, Soil_Temp, Soil_Moisture)%>%
  mutate(Date = mdy_hms(Date))%>%
  separate(`Experimental Unit ID`, c("state","exp"), sep = 3)

head(fluxes)
----
  
# read and select ars weather data ----
ars_weather<-read_csv("../data/ars/ars_weather.csv", skip = 2)

weather<-ars_weather%>%
  select(`Site ID`, Date, `Temperate, air, daily, maximum, degrees Celsius`, `Temperate, air, daily, minimum, degrees Celsius`, Precip, `Total_Net_Radn`)%>%
  mutate(Date = mdy_hms(Date))%>%
  separate(`Site ID`, c("state","exp"), sep = 3)%>%
  filter(state != "ORP")%>%
  select(-exp)%>%
  mutate(Total_Net_Radn = as.numeric(Total_Net_Radn))

head(weather)
----   

# read and select ars weather station data for longitude and latitude ----
ars_latlong<-read_csv("../data/ars/ars_latlong.csv")

latlong<-ars_latlong%>%
  select(`Experimental Unit`, `Latitude of weather station, decimal degrees`, `Longitude of weather station, decimal degrees`)%>%
  rename(lat = `Latitude of weather station, decimal degrees`, long = `Longitude of weather station, decimal degrees`)%>%
  separate(`Experimental Unit`, c("state","exp"), sep = 3)%>%
  filter(state != "ORP")

head(latlong)
----   

# read and select soils series for experiments ----
ars_soilseries<-read_csv("../data/ars/ars_soilseries.csv")

soils<-ars_soilseries%>%
  select(`Experimental Unit`, `Soil series`)%>%
  rename(series = `Soil series`)%>%
  separate(`Experimental Unit`, c("state","exp"), sep = 3)%>%
  filter(state != "ORP")%>%
  #select(-exp)%>%
  distinct()%>%
  mutate(series = word(series, 1))%>%
  na.omit()

head(soils)
----

# read and select daymet ars data ----
daymet_mandan<-read_csv("../data/ars/daymet_mandan.csv", skip = 7)
daymet_mandan$town<-"Mandan"
daymet_morris<-read_csv("../data/ars/daymet_morris.csv", skip = 7)
daymet_morris$town<-"Morris"
daymet_roseville<-read_csv("../data/ars/daymet_roseville.csv", skip = 7)
daymet_roseville$town<-"Roseville"
daymet_university_park<-read_csv("../data/ars/daymet_university_park.csv", skip = 7)
daymet_university_park$town<-"University_Park"
daymet_west_lafayette<-read_csv("../data/ars/daymet_west_lafayette.csv", skip = 7)
daymet_west_lafayette$town<-"West_Lafayette"

daymet<-rbind(daymet_mandan, daymet_morris, daymet_roseville, daymet_university_park, daymet_west_lafayette)

daymet$date<-as.Date(strptime(paste(daymet$year, daymet$yday), format="%Y%j"))
colnames(daymet)<-c("year", "yday", "daymet_prcp", "daymet_radn", "daymet_tmax", "daymet_tmin", "town", "date")

#daymet<-mutate(daymet, if_else )

head(daymet)
   

# get and add soil data ----  

our_soils<-ars%>%
  distinct(series)%>%
  mutate(dom_soil = word(series, 1))%>%
  distinct(dom_soil)%>%
  na.omit()

get_soil_data <- function(series){
  horizons(fetchKSSL(series))%>%
    select(pedon_key, hzn_desgn, sand, silt, clay, oc, ph_h2o)%>%
    filter(grepl('A', hzn_desgn))%>%
    select(-c(hzn_desgn, pedon_key))%>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%
    mutate(series = series)  
}  

brute_errors<-lapply(our_soils$dom_soil, function(series) try(get_soil_data(series)))

ars_soils<-bind_rows(Filter(function(series) !inherits(series, "try-error"), brute_errors))
----  

# join and filter fluxes, weather, and location data ----
ars<-left_join(weather, fluxes, by=c("state", "Date"))
ars<-left_join(ars, soils, by = c("state", "exp"))
ars<-left_join(ars, ars_soils, by = "series" )
ars<-left_join(ars, latlong, by = "state")


colnames(ars)<-c("site", "date", "max_temp", "min_temp", 
                 "precip", "radn", "exp", "N2O", "air_temp",
                 "soil_temp", "soil_moisture", "series", 
                 "sand", "silt", "clay", "oc", "ph_h2o", "exp.y", "lat", "long")

ars<-ars%>%
  select(exp.y)%>%
  mutate(year = year(date), month = month(date), day = yday(date))


head(ars)
---- 
    
  
# define peak by "above average" for now ----
peaks<-ars%>%
  group_by(site)%>%
  summarise(mean_flux=mean(N2O, na.rm = TRUE))%>%
  right_join(ars, by = "site")%>%
  select(-exp, -series)%>%
  group_by(site, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  filter(N2O > mean_flux)

ggplot(peaks, aes(x=date, y=N2O))+
  geom_point()+
  facet_wrap(~ site)


# split sites by when peaks occur, kind of dumb ----

mutate(depth = ifelse((depth == 2.5), 0,
                      ifelse((depth == 10.0), 10,
                             ifelse((depth == 22.5), 20,
                                    ifelse((depth == 45.0), 45,
                                           ifelse((depth == 80.0), 80, 105))))))

peaks%>%
  mutate(season = ifelse((month %in% c(1,2,3,4,10,11,12)), "cold", "warm"))%>%
ggplot(aes(x=site, y=N2O))+
  geom_boxplot()+
  facet_wrap(~season)  

# how R fits a model of N2O over time by site ----

ars%>%
  select(-exp, -series)%>%
  group_by(site, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  #filter(site == "MNM")%>%
  ggplot(aes(x = day, y=N2O))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept=100)+
  facet_wrap(~site)

# 
