# setup ----
library(tidyverse)
library(lubridate)
library(modelr)
library(stringr)
library(aqp)
library(soilDB)


# read and select ars flux data ----
ars_fluxes<-read_csv("../data/ars/ars_fluxes.csv", skip = 2)

fluxes<-ars_fluxes%>%
  select(`Experimental Unit ID`, Date, N2O, Air_Temp, Soil_Temp, Soil_Moisture)%>%
  mutate(Date = mdy_hms(Date))%>%
  separate(`Experimental Unit ID`, c("state","exp"), sep = 3)

head(fluxes)

  
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


# read and select ars weather station data for longitude and latitude ----
ars_latlong<-read_csv("../data/ars/ars_latlong.csv")

latlong<-ars_latlong%>%
  select(`Experimental Unit`, `Latitude of weather station, decimal degrees`, `Longitude of weather station, decimal degrees`)%>%
  rename(lat = `Latitude of weather station, decimal degrees`, long = `Longitude of weather station, decimal degrees`)%>%
  separate(`Experimental Unit`, c("state","exp"), sep = 3)%>%
  filter(state != "ORP")

head(latlong)

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

head(daymet)
   

# get and add soil data ----  

our_soils<-soils%>%
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
  select(-exp.y)%>%
  mutate(year = year(date), month = month(date), day = yday(date))


head(ars)
---- 
    
  
# define peak by "two std dev above mean" for now ----
peaks<-ars%>%
  filter(N2O > 0)%>%
  select(-exp, -series)%>%
  group_by(site, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))
  
  group_by(site)%>%
  summarise(mean_flux=mean(N2O, na.rm = TRUE), std_dev_flux=sd(N2O, na.rm = TRUE))%>%
  mutate(peak_threshold = mean_flux + (2*std_dev_flux))%>%
  right_join(ars, by = "site")%>%
  filter(N2O > peak_threshold)%>%

  

ggplot(filter(peaks, site == "MNM"), aes(x=day, y=N2O, color=as.factor(year)))+
  geom_point()+
  geom_smooth()+
  facet_wrap(~ year)


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
  #group_by(site, date, year, month, day)%>%
  #summarise_each(funs(mean(., na.rm = TRUE)))%>%
  filter(site == "MNM" & year %in% 2004:2008)%>%
  ggplot(aes(x = day, y=N2O))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept=100)+
  facet_grid(year~site)

# looking only at sites that freeze ----
ars_cold<-ars %>% filter(site %in% c("INA", "INT", "INW", "MNM", "MNR", "NDM", "NEM", "NVN", "PAH"))%>%
  mutate(town = ifelse((site %in% c("INA", "INT", "INW")), "West_Lafayette",
                       ifelse((site == "MNM"), "Morris",
                              ifelse((site == "MNR"), "Roseville",
                                     ifelse((site %in% c("NDM", "NEM", "NVN")), "Mandan",
                                            ifelse((site == "PAH"), "University_Park", "NA" ))))))
ars_cold%>%
  select(-exp, -series)%>%
  group_by(site, town, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  filter(year %in% 2004:2011)%>%
  ggplot(aes(x = day, y=N2O))+
  geom_point()+
  geom_smooth()+
  geom_vline(xintercept=100)+
  facet_grid(year~site)

ars_cold%>%
  select(-exp, -series)%>%
  group_by(site, town, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  filter(year %in% 2004:2011)%>%
  ggplot((aes(x = town, y = N2O)))+
  geom_jitter(alpha = .3) 

ars_cold%>%
  select(-exp, -series)%>%
  group_by(site, town, date, year, month, day)%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
ggplot(aes(N2O, fill=town, color=town))+
  geom_density(alpha =.1)+
  xlim(-15, 35)+
  facet_wrap(~year)

  

#define time period of interest lazy way for now (May 30) ----
   #safe to say it doesn't freeze anywhere after 150 days
ars_cold%>%
  filter(day<150)%>%
  ggplot(aes(x=day, y=min_temp))+
  geom_point()+
  geom_hline(yintercept=0)+
  facet_grid(site~year)

  #we end up with the average N2O for each site, each year (response attempt #1)
ars_spring<-ars_cold%>% 
  filter(day<150)%>%
  select(-exp, -series)%>%
  group_by(site, town, year)%>%
  summarise(avg_N2O = (mean(N2O, na.rm = TRUE)))

ggplot(ars_spring, aes(x=year,y=avg_N2O))+
  geom_point()+
  facet_wrap(~site)

#Calculate number of days 0C was reached ----
   #need to re-partition year June-June (keep winter period together)
   
ars_cold<-ars_cold%>%
  mutate(spring_year = ifelse((date >"2003-06-02" & date < "2004-06-01"), 2004,
                        ifelse((date >"2004-06-02" & date < "2005-06-01"), 2005,
                               ifelse((date >"2005-06-02" & date < "2006-06-01"), 2006,
                                      ifelse((date >"2006-06-02" & date < "2007-06-01"), 2007,
                        ifelse((date >"2007-06-02" & date < "2008-06-01"), 2008,
                          ifelse((date >"2008-06-02" & date < "2009-06-01"), 2009,
                             ifelse((date >"2009-06-02" & date < "2010-06-01"), 2010,
                                ifelse((date >"2010-06-02" & date < "2011-06-01"), 2011, 0)))))))))

ars_fdd<-ars_cold%>%
  filter(day<150)%>%
  mutate(fdd = ifelse((min_temp <0), 1, 0))%>%
  group_by(spring_year, site, town)%>%
  mutate(cum_fdd = cumsum(fdd))%>%
  summarise(annual_fdd = max(cum_fdd))%>%
  rename(year = spring_year)%>%
  right_join(ars_spring, by = c("year", "site", "town"))
###########So, now we have ars_fdd as our dataframe for modeling#########

#But we could also define fdd by cumulative degrees

 #Hey, try cumulative degress on days <0 -tomorrow

ars_degrees<-ars_cold%>%
  filter(day<150)%>%
  group_by(spring_year, site, town)%>%
  mutate(cum_degrees = cumsum(min_temp))%>%
  summarise(annual_degrees = max(cum_degrees))%>%
  rename(year = spring_year)%>%
  right_join(ars_spring, by = c("year", "site", "town"))

#Guess I could just put them together

ars_for_mod<-ars_fdd%>%
  left_join(ars_degrees, by = c("year", "site", "town", "avg_N2O"))

ggplot(nomo, aes(x=log(annual_degrees), y = log(avg_N2O)))+
  geom_point()

#Take out Morris, MN because it is very different from all the other sites

nomo<-ars_for_mod%>%
  filter(site != "MNM")%>%
  na.omit()

nomomod<-lm(log(avg_N2O) ~ log(annual_fdd), data=nomo)






