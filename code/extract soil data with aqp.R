library(aqp)
library(soilDB)
library(plyr)

get_soil_data <- function(series){
  horizons(fetchKSSL(series))%>%
    select(pedon_key, hzn_desgn, sand, silt, clay, oc, ph_h2o)%>%
    filter(grepl('A', hzn_desgn))%>%
    select(-c(hzn_desgn, pedon_key))%>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%
    mutate(soil = series)
  
}

soil_list<-c("howard", "webster", "canisteo")

soils<-bind_rows(lapply(soil_list, get_soil_data))

try<-ldply(lapply(our_soils, get_soil_data))


get_soil_data <- function(series){
  horizons(fetchKSSL(series))%>%
    select(pedon_key, hzn_desgn, sand, silt, clay, oc, ph_h2o)%>%
    filter(grepl('A', hzn_desgn))%>%
    select(-c(hzn_desgn, pedon_key))%>%
    summarise_each(funs(mean(., na.rm = TRUE)))%>%
    mutate(soil = series)
  
}  

soil_list<-c("Goldsboro","Noboco")

soils<-ldply(lapply(soil_list, get_soil_data))

horizons(fetchKSSL("Summerton"))%>%
  select(pedon_key, hzn_desgn, sand, silt, clay, oc, ph_h2o)%>%
  filter(grepl('A', hzn_desgn))%>%
  select(-c(hzn_desgn, pedon_key))%>%
  summarise_each(funs(mean(., na.rm = TRUE)))%>%
  mutate(soil = "Summerton")  

myfun <- function(x) {if (x == 1) {stop("bad")} else x}
lapply(1:4, myfun)  # stops from error
L <- lapply(1:4, function(x) try(myfun(x)))

                     And then you can use Filter to get rid of the "bad" cases:
                     
                     Filter(function(x) !inherits(x, "try-error"), L)