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

soils<-ldply(lapply(soil_list, get_soil_data))

soils<-bind_rows(lapply(soil_list, get_soil_data))
