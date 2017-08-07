
library(readr)
library(dplyr)
library(lubridate)
library(rgdal)
# install.packages("NISTunits", dependencies = TRUE)
library(RNetCDF)
library(rgdal)
library(ncdf4)
library(raster)
library(stringr)
library(NISTunits)
library(raster)
library(leaflet)
library(htmlwidgets)
library(webshot)
library(ggplot2)

setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER")
# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER")


#### importing the UAE shapefile to use as extent ####################################

dir <- "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"
### shapefile for UAE
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")

# ----- Transform to EPSG 4326 - WGS84 (required)
shp_UAE <- spTransform(shp_UAE, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# names(shp)

plot(shp_UAE)

######################################################################################
######################################################################################
# list met files #####################################################################
######################################################################################

path <- ".csv$"
filenames <- list.files(pattern = path)

# subset data from 29 March to 4 April 2015 (DUST event)
# make hourly averages of all the variables


# remove rows with NA from the DateTime column
# DB_NCMS <- DB_NCMS[!(is.na(DB_NCMS$DateTime)), ]

# filenames <- filenames[4]
# i <- 35

All_AWS_data <- NULL

for (i in 1:length(filenames)) {
  
  station <- str_sub(filenames[i], start = 1, end = -10)
  if ( station %in% c("Makassib", "Qarnen", "Dalma", "Arylah", "Sir Bani Yas", "Sir Bu Nair")){
    
  }else{
  AWS_data <- read.csv(filenames[i])
  AWS_data <- AWS_data %>%
    mutate(DateTime = mdy_hm(Date.UTC.4.))
  
  # names(AWS_data)
  # [1] "Station"                  "Date.UTC.4."              "Wind.Dir...."            
  # [4] "Wind.Speed..m.s."         "Temp.Dry...C."            "Temp.DewPoint...C."      
  # [7] "Vapour.Press..hPa."       "RelHumidity...."          "Radiation.Global..Wh.m2."
  # [10] "Press.QFF..hPa."          "Prec..mm."                "DateTime"                
  # [13] "date"                     "hour" 
  AWS_data$Wind.Dir....<- as.numeric(AWS_data$Wind.Dir....)
  AWS_data$Prec..mm.<- as.numeric(AWS_data$Prec..mm.)
           
  AWS_data <- AWS_data %>%
    mutate(date = date(DateTime),
           hour = hour(DateTime),
           minute = minute(DateTime),
           station = station)
  

  
  
  # filter data between 29 March and 4 April 2015
  # filter data only at minutes 00
  AWS_data <- AWS_data %>%
    #filter(minute == 0) %>%
    filter(hour == 12 | hour == 11 | hour== 13 | hour== 10) %>%
    filter(date >= "2015-01-01" & date <= "2015-12-31") 
  mean_na<- function(xxx){
    yyy<- mean(xxx, na.rm = T)
    return(yyy)
  }
  AWS_data_check <- AWS_data %>%
    #filter(minute == 0) %>%
    group_by(date,station ) %>%
    summarise(Wind.Dir....=mean_na(Wind.Dir....), 
              Wind.Speed..m.s. =mean_na(Wind.Speed..m.s.), Temp.Dry...C.=mean_na(Temp.Dry...C.),
              Temp.DewPoint...C.=mean_na(Temp.DewPoint...C.), Vapour.Press..hPa.=mean_na(Vapour.Press..hPa.),
              RelHumidity....=mean_na(RelHumidity....),Radiation.Global..Wh.m2.=mean_na(Radiation.Global..Wh.m2.),
              Press.QFF..hPa.=mean_na(Press.QFF..hPa.),Prec..mm.=mean_na(Prec..mm.) ) 
  
  AWS_data_check<- AWS_data_check %>%
    mutate(mon = month(date))%>%
    mutate(year_obs = year(date))%>%
    group_by(mon, station,year_obs)%>%
    summarise(Wind.Dir....=mean_na(Wind.Dir....), 
              Wind.Speed..m.s. =mean_na(Wind.Speed..m.s.), Temp.Dry...C.=mean_na(Temp.Dry...C.),
              Temp.DewPoint...C.=mean_na(Temp.DewPoint...C.), Vapour.Press..hPa.=mean_na(Vapour.Press..hPa.),
              RelHumidity....=mean_na(RelHumidity....),Radiation.Global..Wh.m2.=mean_na(Radiation.Global..Wh.m2.),
              Press.QFF..hPa.=mean_na(Press.QFF..hPa.), Prec..mm.=mean_na(Prec..mm.) ) 
  
   # # make hourly average 
   # AWS_data <- AWS_data %>%
   #  group_by(date,
   #           hour,
   #           station) %>%
   #  dplyr::summarise(wind_direction = mean(Wind.Dir...., na.rm = TRUE),
   #                   wind_speed = mean(Wind.Speed..m.s., na.rm = TRUE),
   #                   RH = mean(RelHumidity...., na.rm = TRUE),
   #                   Radiation = mean(Radiation.Global..Wh.m2., na.rm = TRUE),
   #                   T_dry = mean(Temp.Dry...C., na.rm = TRUE),
   #                   T_dew = mean(Temp.DewPoint...C., nam.rm = TRUE))
   
   
  AWS_data_check <- AWS_data_check %>%
     mutate(wind_direction = Wind.Dir....,
            wind_speed = Wind.Speed..m.s., 
            RH = RelHumidity....,
            Radiation = Radiation.Global..Wh.m2.,
            T_dry = Temp.Dry...C.,
            T_dew = Temp.DewPoint...C.,
            Vapour_pre_hPa= Vapour.Press..hPa.,
            Press_hPa=Press.QFF..hPa.,
            Prec_mm=Prec..mm.)
   
  AWS_data_check <- AWS_data_check %>%
     dplyr::select(station,
                   year_obs,
                   mon,
                   wind_direction,
                   wind_speed,
                   RH,
                   Radiation,
                   T_dry,
                   T_dew,
                   Vapour_pre_hPa,
                   Press_hPa,
                   Prec_mm)
  
   rm(AWS_data)
   All_AWS_data <- rbind(All_AWS_data, AWS_data_check)
  }
   
}

# to calculate the average of wind speed and wind direction  
# 

#write_csv(All_AWS_data, "D:/Air Quality/GWR_with_met/met_refined/AWS_concatenated_met_2015.csv")

write_csv(All_AWS_data,"D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/AWS_concatenated_met_2015.csv")


#############################################################################################
##### create single files for each time stamp ###############################################
#############################################################################################


setwd("D:/Air Quality/GWR_with_met/Result/Monthly/")
All_AWS_data <- read_csv("met_refined_data/AWS_concatenated_met_2015.csv")
# All_AWS_data <- read_csv("AWS_concatenated_DUST_2_April_2015_AVG.csv")

# rebuild DateTime variable
All_AWS_data$DateTime <- paste0(All_AWS_data$year_obs, " ", month.abb[All_AWS_data$mon], " ")


# load coordinates of the monitoring stations:

STATIONS_COORDS <- read_csv("D:/Air Quality/GWR_with_met/met_refined/stations_clean_FK.csv") 
colnames(STATIONS_COORDS) <- c("station", "latitude", "longitude")

# join coordinated of the the station with the total dataset
All_AWS_data_1 <- All_AWS_data %>%
  left_join(STATIONS_COORDS, by = c("station"))

{
# # missing lat and lof of Jabal Jais, Jabal Hafet, Yasat
# 
# # list DateTime
# DateHour <- All_AWS_data[!duplicated(All_AWS_data[c("DateTime")]),]
# # DateHour <- as.list(DateHour[,10])
# DateHour <- as.list(DateHour[,1])
# DateHour <- unlist(DateHour)
# 
# # list stations
# STATIONS_NAMES <- All_AWS_data[!duplicated(All_AWS_data[c("station")]),]
# # STATIONS_NAMES <- as.list(STATIONS_NAMES[,3])
# STATIONS_NAMES <- as.list(STATIONS_NAMES[,4])
# STATIONS_NAMES <- unlist(STATIONS_NAMES)
# 
# # generate a time sequence for the WRF-Chem run at intervals of 1 hour (should be 168 images, 7 DAYS)
# start <- as.POSIXct("2015-03-29 00:00:00")
# interval <- 60 #minutes
# end <- start + as.difftime(7, units="days")
# TS <- seq(from=start, by=interval*60, to=end)   # same time series as the AQ data
# TS <- TS[1:168]
# 
# 
# #  STATIONS_NAMES[1]
# #  DateHour[1]
# # 
# 
# # i <- 3
# # j <- 3
# 
# setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
# setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
# getwd()
# 
# for (i in 1:length(unique(All_AWS_data$mon))) {
#   
#   hourly_AWS_data <- NULL
#   
#   for (j in 1:length(STATIONS_NAMES)) {
#       name_time <- TS[i]
#        AAA <- All_AWS_data %>%
#          filter(DateTime == DateHour[i],
#                 station == as.character(STATIONS_NAMES[j]))
#        hourly_AWS_data <- rbind(hourly_AWS_data, AAA)
#        hourly_AWS_data <- na.omit(hourly_AWS_data)
#        write.csv(hourly_AWS_data, paste0(str_sub(name_time, start = 1, end = -10), "_",
#                                          str_sub(name_time, start = 12, end = -7), "_",
#                                          str_sub(name_time, start = 15, end = -4),
#                                          ".csv"))
#   }
#   }
}




######################################################################################
###### KRIGING FUNCTION ##############################################################
######################################################################################


library(readr)
library(sp)
library(raster)
library(gstat)
library(rgdal)
library(RNetCDF)
library(ncdf4)
library(stringr)



############################## 
#### KRIGING function ########
##############################


# kriging_points <- function(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary"){
  kriging_points <- function(filenames_hourly_NCMS, resl_ras= 0.01, shp_UAE = "D:/website_MODIS/UAE_boundary", xx=8){
    
    all_rasters <- stack()  
    
  #name <- str_sub(filenames_hourly_NCMS, start = 1, end = -5)
    name<- sprintf("%02d",filenames_hourly_NCMS$mon[1])
  federico_AWS <- filenames_hourly_NCMS
#  federico_AWS <- read.csv(filenames_hourly_NCMS[i])

  # remove rows with NA from the column of lat/or long
  federico_AWS <- federico_AWS[!(is.na(federico_AWS$latitude)), ]
  federico_AWS <- federico_AWS[!(is.na(federico_AWS$longitude)), ]
  federico_AWS <- federico_AWS[!(is.na(federico_AWS[[xx]])), ]
  
  
  #masking layer or shapefile
  if (is.character(shp_UAE)) {
    setwd(shp_UAE)
    dir <- shp_UAE
    shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
#    setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
    #setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
    }
  
  limit_x_y <-  extent(shp_UAE)

  federico_AWS$x <- federico_AWS$longitude
  federico_AWS$y <- federico_AWS$latitude
  
  federico_AWS<-as.data.frame(federico_AWS)
  coordinates(federico_AWS) = ~ x + y  ## Set spatial coordinates to create a Spatial object:
  
   plot(federico_AWS)
   # overlay shape of UAE border
   plot(shp_UAE, add=TRUE, lwd=1)
  
  
  ## make a variogram----------------------------------------------------------------
  
  # vargram_T_dry <- variogram(T_dry ~ 1, federico_AWS) # calculates sample variogram values for the dry temperature
  # nn <- floor(length(vargram_T_dry$gamma)/2)
  # var_for_fit<- mean(vargram_T_dry[nn:nrow(vargram_T_dry),3])
  # 
  # 
  # # fit the variogram
  # vargram_T_dry_fit  <- fit.variogram(vargram_T_dry, fit.ranges = FALSE, fit.sills = FALSE,
  #                                   vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
  # 
  # plot(vargram_T_dry)
  # 
  # plot(vargram_T_dry, vargram_T_dry_fit) # plot the sample values, along with the fit model
  

   # vargram_Radiation <- variogram(Radiation ~ 1, federico_AWS) # calculates sample variogram values for the dry temperature
   # nn <- floor(length(vargram_Radiation$gamma)/2)
   # var_for_fit<- mean(vargram_Radiation[nn:nrow(vargram_Radiation),3])
   # 
   # 
   # # fit the variogram
   # vargram_Radiation_fit  <- fit.variogram(vargram_Radiation, fit.ranges = FALSE, fit.sills = FALSE,
   #                                     vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
   # 
   # 
   # plot(vargram_Radiation, vargram_Radiation_fit) # plot the sample values, along with the fit model
   
   
   vargram_WS <- variogram(federico_AWS[[xx]] ~ 1, federico_AWS) # calculates sample variogram values for the dry temperature
   nn <- floor(length(vargram_WS$gamma)/2)
   var_for_fit<- mean(vargram_WS[nn:nrow(vargram_WS),3])
   
   
   # fit the variogram
   vargram_WS_fit  <- fit.variogram(vargram_WS, fit.ranges = FALSE, fit.sills = FALSE,
                                           vgm(var_for_fit, "Sph"), fit.kappa = TRUE)
   
   
   plot(vargram_WS, vargram_WS_fit) # plot the sample values, along with the fit model
   
   
  # make a regular empty grid
  x.range <- as.numeric(c(floor(limit_x_y[1]-1),ceiling(limit_x_y[2]+1)))  # min/max longitude of the interpolation area
  y.range <- as.numeric(c(floor(limit_x_y[3]-1),ceiling(limit_x_y[4]+1)))  # min/max latitude of the interpolation area
  
  
  ## grid at 10km resolution
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = resl_ras),
                     y = seq(from = y.range[1], to = y.range[2], by = resl_ras))  # expand points to grid
  coordinates(grd) <- ~x + y
  gridded(grd) <- TRUE
  
  plot(grd, cex = 1.5, col = "grey")
  points(federico_AWS, pch = 1, col = "red", cex = 1)
  
  
  # f.1 <- as.formula(Precip_in ~ X + Y)
  # perform kriging
  #  dat.krg <- gstat::krige(T_dry ~ 1, federico_AWS, grd, vargram_T_dry_fit, nmax = 50)
  #  dat.krg <- gstat::krige(Radiation ~ 1, federico_AWS, grd, vargram_Radiation_fit, nmax = 50)
  dat.krg <- gstat::krige(federico_AWS[[xx]] ~ 1, federico_AWS, grd, vargram_WS_fit, nmax = 50)
  federico_AWS$T_dry
  r <- raster(dat.krg)
  plot(shp_UAE, add=TRUE, lwd=1)
  projection(r) <- CRS("+proj=longlat +datum=WGS84")
  
  r <- crop(r, extent(shp_UAE))
  r <- mask(r, shp_UAE)
  # transform Wh/m2 into kW/m2
  # r <- r*3.6 
  plot(r)
  
  # stack rasters in the loop----
  all_rasters <- stack(all_rasters,r)
 
  }
  
  # filenames_hourly_NCMS <- filenames_hourly_NCMS[1:3]
  # make kriging of all the files
Total_wind<- stack()
Total_temp<- stack()
Total_RH<- stack()
Total_Radiation<- stack()
Total_dew<- stack()
for (i in 1:12){
  filenames_hourly_NCMS<- All_AWS_data_1%>%
    filter (mon==i)
  names(filenames_hourly_NCMS)
  BBB_wind <- kriging_points(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary", xx=5)
  names(BBB_wind)<- month.name[i]
  BBB_temp <- kriging_points(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary", xx=8)
  names(BBB_temp)<- month.name[i]
  BBB_RH <- kriging_points(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary", xx=6)
  names(BBB_RH)<- month.name[i]
  BBB_Radiation <- kriging_points(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary", xx=7)
  names(BBB_Radiation)<- month.name[i]
  BBB_dew <- kriging_points(filenames_hourly_NCMS, resl_ras= 0.1, shp_UAE = "Z:/_SHARED_FOLDERS/Air Quality/Phase 2/website_MODIS/UAE_boundary", xx=9)
  names(BBB_dew)<- month.name[i]
  
  Total_wind <- stack(Total_wind,BBB_wind)
  Total_temp <- stack(Total_temp,BBB_temp)
  Total_RH <- stack(Total_RH,BBB_RH)
  Total_Radiation <- stack(Total_Radiation,BBB_Radiation)
  Total_dew <- stack(Total_dew,BBB_dew)
  
}

writeRaster(Total_wind, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Wind_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_temp, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Temp_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_RH, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/RH_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_Radiation, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Radiation_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_dew, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/DEW_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)

save(Total_wind,Total_temp,Total_RH,Total_Radiation,Total_dew, file="D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/NCMS_10km.RData")


{
# 
# 
# BBB_wind$December
# plot(BBB_wind$December,main=names(BBB_wind))
# 
# BBB <- lapply(filenames_hourly_NCMS, kriging_points)
#   
#   
#   
#   # make a large stack raster with all the rasters
#   ras_stack <- stack()
#   
#   
# #  jj <- 99
#   
#   for (jj in 1:length(BBB)){      
#     plot(BBB[[jj]])
#     ras <- BBB[[jj]]
#     ras_stack<- stack(ras_stack,ras)
#   }
# 
# # writeRaster(ras_stack, paste0("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Dry_Temperature_NCMS_1km_new.tif"), overwrite = TRUE)
# # writeRaster(ras_stack, paste0("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Irradiation_W_m2_NCMS_1km.tif"), overwrite = TRUE)
# writeRaster(ras_stack, paste0("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Wind_Speed_NCMS_1km.tif"), overwrite = TRUE)
}



#################################################################################
######### AOD for 2015 FROM MODIS ###############################################
#################################################################################


coefi_conver<- 85    # this number is used to change the AOD values to PM2.5 mg/m3 obtaine by the regression of 
# the station values with the satellite values from MODIS (2015-2016)
# could also be changed from the ECWMF model values for AOD and PM2.5
{
  Total_AOD<- stack()
for (kk in 1:12){
  
    ### for the year of 2016
    #i=1
    #coefi_conver<- 85
    
    ### for the year of 2015
    
    year_req<-sprintf("%04d-",2015)
    months_req<-sprintf("%02d",kk)
    
    path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2015_MODIS_processed/2015_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")
    
    datafiles <- Sys.glob(path_name) #Or whatever identifies your files
    resultingStack <- stack()
    for(i in 1:NROW(datafiles)){
      tempraster <- raster(datafiles[i])
      resultingStack <- stack(resultingStack,tempraster)
    }
    
    x <- reclassify(resultingStack, cbind(0, NA))
    r_AOD_2015 <- mean(x, na.rm=TRUE)*coefi_conver
    r_AOD_2015<- projectRaster(r_AOD_2015, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    # str(r_AOD_2015)
    # plot(r_AOD_2015)
    # res(r_AOD_2015)
    
    
    ### averaging all the year
    
    AOD_mean_jan <- r_AOD_2015 #r_AOD_2016,
    #plot(AOD_mean_jan)
    names(AOD_mean_jan)<- month.name[kk]
    
    #writeRaster(AOD_mean_jan, sprintf(paste0('D:/Air Quality/GWR/new_analysis_2013_2015/MODIS/AOD_mean_%02d.tif'),kk),overwrite=TRUE)
    
    #rm(list = ls(all = TRUE))
    
  Total_AOD <-  stack (Total_AOD, AOD_mean_jan)
}
}

#plot(Total_AOD$October, main=names(Total_AOD$October))

writeRaster(Total_AOD, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/AOD_mean_monthly_2015.tif",overwrite=TRUE,
            options="INTERLEAVE=BAND")
save(Total_AOD, file="D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/AOD_mean_monthly_2015.RData")



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
##########   Getting the monthly monitoring values           ########
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$



Total_monitoring_PM25<- stack()


for (i in 1:12){
  # i=1
  
  load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")
  
  ## filtering the data for PM2.5 and 2015
  AQ_data_2015 <- AQ_data_12 %>%
    filter(years == 2015, Pollutant == "PM2.5" ) 
  AQ_data_2015 <- na.omit(AQ_data_2015)
  
  ## filtering for specific month
  AQ_data_PM25 <- AQ_data_2015 %>%
    filter (months==i)
  
  # removing stations with very low monthly observations
  stations_remove <- AQ_data_PM25 %>%   # stations less than 5 day reading are removed to reduce the BIAS
      group_by(Site) %>%
      summarize(station_count= sum(Value == Value))%>%
      filter(station_count <= 5)
  
  # filterig stations with enough observations monthly
  if ( nrow(stations_remove) > 0 ){
    AQ_data_PM25<- AQ_data_PM25%>%
      filter( !(Site %in% c(stations_remove$Site )))
  }
  
  
  # monthly mean of the month
  AQ_data_PM25 <- AQ_data_PM25 %>%
    group_by(Site, years, months) %>%
    summarize(mon_mean= mean(Value, na.rm = T))

  # # monthly mean of january
  # AQ_data_PM25 <- AQ_data_PM25 %>%
  #   group_by(Site) %>%
  #   summarize(sea_mean=mean(mon_mean, na.rm = T))
  
  # goegraphical location of the stations
  
  coordin_site<-filter(AQ_data_2015,  Pollutant == "PM2.5" , months==i)
  coordin_site<-coordin_site %>%
    dplyr::distinct(Site, .keep_all = T)%>%
    dplyr::select(-years)
  
  AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))
  
  AQ_data_PM25<- as.data.frame(AQ_data_PM25)     # to ungroup the variables
  
  AQ_data_PM25 <- AQ_data_PM25 %>%
    dplyr::select(Site,
           Longitude,
           Latitude,
           mon_mean)
  
  # remove all lines with NA
  AQ_data_PM25 <- na.omit(AQ_data_PM25)

  
  
  # selecting 70% of the monitoring stations for the training and the rest 30% 
  # for vallidating the coefficients
  # D:\Air Quality\GWR_with_met\Result\Images\Results of 70_30 rm RH\station for validation
  
  if (!file.exists(paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/",month.name[i],"_training.RData"))){
    # 30% training stations
    pop_station<- AQ_data_PM25 %>%
      dplyr::distinct(Site)
    
    
    training_station <- sample_n(pop_station, floor(0.7*nrow(pop_station)), replace = FALSE)
    validation_station <- subset(pop_station, !(pop_station$Site %in% training_station$Site))
    save(training_station, file=paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/",month.name[i],"_training.RData"))
    save(validation_station, file=paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/",month.name[i],"_validation.RData"))
  }else{
    load(paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/",month.name[i],"_training.RData"))
    load(paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/",month.name[i],"_validation.RData"))
  }
  
  AQ_data_PM25<- AQ_data_PM25%>%
    filter(Site %in% c(training_station$Site))
  
  
  #write.csv( AQ_data_PM25, "D:/Air Quality/federico AQ index/mean_aver_jan_moni.csv",row.names=F)
  
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ###########   kRIGING interpolation of the monitoring values  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  # shapefile of UAE for the kriging
  
  dir <- "D:/Air Quality/GWR/UAE_boundary"
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  plot(shp_UAE)
  
  ### USING THE FUNCTION kriging_func.R
  source("D:/Air Quality/GWR/IDW_function.R")
  ###    kriging_points <- function(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
  
  resl_ras= 0.1
  
  r_moni <- IDW_points(dawit=AQ_data_PM25, resl_ras, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
  names(r_moni)<- month.name[i]
  #plot(r_moni)
  crs(r_moni) <-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  Total_monitoring_PM25<- stack( Total_monitoring_PM25,r_moni )
  rm(list = ls()[!ls() %in% c( "Total_monitoring_PM25")])
}


writeRaster(Total_monitoring_PM25, "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Moni_PM25_mean_monthly_2015.tif",
            options="INTERLEAVE=BAND", overwrite=TRUE)
save(Total_monitoring_PM25, file= "D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Moni_PM25_mean_monthly_2015.RData")

rm(Total_monitoring_PM25)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###########       Geographic Weighted Regression Model        #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Importing and restructuring the rasters

rm(list=ls(all.names = TRUE))

old <- Sys.time()



setwd("D:/Air Quality/GWR_with_met/")

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"

result_monthly<- list()

# OD_mean_jan<-raster("MODIS/AOD_mean_01.tif")
# plot(OD_mean_jan)
for (qq in 1:12){
  #qq=10
  load("D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/NCMS_10km.RData")
  load("D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/Moni_PM25_mean_monthly_2015.RData")
  #load("D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")
  load("D:/Air Quality/GWR_with_met/Result/Monthly/met_refined_data/variables/AOD_mean_monthly_2015.RData")
  # LU_fract_desert<- raster("Land_cover/desert_fraction.tif")#/100 ## constant for all months
  # LU_fract_urban<- raster("Land_cover/urban_fraction.tif")#/100   ## constant for all months
  # plot(Total_AOD$January)
  # AOD from MODIS
  #name_mon<- sprintf("%02d",qq)
  AOD_mean_jan<-Total_AOD[[month.name[qq]]]
  
  #crs(AOD_mean_jan)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
  
  # PM 2.5 from the monitoring stations
  r_moni<- Total_monitoring_PM25[[month.name[qq]]]
  
  # meteorological Varibales from the met stations
  # Wind speed, Temprature , RH , Radiation , DEW
  
  wind_speed<- Total_wind[[month.name[qq]]]
  temp<- Total_temp[[month.name[qq]]]
  DEW<- Total_dew[[month.name[qq]]]
  RH<- Total_RH[[month.name[qq]]]
  Radiation<- Total_Radiation[[month.name[qq]]]
  
  
  #### rearranging the layers
  
  # plot(r_moni)
  # plot(AOD_mean_jan)
  
  AOD_mean_jan <- resample(AOD_mean_jan,r_moni,"bilinear")
  wind_speed <- resample(wind_speed,r_moni,"bilinear")
  temp <- resample(temp,r_moni,"bilinear")
  DEW <- resample(DEW,r_moni,"bilinear")
  RH <- resample(RH,r_moni,"bilinear")
  Radiation <- resample(Radiation,r_moni,"bilinear")
  # plot(r_moni)
  # plot(AOD_mean_jan)
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ####  as SpatialPointsDataFrame layers Method III   ####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
  
  # plot(BIAS)
  AOD_mean_jan_pts <- as.data.frame(AOD_mean_jan, xy=T)
  colnames(AOD_mean_jan_pts)<- c("x", "y", "AOD_mean")
  
  r_moni_pts <- as.data.frame(r_moni, xy=T)
  colnames(r_moni_pts)<- c("x", "y", "Moni")
  
  wind_speed_pts <- as.data.frame(wind_speed, xy=T)
  colnames(wind_speed_pts)<- c("x", "y", "wind_speed")
  temp_pts <- as.data.frame(temp, xy=T)
  colnames(temp_pts)<- c("x", "y", "temp")
  DEW_pts <- as.data.frame(DEW , xy=T)
  colnames(DEW_pts)<- c("x", "y", "DEW")
  RH_pts <- as.data.frame(RH,  xy=T)
  colnames(RH_pts)<- c("x", "y", "RH")
  Radiation_pts <- as.data.frame(Radiation, xy=T)
  colnames(Radiation_pts)<- c("x", "y", "Radiation")
  
  wind_speed_pts <- as.data.frame(wind_speed, xy=T)
  colnames(wind_speed_pts)<- c("x", "y", "wind_speed")
  wind_speed_pts<- na.omit(wind_speed_pts)
  
  mydata<- left_join(r_moni_pts, AOD_mean_jan_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, wind_speed_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, temp_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, DEW_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, RH_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, Radiation_pts,  by = c("x", "y") )
  
  mydata<- na.omit(mydata)
  
  mydata$AOD_mean<-mydata$AOD_mean/85
  mydata$RH<-mydata$RH
  
  library(spgwr)
  
  bwG_pnt <- gwr.sel(Moni ~  AOD_mean + wind_speed  + DEW+  RH ,#+ RH ,#+ temp
                     data= mydata,  coords=cbind( mydata$x , mydata$y),
                     gweight = gwr.Gauss, #RMSE=T,
                     method="cv", verbose = F, show.error.messages = T)
  
  
  gwrG_pnt <- gwr(Moni ~  AOD_mean + wind_speed + DEW+ RH  ,#+RH , #+ temp
                  data= mydata,  bandwidth = bwG_pnt*1.1,  coords=cbind( mydata$x , mydata$y),
                  gweight = gwr.Gauss, hatmatrix = TRUE,predictions = T)
  
  
  resave <- function(..., list = character(), file) {
    previous  <- load(file)
    var.names <- c(list, as.character(substitute(list(...)))[-1L])
    for (var in var.names) assign(var, get(var, envir = parent.frame()))
    save(list = unique(c(previous, var.names)), file = file)
  }
  
  nam <- paste("gwrG_pnt_", month.name[qq], sep = "")
  nam_mydata<- paste("Input_data_", month.name[qq], sep = "")
  assign(nam, gwrG_pnt)
  assign(nam_mydata, mydata )
  if (qq==1){
    save(list= eval(nam), file= paste0(output_folder, "GWR_file.RData"))
    save(list= eval(nam_mydata), file= paste0(output_folder, "Input_mydata_file.RData"))
  }else{
    resave(list = eval(nam), file= paste0(output_folder, "GWR_file.RData"))
    resave(list= eval(nam_mydata), file= paste0(output_folder, "Input_mydata_file.RData"))
  }
  
  #dawit<-load(file)
  #dadada<- as.data.frame(gwrG_pnt$SDF)
  #plot(dadada$localR2)
  
  #plot(mydata$Moni)
  # print(mean(gwrG_pnt$SDF$localR2))
  
  result_monthly<-c(result_monthly, gwrG_pnt)
  
  print(qq)
  print(plot(gwrG_pnt$SDF$localR2, main=qq))
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "old","output_folder")])
  
}



# save(result_monthly, file="D:/Air Quality/GWR_with_met/Result/data/result_regression_cv_70_30_rm_RH.RData")



new <- Sys.time() - old # calculate difference
print(new)


{

  # library(spgwr)
  # #BIAS <- r_moni-r_AOD_sampled
  #
  # r_moni_sp <- as(r_moni, 'SpatialPointsDataFrame')
  # names(r_moni_sp)<- "Moni"
  # r_AOD_sampled_sp<-as(AOD_mean_jan, 'SpatialPointsDataFrame')
  # names(r_AOD_sampled_sp)<- "Modis_AOD"
  #
  #
  # wind_speed_sp <- as(wind_speed, 'SpatialPointsDataFrame')
  # names(wind_speed_sp)<- "wind_speed"
  # temp_sp <- as(temp, 'SpatialPointsDataFrame')
  # names(temp_sp)<- "temp"
  # DEW_sp <- as(DEW, 'SpatialPointsDataFrame')
  # names(DEW_sp)<- "DEW"
  # Radiation_sp <- as(Radiation, 'SpatialPointsDataFrame')
  # names(Radiation_sp)<- "Radiation"
  # RH_sp <- as(RH, 'SpatialPointsDataFrame')
  # names(RH_sp)<- "RH"
  #
  #

  # combined_data_pnt <- cbind(r_moni_sp,r_AOD_sampled_sp, wind_speed_sp,temp_sp,
  #                          DEW_sp, Radiation_sp,RH_sp)
  #
  # plot(ED)


  # bwG_pnt <- gwr.sel( Moni ~  Modis_AOD + wind_speed + temp+ DEW+RH ,
  #                     data= combined_data_pnt,  gweight = gwr.Gauss, #RMSE=T,
  #                     method="cv", verbose = F, show.error.messages = T)

  # if (bwG_pnt < 11){
  # gwrG_pnt <- gwr(BIAS ~  urban_fraction +desert_fraction + ECMWF_DUST_jan_10km + ED,#+ ECMWF_SO4_jan_10km ,#  ED + urban_fraction +desert_fraction + ED + ECMWF_DUST_jan_10km + ECMWF_SO4_jan_10km+ desert_fraction + ECMWF_SALT_jan_10km,#   , #  ECMWF_DUST_jan_10km ++ ECMWF_DUST_jan_10km + ECMWF_BC_jan_10km
  #                 data= combined_data_pnt_ED,  bandwidth = 11,
  #                 gweight = gwr.Gauss, hatmatrix = TRUE)
  # }else{
  # gwrG_pnt <- gwr(Moni ~  Modis_AOD + wind_speed + temp+ DEW+RH ,
  #                 data= combined_data_pnt,  bandwidth = bwG_pnt, # 0.2/bwG_pnt
  #                 gweight = gwr.Gauss, hatmatrix = TRUE,predictions = T)
  # }
}





#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###############################   RESULTS     RESULTS      RESULTS    ################################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$                                 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




rm(list = ls(all = TRUE))

#### LOADING THE RESULT WORKSPACE

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"


loadOneName <- function(objName, file, envir = parent.frame(),
                        assign.on.exit = TRUE) {
  tempEnv <- new.env()
  load(file, envir = tempEnv)
  stopifnot(objName %in% ls(tempEnv))
  if(assign.on.exit) {
    assign(objName, tempEnv[[objName]], envir = envir)
    return(invisible(tempEnv[[objName]]))
  }
  tempEnv[[objName]]
}


cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))




##### ploting the histogram of the r2  ####

data_frame_r2<- data.frame()
r2_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "localR2")])
  r2_data<- dawi[, c( "localR2")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("r2", "month_ind")
  data_frame_r2<-rbind(data_frame_r2,month_ind)
  r2_ras_stack<- stack(r2_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"




data_frame_r2<- cbind(data_frame_r2, month.name[data_frame_r2$month_ind])
colnames(data_frame_r2)[3]<- "Month"

scaleFUN <- function(x) sprintf("%.1f", x)

hp <- ggplot(data_frame_r2, aes(x=r2)) + 
  # qplot( data_frame_r2$r2, y=NULL, data_frame_r2, binwidth= 0.01, xlim=c(0,1), facet_wrap( ~ data_frame_r2$month_ind, ncol=3),
  #        geom="histogram", fill=I("blue" ),alpha=I(.4),ylab ="")+
  geom_histogram (binwidth=0.01,fill=I("blue" ),alpha=I(.8))+ #colour="blue",
  scale_x_continuous(limits = c(0, 1), labels= scaleFUN , name=expression(R^2))+
  scale_y_continuous(name="Counts" )+
  ggtitle(expression(paste("Local ", R^2))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))+
  theme(axis.text=element_text(size=10, face="bold"),
        axis.title=element_text(size=12,face="bold"))+
  facet_wrap( ~ data_frame_r2$month_ind + data_frame_r2$Month, ncol=3, labeller = labeller("label_both", .multi_line = F, sep = ": "))

#hp


png(paste0(output_folder,"histogram_r2.png"),
    width = 1050, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(hp)
dev.off()



#### Ploting the R2 maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


#### R2 maps ###


####### color pallet

cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))


max_val<-ceiling(max(maxValue(r2_ras_stack)))
min_val<-floor(min(minValue(r2_ras_stack)))

stat_dat<- summary(as.vector(r2_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(r2_ras_stack)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-r2_ras_stack
# AOD_plot[AOD_plot < low_IQR ]<- low_IQR
# AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= expression(paste("Monthly Local ", R^2)) ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=""
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Maps_local_r2.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
# # # # # #
# [1] "sum.w"              "(Intercept)"        "AOD_mean"           "wind_speed"         "DEW"               
# [6] "temp"               "RH"                 "(Intercept)_se"     "AOD_mean_se"        "wind_speed_se"     
# [11] "DEW_se"             "temp_se"            "RH_se"              "gwr.e"              "pred"              
# [16] "pred.se"            "localR2"            "(Intercept)_se_EDF" "AOD_mean_se_EDF"    "wind_speed_se_EDF" 
# [21] "DEW_se_EDF"         "temp_se_EDF"        "RH_se_EDF"          "pred.se_EDF"       
# 
# coordi_x_y<- result_monthly$SDF@coords
# names(coordi_x_y)<-c("x", "y")


#####

##### AOD coefficents ####


data_frame_AOD<- data.frame()
AOD_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "AOD_mean")])
  r2_data<- dawi[, c( "AOD_mean")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("AOD_mean", "month_ind")
  data_frame_AOD<-rbind(data_frame_AOD,month_ind)
  AOD_ras_stack<- stack(AOD_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_AOD<- cbind(data_frame_AOD, month.name[data_frame_AOD$month_ind])
colnames(data_frame_AOD)[3]<- "Month"

scaleFUN <- function(x) sprintf("%.1f", x)


#### Ploting the AOD maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(AOD_ras_stack)))
min_val<-floor(min(minValue(AOD_ras_stack)))

stat_dat<- summary(as.vector(AOD_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(AOD_ras_stack)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-AOD_ras_stack
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "AOD Coefficients" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_AOD.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_AOD.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of AOD Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])


#####

##### Wind Speed coefficents ####


data_frame_WS<- data.frame()
WS_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "wind_speed")])
  r2_data<- dawi[, c( "wind_speed")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("wind_speed", "month_ind")
  data_frame_WS<-rbind(data_frame_WS,month_ind)
  WS_ras_stack<- stack(WS_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_WS<- cbind(data_frame_WS, month.name[data_frame_WS$month_ind])
colnames(data_frame_WS)[3]<- "Month"

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(WS_ras_stack)))
min_val<-floor(min(minValue(WS_ras_stack)))

stat_dat<- summary(as.vector(WS_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(WS_ras_stack)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-WS_ras_stack
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "Wind Speed Coefficients" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3,"/(m ", sec^-1,")"))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_WS.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_WS.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Wind Speed Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])





#####

##### RH coefficents ####


data_frame_RH<- data.frame()
RH_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "RH")])
  r2_data<- dawi[, c( "RH")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("RH", "month_ind")
  data_frame_RH<-rbind(data_frame_RH,month_ind)
  RH_ras_stack<- stack(RH_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_RH<- cbind(data_frame_RH, month.name[data_frame_RH$month_ind])
colnames(data_frame_RH)[3]<- "Month"

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(RH_ras_stack)))
min_val<-floor(min(minValue(RH_ras_stack)))

stat_dat<- summary(as.vector(RH_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(RH_ras_stack)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-RH_ras_stack
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "Relative Humidity Coefficients" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3, "%"^-1 ))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_RH.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_RH.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Relative Humidity Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])



#####

##### DEW coefficents ####


data_frame_DEW<- data.frame()
DEW_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "DEW")])
  r2_data<- dawi[, c( "DEW")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("DEW", "month_ind")
  data_frame_DEW<-rbind(data_frame_DEW,month_ind)
  DEW_ras_stack<- stack(DEW_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_DEW<- cbind(data_frame_DEW, month.name[data_frame_DEW$month_ind])
colnames(data_frame_DEW)[3]<- "Month"

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(DEW_ras_stack)))
min_val<-floor(min(minValue(DEW_ras_stack)))

stat_dat<- summary(as.vector(DEW_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(DEW_ras_stack)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-DEW_ras_stack
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "DEW Temprature Coefficients" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3, ~degree~C^-1))
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_DEW.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_DEW.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of DEW Temprature Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])






#####

##### Intercept coefficents ####


data_frame_intercept<- data.frame()
Intercept_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "X.Intercept.")])
  r2_data<- dawi[, c( "X.Intercept.")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Intercept", "month_ind")
  data_frame_intercept<-rbind(data_frame_intercept,month_ind)
  Intercept_ras_stack<- stack(Intercept_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_intercept<- cbind(data_frame_intercept, month.name[data_frame_intercept$month_ind])
colnames(data_frame_intercept)[3]<- "Month"

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(Intercept_ras_stack)))
min_val<-floor(min(minValue(Intercept_ras_stack)))

stat_dat<- summary(as.vector(Intercept_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(Intercept_ras_stack)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-Intercept_ras_stack
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "Intercept" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"Coefficients_intercept.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_Intercept.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Intercepts"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])







#####

##### Prediction & measured maps ####

data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_predict<- cbind(data_frame_predict, month.name[data_frame_predict$month_ind])
colnames(data_frame_predict)[3]<- "Month"

#### monitoring 

load(paste(load_folder,"Input_mydata_file.RData", sep=""))

Moni_ras_stack<-stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("Input_data_", month.name[kk] , sep = "")
  # model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  
  assign("dawi", eval(parse(text = paste("Input_data_", month.name[kk] , sep = ""))) )
  
  ras_r2<- rasterFromXYZ( dawi[, c("x","y", "Moni")])
  #r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  # month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  # colnames(month_ind)<-c("Predicted", "month_ind")
  # data_frame_predict<-rbind(data_frame_predict,month_ind)
  Moni_ras_stack<- stack(Moni_ras_stack,ras_r2)
}



#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(pred_ras_stack)))
min_val<-floor(min(minValue(pred_ras_stack)))

stat_dat<- summary(as.vector(pred_ras_stack))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR

low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))
  


vec_all<- as.vector(pred_ras_stack)
vec_all_measured<- as.vector(Moni_ras_stack)
#vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-pred_ras_stack
#AOD_plot[AOD_plot < low_IQR ]<- low_IQR
#AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 




#### predicted

h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "Predicted PM2.5" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h

### monitoring

h_moni <- rasterVis::levelplot(Moni_ras_stack, 
                          margin=FALSE, main= "Measured PM2.5" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(Moni_ras_stack))) +
  latticeExtra::layer(sp.polygons(shp_UAE))



#### predicted

png(paste0(output_folder,"Predicted_pm25.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()

### monitoring
png(paste0(output_folder,"measured_pm25.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h_moni)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_predicted.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Predicted PM2.5"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()


png(paste0(output_folder,"hist_measured.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all_measured,  breaks=500 , main = paste("Histogram of Measured PM2.5"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()



rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])



#####

###### BIAS ######

data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_predict<- cbind(data_frame_predict, month.name[data_frame_predict$month_ind])
colnames(data_frame_predict)[3]<- "Month"

### building a raster from the monitoring

load(paste(load_folder,"Input_mydata_file.RData", sep=""))

Moni_ras_stack<-stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("Input_data_", month.name[kk] , sep = "")
  # model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  
  assign("dawi", eval(parse(text = paste("Input_data_", month.name[kk] , sep = ""))) )
  
  ras_r2<- rasterFromXYZ( dawi[, c("x","y", "Moni")])
  #r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  # month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  # colnames(month_ind)<-c("Predicted", "month_ind")
  # data_frame_predict<-rbind(data_frame_predict,month_ind)
  Moni_ras_stack<- stack(Moni_ras_stack,ras_r2)
}

BIAS<- Moni_ras_stack-pred_ras_stack

#### Ploting the WS maps

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


max_val<-ceiling(max(maxValue(BIAS)))
min_val<-floor(min(minValue(BIAS)))

stat_dat<- summary(as.vector(BIAS))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 20))# n is the space after IQR


low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(BIAS)
#vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-BIAS
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "BIAS (Monitoring- Predicted)" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("     ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          names.attr=rep(names(AOD_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder,"BIAS.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms

#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_BIAS.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of BIAS"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()




rm(list = ls()[!ls() %in% c( "output_folder", "cols", "shp_UAE","loadOneName","load_folder")])





# if (names(AOD_plot) %in% c("sum.w","X.Intercept.", "AOD_mean", "X.Intercept._se" ,"AOD_mean_se",
#                            "gwr.e", "pred","pred.se","X.Intercept._se_EDF","AOD_mean_se_EDF", "pred.se_EDF"  )) {
#   unit_to<- expression(paste("     ", mu,"g ",m^-3) )
# }
# if ( names(AOD_plot) %in% c( "wind_speed", "wind_speed_se", "wind_speed_se_EDF" ) ){
#   unit_to<- expression(paste("     ", mu,"g ",m^-3,"/(m ", sec^-1,")"))
# }
# if (  names(AOD_plot) %in% c("DEW","DEW_se" , "DEW_se_EDF")){
#   unit_to<- expression(paste("     ", mu,"g ",m^-3, ~degree~C^-1))
# }
# if ( names(AOD_plot) %in% c( "RH", "RH_se","RH_se_EDF" )  ){
#   unit_to<- expression(paste("     ", mu,"g ",m^-3, "%"^-1 ))
# }
# if ( names(AOD_plot) %in% c( "localR2" )  ){
#   unit_to<- NULL
# }
# 
# 



#####


###### impact of the Coefficients ##### 

#### LOADING THE RESULT WORKSPACE

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"


loadOneName <- function(objName, file, envir = parent.frame(),
                        assign.on.exit = TRUE) {
  tempEnv <- new.env()
  load(file, envir = tempEnv)
  stopifnot(objName %in% ls(tempEnv))
  if(assign.on.exit) {
    assign(objName, tempEnv[[objName]], envir = envir)
    return(invisible(tempEnv[[objName]]))
  }
  tempEnv[[objName]]
}


data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}

#### AOD Coefficient

data_frame_AOD<- data.frame()
AOD_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "AOD_mean")])
  r2_data<- dawi[, c( "AOD_mean")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("AOD_mean", "month_ind")
  data_frame_AOD<-rbind(data_frame_AOD,month_ind)
  AOD_ras_stack<- stack(AOD_ras_stack,ras_r2)
}

##### Wind Speed coefficents

data_frame_WS<- data.frame()
WS_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "wind_speed")])
  r2_data<- dawi[, c( "wind_speed")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("wind_speed", "month_ind")
  data_frame_WS<-rbind(data_frame_WS,month_ind)
  WS_ras_stack<- stack(WS_ras_stack,ras_r2)
}

##### RH coefficents 

data_frame_RH<- data.frame()
RH_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "RH")])
  r2_data<- dawi[, c( "RH")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("RH", "month_ind")
  data_frame_RH<-rbind(data_frame_RH,month_ind)
  RH_ras_stack<- stack(RH_ras_stack,ras_r2)
}

##### DEW coefficents 

data_frame_DEW<- data.frame()
DEW_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "DEW")])
  r2_data<- dawi[, c( "DEW")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("DEW", "month_ind")
  data_frame_DEW<-rbind(data_frame_DEW,month_ind)
  DEW_ras_stack<- stack(DEW_ras_stack,ras_r2)
}

##### Intercept coefficents 

data_frame_intercept<- data.frame()
Intercept_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "X.Intercept.")])
  r2_data<- dawi[, c( "X.Intercept.")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Intercept", "month_ind")
  data_frame_intercept<-rbind(data_frame_intercept,month_ind)
  Intercept_ras_stack<- stack(Intercept_ras_stack,ras_r2)
}

impact_AOD<- stack()
impact_WS<- stack( )
impact_DEW<- stack( )
impact_RH<- stack()
impact_CON<- stack( )
 
for( qq in 1:12){
#qq=1

nam <- paste("Input_data_", month.name[qq] , sep = "")
mydata <- loadOneName( eval(nam), file=paste0(load_folder, "Input_mydata_file.RData"))


### AOD impact
AOD_impact<- cbind( mydata$x, mydata$y, (mydata$AOD_mean* na.omit(as.data.frame(AOD_ras_stack[[qq]]))))
names(AOD_impact)<-  c("x", "y", "AOD_Impa")
AOD_coeffi_val <- rasterFromXYZ(AOD_impact[, c("x", "y", "AOD_Impa")])
names(AOD_coeffi_val)<- month.name[qq]
#plot(AOD_coeffi_val)

### WS impact

WS_impact<- cbind( mydata$x, mydata$y, (mydata$wind_speed* na.omit(as.data.frame(WS_ras_stack[[qq]]))))
names(WS_impact)<-  c("x", "y", "WS")
WS_coeffi_val <- rasterFromXYZ(WS_impact[, c("x", "y", "WS")])
names(WS_coeffi_val)<- month.name[qq]
#plot(WS_coeffi_val)

### DEW impact

DEW_impact<- cbind( mydata$x, mydata$y, (mydata$DEW * na.omit(as.data.frame(DEW_ras_stack[[qq]]))))
names(DEW_impact)<-  c("x", "y", "DEW")
DEW_coeffi_val <- rasterFromXYZ(DEW_impact[, c("x", "y", "DEW")])
names(DEW_coeffi_val)<- month.name[qq]
#plot(DEW_coeffi_val)

### RH impact

RH_impact<- cbind( mydata$x, mydata$y, (mydata$RH * na.omit(as.data.frame(RH_ras_stack[[qq]]))))
names(RH_impact)<-  c("x", "y", "RH")
RH_coeffi_val <- rasterFromXYZ(RH_impact[, c("x", "y", "RH")])
names(RH_coeffi_val)<- month.name[qq]
#plot(RH_coeffi_val)

### Constant impact

CON_impact<- cbind( mydata$x, mydata$y, (na.omit(as.data.frame(Intercept_ras_stack[[qq]]))))
names(CON_impact)<-  c("x", "y", "CON")
CON_coeffi_val <- rasterFromXYZ(CON_impact[, c("x", "y", "CON")])
names(CON_coeffi_val)<- month.name[qq]
#plot(CON_coeffi_val)


impact_AOD<- stack(impact_AOD, AOD_coeffi_val )
impact_WS<- stack(impact_WS, WS_coeffi_val )
impact_DEW<- stack(impact_DEW, DEW_coeffi_val )
impact_RH<- stack(impact_RH, RH_coeffi_val )
impact_CON<- stack(impact_CON, CON_coeffi_val )
}



vec_majority<- function(impact_AOD=impact_AOD , IQRD=4){
  
  max_val<-ceiling(max(maxValue(mean(impact_AOD))))
  min_val<-floor(min(minValue(mean(impact_AOD))))
  
  vec_AOD<-  as.vector(na.omit(as.vector(mean(impact_AOD))))
  stat_dat<- summary(as.vector(vec_AOD))
  IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])*IQRD))# n is the space after IQR
  
  low_IQR<- if(floor(min_val) > floor(as.numeric((stat_dat[2]- IQR)))) floor(min_val) else floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<- if(ceiling(max_val) < ceiling(as.numeric((stat_dat[5]+IQR)))) floor(max_val) else ceiling(as.numeric((stat_dat[5]+IQR)))
  vec_AOD<-vec_AOD[ vec_AOD >= low_IQR & vec_AOD <= high_IQR & !is.na(vec_AOD) ]
  AOD_plot <-mean(impact_AOD)
  AOD_plot[AOD_plot < low_IQR ]<- low_IQR
  AOD_plot[ AOD_plot >  high_IQR]<- high_IQR
  return(AOD_plot)
}


vec_AOD<-  vec_majority(impact_AOD=impact_AOD)
vec_WS<-  vec_majority(impact_WS)
vec_DEW<-  vec_majority(impact_DEW)
vec_RH<-  vec_majority(impact_RH)
vec_CON<-  vec_majority(impact_CON)


####### plots of the IMPACT of the Coefficients



h <- rasterVis::levelplot(vec_AOD, 
                          margin=FALSE, main= "AOD Impact" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(minValue(vec_AOD), maxValue(vec_AOD), length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(minValue(vec_AOD), maxValue(vec_AOD), length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
                          ) +
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"AOD_impact.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()

####### WS



h <- rasterVis::levelplot(vec_WS, 
                          margin=FALSE, main= "WS Impact" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(minValue(vec_WS), maxValue(vec_WS), length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(minValue(vec_WS), maxValue(vec_WS), length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
) +
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"WS_impact.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


####### DEW



h <- rasterVis::levelplot(vec_DEW, 
                          margin=FALSE, main= "DEW Impact" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(minValue(vec_DEW), maxValue(vec_DEW), length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(minValue(vec_DEW), maxValue(vec_DEW), length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
) +
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"DEW_impact.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


####### RH



h <- rasterVis::levelplot(vec_RH, 
                          margin=FALSE, main= "RH Impact" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(minValue(vec_RH), maxValue(vec_RH), length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(minValue(vec_RH), maxValue(vec_RH), length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
) +
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"RH_impact.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


####### CON



h <- rasterVis::levelplot(vec_CON, 
                          margin=FALSE, main= "Intercept Impact" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(minValue(vec_CON), maxValue(vec_CON), length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=unique(c(seq(minValue(vec_CON), maxValue(vec_CON), length.out=200))),
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
) +
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"CON_impact.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()





plot( vec_WS)
plot( mean(impact_DEW))
plot( mean(impact_RH))
plot( mean(impact_CON))



#####


###### SEASONAL ANALYSIS #####

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"


loadOneName <- function(objName, file, envir = parent.frame(),
                        assign.on.exit = TRUE) {
  tempEnv <- new.env()
  load(file, envir = tempEnv)
  stopifnot(objName %in% ls(tempEnv))
  if(assign.on.exit) {
    assign(objName, tempEnv[[objName]], envir = envir)
    return(invisible(tempEnv[[objName]]))
  }
  tempEnv[[objName]]
}


data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}


### building a raster from the monitoring

load(paste(load_folder,"Input_mydata_file.RData", sep=""))

Moni_ras_stack<-stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("Input_data_", month.name[kk] , sep = "")
  # model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  
  assign("dawi", eval(parse(text = paste("Input_data_", month.name[kk] , sep = ""))) )
  
  ras_r2<- rasterFromXYZ( dawi[, c("x","y", "Moni")])
  #r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  # month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  # colnames(month_ind)<-c("Predicted", "month_ind")
  # data_frame_predict<-rbind(data_frame_predict,month_ind)
  Moni_ras_stack<- stack(Moni_ras_stack,ras_r2)
}



###### MARCH - MAY #####

pre_mam<- mean(pred_ras_stack[[3:5]])
names(pre_mam)<- "March-May"
moni_mam<- mean(Moni_ras_stack[[3:5]])
names(moni_mam)<- "March-May"

estim<- as.vector(na.omit(values(pre_mam)))
moni<-  as.vector(na.omit(values(moni_mam)))

r_2<-   sum((estim-mean(moni) )^2)/
  sum((moni-mean(moni))^2)

png(paste0(output_folder,"seasonal_mam.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1), pty="s")
print({
  
  plot(values(moni_mam), values(pre_mam), xlim=c(5,80), ylim=c(5,80), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "March-May")
  text(15,50, substitute(R^2==a, list(a = round(r_2, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
  
})
dev.off()






######


#### June - AUGUST  #####

pre_jja<- mean(pred_ras_stack[[6:8]])
names(pre_jja)<- "June-August"
moni_jja<- mean(Moni_ras_stack[[6:8]])
names(moni_jja)<- "June-August"

estim<- as.vector(na.omit(values(pre_jja)))
moni<-  as.vector(na.omit(values(moni_jja)))

r_2<-   sum((estim-mean(moni) )^2)/
  sum((moni-mean(moni))^2)

png(paste0(output_folder,"seasonal_jja.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1), pty="s")
print({
  
  plot(values(moni_jja), values(pre_jja), xlim=c(5,80), ylim=c(5,80), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "June-August")
  text(15,50, substitute(R^2==a, list(a = round(r_2, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
  
})
dev.off()

######


#### September - November  #####

pre_son<- mean(pred_ras_stack[[9:11]])
names(pre_son)<- "September - November"
moni_son<- mean(Moni_ras_stack[[9:11]])
names(moni_son)<- "September - November"

estim<- as.vector(na.omit(values(pre_son)))
moni<-  as.vector(na.omit(values(moni_son)))

r_2<-   sum((estim-mean(moni) )^2)/
  sum((moni-mean(moni))^2)

png(paste0(output_folder,"seasonal_son.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1), pty="s")
print({
  
  plot(values(moni_son), values(pre_son), xlim=c(5,80), ylim=c(5,80), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "September - November")
  text(15,50, substitute(R^2==a, list(a = round(r_2, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
  
})
dev.off()

######



#### December - February  #####

pre_djf<- mean(pred_ras_stack[[c(1:2,12)]])
names(pre_djf)<- "December - February"
moni_djf<- mean(Moni_ras_stack[[c(1:2,12)]])
names(moni_djf)<- "December - February"

estim<- as.vector(na.omit(values(pre_djf)))
moni<-  as.vector(na.omit(values(moni_djf)))

r_2<-   sum((estim-mean(moni) )^2)/
  sum((moni-mean(moni))^2)

png(paste0(output_folder,"seasonal_djf.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1), pty="s")
print({
  
  plot(values(moni_djf), values(pre_djf), xlim=c(5,80), ylim=c(5,80), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "December - February")
  text(15,50, substitute(R^2==a, list(a = round(r_2, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
  
})
dev.off()


######


###### Seasonality maps ####

library(viridis)
library(lattice)

cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)



####### MARCH - MAY #####

all_sea<- stack(pre_mam,pre_djf,pre_jja,pre_son)
seq_lab<- seq(floor(min(minValue(all_sea))), ceiling(max(maxValue(all_sea))), length.out=7)
scales_at<- unique(c(seq(floor(min(minValue(all_sea))), ceiling(max(maxValue(all_sea))), length.out=200)))

h <- rasterVis::levelplot(pre_mam, 
                          margin=FALSE, main= "March - May Predicted" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq_lab)),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=scales_at)+
                          # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                          #names.attr=rep(names(AOD_plot))
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"Seasonal_map_mam.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()




######


####### June - AUGUST #####



h <- rasterVis::levelplot(pre_jja, 
                          margin=FALSE, main= "June - August Predicted" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq_lab)),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=scales_at)+
  # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
  #names.attr=rep(names(AOD_plot))
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"Seasonal_map_jja.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()




######


####### September - November #####



h <- rasterVis::levelplot(pre_son, 
                          margin=FALSE, main= "September - November Predicted" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq_lab)),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=scales_at)+
  # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
  #names.attr=rep(names(AOD_plot))
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"Seasonal_map_son.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()




######


####### December - February #####



h <- rasterVis::levelplot(pre_djf, 
                          margin=FALSE, main= "December - February Predicted" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq_lab)),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title= expression(paste("      ", mu,"g ",m^-3) )
                          ),   
                          ## about the axis
                          par.settings=list(
                            strip.border=list(col='transparent'),
                            strip.background=list(col='transparent'),
                            axis.line=list(col='black')
                          ),
                          scales=list(draw=T, alternating= F),            
                          #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
                          col.regions = cols,
                          at=scales_at)+
  # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
  #names.attr=rep(names(AOD_plot))
  latticeExtra::layer(sp.polygons(shp_UAE))

png(paste0(output_folder,"Seasonal_map_djf.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()




######




###########################################################
################                         ##################
################ Validation (70% - 30%)  ##################
################                         ##################
###########################################################



rm(list = ls(all = TRUE))

#### LOADING THE RESULT WORKSPACE

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"


loadOneName <- function(objName, file, envir = parent.frame(),
                        assign.on.exit = TRUE) {
  tempEnv <- new.env()
  load(file, envir = tempEnv)
  stopifnot(objName %in% ls(tempEnv))
  if(assign.on.exit) {
    assign(objName, tempEnv[[objName]], envir = envir)
    return(invisible(tempEnv[[objName]]))
  }
  tempEnv[[objName]]
}


data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}

#### AOD Coefficient

data_frame_AOD<- data.frame()
AOD_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "AOD_mean")])
  r2_data<- dawi[, c( "AOD_mean")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("AOD_mean", "month_ind")
  data_frame_AOD<-rbind(data_frame_AOD,month_ind)
  AOD_ras_stack<- stack(AOD_ras_stack,ras_r2)
}

##### Wind Speed coefficents ####

data_frame_WS<- data.frame()
WS_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "wind_speed")])
  r2_data<- dawi[, c( "wind_speed")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("wind_speed", "month_ind")
  data_frame_WS<-rbind(data_frame_WS,month_ind)
  WS_ras_stack<- stack(WS_ras_stack,ras_r2)
}

##### RH coefficents ####

data_frame_RH<- data.frame()
RH_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "RH")])
  r2_data<- dawi[, c( "RH")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("RH", "month_ind")
  data_frame_RH<-rbind(data_frame_RH,month_ind)
  RH_ras_stack<- stack(RH_ras_stack,ras_r2)
}

##### DEW coefficents ####

data_frame_DEW<- data.frame()
DEW_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "DEW")])
  r2_data<- dawi[, c( "DEW")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("DEW", "month_ind")
  data_frame_DEW<-rbind(data_frame_DEW,month_ind)
  DEW_ras_stack<- stack(DEW_ras_stack,ras_r2)
}

##### Intercept coefficents ####

data_frame_intercept<- data.frame()
Intercept_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "X.Intercept.")])
  r2_data<- dawi[, c( "X.Intercept.")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Intercept", "month_ind")
  data_frame_intercept<-rbind(data_frame_intercept,month_ind)
  Intercept_ras_stack<- stack(Intercept_ras_stack,ras_r2)
}






#### monthly loop 

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"
All_extracted_data_tr<- NULL
All_extracted_data_val<- NULL

for (qq in 1:12){
#qq=1

load(paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/", month.name[qq],"_validation.RData"))
load(paste0("D:/Air Quality/GWR_with_met/Result/Monthly/Monitoring_PM25/Training_validation/", month.name[qq],"_training.RData"))   
load( file="D:/Air Quality/GWR_with_met/Stations_for_validation/All_stations.RData")

### functions needed for the extraction 
extra_dataframe <- function(x=x, y=y, mydata=mydata, lon_lat= c("x", "y")){
  # x is the longitude of the point to be extracted
  # y is the latitude of the point to be extracted
  # mydata is the dataframe to be extracted from. mydata should contain the x y columns or lon and lat names provided
  # as lon_lat i=2
  
  library(NISTunits)
  all_point<- NULL
  for (i in 1:length(x)){
    a = (sin(NISTdegTOradian(mydata[[lon_lat[2]]]-y[i])/2))^2  + (cos(NISTdegTOradian(mydata[[lon_lat[2]]]))) * (cos (NISTdegTOradian(y[i]))) * (sin(NISTdegTOradian(mydata[[lon_lat[1]]]-x[i])/2))^2
    c = 2*atan2( sqrt(a), sqrt((1-a)))
    d = 6371*c # radius of the earth in km
    min_y <- which(d == min(d))
    close_point<- mydata[min_y,]
    all_point<-rbind(all_point,close_point)
  }
  return(all_point)
}

source("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/extract_pnt_raster.r")

# Latitude Longitude should be the lat and long of the files

###### adding the all stations lat and long information to the training stations

training_station<- left_join(training_station, pop_station, by="Site" )

x= as.vector(training_station$Longitude)
y= as.vector(training_station$Latitude)
site<- as.data.frame(training_station$Site)
names(site)<- "Site"

### importing the raw input data


nam <- paste("Input_data_", month.name[qq] , sep = "")
mydata <- loadOneName( eval(nam), file=paste0(load_folder, "Input_mydata_file.RData"))

data_extraced_raw <- cbind(site, extra_dataframe(x=x, y=y, mydata=mydata)) 

Moni_estimated_tr <- extract_points(raster=pred_ras_stack[[month.name[qq]]], input_stations = training_station )

moni_real_estimate_tr<-cbind(data_extraced_raw$Moni, Moni_estimated_tr, month.name[qq])
names(moni_real_estimate_tr)<- c("Monitoring", "Estimate", "Month")



##### validation



###### adding the all stations lat and long information to the training stations

{
  load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")
  
  ## filtering the data for PM2.5 and 2015
  AQ_data_2015 <- AQ_data_12 %>%
    filter(years == 2015, Pollutant == "PM2.5" ) 
  AQ_data_2015 <- na.omit(AQ_data_2015)
  
  ## filtering for specific month
  AQ_data_PM25 <- AQ_data_2015 %>%
    filter (months==qq)
  
  # removing stations with very low monthly observations
  stations_remove <- AQ_data_PM25 %>%   # stations less than 5 day reading are removed to reduce the BIAS
    group_by(Site) %>%
    summarize(station_count= sum(Value == Value))%>%
    filter(station_count <= 5)
  
  # filterig stations with enough observations monthly
  if ( nrow(stations_remove) > 0 ){
    AQ_data_PM25<- AQ_data_PM25%>%
      filter( !(Site %in% c(stations_remove$Site )))
  }
  
  
  # monthly mean of the month
  AQ_data_PM25 <- AQ_data_PM25 %>%
    group_by(Site, years, months) %>%
    summarize(mon_mean= mean(Value, na.rm = T))
  
  # # monthly mean of january
  # AQ_data_PM25 <- AQ_data_PM25 %>%
  #   group_by(Site) %>%
  #   summarize(sea_mean=mean(mon_mean, na.rm = T))
  
  # goegraphical location of the stations
  
  coordin_site<-filter(AQ_data_2015,  Pollutant == "PM2.5" , months==qq)
  coordin_site<-coordin_site %>%
    dplyr::distinct(Site, .keep_all = T)%>%
    dplyr::select(-years)
  
  AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))
  
  AQ_data_PM25<- as.data.frame(AQ_data_PM25)     # to ungroup the variables
  
  AQ_data_PM25 <- AQ_data_PM25 %>%
    dplyr::select(Site,
           Longitude,
           Latitude,
           mon_mean)
  
  # remove all lines with NA
  AQ_data_PM25 <- na.omit(AQ_data_PM25)
  AQ_data_PM25<- AQ_data_PM25%>%
    filter(Site %in% c(validation_station$Site))
}


validation_station <- left_join(validation_station, pop_station, by="Site" )

x= as.vector(validation_station$Longitude)
y= as.vector(validation_station$Latitude)
site<- as.data.frame(validation_station$Site)
names(site)<- "Site"

### extracting validation data from raw data

data_extraced_raw_val <- cbind(site, extra_dataframe(x=x, y=y, mydata=mydata)) 


AOD_coeffi_val <- extract_points(raster=AOD_ras_stack[[qq]], input_stations = validation_station )
wind_speed_coeffi_val <- extract_points(raster=WS_ras_stack[[qq]], input_stations = validation_station )
DEW_coeffi_val <- extract_points(raster=DEW_ras_stack[[qq]], input_stations = validation_station )
#temp_coeffi_val <- extract_points(raster=Temp_coeffi[[qq]], input_stations = validation_station )
RH_coeffi_val <- extract_points(raster=RH_ras_stack[[qq]], input_stations = validation_station )
Cons_coeffi_val <- extract_points(raster=Intercept_ras_stack[[qq]], input_stations = validation_station )


# "Moni ~  AOD_mean + wind_speed  + DEW+ temp+ RH + constant"

Moni_estimated_val <- data_extraced_raw_val$AOD_mean*AOD_coeffi_val + data_extraced_raw_val$wind_speed*wind_speed_coeffi_val+
  data_extraced_raw_val$RH*RH_coeffi_val+ data_extraced_raw_val$DEW*DEW_coeffi_val+
  Cons_coeffi_val# +data_extraced_raw_val$temp*temp_coeffi_val 

moni_real_estimate_val<-cbind(AQ_data_PM25$mon_mean, Moni_estimated_val, month.name[qq])
names(moni_real_estimate_val)<- c("Monitoring", "Estimate", "Month")

All_extracted_data_tr<- rbind(All_extracted_data_tr,moni_real_estimate_tr)
All_extracted_data_val<- rbind( All_extracted_data_val,moni_real_estimate_val)

rm(list = ls()[!ls() %in% c( "All_extracted_data_tr","All_extracted_data_val", "pred_ras_stack",
                             "AOD_ras_stack","WS_ras_stack","RH_ras_stack","DEW_ras_stack","Intercept_ras_stack", "qq",
                             "loadOneName", "output_folder", "load_folder")])
}




# wind_speed_coeffi_training <- extract_points(raster=WS_coeffi[[qq]], input_stations = training_station )
# DEW_coeffi_training <- extract_points(raster=DEW_coeffi[[qq]], input_stations = training_station )
# temp_coeffi_training <- extract_points(raster=Temp_coeffi[[qq]], input_stations = training_station )
# #RH_coeffi_training <- extract_points(raster=RH_coeffi[[qq]], input_stations = training_station )
# Cons_coeffi_training <- extract_points(raster=Cons_coeffi[[qq]], input_stations = training_station )
# 
# 
# # "Moni ~  AOD_mean + wind_speed  + DEW+ temp+ RH "
# 
# Moni_estimated <- data_extraced_raw$AOD_mean*AOD_coeffi_training + data_extraced_raw$wind_speed*wind_speed_coeffi_training+
#   data_extraced_raw$temp*temp_coeffi_training + data_extraced_raw$DEW*DEW_coeffi_training+
#   Cons_coeffi_training #+data_extraced_raw$RH*RH_coeffi_training 
# 
# moni_real_estimate_tr<-cbind(data_extraced_raw$Moni, Moni_estimated, month.name[qq])
# names(moni_real_estimate_tr)<- c("Monitoring", "Estimate", "Month")
# 

###########################################################
###########################################################
###########################################################

# rsq <- function(x, y) {
#   summary(lm(y~x))$r.squared
# }
# rsq(dawit$Monitoring, dawit$Estimate)
# 
# 
# r_2_tr<-  rsq(All_extracted_data_tr$Monitoring, All_extracted_data_tr$Estimate)

r_2_tr<-   sum((All_extracted_data_tr$Estimate-mean(All_extracted_data_tr$Monitoring) )^2)/
   sum((All_extracted_data_tr$Monitoring-mean(All_extracted_data_tr$Monitoring))^2)

#### r 2 for the validation
dawit<- All_extracted_data_val%>%
  filter( Monitoring <= 70)

r_2_val<- (sum((All_extracted_data_val$Estimate-mean(All_extracted_data_val$Monitoring))^2))/(
  sum((All_extracted_data_val$Monitoring-mean(All_extracted_data_val$Monitoring))^2))



# r_2_val<- sum((All_extracted_data_val$Estimate-mean(All_extracted_data_val$Monitoring ))^2)/
#    sum((All_extracted_data_val$Monitoring-mean(All_extracted_data_val$Monitoring))^2))


#### Scatter plot of the training 
png(paste0(output_folder,"R2_training.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1))
print({
  
  plot(All_extracted_data_tr$Monitoring,All_extracted_data_tr$Estimate, xlim=c(0,150), ylim=c(0,150), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "Training")
  text(10,100, substitute(R^2==a, list(a = round(r_2_tr, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
})
dev.off()


#### Scatter plot of the validation 

png(paste0(output_folder,"R2_validation.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1))
print({
  plot(All_extracted_data_val$Monitoring, All_extracted_data_val$Estimate, xlim=c(0,150), ylim=c(0,150), 
       col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
       ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "Validation")
  text(10,100, substitute(R^2==a, list(a = round(r_2_val, digits=2), cex = 1.2)))
  abline(0,1,lwd=2, col="black")
})
dev.off()



###########################################################
###########################################################
###########################################################

#### RMSE for the training 

dawit<-function(xx){
  zz<- sqrt(sum(xx)/(length(xx)))
}

RMSE_tr<- All_extracted_data_tr%>%
  mutate(diff=(Monitoring-Estimate)^2)%>%
  group_by(Month)%>%
  summarise(RMSE= dawit(diff))


#### RMSE for the validation

RMSE_val<- All_extracted_data_val%>%
  mutate(diff=(Monitoring-Estimate)^2)%>%
  group_by(Month)%>%
  summarise(RMSE= dawit(diff))
mean(RMSE_val$RMSE)
mean(RMSE_tr$RMSE)

# x <- sort(runif(10, min=0, max=10))
# y <- runif(10, min=2, max=5)
# 
# #Polygon-Plot
# plot(x,y, type="n", ylim=c(0,5))
# polygon(c(x[1], x, x[length(x)]), c(0, y, 0), col="green")



library(ggplot2)


png(paste0(output_folder,"R2_train_month.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1))
print({
  sp <- ggplot(All_extracted_data_tr, aes(x=Monitoring, y=Estimate )) + geom_point(shape=3, col="blue")+
    xlim(0,100)+ ylim(0,100)+
    geom_abline(mapping = NULL, data = NULL,  slope=1, intercept=0,
                na.rm = FALSE, show.legend = NA,size = 1)+
    facet_wrap(~ Month, ncol=3, labeller = )
  sp
  
  
})
dev.off()


png(paste0(output_folder,"R2_val_month.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1))
print({
  sp <- ggplot(All_extracted_data_val, aes(x=Monitoring, y=Estimate )) + geom_point(shape=3, col="blue")+
    xlim(0,100)+ ylim(0,100)+
    geom_abline(mapping = NULL, data = NULL,  slope=1, intercept=0,
                na.rm = FALSE, show.legend = NA,size = 1)+
    facet_wrap(~ Month, ncol=3, labeller = )
  sp
  
  
})
dev.off()




# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png Dry_Temperatue_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Irradiance_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Wind_Speed_NCMS_1km_DUST_event_02_April_2015.gif





###########################################################
############   END of Validation    #######################
###########################################################



###########################################################
#######   Start of the Regression using LU    #############
###########################################################

# Sys.setenv(TZ="Asia/Dubai")
# Sys.time()
# help(Startup)

# #########   Land Use layer to be used for the Regression    #######


# load raster land use UAE
##### Lancover == 16  ---> desert area #########

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")
plot(LU)

LU[LU < 16] <- 0
LU[LU > 16] <- 0
LU <- LU/16
plot(LU)
resampled_LU<-aggregate(LU, fact=10, fun= sum ) # changing fact number to change the aggregation of the pixels
LU_fract_desert <- resampled_LU/100

str(LU_fract_desert)
res(LU_fract_desert)
plot(LU_fract_desert)

##### Lancover == 13  ---> urban area #########

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")

plot(LU)

LU[LU < 13] <- 0
LU[LU > 13] <- 0
LU <- LU/13
plot(LU)
resampled_LU<-aggregate(LU,fact=10, fun= sum ) # changing fact number 
LU_fract_urban <- resampled_LU/100

plot(LU_fract_urban)

res(LU_fract_urban)


####### exporting the landuse layers ##### 

 writeRaster(LU_fract_urban, "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/GWR_LU_variables/urban_fraction.tif",overwrite=TRUE)
 writeRaster(LU_fract_desert, "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/GWR_LU_variables/desert_fraction.tif",overwrite=TRUE)

rm(list = ls(all = TRUE))


#### END OF LANDUSE MAPS ####


###########################################################
#######   Start of the GWR Regression using LU  ###########
###########################################################

#### LOADING THE RESULT WORKSPACE

output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/GWR_LU_variables/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"


loadOneName <- function(objName, file, envir = parent.frame(),
                        assign.on.exit = TRUE) {
  tempEnv <- new.env()
  load(file, envir = tempEnv)
  stopifnot(objName %in% ls(tempEnv))
  if(assign.on.exit) {
    assign(objName, tempEnv[[objName]], envir = envir)
    return(invisible(tempEnv[[objName]]))
  }
  tempEnv[[objName]]
}

LU_fract_urban<- raster( "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/GWR_LU_variables/urban_fraction.tif")
LU_fract_desert<- raster( "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Images/GWR_LU_variables/desert_fraction.tif")



data_frame_predict<- data.frame()
pred_ras_stack<- stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("gwrG_pnt_", month.name[kk] , sep = "")
  model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  dawi<- data.frame(model_value$SDF)
  ras_r2<- rasterFromXYZ( dawi[, c("coord.x","coord.y", "pred")])
  r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  colnames(month_ind)<-c("Predicted", "month_ind")
  data_frame_predict<-rbind(data_frame_predict,month_ind)
  pred_ras_stack<- stack(pred_ras_stack,ras_r2)
}

# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"


data_frame_predict<- cbind(data_frame_predict, month.name[data_frame_predict$month_ind])
colnames(data_frame_predict)[3]<- "Month"

### building a raster from the monitoring

load(paste(load_folder,"Input_mydata_file.RData", sep=""))

Moni_ras_stack<-stack()
for (kk in 1:12){
  #kk=1
  nam <- paste("Input_data_", month.name[kk] , sep = "")
  # model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))
  
  assign("dawi", eval(parse(text = paste("Input_data_", month.name[kk] , sep = ""))) )
  
  ras_r2<- rasterFromXYZ( dawi[, c("x","y", "Moni")])
  #r2_data<- dawi[, c( "pred")]
  names(ras_r2)<- month.name[kk]
  # month_ind<- cbind(r2_data, rep (kk, length(r2_data)))
  # colnames(month_ind)<-c("Predicted", "month_ind")
  # data_frame_predict<-rbind(data_frame_predict,month_ind)
  Moni_ras_stack<- stack(Moni_ras_stack,ras_r2)
}



#### CHNAGING THE RESOLUTION OF THE RASTERS TO 1KM

moni_1km<- disaggregate(Moni_ras_stack, fact=10)

pred_1km<- disaggregate(pred_ras_stack, fact=10)





setwd("D:/Air Quality/GWR_with_met/")

#output_folder<- "D:/Air Quality/GWR_with_met/Result/Monthly/Results/Model_GWR_data/"

result_monthly<- list()

# OD_mean_jan<-raster("MODIS/AOD_mean_01.tif")
# plot(OD_mean_jan)

old<- Sys.time()

for (qq in 1:12){
  #qq=10
  
  # layers for the GWR
  moni_1km_mon<- moni_1km[[qq]]
  pred_1km_mon<- pred_1km[[qq]]
  
  LU_fract_urban_arra <- resample(LU_fract_urban,moni_1km_mon,"bilinear")
  LU_fract_desert_arra <- resample(LU_fract_desert,moni_1km_mon,"bilinear")
  

  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ####  as SpatialPointsDataFrame layers Method IIII  ####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
  
  moni_1km_mon_pts <- as.data.frame(moni_1km_mon, xy=T)
  colnames(moni_1km_mon_pts)<- c("x", "y", "Moni")
  
  pred_1km_mon_pts <- as.data.frame(pred_1km_mon, xy=T)
  colnames(pred_1km_mon_pts)<- c("x", "y", "Pred")
  
  UR_fraction_pts <- as.data.frame(LU_fract_urban_arra, xy=T)
  colnames(UR_fraction_pts)<- c("x", "y", "Urban")
  
  DE_fraction_pts <- as.data.frame(LU_fract_desert_arra, xy=T)
  colnames(DE_fraction_pts)<- c("x", "y", "Desert")
  
  mydata<- left_join(moni_1km_mon_pts, pred_1km_mon_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, UR_fraction_pts,  by = c("x", "y") )
  mydata<- left_join(mydata, DE_fraction_pts,  by = c("x", "y") )
  # mydata<- left_join(mydata, DEW_pts,  by = c("x", "y") )
  # mydata<- left_join(mydata, RH_pts,  by = c("x", "y") )
  # mydata<- left_join(mydata, Radiation_pts,  by = c("x", "y") )
  # 
  mydata<- na.omit(mydata)
  

 # save( mydata ,file= paste0(output_folder, "/GWR_LU_variables/",month.name[[qq]],"_mydata.RData"))
  
  rm(list = ls()[!ls() %in% c( "LU_fract_desert","LU_fract_urban","moni_1km","pred_1km", "mydata","qq",
                               "loadOneName", "output_folder", "load_folder")])
  
  
  library(spgwr)
  library(parallel)
  # for parallel processing 
  
  bwG_pnt <- gwr.sel(Moni ~  Pred + Urban  , #+   Desert ,#+ RH ,#+ temp
                     data= mydata,  coords=cbind( mydata$x , mydata$y),
                     gweight = gwr.Gauss, #RMSE=T,
                     method="cv", verbose = F, show.error.messages = T)

  
 
  
  #save( bwG_pnt ,file= paste0(output_folder, "GWR_LU_variables/",month.name[[qq]],"_bwG_pnt.RData"))
  
  
  #load(file= paste0(output_folder, "GWR_LU_variables/",month.name[[qq]],"_bwG_pnt.RData"))
  #load(file= paste0(output_folder, "/GWR_LU_variables/",month.name[[qq]],"_mydata.RData"))
  # Sys.sleep(1800)

  
  gwrG_pnt <- gwr(Moni ~  Pred +   Desert + Urban, #   ,#+RH , #+ temp
                  data= mydata,  bandwidth = bwG_pnt*1.1,  coords=cbind( mydata$x , mydata$y),
                  gweight = gwr.Gauss, hatmatrix = F ,predictions = T , cl=NULL )#,
                  #cl=NULL)

  
  resave <- function(..., list = character(), file) {
    previous  <- load(file)
    var.names <- c(list, as.character(substitute(list(...)))[-1L])
    for (var in var.names) assign(var, get(var, envir = parent.frame()))
    save(list = unique(c(previous, var.names)), file = file)
  }
  
  

  nam <- paste("gwrG_pnt_", month.name[qq], sep = "")
  nam_mydata<- paste("Input_data_", month.name[qq], sep = "")
  assign(nam, gwrG_pnt)
  assign(nam_mydata, mydata )
  if (qq==1){
    save(list= eval(nam), file= paste0(output_folder, "GWR_file.RData"))
    save(list= eval(nam_mydata), file= paste0(output_folder, "Input_mydata_file.RData"))
  }else{
    resave(list = eval(nam), file= paste0(output_folder, "GWR_file.RData"))
    resave(list= eval(nam_mydata), file= paste0(output_folder, "Input_mydata_file.RData"))
  }
  
  #dawit<-load(file)
  #dadada<- as.data.frame(gwrG_pnt$SDF)
  # hist(gwrG_pnt$SDF$Urban, breaks=1000)
  
  
  
  #plot(mydata$Moni)
  # print(mean(gwrG_pnt$SDF$localR2))
  
  #result_monthly<-c(result_monthly, gwrG_pnt)
  
  print(qq)
  print(plot(gwrG_pnt$SDF$localR2, main=qq))
  
  rm(list = ls()[!ls() %in% c( "moni_1km","pred_1km", "result_monthly", "mydata","qq",
                                "loadOneName", "output_folder", "load_folder")])
  
}


# save(result_monthly, file="D:/Air Quality/GWR_with_met/Result/data/result_regression_cv_70_30_rm_RH.RData")

new <- Sys.time() - old # calculate difference
print(new)











