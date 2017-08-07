
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

write_csv(All_AWS_data, "D:/Air Quality/GWR_with_met/met_refined/AWS_concatenated_met_2015.csv")




#############################################################################################
##### create single files for each time stamp ###############################################
#############################################################################################


setwd("D:/Air Quality/GWR_with_met/")
All_AWS_data <- read_csv("met_refined/AWS_concatenated_met_2015.csv")
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
  
   {  
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
   } 
   
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

writeRaster(Total_wind, "D:/Air Quality/GWR_with_met/Variables_raster/Wind_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_temp, "D:/Air Quality/GWR_with_met/Variables_raster/Temp_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_RH, "D:/Air Quality/GWR_with_met/Variables_raster/RH_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_Radiation, "D:/Air Quality/GWR_with_met/Variables_raster/Radiation_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)
writeRaster(Total_dew, "D:/Air Quality/GWR_with_met/Variables_raster/DEW_NCMS_10km.tif", 
            options="INTERLEAVE=BAND", overwrite = TRUE)

save(Total_wind,Total_temp,Total_RH,Total_Radiation,Total_dew, file="D:/Air Quality/GWR_with_met/Variables_raster/NCMS_10km.RData")


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
######### AOD for 2015 ##########################################################
#################################################################################


# coefi_conver<- 85    # this number is used to change the AOD values to PM2.5 mg/m3 obtaine by the regression of 
# the station values with the satellite values from MODIS (2015-2016)
# could also be changed from the ECWMF model values for AOD and PM2.5
{
  Total_AOD<- stack()
for (kk in 1:12){
  
    ### for the year of 2016
    #i=1
    coefi_conver<- 85
    
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

writeRaster(Total_AOD, "D:/Air Quality/GWR_with_met/AOD_MODIS_2015/AOD_mean_monthly_2015.tif",overwrite=TRUE,
            options="INTERLEAVE=BAND")
save(Total_AOD, file="D:/Air Quality/GWR_with_met/AOD_MODIS_2015/AOD_mean_monthly_2015.RData")



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
##########   Getting the monthly monitoring values           ########
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$



Total_monitoring_PM25<- stack()
output_validation<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/"
# i=1
load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")

## filtering the data for PM2.5 and 2015
AQ_data_2015 <- AQ_data_12 %>%
  filter(years == 2015, Pollutant == "PM2.5" ) 
AQ_data_2015 <- na.omit(AQ_data_2015)

## filtering for specific month
AQ_data_PM25 <- AQ_data_2015 #%>%
#  filter (months==i)

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


# Annual mean 
AQ_data_PM25 <- AQ_data_PM25 %>%
  group_by(Site, years) %>%
  summarize(mon_mean= mean(Value, na.rm = T))

# # monthly mean of january
# AQ_data_PM25 <- AQ_data_PM25 %>%
#   group_by(Site) %>%
#   summarize(sea_mean=mean(mon_mean, na.rm = T))

# goegraphical location of the stations

coordin_site<-filter(AQ_data_2015,  Pollutant == "PM2.5" )
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

if (!file.exists(paste0(output_validation,"Annual_training.RData"))){
  # 30% training stations
  pop_station<- AQ_data_PM25 %>%
    dplyr::distinct(Site)
  
  
  training_station <- sample_n(pop_station, floor(0.7*nrow(pop_station)), replace = FALSE)
  validation_station <- subset(pop_station, !(pop_station$Site %in% training_station$Site))
  save(training_station, file=paste0(output_validation,"Annual_training.RData"))
  save(validation_station, file=paste0(output_validation,"Annual_validation.RData"))
}else{
  load(paste0(output_validation,"Annual_training.RData"))
  load(paste0(output_validation,"Annual_validation.RData"))
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
names(r_moni)<- "Annual"
#plot(r_moni)
crs(r_moni) <-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

Total_monitoring_PM25<- r_moni 
rm(list = ls()[!ls() %in% c( "Total_monitoring_PM25")])


 
#plot(Total_monitoring_PM25)

writeRaster(Total_monitoring_PM25, filename="D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/Layers/Moni_PM25_mean_annual_2015.tif",
            overwrite=TRUE)
save(Total_monitoring_PM25, file= "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/Layers/Moni_PM25_mean_annual_2015.RData")

rm(Total_monitoring_PM25)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###########       Geographic Weighted Regression Model        #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


{
  
 # Importing and restructuring the rasters

 rm(list=ls(all.names = TRUE))

 old <- Sys.time()



 setwd("D:/Air Quality/GWR_with_met/")

 output_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/"

 result_monthly<- list()

 # OD_mean_jan<-raster("MODIS/AOD_mean_01.tif")
 # plot(OD_mean_jan)

  #qq=10
  load("D:/Air Quality/GWR_with_met/Variables_raster/NCMS_10km.RData")
  load("D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/Layers/Moni_PM25_mean_annual_2015.RData")
  #load("D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")
  load("D:/Air Quality/GWR_with_met/AOD_MODIS_2015/AOD_mean_monthly_2015.RData")
  # LU_fract_desert<- raster("Land_cover/desert_fraction.tif")#/100 ## constant for all months
  # LU_fract_urban<- raster("Land_cover/urban_fraction.tif")#/100   ## constant for all months
  # plot(Total_AOD$January)
  # AOD from MODIS
  #name_mon<- sprintf("%02d",qq)
  AOD_mean_jan<-mean(Total_AOD)
  #plot(AOD_mean_jan)
  #crs(AOD_mean_jan)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
  
  # PM 2.5 from the monitoring stations
  r_moni<- Total_monitoring_PM25
  
  # meteorological Varibales from the met stations
  # Wind speed, Temprature , RH , Radiation , DEW
  
  wind_speed<- mean(Total_wind)
  temp<- mean(Total_temp)
  DEW<- mean(Total_dew)
  RH<- mean(Total_RH)
  Radiation<- mean(Total_Radiation)
  
  
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
  ####  as dataframe with points layers Method I      ####
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
  
  save( mydata, file=paste0( output_folder, "model_input_file.RData") )
  
  library(spgwr)
  added_val<- list()

  resave <- function(..., list = character(), file) {
    previous  <- load(file)
    var.names <- c(list, as.character(substitute(list(...)))[-1L])
    for (var in var.names) assign(var, get(var, envir = parent.frame()))
    save(list = unique(c(previous, var.names)), file = file)
  }
  
  # produce all the possible combinations of the equation 
  
  for( i in 1:5){
  xx<- list(combn(c("wind_speed" , "DEW" , "temp" ,"RH" ,"Radiation" ), i))
  added_val<- c(added_val,xx)
  }
  nn=1
  for( kk in 1: length(added_val)){
   
    for( qq in 1:ncol(added_val[[kk]]) ){
      name_formula<-   paste0(added_val[[kk]][,qq],sep="+", collapse="")
  
      arra_trail<- substr(name_formula , start= 1 , 
                           stop = nchar(name_formula)-1  )
      dawit<- paste("Moni ~ ", colnames(mydata[4]), "+" , arra_trail )
      
      bwG_pnt <- gwr.sel(dawit ,#+ RH ,#+ temp
                         data= mydata,  coords=cbind( mydata$x , mydata$y),
                         gweight = gwr.Gauss, #RMSE=T,
                         method="cv", verbose = F, show.error.messages = T)
      
      gwrG_pnt <- gwr(dawit ,#+RH , #+ temp
                      data= mydata,  bandwidth = bwG_pnt*1.1,  coords=cbind( mydata$x , mydata$y),
                      gweight = gwr.Gauss, hatmatrix = TRUE,predictions = T)
      
      
      nam <- paste("gwrG_pnt_", sprintf("%02d",nn) , sep = "")
      assign(nam, gwrG_pnt)
      if (nn==1){
        save(list= eval(nam), file= paste0(output_folder, "GWR_file.RData"))

        
      }else{
        resave(list = eval(nam), file= paste0(output_folder, "GWR_file.RData"))
      }
      
      write.table(dawit, file = paste0(output_folder, "combination_formula.csv"), row.names = nn, append = T,
                  col.names = F)  
      print(plot(gwrG_pnt$SDF$localR2, main=nn))
      
      nn<- nn+1
    }
  }
  
    
  #dawit<-load(file)
  #dadada<- as.data.frame(gwrG_pnt$SDF)
  #plot(dadada$localR2)
  
  #plot(mydata$Moni)
  # print(mean(gwrG_pnt$SDF$localR2))

  
rm(list = ls()[!ls() %in% c( "result_monthly", "old","output_folder")])
  




# save(result_monthly, file="D:/Air Quality/GWR_with_met/Result/data/result_regression_cv_70_30_rm_RH.RData")



new <- Sys.time() - old # calculate difference
print(new)

}


####################################
####################################

#### expotrs the test results to CSV files for several tests to choose the best combination


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

output_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/"

model_value <- load(file=paste0(output_folder, "GWR_file.RData"))

nn_all<- length(model_value)


rm(list = ls()[!ls() %in% c( "nn_all", "loadOneName","output_folder")])


for (nn in 1:nn_all){
  #nn=1
  nam <- paste("gwrG_pnt_", sprintf("%02d",nn) , sep = "")
  
  model_value <- loadOneName(eval(nam),file=paste0(output_folder, "GWR_file.RData"))
  
  anova_model<- anova(model_value)
  
  
  ####  test from anova
  
  ## "sim no" ,"Df",  "Sum Sq",  "Mean Sq",  "F value"
  
  write.table(anova_model[3,], file = paste0(output_folder, "anova.csv"), row.names = nn, append = T,
              col.names = F)  
  
  
  #mat<-as.matrix(c(modl_1$statistic, modl_1$estimates, modl_1$p.value ))
  
  # Tests of the residuals from   the GWR book (2002) 
  
  modl_1<- BFC02.gwr.test(model_value)     # compares OLS model fit to GWR model fit
  
  # "sim_no" "F" "SS_OLS_residuals" "SS_GWR_residuals" "p.value" "AICb" "AICh" "AICc"
  
  write.table(t(c(modl_1$statistic, modl_1$estimates, modl_1$p.value , model_value$results$AICb,
                  model_value$results$AICh, model_value$results$AICc)), file = paste0(output_folder, "GWR book.csv"), row.names = nn, append = T,
              col.names = F)  
  
  
  #### Moran's I test for spatial autocorrelation ( of the residuals)
  
  Q1.gal<-nb2listw(knn2nb(knearneigh(as.matrix(model_value$SDF@coords))))
  moran<- moran.test(model_value$SDF$gwr.e, Q1.gal,
                     alternative="two.sided")    
  
  # "sim_no" "Moran_I_standard deviate" "Moran I statistic" "Expectation " "Variance" "p.value"
  write.table(t(c(moran$statistic, moran$estimate, moran$p.value )),
              file = paste0(output_folder, "Moran_I_stat.csv"), row.names = nn, append = T,
              col.names = F)  
  
  # the stat summary of the r2 of the 
  # "sim_no" "Min." "1st_Qu." "Median" "Mean_3rd_Qu." "Max." 
  # summary(model_value$SDF$localR2)
  
  write.table(t(c(summary(model_value$SDF$localR2))),
              file = paste0(output_folder, "r2_stat.csv"), row.names = nn, append = T,
              col.names = F) 
  
  # the stat summary of the Intercept of the 
  #  "sim_no" "Min." "1st_Qu." "Median" "Mean_3rd_Qu." "Max." 
  # summary(model_value$SDF$localR2)
  
  write.table(t(c(summary(model_value$SDF$`(Intercept)`))),
              file = paste0(output_folder, "Intercept_stat.csv"), row.names = nn, append = T,
              col.names = F) 
  
  # the stat summary of the BIAS of the 
  #  "sim_no" "Min." "1st_Qu." "Median" "Mean_3rd_Qu." "Max." 
  # summary(model_value$SDF$localR2)
  
  
  load(paste0( output_folder, "model_input_file.RData"))
  
  BIAS <-mydata$Moni- model_value$SDF$pred
  
  write.table(t(c(summary(BIAS))),
              file = paste0(output_folder, "BIAS_stat.csv"), row.names = nn, append = T,
              col.names = F) 
  
  

}


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

output_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/"


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

nn=17
nam <- paste("gwrG_pnt_", sprintf("%02d",nn) , sep = "")


model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))


# output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/Results of 70_30 rm RH/"



#### ploting the histogram of the r2  ####




data_frame_r2<- data.frame(model_value$SDF$localR2)
colnames(data_frame_r2)<- "r2"

scaleFUN <- function(x) sprintf("%.1f", x)

hp <- ggplot(data_frame_r2, aes(x=r2)) + 
  # qplot( data_frame_r2$r2, y=NULL, data_frame_r2, binwidth= 0.01, xlim=c(0,1), facet_wrap( ~ data_frame_r2$month_ind, ncol=3),
  #        geom="histogram", fill=I("blue" ),alpha=I(.4),ylab ="")+
  geom_histogram (binwidth=0.01,fill=I("blue" ),alpha=I(.8))+ #colour="blue",
  scale_x_continuous(limits = c(0, 1), labels= scaleFUN , name=expression(R^2))+
  scale_y_continuous(name="Counts" )+
  ggtitle(expression(paste("Local ", R^2))) + 
  theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))+
  theme(axis.text=element_text(size=12, face="bold"),
        axis.title=element_text(size=14,face="bold"))

hp

png(paste0(output_folder,"histogram_r2.png"),
    width = 1050, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(hp)
dev.off()


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

######
######
###### ploting the coefficients #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# [1] "sum.w"               "X.Intercept."        "AOD_mean"            "wind_speed"          "DEW"                
# [6] "RH"                  "X.Intercept._se"     "AOD_mean_se"         "wind_speed_se"       "DEW_se"             
# [11] "RH_se"               "gwr.e"               "pred"                "pred.se"             "localR2"            
# [16] "X.Intercept._se_EDF" "AOD_mean_se_EDF"     "wind_speed_se_EDF"   "DEW_se_EDF"          "RH_se_EDF"          
# [21] "pred.se_EDF"         "coord.x"             "coord.y"             "optional"           
# 


rm(list = ls()[!ls() %in% c( "model_value","output_folder")])

library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


#### AOD_mean COEFFICIENTS ####


####### color pallet

cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))


data_frame_SDF<- data.frame(model_value$SDF)

col_names<- colnames(data_frame_SDF)

result_stack<- stack()

for ( i in 1:21){

  dawi<- rasterFromXYZ( data_frame_SDF[, c("coord.x","coord.y", col_names[i])])
  crs(dawi)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
  names(dawi)<- col_names[i]
  result_stack<-stack(result_stack,dawi)
}


for ( i in 1:21){

max_val<-ceiling(max(maxValue(result_stack[[i]])))
min_val<-floor(min(minValue(result_stack[[i]])))


stat_dat<- summary(as.vector(result_stack[[i]]))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(result_stack[[i]])
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-result_stack[[i]]
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 

### getting the right units

if (names(AOD_plot) %in% c("sum.w","X.Intercept.", "AOD_mean", "X.Intercept._se" ,"AOD_mean_se",
                           "gwr.e", "pred","pred.se","X.Intercept._se_EDF","AOD_mean_se_EDF", "pred.se_EDF"  )) {
   unit_to<- expression(paste("     ", mu,"g ",m^-3) )
}
if ( names(AOD_plot) %in% c( "wind_speed", "wind_speed_se", "wind_speed_se_EDF" ) ){
  unit_to<- expression(paste("     ", mu,"g ",m^-3,"/(m ", sec^-1,")"))
}
if (  names(AOD_plot) %in% c("DEW","DEW_se" , "DEW_se_EDF")){
  unit_to<- expression(paste("     ", mu,"g ",m^-3, ~degree~C^-1))
}
if ( names(AOD_plot) %in% c( "RH", "RH_se","RH_se_EDF" )  ){
  unit_to<- expression(paste("     ", mu,"g ",m^-3))
}
if ( names(AOD_plot) %in% c( "localR2" )  ){
  unit_to<- NULL
}




h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= names(AOD_plot) ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=unit_to
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



png(paste0(output_folder, gsub('\\.', '_', names(AOD_plot)), ".png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()



### plots of histograms



#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,gsub('\\.', '_', names(AOD_plot)),"_hist.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=100 , main = paste("Histogram of ", gsub('\\.', '_', names(AOD_plot)) ), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()

}


###### Plot of the observed points ######

###### the monitoring stations

Total_monitoring_PM25<- stack()
output_validation<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/"
# i=1
load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")

## filtering the data for PM2.5 and 2015
AQ_data_2015 <- AQ_data_12 %>%
  filter(years == 2015, Pollutant == "PM2.5" ) 
AQ_data_2015 <- na.omit(AQ_data_2015)

## filtering for specific month
AQ_data_PM25 <- AQ_data_2015 #%>%
#  filter (months==i)

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


# Annual mean 
AQ_data_PM25 <- AQ_data_PM25 %>%
  group_by(Site, years) %>%
  summarize(mon_mean= mean(Value, na.rm = T))

# # monthly mean of january
# AQ_data_PM25 <- AQ_data_PM25 %>%
#   group_by(Site) %>%
#   summarize(sea_mean=mean(mon_mean, na.rm = T))

# goegraphical location of the stations

coordin_site<-filter(AQ_data_2015,  Pollutant == "PM2.5" )
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


output_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/"


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

nn=17
nam <- paste("gwrG_pnt_", sprintf("%02d",nn) , sep = "")


model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))


library(viridis)
library(lattice)

dir <- "D:/Air Quality/GWR/UAE_boundary"
shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
plot(shp_UAE)


####### color pallet

cool = rainbow(100, start=rgb2hsv(col2rgb('green'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(100, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('green'))[1])
#middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
cols = c(rev(cool),  rev(warm))


data_frame_SDF<- data.frame(model_value$SDF)

col_names<- colnames(data_frame_SDF)


result_stack<- stack()
dawi<- rasterFromXYZ( data_frame_SDF[, c("coord.x","coord.y", "pred")])
crs(dawi)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
names(dawi)<- "pred"
result_stack<-stack(result_stack,dawi)

i=1
SPDF_observed <- AQ_data_PM25

max_val<-ceiling(max(maxValue(result_stack[[i]])))
min_val<-floor(min(minValue(result_stack[[i]])))


stat_dat<- summary(as.vector(result_stack[[i]]))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<- if (min_val < min(SPDF_observed$mon_mean)) min_val else floor(min(SPDF_observed$mon_mean) ) # floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<- if (max_val > max(SPDF_observed$mon_mean )) max_val else ceiling(max(SPDF_observed$mon_mean ) )#ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(result_stack[[i]])
#vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-result_stack[[i]]
#AOD_plot[AOD_plot < low_IQR ]<- low_IQR
#AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


if (names(AOD_plot) %in% c("sum.w","X.Intercept.", "AOD_mean", "X.Intercept._se" ,"AOD_mean_se",
                           "gwr.e", "pred","pred.se","X.Intercept._se_EDF","AOD_mean_se_EDF", "pred.se_EDF"  )) {
  unit_to<- expression(paste("PM2.5 (", mu,"g ",m^-3, ")") )
}

##### creating spatial data frame of the observed points

# 
# 
# # coordinates(SPDF_observed) <- ~Longitude+Latitude
# # 
# # layer(sp.points(xy, pch=ifelse(pts$z1 < 0.5, 2, 3), cex=2, col=1), columns=1) +
# #   layer(sp.points(xy, pch=ifelse(pts$z2 < 0.5, 2, 3), cex=2, col=1), columns=2)
# # 
# # stack.SpatialPointsDataFrame()
# # library(sp)
# # library(rgdal)
# # library(ggplot2)
# # library(rgeos)
# 
# dawit<- rasterFromXYZ(AQ_data_PM25[, c("Longitude","Latitude", "mon_mean")])
# 
# # lab_val<- floor(as.numeric( seq(low_IQR, high_IQR, length.out=7)))
# # break_indi<- NULL
# # for ( jj in 1: length (lab_val)){
# # ind_lab<- which(min(abs(unique(c(seq(low_IQR, high_IQR, length.out=200)))-lab_val[jj]))== 
# #                   abs(unique(c(seq(low_IQR, high_IQR, length.out=200)))-lab_val[jj] ))
# # break_indi<- c(break_indi, ind_lab )
# # }
# # 
# # if( min(SPDF_observed$mon_mean) < min_val){
# # l_p<-min_val}else{
# #   l_p<-min(SPDF_observed$mon_mean)
# # }
# # if( max(SPDF_observed$mon_mean)< max_val){
# #   u_p<-max(SPDF_observed$mon_mean)}else{
# #     u_p<-max_val
# #   }
# # 
# #  break_point<-  unique(floor(as.numeric( c(low_IQR,  seq(l_p, u_p, length.out=7) , high_IQR))))
  
break_point<- floor(as.numeric( seq(low_IQR, high_IQR, length.out=7)))

UAE_polygon <- fortify(shp_UAE)
gwr.point1<-ggplot(SPDF_observed, aes(x=Longitude,y=Latitude))+
  geom_polygon(data=UAE_polygon,aes(long, lat, group = group) , fill=NA,
               colour = "black", size = 0.7) + #alpha("darkred", 1/2)
  #scale_fill_manual(values = c("skyblue", "grey97"))+
  geom_point(aes(colour=SPDF_observed$mon_mean), size= 4)+
  scale_colour_gradientn(colours= cols, guide = guide_colorbar(barwidth = 0.5, barheight = 15), breaks= break_point, 
                         limits=c(min(break_point), max(break_point) ))+
                          #labels=floor(as.numeric( seq(low_IQR, high_IQR, length.out=7)))
  #guides(fill = guide_colorbar(barwidth = 0.5, barheight = 30))+
  coord_fixed()+
  labs(colour=unit_to)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(),axis.line = element_line(colour = "black"),
        panel.border =element_rect(colour = "black",fill = NA))+
  coord_fixed()

png(paste0(output_folder,  "observed_stations.png"),
    width = 1680, height = 1050, units = "px", pointsize = 15,
    bg = "white", res = 150)
print(gwr.point1)
dev.off()


###### predicted 


h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= names(AOD_plot) ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=unit_to
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



png(paste0(output_folder, gsub('\\.', '_', names(AOD_plot)), ".png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()



# ### plots of histograms
# 
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,gsub('\\.', '_', names(AOD_plot)),"_hist.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=100 , main = paste("Histogram of ", gsub('\\.', '_', names(AOD_plot)) ), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 

###### Monitoring ( OBSERVED from Stations Spatial) ######

load("D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/model_input_file.RData")

moni_data<- rasterFromXYZ( mydata[, c("x","y", "Moni")])
names(moni_data)<- "Moni"
crs(moni_data)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

h <- rasterVis::levelplot(moni_data, 
                          margin=FALSE, main= names(moni_data) ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=unit_to
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
                          names.attr=rep(names(moni_data))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h


png(paste0(output_folder, "observed_spatial.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()





###### BIAS ######

load("D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/model_input_file.RData")

moni_data<- rasterFromXYZ( mydata[, c("x","y", "Moni")])
pred_model<- result_stack$pred

BIAS<- moni_data-pred_model
names(BIAS)<-"BIAS"

max_val<-ceiling(max(maxValue(BIAS)))
min_val<-floor(min(minValue(BIAS)))


stat_dat<- summary(as.vector(BIAS))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


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
                          margin=FALSE, main= names(AOD_plot) ,
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



png(paste0(output_folder, gsub('\\.', '_', names(AOD_plot)), ".png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()



### plots of histograms



#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,gsub('\\.', '_', names(AOD_plot)),"_hist.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=50 , main = paste("Histogram of ", gsub('\\.', '_', names(AOD_plot)) ), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()



# #### Wind Speed COEFFICIENTS ####
# 
# 
# WSpeed_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "wind_speed")])
#   names(dawi)<- month.name[(kk+1)]
#   WSpeed_coeffi<-stack(WSpeed_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(WSpeed_coeffi)))
# min_val<-floor(min(minValue(WSpeed_coeffi)))
# 
# stat_dat<- summary(as.vector(WSpeed_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])*2))# n is the space after IQR
# 
# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# 
# 
# vec_all<- as.vector(WSpeed_coeffi)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# WSpeed_plot <-WSpeed_coeffi
# WSpeed_plot[WSpeed_plot < low_IQR ]<- low_IQR
# WSpeed_plot[ WSpeed_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(WSpeed_plot, 
#                           margin=FALSE, main= "Wind Speed COEFFICIENTS" ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3,"/(m ", sec^-1,")"))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(WSpeed_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_WSpeed.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_WSpeed.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of Wind Speed Coefficients"), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# 
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# 
# 
# 
# #### DEW COEFFICIENTS ####
# 
# 
# DEW_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "DEW")])
#   names(dawi)<- month.name[(kk+1)]
#   DEW_coeffi<-stack(DEW_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(DEW_coeffi)))
# min_val<-floor(min(minValue(DEW_coeffi)))
# 
# stat_dat<- summary(as.vector(DEW_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
# 
# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# 
# 
# vec_all<- as.vector(DEW_coeffi)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# DEW_plot <-DEW_coeffi
# DEW_plot[DEW_plot < low_IQR ]<- low_IQR
# DEW_plot[ DEW_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(DEW_plot, 
#                           margin=FALSE, main= "DEW Temp. COEFFICIENTS" ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3, ~degree~C^-1))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(DEW_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_DEW.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_DEW.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of DEW Temp. Coefficients"), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# 
# 
# #### TEMP. COEFFICIENTS ####
# 
# 
# Temp_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "temp")])
#   names(dawi)<- month.name[(kk+1)]
#   Temp_coeffi<-stack(Temp_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(Temp_coeffi)))
# min_val<-floor(min(minValue(Temp_coeffi)))
# 
# stat_dat<- summary(as.vector(Temp_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
# 
# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# 
# 
# vec_all<- as.vector(Temp_coeffi)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# Temp_plot <-Temp_coeffi
# Temp_plot[Temp_plot < low_IQR ]<- low_IQR
# Temp_plot[ Temp_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(Temp_plot, 
#                           margin=FALSE, main= "Temp. COEFFICIENTS" ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3, ~degree~C^-1))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(Temp_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_temp.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_temp.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of Temp. Coefficients"), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# 
# 
# # #### RH COEFFICIENTS ####
# # 
# # 
# # RH_coeffi<- stack()
# # 
# # for (kk in 0:11){
# #   
# #   dawi<- result_monthly[[kk*12+1]]
# #   # dawi<- cbind(dawi,coordi_x_y)
# #   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "RH")])
# #   names(dawi)<- month.name[(kk+1)]
# #   RH_coeffi<-stack(RH_coeffi,dawi)
# # }
# # 
# # max_val<-ceiling(max(maxValue(RH_coeffi)))
# # min_val<-floor(min(minValue(RH_coeffi)))
# # 
# # stat_dat<- summary(as.vector(RH_coeffi))
# # IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
# # 
# # low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# # high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# # 
# # 
# # vec_all<- as.vector(RH_coeffi)
# # vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# # 
# # xxx<- pretty( vec_all, n=10)
# # xxx<- (c(min_val, xxx, max_val))
# # 
# # RH_plot <-RH_coeffi
# # RH_plot[RH_plot < low_IQR ]<- low_IQR
# # RH_plot[ RH_plot >  high_IQR]<- high_IQR
# # 
# # ## 
# # 
# # 
# # ### plots of maps 
# # 
# # 
# # 
# # h <- rasterVis::levelplot(RH_plot, 
# #                           margin=FALSE, main= "Relative Humidity COEFFICIENTS" ,
# #                           ## about colorbar
# #                           colorkey=list(
# #                             space='right',                   
# #                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
# #                                          font=3),
# #                             axis.line=list(col='black'),
# #                             width=0.75,
# #                             title=expression(paste("     ", mu,"g ",m^-3))
# #                           ),   
# #                           ## about the axis
# #                           par.settings=list(
# #                             strip.border=list(col='transparent'),
# #                             strip.background=list(col='transparent'),
# #                             axis.line=list(col='black')
# #                           ),
# #                           scales=list(draw=T, alternating= F),            
# #                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
# #                           col.regions = cols,
# #                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
# #                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
# #                           names.attr=rep(names(RH_plot))) +
# #   latticeExtra::layer(sp.polygons(shp_UAE))
# # #h
# # 
# # 
# # 
# # png(paste0(output_folder,"Coefffi_RH.png"),
# #     width = 1680, height = 1050, units = "px", pointsize = 30,
# #     bg = "white", res = 150)
# # print(h)
# # dev.off()
# # 
# # 
# # ### plots of histograms
# # 
# # 
# # #vec_all<- as.vector(AOD_plot)
# # png(paste0(output_folder,"hist_RH.png"),
# #     width = 1680, height = 1050, units = "px", pointsize = 30,
# #     bg = "white", res = 150)
# # print({
# #   histogram(vec_all,  breaks=500 , main = paste("Histogram of Relative Humidity Coefficients"), type="percent",
# #             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# # })
# # dev.off()
# # 
# # 
# # rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# # 
# 
# 
# #### Prediction COEFFICIENTS ####
# 
# 
# Pre_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "pred")])
#   names(dawi)<- month.name[(kk+1)]
#   Pre_coeffi<-stack(Pre_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(Pre_coeffi)))
# min_val<-floor(min(minValue(Pre_coeffi)))
# 
# stat_dat<- summary(as.vector(Pre_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
# 
# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# 
# 
# vec_all<- as.vector(Pre_coeffi)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# pre_plot <-Pre_coeffi
# pre_plot[pre_plot < low_IQR ]<- low_IQR
# pre_plot[ pre_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(pre_plot, 
#                           margin=FALSE, main= "Prediction PM2.5 " ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(pre_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_Pred.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_pred.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of Predition of PM2.5"), type="percent",
#             xlab= list("PM2.5",cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE", "output_folder")])
# 
# 
# 
# #### Intercept ####
# 
# 
# intercept_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "X.Intercept.")])
#   names(dawi)<- month.name[(kk+1)]
#   intercept_coeffi<-stack(intercept_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(intercept_coeffi)))
# min_val<-floor(min(minValue(intercept_coeffi)))
# 
# stat_dat<- summary(as.vector(intercept_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR
# 
# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
# 
# 
# vec_all<- as.vector(intercept_coeffi)
# vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# intercept_plot <-intercept_coeffi
# intercept_plot[intercept_plot < low_IQR ]<- low_IQR
# intercept_plot[ intercept_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(intercept_plot, 
#                           margin=FALSE, main= "Intercept (Error)" ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(intercept_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_Intercept.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_Intercept.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of Intercept (Error)"), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# 
# 
# 
# ######

# ###### BIAS ######
# 
# load("D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")
# 
# 
# Pre_coeffi<- stack()
# 
# for (kk in 0:11){
#   
#   dawi<- result_monthly[[kk*12+1]]
#   # dawi<- cbind(dawi,coordi_x_y)
#   dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "pred")])
#   names(dawi)<- month.name[(kk+1)]
#   Pre_coeffi<-stack(Pre_coeffi,dawi)
# }
# 
# BIAS_coeffi<- stack()
# 
# for (kk in 1:12){
#   
#   
#   crs(Pre_coeffi[[month.name[kk]]])<- " +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#   r_moni<- projectRaster(from=Total_monitoring_PM25[[month.name[kk]]],to=Pre_coeffi[[month.name[kk]]])
#   dawi<- r_moni-Pre_coeffi[[month.name[kk]]]
#   names(dawi)<- month.name[kk]
#   BIAS_coeffi<-stack(BIAS_coeffi,dawi)
# }
# 
# max_val<-ceiling(max(maxValue(BIAS_coeffi)))
# min_val<-floor(min(minValue(BIAS_coeffi)))
# 
# stat_dat<- summary(as.vector(BIAS_coeffi))
# # IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2]))*20)# n is the space after IQR
#  IQR<- floor(as.numeric((stat_dat[6]-stat_dat[1])))# n is the space after IQR
# 
# # low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# # high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
#  low_IQR<-floor(as.numeric((stat_dat[1])))
#  high_IQR<- ceiling(as.numeric((stat_dat[6])))
# 
# vec_all<- as.vector(BIAS_coeffi)
# #vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
# 
# xxx<- pretty( vec_all, n=10)
# xxx<- (c(min_val, xxx, max_val))
# 
# BIAS_plot <-BIAS_coeffi
# # BIAS_plot[BIAS_plot < low_IQR ]<- low_IQR
# # BIAS_plot[ BIAS_plot >  high_IQR]<- high_IQR
# 
# ## 
# 
# 
# ### plots of maps 
# 
# 
# 
# h <- rasterVis::levelplot(BIAS_plot, 
#                           margin=FALSE, main= "BIAS" ,
#                           ## about colorbar
#                           colorkey=list(
#                             space='right',                   
#                             labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
#                                          font=3),
#                             axis.line=list(col='black'),
#                             width=0.75,
#                             title=expression(paste("     ", mu,"g ",m^-3))
#                           ),   
#                           ## about the axis
#                           par.settings=list(
#                             strip.border=list(col='transparent'),
#                             strip.background=list(col='transparent'),
#                             axis.line=list(col='black')
#                           ),
#                           scales=list(draw=T, alternating= F),            
#                           #col.regions = colorRampPalette(c("blue", "white","red"))(1e3),
#                           col.regions = cols,
#                           at=unique(c(seq(low_IQR, high_IQR, length.out=200))),
#                           # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
#                           names.attr=rep(names(BIAS_plot))) +
#   latticeExtra::layer(sp.polygons(shp_UAE))
# #h
# 
# 
# 
# png(paste0(output_folder,"Coefffi_BIAS.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print(h)
# dev.off()
# 
# 
# ### plots of histograms
# 
# 
# #vec_all<- as.vector(AOD_plot)
# png(paste0(output_folder,"hist_BIAS.png"),
#     width = 1680, height = 1050, units = "px", pointsize = 30,
#     bg = "white", res = 150)
# print({
#   histogram(vec_all,  breaks=500 , main = paste("Histogram of BIAS"), type="percent",
#             xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
# })
# dev.off()
# 
# 
# rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])
# 
# 
# 
# 
# #DEW
# 
#####



###########################################################
################                         ##################
################ Validation (70% - 30%)  ##################
################                         ##################
###########################################################



rm(list = ls(all = TRUE))

#### LOADING THE RESULT WORKSPACE

output_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/images/"
load_folder<- "D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/data_output/Model GWR/"


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

nn=17
nam <- paste("gwrG_pnt_", sprintf("%02d",nn) , sep = "")


model_value <- loadOneName( eval(nam), file=paste0(load_folder, "GWR_file.RData"))

load(paste0( load_folder, "model_input_file.RData"))

data_frame_SDF<- data.frame(model_value$SDF)

col_names<- colnames(data_frame_SDF)

result_stack<- stack()

for ( i in 1:21){
  
  dawi<- rasterFromXYZ( data_frame_SDF[, c("coord.x","coord.y", col_names[i])])
  crs(dawi)<- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 "
  names(dawi)<- col_names[i]
  result_stack<-stack(result_stack,dawi)
}



#####################################





All_extracted_data_tr<-NULL
All_extracted_data_val<- NULL

#### importing the validation and training stations
load("D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/Annual_training.RData")
load("D:/Air Quality/GWR_with_met/Result/Annual/ALL_variables/station for validation/Annual_validation.RData")
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

data_extraced_raw <- cbind(site, extra_dataframe(x=x, y=y, mydata=mydata)) 

AOD_coeffi_training <- extract_points(raster= result_stack$AOD_mean, input_stations = training_station )
wind_speed_coeffi_training <- extract_points(raster=result_stack$wind_speed, input_stations = training_station )
DEW_coeffi_training <- extract_points(raster=result_stack$DEW, input_stations = training_station )
#temp_coeffi_training <- extract_points(raster=result_stack$temp, input_stations = training_station )
RH_coeffi_training <- extract_points(raster=result_stack$RH, input_stations = training_station )
Cons_coeffi_training <- extract_points(raster=result_stack$X.Intercept., input_stations = training_station )




# "Moni ~  AOD_mean + wind_speed  + DEW+ temp+ RH "

Moni_estimated <- data_extraced_raw$AOD_mean*AOD_coeffi_training + data_extraced_raw$wind_speed*wind_speed_coeffi_training+
  data_extraced_raw$RH*RH_coeffi_training + data_extraced_raw$DEW*DEW_coeffi_training+
  Cons_coeffi_training #+ data_extraced_raw$temp*temp_coeffi_training

moni_real_estimate_tr<-cbind(data_extraced_raw$Moni, Moni_estimated)
names(moni_real_estimate_tr)<- c("Monitoring", "Estimate")

     
##### validation

###### adding the all stations lat and long information to the training stations

{
  load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")
  
  ## filtering the data for PM2.5 and 2015
  AQ_data_2015 <- AQ_data_12 %>%
    filter(years == 2015, Pollutant == "PM2.5" ) 
  AQ_data_2015 <- na.omit(AQ_data_2015)
  
  ## filtering for specific month
  AQ_data_PM25 <- AQ_data_2015 
  
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
    group_by(Site, years) %>%
    summarize(mon_mean= mean(Value, na.rm = T))
  
  # # monthly mean of january
  # AQ_data_PM25 <- AQ_data_PM25 %>%
  #   group_by(Site) %>%
  #   summarize(sea_mean=mean(mon_mean, na.rm = T))
  
  # goegraphical location of the stations
  
  coordin_site<-filter(AQ_data_2015,  Pollutant == "PM2.5" )
  coordin_site<-coordin_site %>%
    dplyr::distinct(Site, .keep_all = T)%>%
    select(-years)
  
  AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))
  
  AQ_data_PM25<- as.data.frame(AQ_data_PM25)     # to ungroup the variables
  
  AQ_data_PM25 <- AQ_data_PM25 %>%
    select(Site,
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
   
   
   AOD_coeffi_val <- extract_points(raster=result_stack$AOD_mean, input_stations = validation_station )
   wind_speed_coeffi_val <- extract_points(raster=result_stack$wind_speed, input_stations = validation_station )
   DEW_coeffi_val <- extract_points(raster=result_stack$DEW, input_stations = validation_station )
   #temp_coeffi_val <- extract_points(raster=result_stack$temp, input_stations = validation_station )
   RH_coeffi_val <- extract_points(raster=result_stack$RH, input_stations = validation_station )
   Cons_coeffi_val <- extract_points(raster=result_stack$X.Intercept., input_stations = validation_station )
   
   # moni_raster<- extract_points(data_extraced_raw_val$Moni, input_stations = validation_station)
   # 
   # Moni_coeffi_val <- extract_points(raster= mydata$Moni, input_stations = validation_station )
   
   # "Moni ~  AOD_mean + wind_speed  + DEW+ temp+ RH + constant"
   
   Moni_estimated_val <- data_extraced_raw_val$AOD_mean*AOD_coeffi_val + data_extraced_raw_val$wind_speed*wind_speed_coeffi_val+
     data_extraced_raw_val$RH*RH_coeffi_val + data_extraced_raw_val$DEW*DEW_coeffi_val+
      Cons_coeffi_val#+ +data_extraced_raw_val$temp*temp_coeffi_val
   
   moni_real_estimate_val<-cbind(AQ_data_PM25$mon_mean, Moni_estimated_val)
   names(moni_real_estimate_val)<- c("Monitoring", "Estimate")
   
   
   
  
   
   ##########
   # concateneting the extracted data






###########################################################
###########################################################
###########################################################



#### r 2 for the training 
   rsq <- function(x, y) {
     summary(lm(y~x))$r.squared
   }
   
   
   
r_2_tr<-  rsq(moni_real_estimate_tr$Monitoring, moni_real_estimate_tr$Estimate)

# r_2_tr<- sum((moni_real_estimate_val$Estimate-mean(moni_real_estimate_val$Monitoring ))^2)/
#   (sum((moni_real_estimate_val$Monitoring-mean(moni_real_estimate_val$Monitoring))^2)+sum((moni_real_estimate_val$Estimate-mean(moni_real_estimate_val$Monitoring ))^2))

#### r 2 for the validation

r_2_val<- rsq(moni_real_estimate_val$Monitoring, moni_real_estimate_val$Estimate)

# r_2_val<- sum((All_extracted_data_val$Estimate-mean(All_extracted_data_val$Monitoring ))^2)/
#   sum((All_extracted_data_val$Monitoring-mean(All_extracted_data_val$Monitoring))^2)


#### Scatter plot of the training 
png(paste0(output_folder,"R2_training.png"), width = 1680, height = 1050, units = "px", pointsize = 20,
    bg = "white", res = 150)
par(mar=c(4,4.5,2,1))
print({

plot(moni_real_estimate_tr$Monitoring,moni_real_estimate_tr$Estimate, xlim=c(0,150), ylim=c(0,150), 
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
plot(moni_real_estimate_val$Monitoring, moni_real_estimate_val$Estimate, xlim=c(0,150), ylim=c(0,150), 
     col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
     ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), lwd=2, main= "Validation")
text(10,100, substitute(R^2==a, list(a = round(r_2_val, digits=2), cex = 1.2)))
abline(0,1,lwd=2, col="black")
})
dev.off()






###########################################################
###########################################################
###########################################################



# #### RMSE for the training 
# 
# dawit<-function(xx){
#   zz<- sqrt(sum(xx)/(length(xx)))
# }
# 
# RMSE_tr<- moni_real_estimate_tr%>%
#   mutate(diff=(Monitoring-Estimate)^2)%>%
#   #group_by(Month)%>%
#   summarise(RMSE= dawit(diff))
# 
# 
# #### RMSE for the validation
# 
# RMSE_val<- moni_real_estimate_val%>%
#   mutate(diff=(Monitoring-Estimate)^2)%>%
#   #group_by(Month)%>%
#   summarise(RMSE= dawit(diff))
# mean(RMSE_val$RMSE)
# mean(RMSE_tr$RMSE)
# 
# # x <- sort(runif(10, min=0, max=10))
# # y <- runif(10, min=2, max=5)
# # 
# # #Polygon-Plot
# # plot(x,y, type="n", ylim=c(0,5))
# # polygon(c(x[1], x, x[length(x)]), c(0, y, 0), col="green")
# 
# 
# 
# library(ggplot2)
# sp <- ggplot() + 
#   #geom_point(data=All_extracted_data_val, aes(x=Monitoring, y=Estimate ), shape=3, col="blue")+
#   geom_point(data=moni_real_estimate_tr, aes(x=Monitoring, y=Estimate ),shape=3, col="red")+
#   xlim(0,100)+ ylim(0,100)+
#   geom_abline(mapping = NULL, data = NULL,  slope=1, intercept=0,
#               na.rm = FALSE, show.legend = NA,size = 1)
#   
# sp#+facet_wrap(~ Month, ncol=3)
# 
# 
# 
# 
# sp <- ggplot(moni_real_estimate_val, aes(x=Monitoring, y=Estimate )) + geom_point(shape=3, col="blue")+
#   xlim(0,100)+ ylim(0,100)+
#   geom_abline(mapping = NULL, data = NULL,  slope=1, intercept=0,
#               na.rm = FALSE, show.legend = NA,size = 1)#+
#   #facet_wrap(~ Month, ncol=3)
# sp





# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png Dry_Temperatue_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Irradiance_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Wind_Speed_NCMS_1km_DUST_event_02_April_2015.gif
  









