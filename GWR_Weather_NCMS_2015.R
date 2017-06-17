
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


coefi_conver<- 85    # this number is used to change the AOD values to PM2.5 mg/m3 obtaine by the regression of 
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
for (i in 1:12){
  # i=1
  load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2013_2015.RData")
  AQ_data_PM25 <- AQ_data_12 %>%
    filter(Pollutant == "PM2.5" & years == 2015) %>%
    #filter(Date == as.Date("2016-08-26"))      # DG 
    filter (months==i)
  
  # monthly mean of january
  AQ_data_PM25 <- AQ_data_PM25 %>%
    group_by(Site, years) %>%
    summarize(mon_mean= mean(Value, na.rm = T))
  
  # # monthly mean of january
  # AQ_data_PM25 <- AQ_data_PM25 %>%
  #   group_by(Site) %>%
  #   summarize(sea_mean=mean(mon_mean, na.rm = T))
  
  coordin_site<-filter(AQ_data_12,  Pollutant == "PM2.5" )
  coordin_site<-coordin_site %>%
    dplyr::distinct(Site, .keep_all = T)
  
  
  AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))
  
  
  AQ_data_PM25 <- AQ_data_PM25 %>%
    select(Site,
           Longitude,
           Latitude,
           mon_mean)
  
  # remove all lines with NA
  AQ_data_PM25 <- na.omit(AQ_data_PM25)
  
  AQ_data_PM25<- as.data.frame(AQ_data_PM25)  # to ungroup the variables
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


writeRaster(Total_monitoring_PM25, "D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.tif",
            options="INTERLEAVE=BAND", overwrite=TRUE)
save(Total_monitoring_PM25, file= "D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")

rm(Total_monitoring_PM25)


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###########       Geographic Weighted Regression Model        #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


# Importing and restructuring the rasters

rm(list=ls(all.names = TRUE))

old <- Sys.time()



setwd("D:/Air Quality/GWR_with_met/")

result_monthly<- list()

# OD_mean_jan<-raster("MODIS/AOD_mean_01.tif")
# plot(OD_mean_jan)
for (qq in 1:12){
  #qq=10
  load("D:/Air Quality/GWR_with_met/Variables_raster/NCMS_10km.RData")
  load("D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")
  load("D:/Air Quality/GWR_with_met/AOD_MODIS_2015/AOD_mean_monthly_2015.RData")
  # LU_fract_desert<- raster("Land_cover/desert_fraction.tif")#/100 ## constant for all months
  # LU_fract_urban<- raster("Land_cover/urban_fraction.tif")#/100   ## constant for all months
  
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
  
  plot(r_moni)
  plot(AOD_mean_jan)
  
  AOD_mean_jan <- resample(AOD_mean_jan,r_moni,"bilinear")
  wind_speed <- resample(wind_speed,r_moni,"bilinear")
  temp <- resample(temp,r_moni,"bilinear")
  DEW <- resample(DEW,r_moni,"bilinear")
  RH <- resample(RH,r_moni,"bilinear")
  Radiation <- resample(Radiation,r_moni,"bilinear")
  plot(r_moni)
  plot(AOD_mean_jan)
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ####  as SpatialPointsDataFrame layers Method IIII  ####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
  
  #plot(BIAS)
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
  mydata$RH<-mydata$RH/100
  
  library(spgwr)

  bwG_pnt <- gwr.sel(Moni ~  AOD_mean + wind_speed  + DEW+ temp+ RH ,#+ RH ,#+ temp
                 data= mydata,  coords=cbind( mydata$x , mydata$y),
                 gweight = gwr.Gauss, #RMSE=T,
                 method="cv", verbose = F, show.error.messages = T)
  

  gwrG_pnt <- gwr(Moni ~  AOD_mean + wind_speed + DEW+ temp+RH  ,#+RH , #+ temp
              data= mydata,  bandwidth = bwG_pnt*2,  coords=cbind( mydata$x , mydata$y),
              gweight = gwr.Gauss, hatmatrix = TRUE,predictions = T)

  
  #dadada<- as.data.frame(gwrG_pnt$SDF)
  #plot(dadada$localR2)
  
  #plot(mydata$Moni)
  # print(mean(gwrG_pnt$SDF$localR2))
  
  result_monthly<-c(result_monthly, gwrG_pnt)
  
  print(qq)
  print(plot(gwrG_pnt$SDF$localR2, main=qq))
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "old")])
  
}

save(result_monthly, file="D:/Air Quality/GWR_with_met/Result/result_regression.RData")



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

load("D:/Air Quality/GWR_with_met/Result/result_regression.RData")

output_folder<- "D:/Air Quality/GWR_with_met/Result/Images/"

#### ploting the histogram of the r2 for all the months ####

data_frame_r2<- data.frame()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- as.data.frame( dawi$localR2)
  month_ind<- cbind(dawi, rep (kk+1,nrow(dawi)))
  colnames(month_ind)<-c("r2", "month_ind")
  data_frame_r2<-rbind(data_frame_r2,month_ind)
}


data_frame_r2<- cbind(data_frame_r2, month.name[data_frame_r2$month_ind])

scaleFUN <- function(x) sprintf("%.1f", x)

hp <- ggplot(data_frame_r2, aes(x=r2)) + 
  # qplot( data_frame_r2$r2, y=NULL, data_frame_r2, binwidth= 0.01, xlim=c(0,1), facet_wrap( ~ data_frame_r2$month_ind, ncol=3),
  #        geom="histogram", fill=I("blue" ),alpha=I(.4),ylab ="")+
  geom_histogram (binwidth=0.01,colour="red",fill=I("red" ),alpha=I(.8))+
  scale_x_continuous(limits = c(0, 1), labels= scaleFUN )+
  scale_y_continuous(name="")

hp<- hp+ facet_wrap( ~ data_frame_r2$month_ind + data_frame_r2$`month.name[data_frame_r2$month_ind]`)+
  theme(strip.text.x = element_text(size = 11,  angle = 0))

#hp 




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
# # # # # #
# [1] "sum.w"              "(Intercept)"        "AOD_mean"           "wind_speed"         "DEW"               
# [6] "temp"               "RH"                 "(Intercept)_se"     "AOD_mean_se"        "wind_speed_se"     
# [11] "DEW_se"             "temp_se"            "RH_se"              "gwr.e"              "pred"              
# [16] "pred.se"            "localR2"            "(Intercept)_se_EDF" "AOD_mean_se_EDF"    "wind_speed_se_EDF" 
# [21] "DEW_se_EDF"         "temp_se_EDF"        "RH_se_EDF"          "pred.se_EDF"       
# 
# coordi_x_y<- result_monthly$SDF@coords
# names(coordi_x_y)<-c("x", "y")



rm(list = ls()[!ls() %in% c( "result_monthly","output_folder")])

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


AOD_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
 # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "AOD_mean")])
  names(dawi)<- month.name[(kk+1)]
  AOD_coeffi<-stack(AOD_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(AOD_coeffi)))
min_val<-floor(min(minValue(AOD_coeffi)))

stat_dat<- summary(as.vector(AOD_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(AOD_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

AOD_plot <-AOD_coeffi
AOD_plot[AOD_plot < low_IQR ]<- low_IQR
AOD_plot[ AOD_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(AOD_plot, 
                          margin=FALSE, main= "AOD COEFFICIENTS" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ",mu,"g ",m^-3))
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



png(paste0(output_folder,"Coefffi_AOD.png"),
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





rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])



#### Wind Speed COEFFICIENTS ####


WSpeed_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "wind_speed")])
  names(dawi)<- month.name[(kk+1)]
  WSpeed_coeffi<-stack(WSpeed_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(WSpeed_coeffi)))
min_val<-floor(min(minValue(WSpeed_coeffi)))

stat_dat<- summary(as.vector(WSpeed_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])*2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(WSpeed_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

WSpeed_plot <-WSpeed_coeffi
WSpeed_plot[WSpeed_plot < low_IQR ]<- low_IQR
WSpeed_plot[ WSpeed_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(WSpeed_plot, 
                          margin=FALSE, main= "Wind Speed COEFFICIENTS" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3,"/(m ", sec^-1,")"))
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
                          names.attr=rep(names(WSpeed_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_WSpeed.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms


#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_WSpeed.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Wind Speed Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()





rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])



#### DEW COEFFICIENTS ####


DEW_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "DEW")])
  names(dawi)<- month.name[(kk+1)]
  DEW_coeffi<-stack(DEW_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(DEW_coeffi)))
min_val<-floor(min(minValue(DEW_coeffi)))

stat_dat<- summary(as.vector(DEW_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(DEW_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

DEW_plot <-DEW_coeffi
DEW_plot[DEW_plot < low_IQR ]<- low_IQR
DEW_plot[ DEW_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(DEW_plot, 
                          margin=FALSE, main= "DEW Temp. COEFFICIENTS" ,
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
                          names.attr=rep(names(DEW_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_DEW.png"),
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
  histogram(vec_all,  breaks=500 , main = paste("Histogram of DEW Temp. Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])


#### TEMP. COEFFICIENTS ####


Temp_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "temp")])
  names(dawi)<- month.name[(kk+1)]
  Temp_coeffi<-stack(Temp_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(Temp_coeffi)))
min_val<-floor(min(minValue(Temp_coeffi)))

stat_dat<- summary(as.vector(Temp_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(Temp_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

Temp_plot <-Temp_coeffi
Temp_plot[Temp_plot < low_IQR ]<- low_IQR
Temp_plot[ Temp_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(Temp_plot, 
                          margin=FALSE, main= "Temp. COEFFICIENTS" ,
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
                          names.attr=rep(names(Temp_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_temp.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms


#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_temp.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Temp. Coefficients"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])


#### RH COEFFICIENTS ####


RH_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "RH")])
  names(dawi)<- month.name[(kk+1)]
  RH_coeffi<-stack(RH_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(RH_coeffi)))
min_val<-floor(min(minValue(RH_coeffi)))

stat_dat<- summary(as.vector(RH_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(RH_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

RH_plot <-RH_coeffi
RH_plot[RH_plot < low_IQR ]<- low_IQR
RH_plot[ RH_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(RH_plot, 
                          margin=FALSE, main= "Relative Humidity COEFFICIENTS" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3))
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
                          names.attr=rep(names(RH_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_RH.png"),
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


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])



#### Prediction COEFFICIENTS ####


Pre_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "pred")])
  names(dawi)<- month.name[(kk+1)]
  Pre_coeffi<-stack(Pre_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(Pre_coeffi)))
min_val<-floor(min(minValue(Pre_coeffi)))

stat_dat<- summary(as.vector(Pre_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(Pre_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

pre_plot <-Pre_coeffi
pre_plot[pre_plot < low_IQR ]<- low_IQR
pre_plot[ pre_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(pre_plot, 
                          margin=FALSE, main= "Prediction PM2.5 " ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3))
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
                          names.attr=rep(names(pre_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_Pred.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(h)
dev.off()


### plots of histograms


#vec_all<- as.vector(AOD_plot)
png(paste0(output_folder,"hist_pred.png"),
    width = 1680, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print({
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Predition of PM2.5"), type="percent",
            xlab= list("PM2.5",cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE", "output_folder")])



#### Intercept ####


intercept_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "X.Intercept.")])
  names(dawi)<- month.name[(kk+1)]
  intercept_coeffi<-stack(intercept_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(intercept_coeffi)))
min_val<-floor(min(minValue(intercept_coeffi)))

stat_dat<- summary(as.vector(intercept_coeffi))
IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 2))# n is the space after IQR

low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))


vec_all<- as.vector(intercept_coeffi)
vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

intercept_plot <-intercept_coeffi
intercept_plot[intercept_plot < low_IQR ]<- low_IQR
intercept_plot[ intercept_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(intercept_plot, 
                          margin=FALSE, main= "Intercept (Error)" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3))
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
                          names.attr=rep(names(intercept_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_Intercept.png"),
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
  histogram(vec_all,  breaks=500 , main = paste("Histogram of Intercept (Error)"), type="percent",
            xlab= list(expression(beta),cex = 1.5) , ylab=list("%",cex = 1.5 ), col="black", scales=list(cex = 1.5))
})
dev.off()


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])



######

###### BIAS ######

load("D:/Air Quality/GWR_with_met/Monitoring/Moni_PM25_mean_monthly_2015.RData")


Pre_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  # dawi<- cbind(dawi,coordi_x_y)
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("coord.x","coord.y", "pred")])
  names(dawi)<- month.name[(kk+1)]
  Pre_coeffi<-stack(Pre_coeffi,dawi)
}

BIAS_coeffi<- stack()

for (kk in 1:12){
  
  
  crs(Pre_coeffi[[month.name[kk]]])<- " +proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  r_moni<- projectRaster(from=Total_monitoring_PM25[[month.name[kk]]],to=Pre_coeffi[[month.name[kk]]])
  dawi<- r_moni-Pre_coeffi[[month.name[kk]]]
  names(dawi)<- month.name[kk]
  BIAS_coeffi<-stack(BIAS_coeffi,dawi)
}

max_val<-ceiling(max(maxValue(BIAS_coeffi)))
min_val<-floor(min(minValue(BIAS_coeffi)))

stat_dat<- summary(as.vector(BIAS_coeffi))
# IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2]))*20)# n is the space after IQR
 IQR<- floor(as.numeric((stat_dat[6]-stat_dat[1])))# n is the space after IQR

# low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
# high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
 low_IQR<-floor(as.numeric((stat_dat[1])))
 high_IQR<- ceiling(as.numeric((stat_dat[6])))

vec_all<- as.vector(BIAS_coeffi)
#vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all, n=10)
xxx<- (c(min_val, xxx, max_val))

BIAS_plot <-BIAS_coeffi
# BIAS_plot[BIAS_plot < low_IQR ]<- low_IQR
# BIAS_plot[ BIAS_plot >  high_IQR]<- high_IQR

## 


### plots of maps 



h <- rasterVis::levelplot(BIAS_plot, 
                          margin=FALSE, main= "BIAS" ,
                          ## about colorbar
                          colorkey=list(
                            space='right',                   
                            labels= list(at= floor(as.numeric( seq(low_IQR, high_IQR, length.out=7))),
                                         font=3),
                            axis.line=list(col='black'),
                            width=0.75,
                            title=expression(paste("     ", mu,"g ",m^-3))
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
                          names.attr=rep(names(BIAS_plot))) +
  latticeExtra::layer(sp.polygons(shp_UAE))
#h



png(paste0(output_folder,"Coefffi_BIAS.png"),
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


rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE","output_folder")])




#DEW

#####



setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data")
path <- ".csv$"
filenames_hourly_NCMS <- list.files(pattern = path)

setwd("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters")
setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters")

# temperature
# NCMS_DRY_TEMP_STACK_image <- stack("Dry_Temperature_NCMS_1km_new.tif")


# radiation
NCMS_DRY_TEMP_STACK_image <- stack("Irradiation_W_m2_NCMS_1km.tif")/3.6
NCMS_DRY_TEMP_STACK_image[NCMS_DRY_TEMP_STACK_image < 0] <- 0

# wind speed
# NCMS_DRY_TEMP_STACK_image <- stack("Wind_Speed_NCMS_1km.tif")


# plot(NCMS_DRY_TEMP_STACK_image[[34]])

# i <- 83
# j <- 83

# min_val <- 9.256
# max_val <- 44

vec_all <- as.vector(NCMS_DRY_TEMP_STACK_image)

max_val<- ceiling(max(vec_all, na.rm = T))
min_val<- floor(min(vec_all,  na.rm = T))


stat_dat <- summary(vec_all)
IQR <- floor(as.numeric((stat_dat[5]-stat_dat[2])* 1.5))# n is the space after IQR

low_IQR <-floor(as.numeric((stat_dat[2]- IQR)))
high_IQR <-floor(as.numeric((stat_dat[5]+IQR)))


vec_all_1 <- vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]

xxx<- pretty( vec_all_1, n=15)

{
if (max_val <= max(xxx)){
xxx<- unique(c( xxx))
}else{
xxx<- unique(c( xxx, max_val))
}

if (min_val >= min(xxx)){
  xxx<- unique(c( xxx))
}else{
  xxx<- unique(c(min_val, xxx))
}
}



cool = rainbow(50, start=rgb2hsv(col2rgb('cyan'))[1], end=rgb2hsv(col2rgb('blue'))[1])
warm = rainbow(50, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('yellow'))[1])
cols = c(rev(cool), rev(warm))
mypalette <- colorRampPalette(cols)(255)

pal = colorBin(mypalette, bin = xxx, domain = min_val:max_val, na.color = "transparent")



for (i in 1:length(filenames_hourly_NCMS)) {
  # load the stacked raster with all the images
  # NCMS_STACK_image <- raster("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Dry_Temperature_NCMS_1km.tif", band = i)
  # NCMS_STACK_image <- raster("D:/Dust_Event_UAE_2015/AWS_2015 WEATHER/dust_event_outputs/hourly_data/rasters/Irradiation_W_m2_NCMS_1km.tif", band = i)/3.6
    NCMS_STACK_image <-  NCMS_DRY_TEMP_STACK_image[[i]]
    plot(NCMS_STACK_image)
  

  name_time <- str_sub(filenames_hourly_NCMS[i], start = 1, end = -5)
  
  tag_time <- paste0(str_sub(name_time, start = 1, end = -7), " ",
              str_sub(name_time, start = 12, end = -4), ":",
               str_sub(name_time, start = 15, end = -1))
  
  
 
  
  # define popup for time scene
  "h1 { font-size: 3px;}"
  content <- paste('<h1><strong>', tag_time,'', sep = "")
  
  map <- leaflet() %>% 
    addTiles() %>% 
    addTiles(group = "OSM (default)") %>%
    addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
    addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
    addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
    
    addPopups(54, 25, content,
              options = popupOptions(closeButton = FALSE)) %>%
    
    addRasterImage(NCMS_STACK_image, 
                   colors = pal, 
                   opacity = 0.5, group = "Dry_Temp_NCMS") %>%
    addLayersControl(
      baseGroups = c("Toner Lite" ,"Road map" ,"Satellite"),
      overlayGroups = "Dry_Temp_NCMS",
      options = layersControlOptions(collapsed = TRUE)) %>%
    addLegend("bottomright", pal = pal, values = c(min_val, max_val),
            #  title = "<br><strong>Dry Temp.(<sup>°</sup>C): </strong>",
              title = "<br><strong>W/m<sup>2</sup> : </strong>",
              labFormat = labelFormat(prefix = ""),
              opacity = 0.5)
  map
  
  ## This is the png creation part
  saveWidget(map, 'temp.html', selfcontained = FALSE)
  webshot('temp.html', file = paste0(name_time,".png"), vwidth = 1100, vheight = 900,
          cliprect = 'viewport')
  
  } 

# to make a movie.......
# to use with ImageMagik using the commnad line cmd in windows
# cd into the directory where there are the png files
# magick -delay 50 -loop 0 *.png Dry_Temperatue_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Irradiance_NCMS_1km_DUST_event_02_April_2015.gif
# magick -delay 50 -loop 0 *.png Wind_Speed_NCMS_1km_DUST_event_02_April_2015.gif
  









