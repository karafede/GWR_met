
library(rgdal)
library(ggplot2)
library(gstat)
library(sp)
library(raster)   
library(plyr)
library(dplyr)
library(leaflet)
library(htmltools)
library(readr)
library(threadr)
library(htmlwidgets)
# library(ncdf4)
# library(RNetCDF)
# library(fields)
# library(rgeos)

### added
# library(maps)
# library(geoR)
# library(spatial)
# library(gstat)



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
##########   Getting the monthly monitoring values           ########
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/dawit Data/Hourly Database format CSV/Arranged dates/R files/filtered_4_box")
#setwd("D:/Daily filtered with 4 boxplot")
# setwd("disk3/fkaragulian/GWR/Daily filtered with 4 boxplot")




EAD_data_2013 <- read_csv("database_EAD_ 2013 _hourly_filtered.csv")[-1]
EAD_data_2014 <- read_csv("database_EAD_ 2014 _hourly_filtered.csv")[-1]
EAD_data_2015 <- read_csv("database_EAD_ 2015 _hourly_filtered.csv")[-1]
EAD_data_2016 <- read_csv("database_EAD_ 2016 _hourly_filtered.csv")[-1]

DM_data_2013 <- read_csv("database_DM_ 2013 _hourly_filtered.csv")[-1]
DM_data_2014 <- read_csv("database_DM_ 2014 _hourly_filtered.csv")[-1]
DM_data_2015 <- read_csv("database_DM_ 2015 _hourly_filtered.csv")[-1]
DM_data_2016 <- read_csv("database_DM_ 2016 _hourly_filtered.csv")[-1]

NCMS_data_2013 <- read_csv("database_NCMS_ 2013 _hourly_filtered.csv")[-1]
NCMS_data_2014 <- read_csv("database_NCMS_ 2014 _hourly_filtered.csv")[-1]
NCMS_data_2015 <- read_csv("database_NCMS_ 2015 _hourly_filtered.csv")[-1]
NCMS_data_2016 <- read_csv("database_NCMS_ 2016 _hourly_filtered.csv")[-1]

AQ_data <- rbind(EAD_data_2013, EAD_data_2014, EAD_data_2015, EAD_data_2016, 
                 DM_data_2013, DM_data_2014, DM_data_2015, DM_data_2016,
                 NCMS_data_2013, NCMS_data_2014, NCMS_data_2015, NCMS_data_2016)

str(AQ_data)

AQ_data<- AQ_data%>%
  mutate(DateTime = DateTime+300)

AQ_data <- AQ_data %>%
  mutate(Date= ymd_hms(DateTime))%>%
  mutate(hou= hour(Date))


AQ_data_12 <- AQ_data %>%
  filter(hou == 16)

AQ_data_12[,2][AQ_data_12[,2]=="DUBAIAIRPORT"]<- "DUBAI AIR PORT"



AQ_data_12  <- AQ_data_12 %>%
  #mutate(Date = ymd(Date, tz = "UTC"))%>%
  mutate(months=month(Date))%>%                # DG adding month
  mutate(years=year(Date))                     # DG adding year

save( AQ_data_12, file="D:/Air Quality/GWR/Saves from GWR script/result_regression/station.RData")

rm(list = ls(all = TRUE))

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$


for (i in 1:12){
  # i=1
  load("D:/Air Quality/GWR/Saves from GWR script/result_regression/station.RData")
  AQ_data_PM25 <- AQ_data_12 %>%
    filter(Pollutant == "PM2.5") %>%
    #filter(Date == as.Date("2016-08-26"))      # DG 
    filter (months==i)
  
  # monthly mean of january
  AQ_data_PM25 <- AQ_data_PM25 %>%
    group_by(Site, years) %>%
    summarize(mon_mean= mean(Value, na.rm = T))
  
  # monthly mean of january
  AQ_data_PM25 <- AQ_data_PM25 %>%
    group_by(Site) %>%
    summarize(sea_mean=mean(mon_mean, na.rm = T))
  
  coordin_site<-filter(AQ_data_12,  Pollutant == "PM2.5" )
  coordin_site<-coordin_site %>%
    dplyr::distinct(Site, .keep_all = T)
  
  
  AQ_data_PM25<- left_join(AQ_data_PM25, coordin_site, by= c("Site"= "Site" ))
  
  
  AQ_data_PM25 <- AQ_data_PM25 %>%
    select(Site,
           Longitude,
           Latitude,
           sea_mean)
  
  # remove all lines with NA
  AQ_data_PM25 <- na.omit(AQ_data_PM25)
  
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
  source("D:/Air Quality/GWR/kriging_func.R")
  ###    kriging_points <- function(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
  
  resl_ras= 0.1
  
  r_moni <-kriging_points(dawit=AQ_data_PM25, resl_ras, shp_UAE = "D:/Air Quality/GWR/UAE_boundary"  )
  str(r_moni)
  #"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  writeRaster(r_moni, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/in_situ/in_situ_kriging_UAE_","%02d", ".tif"),i), overwrite = TRUE)
  rm(list = ls(all = TRUE))
  
}


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# #########   Averaging the monthly AOD values from MODIS     #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
coefi_conver<- 60    # this number is used to change the AOD values to PM2.5 mg/m3 obtaine by the regression of 
                     # the station values with the satellite values from MODIS (2015-2016)
                     # could also be changed from the ECWMF model values for AOD and PM2.5

for (kk in 1:12){
  {
    ### for the year of 2016
    #i=1
    coefi_conver<- 60
    
    year_req<-sprintf("%04d-",2016)
    months_req<-sprintf("%02d",kk)
    
    path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2016_MODIS_processed/2016_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")
    
    datafiles <- Sys.glob(path_name) #Or whatever identifies your files
    resultingStack <- stack()
    for(i in 1:NROW(datafiles)){
      tempraster <- raster(datafiles[i])
      resultingStack <- stack(resultingStack,tempraster)
    }
    
    x <- reclassify(resultingStack, cbind(0, NA))
    r_AOD_2016 <- mean(x, na.rm=TRUE)*coefi_conver
    r_AOD_2016<- projectRaster(r_AOD_2016, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    str(r_AOD_2016)
    plot(r_AOD_2016)
    res(r_AOD_2016)
    
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
    str(r_AOD_2015)
    plot(r_AOD_2015)
    res(r_AOD_2015)
    
    
    ### for the year of 2014
    
    year_req<-sprintf("%04d-",2014)
    months_req<-sprintf("%02d",kk)
    
    path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2014_MODIS_processed/2014_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")
    
    datafiles <- Sys.glob(path_name) #Or whatever identifies your files
    resultingStack <- stack()
    for(i in 1:NROW(datafiles)){
      tempraster <- raster(datafiles[i])
      resultingStack <- stack(resultingStack,tempraster)
    }
    
    x <- reclassify(resultingStack, cbind(0, NA))
    r_AOD_2014 <- mean(x, na.rm=TRUE)*coefi_conver
    r_AOD_2014<- projectRaster(r_AOD_2014, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    str(r_AOD_2014)
    plot(r_AOD_2014)
    res(r_AOD_2014)
    
    
    ### for the year of 2013
    
    year_req<-sprintf("%04d-",2013)
    months_req<-sprintf("%02d",kk)
    
    path_name<-paste("Z:/_SHARED_FOLDERS/Air Quality/Phase 1/Pathflow of Phase I_DG/MODIS_LAADS_NASA/2013_MODIS_processed/2013_AOD_tiff_1km/" , year_req ,months_req ,"-**.tif", sep = "")
    
    datafiles <- Sys.glob(path_name) #Or whatever identifies your files
    resultingStack <- stack()
    for(i in 1:NROW(datafiles)){
      tempraster <- raster(datafiles[i])
      resultingStack <- stack(resultingStack,tempraster)
    }
    
    x <- reclassify(resultingStack, cbind(0, NA))
    r_AOD_2013 <- mean(x, na.rm=TRUE)*coefi_conver
    r_AOD_2013<- projectRaster(r_AOD_2013, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    str(r_AOD_2013)
    plot(r_AOD_2013)
    res(r_AOD_2013)
    
    
    ### averaging all the years
    
    AOD_mean_jan <- overlay(r_AOD_2016, r_AOD_2015, r_AOD_2014, r_AOD_2013, fun= mean)
    plot(AOD_mean_jan)
    
    writeRaster(AOD_mean_jan, sprintf(paste0('D:/Air Quality/GWR/Saves from GWR script/MODIS/AOD_mean_%02d.tif'),kk),overwrite=TRUE)
    
    rm(list = ls(all = TRUE))
  
  }
}




#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# #########   Land Use layer to be used for the Regression    #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# load raster land use UAE
# # # # # Lancover == 16  ---> desert area # ## ## ##

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")
plot(LU)

LU[LU < 16] <- 0
LU[LU > 16] <- 0
LU <- LU/16
plot(LU)
resampled_LU<-aggregate(LU, fact=100, fun= sum ) # changing fact number to change the aggregation of the pixels
LU_fract_desert <- resampled_LU/100

str(LU_fract_desert)
res(LU_fract_desert)
plot(LU_fract_desert)

## ## Lancover == 13  ---> urban area ## ## ## ###

LU <- raster("D:/Air Quality/GWR/AD_DUBAI_Modis.tif")

plot(LU)

LU[LU < 13] <- 0
LU[LU > 13] <- 0
LU <- LU/13
plot(LU)
resampled_LU<-aggregate(LU,fact=100, fun= sum ) # changing fact number 
LU_fract_urban <- resampled_LU/100

plot(LU_fract_urban)

res(LU_fract_urban)


####### exporting the landuse layers

writeRaster(LU_fract_urban, "D:/Air Quality/GWR/Saves from GWR script/Land_cover/urban_fraction.tif",overwrite=TRUE)
writeRaster(LU_fract_desert, "D:/Air Quality/GWR/Saves from GWR script/Land_cover/desert_fraction.tif",overwrite=TRUE)

rm(list = ls(all = TRUE))



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

################# Layers from the ECMWF model   #####################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$

for (i in 1:12){
  # i=1
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  ###### Layers from the ECMWF model at 40 km resolution (SO4)  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")
    ########### 2013
    
    ECMWF_SO4_2013 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/ECMWF_suaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/Layer_names_suaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SO4_2013)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2013,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SO4_2013_jan<- mean( subset(ECMWF_SO4_2013,subset_tr), na.rm = T )
    
    ##### 2014
    
    ECMWF_SO4_2014 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/ECMWF_suaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/Layer_names_suaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SO4_2014)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2014,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SO4_2014_jan<- mean( subset(ECMWF_SO4_2014,subset_tr), na.rm = T )
    
    ##### 2015
    
    ECMWF_SO4_2015 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/ECMWF_suaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/Layer_names_suaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SO4_2015)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2015,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SO4_2015_jan<- mean( subset(ECMWF_SO4_2015,subset_tr), na.rm = T )
    
    ##### 2016
    
    ECMWF_SO4_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_suaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_suaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SO4_2016)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SO4_2016_jan<- mean( subset(ECMWF_SO4_2016,subset_tr), na.rm = T )
    
    
    ECMWF_SO4_jan<- overlay(ECMWF_SO4_2013_jan,ECMWF_SO4_2014_jan,ECMWF_SO4_2015_jan,ECMWF_SO4_2016_jan,fun=mean)
    
    
    ##### kriging to 10km 
    ECMWF_SO4_jan_10km <-kriging_points(dawit=ECMWF_SO4_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_SO4_jan_10km)<-"SO4_ECMWF"
    plot(ECMWF_SO4_jan_10km)
    
    writeRaster(ECMWF_SO4_jan_10km, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/ECMWF/ECMWF_SO4_%02d_10km.tif"),i) ,overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
  
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (DUST_ECMWF)  #####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")
    ########### 2013
    
    ECMWF_DUST_2013 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/ECMWF_duaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/Layer_names_duaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_DUST_2013)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2013,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_DUST_2013_jan<- mean( subset(ECMWF_DUST_2013,subset_tr), na.rm = T )
    
    ##### 2014
    
    ECMWF_DUST_2014 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/ECMWF_duaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/Layer_names_duaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_DUST_2014)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2014,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_DUST_2014_jan<- mean( subset(ECMWF_DUST_2014,subset_tr), na.rm = T )
    
    ##### 2015
    
    ECMWF_DUST_2015 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/ECMWF_duaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/Layer_names_duaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_DUST_2015)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2015,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_DUST_2015_jan<- mean( subset(ECMWF_DUST_2015,subset_tr), na.rm = T )
    
    ##### 2016
    
    ECMWF_DUST_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_duaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_duaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_DUST_2016)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_DUST_2016_jan<- mean( subset(ECMWF_DUST_2016,subset_tr), na.rm = T )
    
    
    ECMWF_DUST_jan<- overlay(ECMWF_DUST_2013_jan,ECMWF_DUST_2014_jan,ECMWF_DUST_2015_jan,ECMWF_DUST_2016_jan,fun=mean)
    
    
    ##### kriging to 10km 
    ECMWF_DUST_jan_10km <-kriging_points(dawit=ECMWF_DUST_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_DUST_jan_10km)<-"DUST_ECMWF"
    plot(ECMWF_DUST_jan_10km)
    
    writeRaster(ECMWF_DUST_jan_10km, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/ECMWF/ECMWF_DUST_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (SALT_ECMWF)  #####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")
    ########### 2013
    
    ECMWF_SALT_2013 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/ECMWF_ssaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/Layer_names_ssaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SALT_2013)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2013,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SALT_2013_jan<- mean( subset(ECMWF_SALT_2013,subset_tr), na.rm = T )
    
    ##### 2014
    
    ECMWF_SALT_2014 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/ECMWF_ssaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/Layer_names_ssaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SALT_2014)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2014,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SALT_2014_jan<- mean( subset(ECMWF_SALT_2014,subset_tr), na.rm = T )
    
    ##### 2015
    
    ECMWF_SALT_2015 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/ECMWF_ssaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/Layer_names_ssaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SALT_2015)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2015,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SALT_2015_jan<- mean( subset(ECMWF_SALT_2015,subset_tr), na.rm = T )
    
    ##### 2016
    
    ECMWF_SALT_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_ssaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_ssaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SALT_2016)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_SALT_2016_jan<- mean( subset(ECMWF_SALT_2016,subset_tr), na.rm = T )
    
    
    ECMWF_SALT_jan<- overlay(ECMWF_SALT_2013_jan,ECMWF_SALT_2014_jan,ECMWF_SALT_2015_jan,ECMWF_SALT_2016_jan,fun=mean)
    
    
    ##### kriging to 10km 
    ECMWF_SALT_jan_10km <-kriging_points(dawit=ECMWF_SALT_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_SALT_jan_10km)<-"SALT_ECMWF"
    plot(ECMWF_SALT_jan_10km)
    
    writeRaster(ECMWF_SALT_jan_10km, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/ECMWF/ECMWF_SALT_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (BC_ECMWF)  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")
    ########### 2013
    
    ECMWF_BC_2013 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/ECMWF_bcaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/Layer_names_bcaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_BC_2013)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2013,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_BC_2013_jan<- mean( subset(ECMWF_BC_2013,subset_tr), na.rm = T )
    
    ##### 2014
    
    ECMWF_BC_2014 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/ECMWF_bcaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/Layer_names_bcaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_BC_2014)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2014,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_BC_2014_jan<- mean( subset(ECMWF_BC_2014,subset_tr), na.rm = T )
    
    ##### 2015
    
    ECMWF_BC_2015 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/ECMWF_bcaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/Layer_names_bcaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_BC_2015)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2015,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_BC_2015_jan<- mean( subset(ECMWF_BC_2015,subset_tr), na.rm = T )
    
    ##### 2016
    
    ECMWF_BC_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_bcaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_bcaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_BC_2016)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_BC_2016_jan<- mean( subset(ECMWF_BC_2016,subset_tr), na.rm = T )
    
    
    ECMWF_BC_jan<- overlay(ECMWF_BC_2013_jan,ECMWF_BC_2014_jan,ECMWF_BC_2015_jan,ECMWF_BC_2016_jan,fun=mean)
    
    
    ##### kriging to 10km 
    ECMWF_BC_jan_10km <-kriging_points(dawit=ECMWF_BC_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_BC_jan_10km)<-"BC_ECMWF"
    plot(ECMWF_BC_jan_10km)
    
    writeRaster(ECMWF_BC_jan_10km, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/ECMWF/ECMWF_BC_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (OM_ECMWF)  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")
    ########### 2013
    
    ECMWF_OM_2013 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/ECMWF_omaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2013/Layer_names_omaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_OM_2013)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2013,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_OM_2013_jan<- mean( subset(ECMWF_OM_2013,subset_tr), na.rm = T )
    
    ##### 2014
    
    ECMWF_OM_2014 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/ECMWF_omaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2014/Layer_names_omaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_OM_2014)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2014,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_OM_2014_jan<- mean( subset(ECMWF_OM_2014,subset_tr), na.rm = T )
    
    ##### 2015
    
    ECMWF_OM_2015 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/ECMWF_omaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2015/Layer_names_omaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_OM_2015)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2015,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_OM_2015_jan<- mean( subset(ECMWF_OM_2015,subset_tr), na.rm = T )
    
    ##### 2016
    
    ECMWF_OM_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_omaod550.tif")
    
    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_omaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_OM_2016)<- name_time
    
    
    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)
    
    subset_tr<- subset(name_time, grepl(name_sub,name_time))
    
    ECMWF_OM_2016_jan<- mean( subset(ECMWF_OM_2016,subset_tr), na.rm = T )
    
    
    ECMWF_OM_jan<- overlay(ECMWF_OM_2013_jan,ECMWF_OM_2014_jan,ECMWF_OM_2015_jan,ECMWF_OM_2016_jan,fun=mean)
    
    
    ##### kriging to 10km 
    ECMWF_OM_jan_10km <-kriging_points(dawit=ECMWF_OM_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_OM_jan_10km)<-"OM_ECMWF"
    plot(ECMWF_OM_jan_10km)
    
    writeRaster(ECMWF_OM_jan_10km, sprintf(paste0("D:/Air Quality/GWR/Saves from GWR script/ECMWF/ECMWF_OM_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
  }
  
}








#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###########       Geographic Weighted Regression Model        #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



# Importing and restructuring the rasters

old <- Sys.time()
setwd("D:/Air Quality/GWR/Saves from GWR script/")

result_monthly<- list()
{
  
  for (qq in 1:12){
    #qq=3
    
    LU_fract_desert<- raster("Land_cover/desert_fraction.tif")#/100 ## constant for all months
    LU_fract_urban<- raster("Land_cover/urban_fraction.tif")#/100   ## constant for all months
    
    #plot(ED)
    name_mon<- sprintf("%02d",qq)
    AOD_mean_jan<-raster(paste0("MODIS/AOD_mean_",name_mon,".tif"))
    r_moni<- raster(paste0("in_situ/in_situ_kriging_UAE_",name_mon,".tif"))
    #plot(LU_fract_urban)
    # ECMWF variable to form layers
    
    ECMWF_OM_ECMWF_jan_10km<- raster(paste0("ECMWF/ECMWF_OM_",name_mon,"_10km.tif"))*60
    ECMWF_BC_ECMWF_jan_10km<- raster(paste0("ECMWF/ECMWF_BC_",name_mon,"_10km.tif"))*60
    ECMWF_SALT_ECMWF_jan_10km<- raster(paste0("ECMWF/ECMWF_SALT_",name_mon,"_10km.tif"))*60
    ECMWF_DUST_ECMWF_jan_10km<- raster(paste0("ECMWF/ECMWF_DUST_",name_mon,"_10km.tif"))*60
    ECMWF_SO4_jan_10km<- raster(paste0("ECMWF/ECMWF_SO4_",name_mon,"_10km.tif"))*60
    
    
    #### 
    
    plot(ECMWF_BC_ECMWF_jan_10km)
    res(r_moni)
    
    
    
    
    LU__urban10km <- resample(LU_fract_urban,r_moni,"bilinear")
    LU__desert10km <- resample(LU_fract_desert,r_moni,"bilinear")
    r_AOD_sampled <- resample(AOD_mean_jan,r_moni,"bilinear")
    
    # ECMWF variable to form layers
    
    ECMWF_OM_ECMWF_jan_10km <- resample(ECMWF_OM_ECMWF_jan_10km,r_moni,"bilinear")
    ECMWF_BC_ECMWF_jan_10km <- resample(ECMWF_BC_ECMWF_jan_10km,r_moni,"bilinear")
    ECMWF_SALT_ECMWF_jan_10km <- resample(ECMWF_SALT_ECMWF_jan_10km,r_moni,"bilinear")
    ECMWF_DUST_ECMWF_jan_10km <- resample(ECMWF_DUST_ECMWF_jan_10km,r_moni,"bilinear")
    ECMWF_SO4_jan_10km <- resample(ECMWF_SO4_jan_10km,r_moni,"bilinear")
    
    #### masking the layers 
    
    LU__urban10km <- mask(LU__urban10km, r_moni)
    LU__desert10km <- mask(LU__desert10km, r_moni)
    r_AOD_sampled <- mask(r_AOD_sampled, r_moni)
    
    ECMWF_OM_ECMWF_jan_10km <- mask(ECMWF_OM_ECMWF_jan_10km, r_moni)
    ECMWF_BC_ECMWF_jan_10km <- mask(ECMWF_BC_ECMWF_jan_10km, r_moni)
    ECMWF_SALT_ECMWF_jan_10km <- mask(ECMWF_SALT_ECMWF_jan_10km, r_moni)
    ECMWF_DUST_ECMWF_jan_10km <- mask(ECMWF_DUST_ECMWF_jan_10km, r_moni)
    ECMWF_SO4_jan_10km <- mask(ECMWF_SO4_jan_10km, r_moni)
    
    
    #AAA_new<- rasterToPoints(BIAS)
    
    plot(r_AOD_sampled)
    
    # diff between in situ data and PM25_AOD
    BIAS <- r_moni-r_AOD_sampled
    
    {
      ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      ##### changing the layers to points    Method I  ######
      ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      # plot(BIAS)
      # BIAS_pts <- rasterToPoints(BIAS)
      # LU_desert_pts <- rasterToPoints(LU__desert10km)
      # LU_urban_pts <- rasterToPoints(LU__urban10km)
      # AOD_pts <- rasterToPoints(r_AOD_sampled)
      # 
      # 
      # mydata <- cbind(BIAS_pts[,1],
      #                 BIAS_pts[,2],
      #                 BIAS_pts[,3],
      #                 LU_desert_pts[,3],
      #                 LU_urban_pts[,3])
      # mydata<-as.data.frame(mydata)
      # 
      # colnames(mydata) <- c("Lon", "Lat", "BIAS", "desert", "urban")
      # library(spgwr)
      # 
      # bwG <- gwr.sel(BIAS ~ desert ,#+ urban ,
      #                data= mydata, coords = cbind( mydata$Lon , mydata$Lat), gweight = gwr.Gauss,
      #                verbose = FALSE)
      # 
      # gwrG <- gwr(BIAS ~ desert + urban ,
      #             data= mydata, coords = cbind( mydata$Lon , mydata$Lat), bandwidth = bwG,
      #             gweight = gwr.Gauss, hatmatrix = TRUE)
      
      
      ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      ##### as SpatialPolygonsDataFrame layers Method II ####
      ##$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      
      
      # library(spgwr)
      # BIAS <- r_moni-r_AOD_sampled
      # 
      # BIAS <- as(BIAS, 'SpatialPolygonsDataFrame')
      # LU__desert10km <- as(LU__desert10km, 'SpatialPolygonsDataFrame')
      # LU__urban10km <- as(LU__urban10km, 'SpatialPolygonsDataFrame')
      # r_AOD_sampled <- as(r_AOD_sampled, 'SpatialPolygonsDataFrame')
      # 
      # 
      # combined_data<-cbind(BIAS,LU__desert10km,LU__urban10km,r_AOD_sampled)
      # 
      # combined_data$desert_fraction
      # combined_data$layer
      # combined_data$urban_fraction
      # 
      # bwG <- gwr.sel(layer ~ urban_fraction + desert_fraction ,
      #                data= combined_data,  gweight = gwr.Gauss,
      #                verbose = FALSE)
      # gwrG <- gwr(layer ~ urban_fraction + desert_fraction  ,
      #             data= combined_data,  bandwidth = bwG,
      #             gweight = gwr.Gauss, hatmatrix = TRUE)
      
    }
    
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    ####  as SpatialPointsDataFrame layers Method IIII  ####
    #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
    library(spgwr)
    #BIAS <- r_moni-r_AOD_sampled
    
    BIAS_sp <- as(BIAS, 'SpatialPointsDataFrame')
    names(BIAS_sp)<- "BIAS"
    LU__desert10km_sp <- as(LU__desert10km, 'SpatialPointsDataFrame')
    names(LU__desert10km_sp)<- "desert_fraction"
    LU__urban10km_sp <- as(LU__urban10km, 'SpatialPointsDataFrame')
    names(LU__urban10km_sp)<- "urban_fraction"
    r_AOD_sampled_sp <- as(r_AOD_sampled, 'SpatialPointsDataFrame')
    names(r_AOD_sampled_sp)<- "BIAS"
    ECMWF_OM_ECMWF_jan_10km_sp <- as(ECMWF_OM_ECMWF_jan_10km, 'SpatialPointsDataFrame')
    names(ECMWF_OM_ECMWF_jan_10km_sp)<- "ECMWF_OM_jan_10km"
    ECMWF_BC_ECMWF_jan_10km_sp <- as(ECMWF_BC_ECMWF_jan_10km, 'SpatialPointsDataFrame')
    names(ECMWF_BC_ECMWF_jan_10km_sp)<- "ECMWF_BC_jan_10km"
    ECMWF_SALT_ECMWF_jan_10km_sp <- as(ECMWF_SALT_ECMWF_jan_10km, 'SpatialPointsDataFrame')
    names(ECMWF_SALT_ECMWF_jan_10km_sp)<- "ECMWF_SALT_jan_10km"
    ECMWF_DUST_ECMWF_jan_10km_sp <- as(ECMWF_DUST_ECMWF_jan_10km, 'SpatialPointsDataFrame')
    names(ECMWF_DUST_ECMWF_jan_10km_sp)<- "ECMWF_DUST_jan_10km"
    ECMWF_SO4_jan_10km_sp <- as(ECMWF_SO4_jan_10km, 'SpatialPointsDataFrame')
    names(ECMWF_SO4_jan_10km_sp)<- "ECMWF_SO4_jan_10km"
    
    # Elevation Difference importing as a tiff
    
    ED<- raster("ED_10km.tif")   ## constant for all months
    ED_sp<- as(ED, 'SpatialPointsDataFrame')
    names(ED_sp)<- "ED"
    
    
    combined_data_pnt<-cbind(BIAS_sp,LU__desert10km_sp,LU__urban10km_sp, ECMWF_OM_ECMWF_jan_10km_sp,ECMWF_BC_ECMWF_jan_10km_sp,ECMWF_SALT_ECMWF_jan_10km_sp, ECMWF_DUST_ECMWF_jan_10km_sp, ECMWF_SO4_jan_10km_sp )
    
    combined_data_pnt_ED<- cbind(combined_data_pnt, ED_sp)
    #plot(ED)
    
    
    bwG_pnt <- gwr.sel(BIAS ~   urban_fraction + desert_fraction + ED + ECMWF_DUST_jan_10km + ECMWF_SO4_jan_10km, # + ECMWF_SALT_jan_10km  , #+ ECMWF_SALT_jan_10km,# + ECMWF_SALT_jan_10km + ECMWF_DUST_jan_10km, #+ desert_fraction ++ ECMWF_DUST_jan_10km +ECMWF_SO4_jan_10km++    ECMWF_DUST_jan_10km +ECMWF_DUST_jan_10km + ECMWF_SO4_jan_10km
                       data= combined_data_pnt_ED,  gweight = gwr.Gauss,
                       method="cv", verbose = F, show.error.messages = T)
    
    if (bwG_pnt < 11){
    gwrG_pnt <- gwr(BIAS ~ urban_fraction + desert_fraction + ED + ECMWF_DUST_jan_10km + ECMWF_SO4_jan_10km ,#+ desert_fraction + ECMWF_SALT_jan_10km,#   , #  ECMWF_DUST_jan_10km ++ ECMWF_DUST_jan_10km + ECMWF_BC_jan_10km
                    data= combined_data_pnt_ED,  bandwidth = 11,
                    gweight = gwr.Gauss, hatmatrix = TRUE)
    }else{
      gwrG_pnt <- gwr(BIAS ~ urban_fraction + desert_fraction + ED + ECMWF_DUST_jan_10km + ECMWF_SO4_jan_10km ,#+ desert_fraction + ECMWF_SALT_jan_10km,#   , #  ECMWF_DUST_jan_10km ++ ECMWF_DUST_jan_10km + ECMWF_BC_jan_10km
                      data= combined_data_pnt_ED,  bandwidth = bwG_pnt,
                      gweight = gwr.Gauss, hatmatrix = TRUE)
    }
    
    
    
    
    result_monthly<-c(result_monthly, gwrG_pnt)
    
    qq
    
    rm(list = ls()[!ls() %in% c( "result_monthly", "old")])
    
  }
  
  save(result_monthly, file="D:/Air Quality/GWR/Saves from GWR script/result_regression/result_regression_11.RData")

}

new <- Sys.time() - old # calculate difference
print(new)


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

load("D:/Air Quality/GWR/Saves from GWR script/result_regression/result_regression_11.RData")



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

hp 




png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11_v1/histogram_r2.png",
    width = 1050, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
(hp)
dev.off()


# 
# hp <- ggplot(data_frame_r2, aes(x=r2)) + 
#   # qplot( data_frame_r2$r2, y=NULL, data_frame_r2, binwidth= 0.01, xlim=c(0,1), facet_wrap( ~ data_frame_r2$month_ind, ncol=3),
#   #        geom="histogram", fill=I("blue" ),alpha=I(.4),ylab ="")+
#   geom_histogram (binwidth=0.01,colour="white",fill=I("blue" ),alpha=I(.4))+
#   scale_x_continuous(limits = c(0, 1))+
#   scale_y_continuous(name="")
# 
# hp<- hp+ facet_wrap( ~ data_frame_r2$month_ind + data_frame_r2$`month.name[data_frame_r2$month_ind]`)
# 
# hp
# 


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###### ploting the coefficients #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


{
  
  rm(list = ls()[!ls() %in% c( "result_monthly")])
  
  library(viridis)
  library(lattice)
  
  dir <- "D:/Air Quality/GWR/UAE_boundary"
  shp_UAE <- readOGR(dsn = dir, layer = "uae_emirates")
  shp_UAE <- spTransform(shp_UAE, CRS("+init=epsg:4326"))
  plot(shp_UAE)
  
  
  #### DUST COEFFICIENTS
  
  
  ####### color pallet
  
  cool = rainbow(20, start=rgb2hsv(col2rgb('#00FFF2FF'))[1], end=rgb2hsv(col2rgb('blue'))[1])
  warm = rainbow(20, start=rgb2hsv(col2rgb('red'))[1], end=rgb2hsv(col2rgb('#FF8600FF'))[1])
  middle = rainbow(215, start=rgb2hsv(col2rgb('#FF8600FF'))[1], end=rgb2hsv(col2rgb('green'))[1])
  cols = c(rev(cool),rev(middle),  rev(warm))
  
  # # # # # #
  
  
  
  Dust_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "ECMWF_DUST_jan_10km")])
    names(dawi)<- month.name[(kk+1)]
    Dust_coeffi<-stack(Dust_coeffi,dawi)
  }
  
  max_val<-ceiling(max(maxValue(Dust_coeffi)))
  min_val<-floor(min(minValue(Dust_coeffi)))
  
  stat_dat<- summary(as.vector(Dust_coeffi))
  IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(Dust_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  dust_plot <-Dust_coeffi
  dust_plot[dust_plot < low_IQR ]<- low_IQR
  dust_plot[ dust_plot >  high_IQR]<- high_IQR
  
  ## 
  
  
  ### plots of maps
  {
    
    
    
    h <- rasterVis::levelplot(Dust_coeffi, 
                              margin=FALSE, main= "DUST COEFFICIENTS" ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3),
                                axis.line=list(col='black'),
                                width=0.75
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(Dust_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
  }
  
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Coefffi_Dust.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  ### plots of histograms
  
  {
    vec_all<- as.vector(Dust_coeffi)
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_DUST.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
   
    histogram(vec_all,  breaks=500 , main = paste("Histogram of DUST Coefficients"), 
              xlab= "Beta", ylab="count", col="black")
    dev.off()
    
  }
  
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  #### SO4 COEFFICIENTS
  
  
  So4_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "ECMWF_SO4_jan_10km")])
    names(dawi)<- month.name[(kk+1)]
    So4_coeffi<-stack(So4_coeffi,dawi)
  }
  
  
  
  max_val<-ceiling(max(maxValue(So4_coeffi)))
  min_val<-floor(min(minValue(So4_coeffi)))
  
  stat_dat<- summary(as.vector(So4_coeffi))
  IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(So4_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(So4_coeffi, 
                              margin=FALSE, main= "SO4 COEFFICIENTS" ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3),
                                axis.line=list(col='black'),
                                width=0.75
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(So4_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
    
  }
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Coefffi_SO4.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_SO4.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of SO4 Coefficients"), 
              xlab= "Beta", ylab="count", col="black")
    dev.off()
    
  }
  
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  # [1] "sum.w"                      "(Intercept)"                "urban_fraction"             "desert_fraction"           
  # [5] "ED"                         "ECMWF_DUST_jan_10km"        "ECMWF_SO4_jan_10km"         "(Intercept)_se"            
  # [9] "urban_fraction_se"          "desert_fraction_se"         "ED_se"                      "ECMWF_DUST_jan_10km_se"    
  # [13] "ECMWF_SO4_jan_10km_se"      "gwr.e"                      "pred"                       "pred.se"                   
  # [17] "localR2"                    "(Intercept)_se_EDF"         "urban_fraction_se_EDF"      "desert_fraction_se_EDF"    
  # [21] "ED_se_EDF"                  "ECMWF_DUST_jan_10km_se_EDF" "ECMWF_SO4_jan_10km_se_EDF"  "pred.se_EDF"    
  
  #### URBAN COEFFICIENTS
  
  urban_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "urban_fraction")])
    names(dawi)<- month.name[(kk+1)]
    urban_coeffi<-stack(urban_coeffi,dawi)
  }
  
  
  max_val<-ceiling(max(maxValue(urban_coeffi)))
  min_val<-floor(min(minValue(urban_coeffi)))
  
  stat_dat<- summary(as.vector(urban_coeffi))
  IQR<- floor(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(urban_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(urban_coeffi, 
                              margin=FALSE, main= "URBAN COEFFICIENT" ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3, cex= 0.3),
                                axis.line=list(col='black'),
                                width=1
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(urban_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
    
  }
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Coefffi_URBAN.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_URBAN.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of URBAN Coefficients"), 
              xlab= "Beta", ylab="count", col="black")
    dev.off()
    
  }
  
  
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  
  
  
  #### Desert COEFFICIENTS
  
  desert_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "desert_fraction")])
    names(dawi)<- month.name[(kk+1)]
    desert_coeffi<-stack(desert_coeffi,dawi)
  }
  
  
  max_val<-ceiling(max(maxValue(desert_coeffi)))
  min_val<-floor(min(minValue(desert_coeffi)))
  
  stat_dat<- summary(as.vector(desert_coeffi))
  IQR<- ceiling(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(desert_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(desert_coeffi, 
                              margin=FALSE, main= "DESERT COEFFICIENT" ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3, cex= 1),
                                axis.line=list(col='black'),
                                width=1
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(desert_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
    
  }
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Coefffi_DESERT.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_DESERT.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of DESERT Coefficients"), 
              xlab= "Beta", ylab="count", col="black")
    dev.off()
    
  }
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  
  
  
  
  #### ED COEFFICIENTS
  
  ED_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "ED")])
    names(dawi)<- month.name[(kk+1)]
    ED_coeffi<-stack(ED_coeffi,dawi)
  }
  
  
  max_val<-ceiling(max(maxValue(ED_coeffi)))
  min_val<-floor(min(minValue(ED_coeffi)))
  
  stat_dat<- summary(as.vector(ED_coeffi))
  IQR<- ceiling(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(ED_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  #hist(vec_all, breaks=100)
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(ED_coeffi, 
                              margin=FALSE, main= "ED COEFFICIENT" ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3, cex= 1),
                                axis.line=list(col='black'),
                                width=1
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(ED_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
    
  }
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Coefffi_ED.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_ED.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of ED Coefficient"), 
              xlab= "Beta", ylab="count", col="black")
    dev.off()
    
  }
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  
  
  
  #### PRED COEFFICIENTS
  
  PRED_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "pred")])
    names(dawi)<- month.name[(kk+1)]
    PRED_coeffi<-stack(PRED_coeffi,dawi)
  }
  
  
  max_val<-ceiling(max(maxValue(PRED_coeffi)))
  min_val<-floor(min(minValue(PRED_coeffi)))
  
  stat_dat<- summary(as.vector(PRED_coeffi))
  IQR<- ceiling(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(PRED_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  #hist(vec_all, breaks=100)
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(PRED_coeffi, 
                              margin=FALSE, main= "Predicted BIAS " ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3, cex= 1),
                                axis.line=list(col='black'),
                                width=1
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(PRED_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
  }
  
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Pred_bias.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_pre_BIAS.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of Predicted BIAS"), 
              xlab= "Beta", ylab="count", col="black")
    
    histogram(vec_all,  breaks=500 , main = paste("Histogram of Predicted BIAS"), type="density",
              xlab= "Beta", ylab="count", col="black")
    
    dev.off()
    
  }
  
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
  
  
  
  
  #### Constant COEFFICIENTS
  
  Cons_coeffi<- stack()
  
  for (kk in 0:11){
    
    dawi<- result_monthly[[kk*12+1]]
    dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "X.Intercept.")])
    names(dawi)<- month.name[(kk+1)]
    Cons_coeffi<-stack(Cons_coeffi,dawi)
  }
  
  
  max_val<-ceiling(max(maxValue(Cons_coeffi)))
  min_val<-floor(min(minValue(Cons_coeffi)))
  
  stat_dat<- summary(as.vector(Cons_coeffi))
  IQR<- ceiling(as.numeric((stat_dat[5]-stat_dat[2])* 4))# n is the space after IQR
  
  low_IQR<-floor(as.numeric((stat_dat[2]- IQR)))
  high_IQR<-ceiling(as.numeric((stat_dat[5]+IQR)))
  
  
  vec_all<- as.vector(Cons_coeffi)
  vec_all<-vec_all[ vec_all >= low_IQR & vec_all <= high_IQR & !is.na(vec_all) ]
  
  xxx<- pretty( vec_all, n=10)
  xxx<- (c(min_val, xxx, max_val))
  
  
  #hist(vec_all, breaks=100)
  
  # plot(So4_coeffi$January)
  # plot(Dust_coeffi$January)
  
  
  ### plots
  {
    
    h <- rasterVis::levelplot(Cons_coeffi, 
                              margin=FALSE, main= "Constant Coeffi. (ERORR) " ,
                              ## about colorbar
                              colorkey=list(
                                space='right',                   
                                labels=list(at= floor(as.numeric(c(floor(stat_dat[1]), seq(low_IQR, high_IQR, length.out=7), ceiling(stat_dat[6])))),
                                            font=3, cex= 1),
                                axis.line=list(col='black'),
                                width=1
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
                              at=unique(c(seq(min_val, low_IQR , length.out=20),
                                          seq(low_IQR, high_IQR, length.out=215),
                                          seq(high_IQR, max_val, length.out=20))),
                              # at=c(seq(stat_dist[1], ceiling(stat_dist[6]), length.out=256)),
                              names.attr=rep(names(Cons_coeffi))) +
      latticeExtra::layer(sp.polygons(shp_UAE))
    h
  }
  
  
  png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/Constant_coeff.png",
      width = 1680, height = 1050, units = "px", pointsize = 30,
      bg = "white", res = 150)
  (h)
  dev.off()
  
  
  ### plots of histograms
  
  {
    png("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/Out_put_images/11/hist_cons_error.png",
        width = 1680, height = 1050, units = "px", pointsize = 30,
        bg = "white", res = 150)
    histogram(vec_all,  breaks=500 , main = paste("Histogram of Intercept (Error)"), 
              xlab= "Beta", ylab="count", col="black")
    
    dev.off()
    
  }
  
  
  rm(list = ls()[!ls() %in% c( "result_monthly", "cols", "shp_UAE")])
  
}



#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
###### ploting the ERRORS #############
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$






#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
################### Map  all layers data with Leaflet #################################
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




# define color palette

min_in_situ = minValue(in_situ_IDW_UAE)
min_in_situ
max_in_situ = maxValue(in_situ_IDW_UAE)
max_in_situ

min_PM25_AOD = minValue(PM25_AOD_tiff)
min_PM25_AOD
max_PM25_AOD = maxValue(PM25_AOD_tiff)
max_PM25_AOD

min_PM25_ADJ_AOD = minValue(PM25_AOD_ADJ_tiff)
min_PM25_ADJ_AOD
max_PM25_ADJ_AOD = maxValue(PM25_AOD_ADJ_tiff)
max_PM25_ADJ_AOD

MIN_PAL <- 3.5
MAX_PAL <- 70.5

# pal_PM25_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                              getValues(in_situ_IDW_UAE),na.color = "transparent")
# 
# pal_PM25_AOD_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                                  getValues(PM25_AOD_tiff),na.color = "transparent")
# 
# pal_PM25_AOD_ADJ_1km <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
#                                  getValues(PM25_AOD_ADJ_tiff),na.color = "transparent")

pal <- colorNumeric(c("#9999FF", "#FFFF00", "#FF0000", "#ff8000"),
                    c(MIN_PAL, MAX_PAL),na.color = "transparent")

pal_LU_1km <- colorNumeric(c("#0000ff", "#ffff00", "#ff0000"),
                           getValues(LU_1km),na.color = "transparent")

map <- leaflet() %>%
  # setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Road map") %>%
  addProviderTiles("Thunderforest.Landscape", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
  
  addRasterImage(in_situ_IDW_UAE, colors = pal, opacity = 0.5,
                 group = "in situ PM25") %>%
  addRasterImage(PM25_AOD_tiff, colors = pal, opacity = 0.5,
                 group = "PM25 AOD 1km") %>%
  addRasterImage(PM25_AOD_ADJ_tiff, colors = pal, opacity = 0.5,
                 group = "PM25 AOD ADJ 1km") %>%
  addRasterImage(LU_1km, colors = pal_LU_1km, opacity = 0.5,
                 group = "LU 1km") %>%
  addLegend("bottomright", pal = pal, values = c(MIN_PAL, MAX_PAL), 
            title = "<br><strong>PM25</strong>",
            labFormat = labelFormat(prefix = ""),
            opacity = 0.6) %>%
  
  map

# save map
saveWidget(map, paste0("ADJ_AOD_PM25_LU.html"), selfcontained = FALSE)