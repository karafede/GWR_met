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


EAD_data_2016 <- read_csv("database_EAD_ 2016 _hourly_filtered.csv")[-1]


DM_data_2016 <- read_csv("database_DM_ 2016 _hourly_filtered.csv")[-1]


NCMS_data_2016 <- read_csv("database_NCMS_ 2016 _hourly_filtered.csv")[-1]

AQ_data <- rbind( EAD_data_2016, 
                 DM_data_2016,
                 NCMS_data_2016)

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

save( AQ_data_12, file="D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2016_IDW.RData")

rm(list = ls(all = TRUE))


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$


for (i in 1:12){
  # i=4
  load("D:/Air Quality/GWR/new_analysis_2013_2015/result_Rdata/station_2016_IDW.RData")
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
  source("D:/Air Quality/GWR/IDW_function.R")
  ###    kriging_points <- function(dawit, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
  
  resl_ras= 0.1
  
  r_moni <- IDW_points(dawit=AQ_data_PM25, resl_ras, shp_UAE = "D:/Air Quality/GWR/UAE_boundary"  )
  str(r_moni)
  hist(r_moni, breaks=10)
  #"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  writeRaster(r_moni, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/in_situ_IDW/in_situ_kriging_UAE_","%02d", ".tif"),i), overwrite = TRUE)
  
  rm(list = ls(all = TRUE))
  
}


# rater_3_years<- raster("D:/Air Quality/GWR/Saves from GWR script/in_situ/in_situ_kriging_UAE_01.tif")
# plot(rater_3_years)
# hist(rater_3_years, breaks= 100)

# #########   Averaging the monthly AOD values from MODIS     #######
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#$$$$$ monthly loop $$$$$$$$$
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$
coefi_conver<- 85    # this number is used to change the AOD values to PM2.5 mg/m3 obtaine by the regression of 
# the station values with the satellite values from MODIS (2015-2016)
# could also be changed from the ECWMF model values for AOD and PM2.5

for (kk in 1:12){
  {
    ### for the year of 2016
    #i=1
    coefi_conver<- 85
    
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
    
   ### averaging all the years
    
    AOD_mean_jan <- r_AOD_2016 #overlay( r_AOD_2015, r_AOD_2014, r_AOD_2013, fun= mean) #r_AOD_2016,
    plot(AOD_mean_jan)
    
    writeRaster(AOD_mean_jan, sprintf(paste0('D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/MODIS/AOD_mean_%02d.tif'),kk),overwrite=TRUE)
    
    rm(list = ls(all = TRUE))
    
  }
}


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

    
    ##### 2016
    
    ECMWF_SO4_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_suaod550.tif")

    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_suaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SO4_2016)<- name_time


    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)

    subset_tr<- subset(name_time, grepl(name_sub,name_time))

    ECMWF_SO4_2016_jan<- mean( subset(ECMWF_SO4_2016,subset_tr), na.rm = T )



    ECMWF_SO4_jan<-  ECMWF_SO4_2016_jan #overlay(ECMWF_SO4_2013_jan,ECMWF_SO4_2014_jan,ECMWF_SO4_2015_jan,fun=mean)# ECMWF_SO4_2016_jan
    
    
    ##### kriging to 10km 
    ECMWF_SO4_jan_10km <-kriging_points(dawit=ECMWF_SO4_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_SO4_jan_10km)<-"SO4_ECMWF"
    plot(ECMWF_SO4_jan_10km)
    
    writeRaster(ECMWF_SO4_jan_10km, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/ECMWF/ECMWF_SO4_%02d_10km.tif"),i) ,overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (DUST_ECMWF)  #####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")

    
    ##### 2016
    
    ECMWF_DUST_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_duaod550.tif")

    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_duaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_DUST_2016)<- name_time


    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)

    subset_tr<- subset(name_time, grepl(name_sub,name_time))

    ECMWF_DUST_2016_jan<- mean( subset(ECMWF_DUST_2016,subset_tr), na.rm = T )

    
    ECMWF_DUST_jan<- ECMWF_DUST_2016_jan #overlay(ECMWF_DUST_2013_jan,ECMWF_DUST_2014_jan,ECMWF_DUST_2015_jan,fun=mean) #,ECMWF_DUST_2016_jan
    
    
    ##### kriging to 10km 
    ECMWF_DUST_jan_10km <-kriging_points(dawit=ECMWF_DUST_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_DUST_jan_10km)<-"DUST_ECMWF"
    plot(ECMWF_DUST_jan_10km)
    
    writeRaster(ECMWF_DUST_jan_10km, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/ECMWF/ECMWF_DUST_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (SALT_ECMWF)  #####
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")

    
    ##### 2016
    
    ECMWF_SALT_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_ssaod550.tif")

    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_ssaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_SALT_2016)<- name_time


    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)

    subset_tr<- subset(name_time, grepl(name_sub,name_time))

    ECMWF_SALT_2016_jan<- mean( subset(ECMWF_SALT_2016,subset_tr), na.rm = T )

    
    ECMWF_SALT_jan<- ECMWF_SALT_2016_jan # overlay(ECMWF_SALT_2013_jan,ECMWF_SALT_2014_jan,ECMWF_SALT_2015_jan,fun=mean) #,ECMWF_SALT_2016_jan
    
    
    ##### kriging to 10km 
    ECMWF_SALT_jan_10km <-kriging_points(dawit=ECMWF_SALT_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_SALT_jan_10km)<-"SALT_ECMWF"
    plot(ECMWF_SALT_jan_10km)
    
    writeRaster(ECMWF_SALT_jan_10km, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/ECMWF/ECMWF_SALT_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (BC_ECMWF)  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")

    
    ##### 2016
    
    ECMWF_BC_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_bcaod550.tif")

    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_bcaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_BC_2016)<- name_time


    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)

    subset_tr<- subset(name_time, grepl(name_sub,name_time))

    ECMWF_BC_2016_jan<- mean( subset(ECMWF_BC_2016,subset_tr), na.rm = T )

    
    ECMWF_BC_jan<- ECMWF_BC_2016_jan # overlay(ECMWF_BC_2013_jan,ECMWF_BC_2014_jan,ECMWF_BC_2015_jan,fun=mean)#,ECMWF_BC_2016_jan
    
    
    ##### kriging to 10km 
    ECMWF_BC_jan_10km <-kriging_points(dawit=ECMWF_BC_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_BC_jan_10km)<-"BC_ECMWF"
    plot(ECMWF_BC_jan_10km)
    
    writeRaster(ECMWF_BC_jan_10km, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/ECMWF/ECMWF_BC_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
    
  }
  
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  #### Layers from the ECMWF model at 40 km resolution (OM_ECMWF)  #######
  #$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  
  {
    source("D:/Air Quality/GWR/Saves from GWR script/ECMWF_kriging_fun.R")

    
    
    ##### 2016
    
    ECMWF_OM_2016 <-  stack("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/ECMWF_omaod550.tif")

    name_time<- (read.csv("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_V1/ECMWF model files/2016/Layer_names_omaod550.csv")[2])
    name_time<- as.character(name_time[,1])
    names(ECMWF_OM_2016)<- name_time


    name_sub<-sprintf("ECMWF_%04d.%02d.",2016,i)

    subset_tr<- subset(name_time, grepl(name_sub,name_time))

    ECMWF_OM_2016_jan<- mean( subset(ECMWF_OM_2016,subset_tr), na.rm = T )

    
    ECMWF_OM_jan<- ECMWF_OM_2016_jan #overlay(ECMWF_OM_2013_jan,ECMWF_OM_2014_jan,ECMWF_OM_2015_jan,fun=mean) #,ECMWF_OM_2016_jan
    
    
    ##### kriging to 10km 
    ECMWF_OM_jan_10km <-kriging_points(dawit=ECMWF_OM_jan, resl_ras= 0.1, shp_UAE = "D:/Air Quality/GWR/UAE_boundary")
    names(ECMWF_OM_jan_10km)<-"OM_ECMWF"
    plot(ECMWF_OM_jan_10km)
    
    writeRaster(ECMWF_OM_jan_10km, sprintf(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/ECMWF/ECMWF_OM_%02d_10km.tif"),i), overwrite=TRUE)
    
    rm(list = ls()[!ls() %in% c( "i")])
  }
  
}








