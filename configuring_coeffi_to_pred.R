
load("D:/Air Quality/GWR_with_met/Result/result_regression.RData")

name <- str_sub("result_regression_DU_UR_DE_ED", start = 19, end=-1)

rm(list = ls()[!ls() %in% c( "result_monthly","name","dawit", "qq")])


Cons_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "X.Intercept.")])
  names(dawi)<- month.name[(kk+1)]
  Cons_coeffi<-stack(Cons_coeffi,dawi)
}

#plot(Cons_coeffi[[12]])
#DU_UR_DE_ED

Dust_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "ECMWF_DUST_jan_10km")])
  names(dawi)<- month.name[(kk+1)]
  Dust_coeffi<-stack(Dust_coeffi,dawi)
}

#plot(Dust_coeffi[[12]])

Urban_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "urban_fraction")])
  names(dawi)<- month.name[(kk+1)]
  Urban_coeffi<-stack(Urban_coeffi,dawi)
}
#plot(Urban_coeffi[[12]])

Desert_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "desert_fraction")])
  names(dawi)<- month.name[(kk+1)]
  Desert_coeffi<-stack(Desert_coeffi,dawi)
}
#plot(Desert_coeffi[[12]])


ED_coeffi<- stack()

for (kk in 0:11){
  
  dawi<- result_monthly[[kk*12+1]]
  dawi<- rasterFromXYZ( as.data.frame(dawi)[, c("x","y", "ED")])
  names(dawi)<- month.name[(kk+1)]
  ED_coeffi<-stack(ED_coeffi,dawi)
}
#plot(ED_coeffi[[12]])

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####


#$$$$$$$ constant rasters ####

setwd("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/")

LU_fract_desert<- raster("Land_cover/desert_fraction.tif")#/100 ## constant for all months
names(LU_fract_desert)<- "DESERT"
LU_fract_urban<- raster("Land_cover/urban_fraction.tif")#/100   ## constant for all months
names(LU_fract_urban)<-"URBAN"
ED<- raster("ED_10km.tif")   ## constant for all months
names(ED)<- "ED"

#plot(ED)

#$$$$$$$ variable rasters (ECWMF and MODIS) #### 
data_all<- data.frame()

for (qq in (1:12)){
##qq= 4  months of the year

name_mon<- sprintf("%02d",qq)
AOD_mean_jan<-raster(paste0("MODIS/AOD_mean_",name_mon,".tif"))
r_moni<- raster(paste0("in_situ_IDW/in_situ_kriging_UAE_",name_mon,".tif"))
#plot(r_moni)


# ECMWF variable to form layers

ECMWF_OM_ECMWF_jan_10km_1<- raster(paste0("ECMWF/ECMWF_OM_",name_mon,"_10km.tif"))
ECMWF_BC_ECMWF_jan_10km_1<- raster(paste0("ECMWF/ECMWF_BC_",name_mon,"_10km.tif"))
ECMWF_SALT_ECMWF_jan_10km_1<- raster(paste0("ECMWF/ECMWF_SALT_",name_mon,"_10km.tif"))
ECMWF_DUST_ECMWF_jan_10km_1<- raster(paste0("ECMWF/ECMWF_DUST_",name_mon,"_10km.tif"))
ECMWF_SO4_jan_10km_1<- raster(paste0("ECMWF/ECMWF_SO4_",name_mon,"_10km.tif"))

sum_ECMWF<- ECMWF_OM_ECMWF_jan_10km_1+ECMWF_BC_ECMWF_jan_10km_1+ECMWF_SALT_ECMWF_jan_10km_1+
  ECMWF_DUST_ECMWF_jan_10km_1+ECMWF_SO4_jan_10km_1
#plot(ECMWF_DUST_ECMWF_jan_10km)

ECMWF_OM_ECMWF_jan_10km<- ECMWF_OM_ECMWF_jan_10km_1*100/sum_ECMWF
ECMWF_BC_ECMWF_jan_10km<- ECMWF_BC_ECMWF_jan_10km_1*100/sum_ECMWF
ECMWF_SALT_ECMWF_jan_10km<- ECMWF_SALT_ECMWF_jan_10km_1*100/sum_ECMWF
ECMWF_DUST_ECMWF_jan_10km<- ECMWF_DUST_ECMWF_jan_10km_1*100/sum_ECMWF
ECMWF_SO4_jan_10km<- ECMWF_SO4_jan_10km_1*100/sum_ECMWF


##### rearranging the rasters for resolution and extent

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

#plot(r_moni)

ECMWF_DUST_ECMWF_jan_10km<- resample(ECMWF_DUST_ECMWF_jan_10km,Dust_coeffi[[1]], "bilinear")
r_AOD_sampled<- resample(r_AOD_sampled,Dust_coeffi[[1]], "bilinear")
LU_fract_desert<- resample(LU_fract_desert,Dust_coeffi[[1]], "bilinear")
LU_fract_urban<- resample(LU_fract_urban,Dust_coeffi[[1]], "bilinear")
ED<- resample(ED,Dust_coeffi[[1]], "bilinear")
r_moni<- resample(r_moni,Dust_coeffi[[1]], "bilinear")

mon_mon=qq
r_moni_estimated <- r_AOD_sampled + ECMWF_DUST_ECMWF_jan_10km*Dust_coeffi[[mon_mon]] +LU_fract_desert*Desert_coeffi[[mon_mon]]+
  LU_fract_urban*Urban_coeffi[[mon_mon]]+ ED*ED_coeffi[[mon_mon]]# + Cons_coeffi[[mon_mon]]




plot(r_moni_estimated)

hist(r_moni_estimated, breaks=100)

plot(Desert_coeffi[[1]])

source("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/extract_pnt_raster.r")
load( file= "D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/coordin_site.RData")

r_moni_estimated_points<- extract_points(raster=r_moni_estimated, input_stations = coordin_site)
r_moni_points<- extract_points(raster=r_moni, input_stations = coordin_site)


data_moni<- as.data.frame(r_moni_points)
colnames(data_moni)<- "Monitoring"
data_est<- as.data.frame(r_moni_estimated_points)
colnames(data_est)<- "Estimates"

data<- cbind(data_moni,data_est)

data<-na.omit(data)

png(paste0("D:/Air Quality/GWR/new_analysis_2013_2015/Validation_2016/result_images/scatter_mon_est_opt_all_woc_",name_mon,".png"),
    width = 1680, height = 1050, units = "px", pointsize = 15,
    bg = "white", res = 150)

print({ 
plot(data_all, xlim=c(0, 1000), ylim=c(0, 1000), col="blue",cex = 0.5,pch=8, xlab=expression(paste("Monitoring ( ",mu,"g ",m^-3," )")),
     ylab= expression(paste("Estimates ( ",mu,"g ",m^-3," )")), main = month.name[as.numeric(name_mon)])
abline(0,1,lwd=2)
})
dev.off()
data_all<- rbind(data_all, data )

}


