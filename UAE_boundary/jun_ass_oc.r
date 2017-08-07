
#### Jub's assignment ####

file="Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/data/s100000.txt"
fre_col<- read.csv(file, header = F, sep = "\t",skip = 17, nrows=2048)[,1]

L_w <-NULL
L_s <-NULL
L_r<-NULL
file_list<- list.files("Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/data/", pattern = ".txt")
for (kk in 1:45){
  if (kk<= 15){
    xx<- read.csv(paste0("Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/data/",file_list[kk]), header = F, sep = "\t",skip = 17, nrows=2048)
    L_s<- cbind(L_s, xx[,2]/10000)# integration time is 10000
    
  }
  if (kk > 30){
    xx<- read.csv(paste0("Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/data/",file_list[kk]), header = F, sep = "\t",skip = 17, nrows=2048)
    L_r<- cbind(L_r, xx[,2]/7000) # integration time 7000
    
  }
  if(kk>15 & kk<=30){
    xx<- read.csv(paste0("Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/data/",file_list[kk]), header = F, sep = "\t",skip = 17, nrows=2048)
    L_w<- cbind(L_w, xx[,2]/60000) # integration time 60000
    
  }
  
  rm(xx)
}
  
### sky water reference

R_p<- read.table("Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/calibrated_reflectance_for_gray_plate.dat", header = F, sep = "",skip = 2, nrows=2048)
L_s_mean<- rowMeans(L_s, na.rm=T)
L_w_mean<- rowMeans(L_w, na.rm=T)
L_r_mean<- rowMeans(L_r, na.rm=T)

plot( fre_col, L_s_mean )
plot( fre_col, L_w_mean )
plot( fre_col, L_r_mean )

LW<- (L_w_mean - 0.028*L_s_mean)# rho is used as 0.028
Es<- pi* L_r_mean/ R_p[,2]

r_s<- LW/Es


R_s<- cbind(R_p[,1],r_s)

plot(R_s)

write.table(R_s, "Z:/_SHARED_FOLDERS/ocean_RS_related/Rrs_example/result_DG.txt", sep = ",", row.names = F, col.names =c("Lamda", "reflectance"))

