

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(splines)
library(plyr)


setwd("Z:/_SHARED_FOLDERS/Air Quality/Phase 2/PhD_DG/GWR_with_met")

mydata <- read.csv("PM25_MET_data_kriged.csv")


####### equation ########################################################

lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

#####################################################################################

# plot with regression line-----

ggplot(mydata, aes(x=Moni, y=wind_speed)) +
  theme_bw() +
  geom_point(size = 2, color='black') +    # Use hollow circles
  geom_smooth(method=lm) +  # Add linear regression line 
  scale_x_continuous(name ="AOD (dust)") +
  ylab(expression(paste(PM[10], " (µg/",m^3, ")"))) +
  theme(axis.title.y = element_text(face="bold", colour="black", size=12),
        axis.text.y  = element_text(angle=0, vjust=0.5, size=13)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size=12),
        axis.text.x  = element_text(angle=0, vjust=0.5, size=13)) +
  geom_text(aes(x = 10, y = 0.5, label = lm_eqn(lm(wind_speed ~ temp , mydata))),
            size = 5,
            color = "red",
            parse = TRUE)  
  # ylim(c(0,90)) 


png(paste0(output_folder,"correlation_PM15_met.png"),
    width = 1300, height = 1050, units = "px", pointsize = 30,
    bg = "white", res = 150)
print(plot)
dev.off()



