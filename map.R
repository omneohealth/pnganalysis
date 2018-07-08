####################################################################
#Install and load required libraries if not iinstalled
#####################################################################

#require pacgs maptool, rgeos, rgdal, raster

if(!require (maptools, quietly = TRUE)) install.packages("maptools") #if maptools required but not installed, install
if(!require (rgeos, quietly = TRUE)) install.packages("rgeos")
if(!require (rgdal, quietly = TRUE)) install.packages("rgdal")
if(!require (raster, quietly = TRUE)) install.packages("raster")

# install png R package from OMNeohealth in Github; devtools

if(!require (devtools, quietly = TRUE)) install.packages("devtools") #if devtools required but not installed, install
install_github("OMNeohealth/papuanewguinea")                         # install
library(papuanewguinea)

##########################################################################
#learn spatial data structure and practice plotting
##########################################################################

#view spatial data structure of country, province, district, llg

country
province
district
llg

country@data
province@data
district@data
llg@data

head (healthfacility)

###########################################################################
# visuaised data
##########################################################################

plot(country)
plot(country, lwd =2)
plot(country, lwd =2, border = "red")
plot(country, lwd =2, border = "red", col = "blue")
plot(country, lty =2, lwd =2, border = "red", col = "blue")

plot(province)
plot(province, lwd =2)
plot(province, lwd =2, border = "red")

plot(district)
plot(llg)

##########################################################################
# mapping variations, change colors, customise map
#########################################################################

#colour province 8, 9, 20, 22 green, rest blues
plot(province, lwd =2, border = "gray50", 
     col= ifelse(province@data$ADM1_PCODE %in% c("08","09", "20", "22"), "green", "blue"))


############################################################################
#create a sample  data for immunisation

imm<- sample (0:100, 22, replace= TRUE)
province@data$imm <- imm
head(province@data)

?cut
 cut(province@data$imm, breaks= c(0, 20 , 40, 60 , 80, 100))
 province@data$immclass<- cut(province@data$imm, breaks= c(0, 20 , 40, 60 , 80, 100), labels= FALSE)

 province@data$immclass 
 
#create a colour scheme
 colourscheme<- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
   
 plot(province, lwd =2, border = "gray50", 
      col= ifelse(province@data$immclass == 0, colourscheme[1],
                  ifelse(province@data$immclass == 1, colourscheme[2],
                         ifelse( province@data$immclass == 2, colourscheme[3],
                                 ifelse(province@data$immclass == 3, colourscheme[4],
                                        ifelse(province@data$immclass == 4, colourscheme[5], colourscheme[6]))))))

 plot(province, lwd=1, border = "gray50",
     col= colourscheme[province@data$immclass +1])
 
 text (x=province, labels="ADM1_EN", cex= 0.3)
 
 ###########################################################################################
 #map maternal mortality
 ###########################################################################################
 
 #read province data
 pdata<- read.csv("provincedata.csv")

 # we want only the data from 2015, creating a new variable pdata2015
 pdata2015<- pdata[pdata$year == 2015, ] 

 #create a variable of total maternal deaths
 pdata2015$totaldead <- pdata2015$deadnothf + pdata2015$deadhf
 