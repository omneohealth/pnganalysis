#####################################
#
# Install and load required libraries if not installed.
#
##########################
#
# Require maptools, rgeos, rgdal, raster
#
if(!require(maptools, quitely = TRUE)) install.packages(("maptools")) # If maptools required byt not installed, install
if(!require(rgeos, quietly = TRUE)) install.packages(("rgeos"))       # If rgeos required but not installed, install
if(!require(rgdal, quietly = TRUE)) install.packages(("rgdal"))       # If rgdal required but not installed, install
if(!require(raster, quietly = TRUE)) install.packages(("raster"))     # If raster required but not installed, install
#
# Install papuanewguinea R package from OMNHealth Github
#
if(!require(devtools, quietly = TRUE)) install.packages(("devtools")) # If devtools required but not installed, install
install_github("OMNeoHealth/papuanewguinea")
#
library(papuanewguinea)       # Load papuanewguinea
#
#########################################
#
# Learn spatial data structure and practise plotting
#
########################################
#
#View spatial data structure of courntry, province, distrit, llg
#
###################
country
province
district
llg
##################
country@data
province@data
district@data
llg@data

healthfacility

head(healthfacility)
#
#
# Plotting Maps now of the country
plot(country)
plot(country, lwd = 2)
plot(country, lwd = 2, border = "red")
plot(country, lwd = 2, border = "red", col = "blue")
plot(country, lty = 2, lwd = 2, border = "red", col = "blue")
#
#
#
plot(province)
plot(province, lwd = 2)
plot(province, lwd = 2, border = "red")
plot(province, lwd = 2, border = "red", col = "blue")
plot(province, lty = 2, lwd = 2, border = "red", col = "blue")
#
#Mapping Variation
#
# Colour province 1,5, 10 red and rest blue
#
plot(province, lwd = 1, border = "gray50",
     col = ifelse(province@data$data$ADM1_PCODE %in% c("01", "05", "10"), "red", "blue"))

#province pcode 08, 09, 20, 22 green, rest blues

plot(province, lwd = 1, border = "gray50",
     col = ifelse(province@data$data$ADM1_PCODE %in% c("08", "09", "20", "22"), "green", "red"))

#create sample data for immuinsation coverage per province
#
imm <-sample(0:100, 22, replace = FALSE)

province@data$imm <-imm

head(province@data)

?cut

cut(province@data$imm, breaks = c(0, 20, 40, 60, 80, 100))

cut(province@data$imm, breaks = c(0, 20, 40, 60, 80, 100), labels = FALSE)

province@data$immclass <-cut(x = province@data$imm,
                             breaks = c(0, 20, 40, 80, 100),
                             labels = FALSE)

province@data$immclass

colourscheme <- c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

colourscheme 
  
plot(province, lwd = 1, border = "gray50",
     col = ifelse(province@data$immclass == 0, colourscheme[1],
                  ifelse(province@data$immclass == 1, colourscheme[2],
                         ifelse(province@data$immclass == 2, colourscheme[3],
                                ifelse(province@data$immclass == 3, colourscheme[4],
                                       ifelse(province@data$immclass == 4, colourscheme[5], colourscheme[6]))))))

plot(province, lwd = 1, border = "gray50",
     col = colourscheme[province@data$immclass + 1])                                                

text(x = province, labels = "ADM1_EN", cex = 0.3)

#############################################################
#
# Map Maternal Mortality
#
###############################################################
#
# Read province Data
#
pdata <- read.csv("provincedata.csv")
#
head(pdata)
#
pdata2015 <-pdata[pdata$year == 2015, ]

pdata2015$totaldead <- pdata2015$deadhf + pdata2015$deadnothf





























