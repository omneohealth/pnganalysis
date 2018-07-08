################################################################################
#
# Install and load required libraries if not installed
#
################################################################################
#
# Require maptools, rgeos, rgdal, raster
#
if(!require(maptools, quietly = TRUE)) install.packages("maptools") # If maptools required but not installed, install
if(!require(rgeos, quietly = TRUE)) install.packages("rgeos")       # If rgeos required but not installed, install
if(!require(rgdal, quietly = TRUE)) install.packages("rgdal")       # If rgdal required but not installed, install
if(!require(raster, quietly = TRUE)) install.packages("raster")     # If raster required but not installed, install
#
# install papuanewguinea R package from OMNeoHealth Github; devtools
#
if(!require(devtools, quietly = TRUE)) install.packages("devtools") # If devtools required but not installed, install
install_github("OMNeoHealth/papuanewguinea")                        # Install OMNeoHealth/papuanewguinea from GitHub
library(papuanewguinea)                                            # Load papuanewguinea package


################################################################################
#
# Learn spatial data structure and practice plotting
#
################################################################################
#
# View spatial data structure of country, province, district, llg
#
country
province
district
llg
#
#
#
country@data
province@data
district@data
llg@data


head(healthfacility)

#
#
#
plot(country)
plot(country, lwd = 2)
plot(country, lwd = 2, border = "red")
plot(country, lwd = 2, border = "red", col = "blue")
plot(country, lty = 2, lwd = 2, border = "red", col = "blue")

plot(province)
plot(province, lwd = 2)
plot(province, lwd = 2, border = "red")
plot(province, lwd = 2, border = "red", col = "blue")
plot(province, lty = 2, lwd = 2, border = "red", col = "blue")

################################################################################
#
# Mapping variations (like mapping data)
#
################################################################################
#
# Colour province 1, 5, 10 red, rest blue
#
plot(province, lwd = 1, border = "gray50", 
     col = ifelse(province@data$ADM1_PCODE %in% c("01", "05", "10"), "red", "blue"))
#
# province pcode 08, 09, 20, 22 in green, rest blue
#


#
# create sample data for immunisation coverage per province
#
imm <- sample(0:100, 22, replace = TRUE)

province@data$imm <- imm

head(province@data)

?cut

province@data$immclass <- base::cut(x = province@data$imm, 
  breaks = c(0, 20, 40, 60, 80, 100), 
  labels = FALSE)

colourscheme <- c("#eff3ff", "#c6dbef", "#9ecae1", 
                  "#6baed6", "#3182bd", "#08519c")


plot(province, lwd = 1, border = "gray50", 
     col = ifelse(province@data$immclass == 0, colourscheme[1],
             ifelse(province@data$immclass == 1, colourscheme[2],
               ifelse(province@data$immclass == 2, colourscheme[3],
                 ifelse(province@data$immclass == 3, colourscheme[4],
                   ifelse(province@data$immclass == 4, colourscheme[5], colourscheme[6]))))))

plot(province, lwd = 1, border = "gray50", 
     col = colourscheme[province@data$immclass + 1])

text(x = province, labels = "ADM1_EN", cex = 0.3)

################################################################################
#
# Map maternal mortality
#
################################################################################
#
# Read province data
#
pdata <- read.csv("provincedata.csv")
head(pdata)

pdata2015 <- pdata[pdata$year == 2015, ]

#
# Add deadhf + deadnothf for total deaths
#
pdata2015$totaldead <- pdata2015$deadhf + pdata2015$deadnothf

popdata <- sample(100000:300000, 22, replace = FALSE)
popdata
pop <- data.frame("pcode" = 1:22, "pop" = popdata)


pdata2015 <- merge(pdata2015, pop, by = "pcode")


