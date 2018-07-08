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
library(papuanewguinea)                                             # Load papuanewguinea package


################################################################################
#
# Learn spatial data structure and practice plotting
#
################################################################################
#
# View spatial data structure of country, province, district, llg
#






