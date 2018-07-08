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
