################################################################
### Uncomment to load the packages used in this analysis
#lab.packages <- c("lattice", "gmodels", "car","ggplot2","extrafont","ggthemes","zoo","reshape2","maptools","rgdal","rgeos","ggmap","sp","hexbin",")
#install.packages(pkgs=lab.packages)

# loads packages into memory
library(lattice)
library(gmodels)
library(car)
library(plyr)
library(ggplot2) ## The grammar of graphics!
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(zoo) ## Manage reformatting of date
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
library(RColorBrewer) ## Color palette
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
library(plyr)
gpclibPermit()
library(lubridate)
library(date)
library(gdata)
library(gridExtra)
library(scales)
