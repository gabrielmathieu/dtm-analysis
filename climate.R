## Winterisation analysis based on bioclim dataset
### http://www.worldclim.org/
#  temperature data are in °C * 10. This means that a value of 231 represents 23.1 °C. 

# interesting variables to look at are: 

#BIO5 = Max Temperature of Warmest Month
#BIO6 = Min Temperature of Coldest Month
#BIO10 = Mean Temperature of Warmest Quarter
#BIO11 = Mean Temperature of Coldest Quarter

#tmin = getData('worldclim', var='bio', res=0.5, lon=44, lat=32)


bio5 <- raster("~/unhcr_r_project/displacement/shp/bio5_17.tif",native=T)
bio6 <- raster("~/unhcr_r_project/displacement/shp/bio6_17.tif",native=T)
bio10 <- raster("~/unhcr_r_project/displacement/shp/bio10_17.tif",native=T)
bio11 <- raster("~/unhcr_r_project/displacement/shp/bio11_17.tif",native=T)

#Create a raster stack and extract values for all rasters.
bioall <- stack(bio5,bio6,bio10,bio11)
rm(bio5)
rm(bio6)
rm(bio10)
rm(bio11)


#plot(rdr)

# Analysing the raster
#cellStats(tmin, stat="mean")
#cellStats(tmin, stat="skew")
#quantile(tmin, probs = c(0.25, 0.50, 0.75))

## extraction information per location 
rm(masterloc)
masterloc <- aggregate(cbind( total, Master.Families, IDPs.in.Camps.or.transit.camps , School.Building ,Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction , Collective.centres ,
                              Informal.settlements , Military.Camps , Unknown.or.other) ~ Governorate+District+Place+Longitude+Latitude,
                       data = master, FUN = sum, na.rm = TRUE)

# Converting the dataframe in a spatial dataframe
coordinates(masterloc) <- c("Longitude", "Latitude")


# extract values
rm(masterlocal.bioall)
masterlocal.bioall <- extract(bioall, masterloc, df=TRUE)
## reformat temperature
masterlocal.bioall$bio5_17 <- masterlocal.bioall$bio5_17 /10
masterlocal.bioall$bio6_17 <- masterlocal.bioall$bio6_17 /10	
masterlocal.bioall$bio10_17	 <- masterlocal.bioall$bio10_17 /10
masterlocal.bioall$bio11_17 <- masterlocal.bioall$bio11_17 /10

## Classify temperature

masterlocal.bioall$maxtemp <-as.factor(findCols(classIntervals(masterlocal.bioall$bio5_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
masterlocal.bioall$maxtemp <-revalue(masterlocal.bioall$maxtemp, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))

masterlocal.bioall$mintemp <-as.factor(findCols(classIntervals(masterlocal.bioall$bio6_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
masterlocal.bioall$mintemp <-revalue(masterlocal.bioall$mintemp, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))

masterlocal.bioall$meanwarm <-as.factor(findCols(classIntervals(masterlocal.bioall$bio10_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
masterlocal.bioall$meanwarm <-revalue(masterlocal.bioall$meanwarm, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))

masterlocal.bioall$meancold <-as.factor(findCols(classIntervals(masterlocal.bioall$bio11_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
masterlocal.bioall$meancold <-revalue(masterlocal.bioall$meancold, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))


## merge this with points
rm(masterlocation)
masterlocation <- aggregate(cbind( total, Master.Families, IDPs.in.Camps.or.transit.camps , School.Building ,Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction , Collective.centres ,
                              Informal.settlements , Military.Camps , Unknown.or.other) ~ Governorate+District+Place+Longitude+Latitude,
                       data = master, FUN = sum, na.rm = TRUE)
masterlocation <- merge(x=masterlocation, y= masterlocal.bioall, by="row.names",all.x=TRUE)
# write to file
write.csv(masterlocation, '~/unhcr_r_project/displacement/out/data/temperature.csv', row.names=FALSE)


############ For the camps

## We need to load the existing camp description with capacity and current population
idpcamp <- read.csv("~/unhcr_r_project/displacement/data/idpcamp.csv")
coordinates(idpcamp) <- c("LONGITUDE", "LATITUDE")

# extract values
rm(idpcamp.bioall)
idpcamp.bioall <- extract(bioall, idpcamp, df=TRUE)
## reformat temperature
idpcamp.bioall$bio5_17 <- idpcamp.bioall$bio5_17 /10
idpcamp.bioall$bio6_17 <- idpcamp.bioall$bio6_17 /10  
idpcamp.bioall$bio10_17   <- idpcamp.bioall$bio10_17 /10
idpcamp.bioall$bio11_17 <- idpcamp.bioall$bio11_17 /10

## Classify temperature
idpcamp.bioall$maxtemp <-as.factor(findCols(classIntervals(idpcamp.bioall$bio5_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
idpcamp.bioall$maxtemp <-revalue(idpcamp.bioall$maxtemp, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))
idpcamp.bioall$mintemp <-as.factor(findCols(classIntervals(idpcamp.bioall$bio6_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
idpcamp.bioall$mintemp <-revalue(idpcamp.bioall$mintemp, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))
idpcamp.bioall$meanwarm <-as.factor(findCols(classIntervals(idpcamp.bioall$bio10_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
idpcamp.bioall$meanwarm <-revalue(idpcamp.bioall$meanwarm, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))
idpcamp.bioall$meancold <-as.factor(findCols(classIntervals(idpcamp.bioall$bio11_17, n=11, style="fixed",fixedBreaks=c(0,5,10,15,20,25,30,35,40,45,50,55))))
idpcamp.bioall$meancold <-revalue(idpcamp.bioall$meancold, c("1"="0-5°C", "2"="5-10°C", "3"="10-15°C", "4"="15-20°C", "5"="20-25°C", "6"="25-30°C", "7"="30-35°C", "8"="35-40°C", "9"="40-45°C", "10"="45-50°C", "11"="50-55°C"))

## merge this with points
rm(idpcampbio)
idpcampbio <- read.csv("~/unhcr_r_project/displacement/data/idpcamp.csv")
idpcampbio <- merge(x=idpcampbio, y= idpcamp.bioall, by="row.names",all.x=TRUE)
# write to file
write.csv(idpcampbio, '~/unhcr_r_project/displacement/out/data/temperaturecamp.csv', row.names=FALSE)

## Let's prepare a few  plot

#?extract
# write to file
write.csv(masterlocation, '~/unhcr_r_project/displacement/out/masterlocation.txt', row.names=FALSE)
