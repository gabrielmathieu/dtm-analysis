##########################################################
### Displacement Tracking Matrix analysis #################
###########################################################

## Edouard Legoupil -Information Management Officer- legoupil..at..unhcr.org

## Loading all 
source("~/unhcr_r_project/displacement/packages.R")

### Dataset from http://iomiraq.net/dtm-page
## See also http://www.humanitarianresponse.info/applications/data/datasets/locations/iraq
rm(master)

#master <- read.csv("~/unhcr_r_project/displacement/data/final_18082014_dtm_master.csv", header=F)
#master <- read.csv("~/unhcr_r_project/displacement/data/DTM_Dataset_-_01092014_final_0.csv", header=F)
#master_14092014 <- read.csv("~/unhcr_r_project/displacement/data/DTM_Dataset_14092014.csv", header=F)
master <- read.csv("~/unhcr_r_project/displacement/data/DTM_Dataset_28092014.csv", header=F)

#master_or <- read.csv("~/unhcr_r_project/displacement/data/DTM_Dataset_28092014.csv", header=F)
#label <- read.csv("~/unhcr_r_project/displacement/data/label.csv")
#names(master_or) <- label[,3]
#rm(label)
#master_or$Master.Families2 <- as.numeric(master_or$Master.Families)
#test <- master$Master.Families - (master$total/6)

## Reorganising the dataset -- parse the data, add aggreated column, prepare some ranking
source("~/unhcr_r_project/displacement/recode.R")

## Generating a series of graph on asylum, orgin, data and accomodation
source("~/unhcr_r_project/displacement/plot.R")

## Aggregating data at level1 & level 2 and joining it with population information as well as refugee information
## Result is govnames and disnames
source("~/unhcr_r_project/displacement/aggregated.R")


## Reapportion to the 1.8 millions figures and generate graph for the one pager
## Result is govnames and disnames
source("~/unhcr_r_project/displacement/caseload.R")

## prepare some classification of sites based on population level -- data is aggregated per sites -- not by report
## Result is govnames and disnames
source("~/unhcr_r_project/displacement/classification.R")

## Relocation simulation based on camp absorption capacity
source("~/unhcr_r_project/displacement/simulation.R")

## Prioritisation plan of locations to be assessed based on IOM baseline
source("~/unhcr_r_project/displacement/baseline.R")

## Choroplets maps based on aggregated information at level1 and level2
source("~/unhcr_r_project/displacement/mapchoro.R")

## Point based maps looking at accomodation and priority needs
source("~/unhcr_r_project/displacement/map.R")



## Looking at histograme of reported location based on population size and accomodation
source("~/unhcr_r_project/displacement/histo.R")

## Winterisation analysis based on bioclim dataset
### http://www.worldclim.org/
source("~/unhcr_r_project/displacement/climate.R")