## http://www.moeding.net/archives/32-Metric-prefixes-for-ggplot2-scales.html
format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}



## Plot per month of Arrival
rm(master.month)
master.month <- melt(master, id=c(43), measure=c(42))
master.month <- dcast(master.month,  Month.Displacement ~ variable, sum)

#master.month <- master.month[order(-master.month$Month.Displacement),]

rm(plotmonth)
plotmonth <- ggplot(data=master.month, aes(x=Month.Displacement , y=total))+
  geom_bar(stat="identity", fill="#5B92E5", colour="#808080")+
  labs(x = "Displacement Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per month of reported displacement")+
  theme_bw()
#theme_wsj()

# Save this!
ggsave("~/unhcr_r_project/displacement/test.png", plotmonth, width=8, height=6,units="in", dpi=300)


#########################################################################
#########################################################################



## Plot per pre-june /postjune
rm(master.monthjune)
master.monthjune <- melt(master, id=c(60), measure=c(42))
master.monthjune <- dcast(master.monthjune, datecut ~ variable, sum)
rm(plotmonthjune)
plotmonthjune <- ggplot(data=master.monthjune, aes(x=datecut , y=total))+
  geom_bar(stat="identity", fill="#5B92E5", colour="#808080")+
  labs(x = "Displacement Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs Jan-June 2014 / post-June 2014")+
  theme_bw()
#theme_wsj()
ggsave("~/unhcr_r_project/displacement/out/plot/plot-monthjune.png", plotmonthjune, width=8, height=6,units="in", dpi=300)
rm(master.monthjune)
rm(plotmonthjune)

#########################################################################
#########################################################################


## Plot accomodation
#names(master)
rm(master.accomodationall)
master.accomodationall <- melt(master, id=c(7), measure=c(19:31))
#master.accomodationall <- dcast(master.accomodation, Place + Governorate +  District ~ variable, sum)
#names(master.accomodationall)
master.accomodationall$variable <- factor(master.accomodationall$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))

rm(plotaccomodationall)
plotaccomodationall <- ggplot(data=master.accomodationall, aes(x=variable , y=value))+
  geom_bar(stat="identity",)+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per Accomodation type")+ 
  coord_flip()+
  theme_bw()

ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationall.png", plotaccomodationall, width=8, height=6,units="in", dpi=300)



#########################################################################
#########################################################################

## Let's focus on the main governorate for better leigibility of the facetted graph
master.accomodationall <- subset(master.accomodationall, Governorate=="Anbar" |
                                   Governorate=="Kirkuk" |
                                   Governorate=="Ninewa"|
                                   Governorate=="Baghdad"|
                                   Governorate=="Najaf"|
                                   Governorate=="Diyalah")

rm(plotaccomodationallgov)
plotaccomodationallgov <- ggplot(data=master.accomodationall, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(strip.text.x = element_text(size = 6, angle = 90),strip.text.y = element_text(size = 4, angle = 90))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_bw()

# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationallgov.png", plotaccomodationallgov, width=8, height=6,units="in", dpi=300)


#########################################################################
#########################################################################

## Let's focus on the kri governorate for better leigibility of the facetted graph
master.accomodationallk <- subset(master.accomodationall, Governorate=="Dahuk" |
                                   Governorate=="Erbil" |
                                   Governorate=="Sulaymaniyah")
master.accomodationallkr <- dcast(master.accomodationallk, Governorate ~ variable, sum)
rm(plotaccomodationallgovk)
plotaccomodationallgovk <- ggplot(data=master.accomodationallk, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(strip.text.x = element_text(size = 6, angle = 90),strip.text.y = element_text(size = 4, angle = 90))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_bw()
#plotaccomodationallgovk <-plotaccomodationallgovk +   geom_text(aes(label = value))  #add labels at centroids

# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationallgovk.png", plotaccomodationallgovk, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################


rm(master.accomodationallmonthk)
master.accomodationallmonthk <- melt(master, id=c(7,43), measure=c(19:31))
master.accomodationallmonthk <- subset(master.accomodationallmonthk, Governorate=="Dahuk" |
                                    Governorate=="Erbil" |
                                    Governorate=="Sulaymaniyah")

#levels(master.accomodation$variable)
## Change order
master.accomodationallmonthk$variable <- factor(master.accomodationallmonthk$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))


rm(plotaccomodationallmonthk)
plotaccomodationallmonthk <- ggplot(data=master.accomodationallmonthk, aes(x=variable , y=value))+
  facet_grid( Governorate ~ Month.Displacement)+
  geom_bar(stat="identity",)+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_bw()


ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationallmonthk.png", plotaccomodationallmonthk, width=8, height=6,units="in", dpi=300)





rm(master.accomodationallmonth)
master.accomodationallmonth <- melt(master, id=c(43), measure=c(19:31))

#levels(master.accomodation$variable)
## Change order
master.accomodationallmonth$variable <- factor(master.accomodationallmonth$variable, levels = c(
  "Unknown.or.other",
  "Informal.settlements",
  "School.Building",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Military.Camps",
  "Mosques.Holly.Shrines",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))


rm(plotaccomodationallmonth)
plotaccomodationallmonth <- ggplot(data=master.accomodationallmonth, aes(x=variable , y=value))+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  geom_bar(stat="identity",)+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_bw()


ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationallmonth.png", plotaccomodationallmonth, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################


rm(master.accomodation)
master.accomodation <- melt(master, id=c(7), measure=c(63:68))
#master.accomodation <- melt(master, id=c(1), measure=c(19,63,64,65,66,28,31))

#levels(master.accomodation$variable)
## Change order
master.accomodation$variable <- factor(master.accomodation$variable, levels = c(
  "Hosted.or.Owned.Accomodation",
  "Rented.Accomodation",
  "Organised.site",
  "Improvised.site",
  "Squatted.schools",
  "Open.air"
  ))


#master.accomodation <- aggregate(value  ~ Governorate, data = master.accomodation, FUN = sum, na.rm = TRUE)
#master.accomodation <- dcast(master.accomodation, Governorate ~ variable, sum)

#master.accomodationall <- melt(master, id=c(11,7,8), measure=c(19,63,64,65,28,31))
#master.accomodationall <- dcast(master.accomodation, Place + Governorate +	District ~ variable, sum)
#names(master.accomodationall)

# 

rm(plotaccomodation)
plotaccomodation <- ggplot(data=master.accomodation, aes(x=variable , y=value))+
  geom_bar(stat="identity",)+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs per Accomodation type")+ 
  coord_flip()+
  theme_bw()
#theme_wsj()

plotaccomodation
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodation.png", plotaccomodation, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################

rm(plotaccomodationgov)
plotaccomodationgov <- ggplot(data=master.accomodation, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(axis.text.y = element_text(size = 8))+
  ggtitle("Total IDPs per Accomodation type and Governorate")+
  coord_flip()+
  theme_bw()
#theme_wsj()

plotaccomodationgov
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationgov.png", plotaccomodationgov, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################

rm(master.accomodationmonth)
master.accomodationmonth <- melt(master, id=c(43), measure=c(63:68))

#levels(master.accomodation$variable)
## Change order
master.accomodationmonth$variable <- factor(master.accomodation$variable, levels = c(
  "Hosted.or.Owned.Accomodation",
  "Rented.Accomodation",
  "Organised.site",
  "Improvised.site",
  "Squatted.schools",
  "Open.air"
))


rm(plotaccomodationmonth)
plotaccomodationmonth <- ggplot(data=master.accomodationmonth, aes(x=variable , y=value))+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  geom_bar(stat="identity",)+
  labs(x = "Arrival Month", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  #theme(element_text(size=8)) +
  ggtitle("Total IDPs per accomodation type and reported displacement date")+
  coord_flip()+
  theme_bw()
#theme_wsj()
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-accomodationmonth.png", plotaccomodationmonth, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################



rm(master.gov)
master.gov <- melt(master, id=c(7), measure=c(42))
#master.gov <- melt(master, id=c(1), measure=c(42))
master.gov <- dcast(master.gov, Governorate ~ variable, sum)
master.gov <- master.gov[order(-master.gov$total),]
master.gov$Governorate <- factor(master.gov$Governorate, levels = master.gov[order(master.gov$total), 1])

rm(plotgov)
plotgov <- ggplot(data=master.gov, aes(x=Governorate , y=total))+
  geom_bar(stat="identity",fill="#5B92E5", colour="#808080")+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates")+
  coord_flip()+
  theme_bw()
#theme_wsj()
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-gov.png", plotgov, width=8, height=6,units="in", dpi=300)


## Stacked chart arrival per month
rm(master.govmonth)
master.govmonth <- melt(master, id=c(7,43), measure=c(42))
#master.govmonth <- melt(master, id=c(1,43), measure=c(42))
master.govmonth <- dcast(master.govmonth, Governorate + Month.Displacement ~ variable, sum)
## Reordering factor for the month
#str(master.govmonth)
#as.factor(master.govmonth$Month.Displacement)
#levels(master.govmonth$Month.Displacement)
#master.govmonth$Month.Displacement <- factor(master.govmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))
#levels(master.govmonth$Month.Displacement)
master.govmonth$Governorate <- factor(master.govmonth$Governorate, levels = master.gov[order(master.gov$total), 1])
#master.govmonth <- master.govmonth[order(master.govmonth$Month.Displacement),]

rm(plotgovmonth)
plotgovmonth <- ggplot(data=master.govmonth, aes(x=Governorate , y=total))+
  geom_bar(stat="identity")+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates and per Month of arrival")+
  scale_fill_stata()+
  coord_flip()+
  theme_bw()

ggsave("~/unhcr_r_project/displacement/out/plot/plot-gov-month.png", plotgovmonth, width=8, height=6,units="in", dpi=300)

####################
## Plot per origin !

# Reorgnise the matrix in order to compute pivot tables
# Tuto: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/
rm(master.origin)
# Melt and cast!
master.origin <- melt(master, id=c(5), measure=c(42))
#master.origin <- melt(master, id=c(6), measure=c(42))
master.origin <- dcast(master.origin, Origin.Governorate ~ variable, sum)
# Reorder the dataframe based on the total
master.origin <- master.origin[order(-master.origin$total),]

## Reorder the level of the factor based on the total -- order of the level are used in plot!
master.origin$Origin.Governorate <- factor(master.origin$Origin.Governorate, levels = master.origin[order(master.origin$total), 1])
## Reverse the order of those level -- might not be usefull if coord_flip
#master.origin$Origin.Governorate <- factor(master.origin$Origin.Governorate, levels=rev(levels(master.origin$Origin.Governorate)) )
## Check level
#levels(master.origin$Origin.Governorate)

rm(plotorigin)
plotorigin <- ggplot(data=master.origin, aes(x=Origin.Governorate , y=total))+
  geom_bar(stat="identity", fill="#5B92E5", colour="#808080")+
  labs(x = "Governorate of Origin", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorates of Origin")+
  coord_flip()+
  theme_bw()
#theme_wsj()

# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-origin.png", plotorigin, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################


## Stacked chart arrival per month
rm(master.govorigin)
master.govorigin <- melt(master, id=c(7,5), measure=c(42))
#master.govorigin <- melt(master, id=c(1,6), measure=c(42))
master.govorigin <- dcast(master.govorigin, Governorate + Origin.Governorate ~ variable, sum)
## Reordering factor for the month
#str(master.govmonth)
#as.factor(master.govmonth$Month.Displacement)
#levels(master.govmonth$Month.Displacement)
#master.govmonth$Month.Displacement <- factor(master.govmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))
#levels(master.govmonth$Month.Displacement)
master.govorigin$Governorate <- factor(master.govorigin$Governorate, levels = master.gov[order(master.gov$total), 1])
#master.govmonth <- master.govmonth[order(master.govmonth$Month.Displacement),]

rm(plotgovorigin)
plotgovorigin <- ggplot(data=master.govorigin, aes(x=Governorate , y=total))+
  geom_bar(stat="identity")+
  facet_wrap(  ~ Origin.Governorate, ncol=3)+
  labs(x = "Governorate of Asylum", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by governorates and per governorate of origin")+
  coord_flip()+
  theme_bw()
#theme_wsj()
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-gov-origin.png", plotgovorigin, width=8, height=6,units="in", dpi=300)


## Stacked chart arrival per month from Origin
rm(master.originmonth)
master.originmonth <- melt(master, id=c(5,43), measure=c(42))
#master.originmonth <- melt(master, id=c(6,43), measure=c(42))
master.originmonth <- dcast(master.originmonth, Origin.Governorate + Month.Displacement ~ variable, sum)
#as.factor(master.originmonth$Month.Displacement)
#master.originmonth$Month.Displacement <- factor(master.originmonth$Month.Displacement, levels = c("Dec-13","Jan-14","Mar-14", "Apr-14","May-14","Jun-14","Jul-14","Aug-14"))

#master.originmonth <- master.originmonth[order(-master.originmonth$Month.Displacement),]
master.originmonth$Origin.Governorate <- factor(master.originmonth$Origin.Governorate,levels = master.origin[order(master.origin$total), 1])

rm(plotoriginmonth)
plotoriginmonth <- ggplot(data=master.originmonth, aes(x=Origin.Governorate , y=total))+
  geom_bar(stat="identity")+
  facet_wrap(  ~ Month.Displacement, ncol=3)+
  labs(x = "Governorate of Origin", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Total IDPs by Governorate of Origin")+
  coord_flip()+
   theme_bw()
  #theme_wsj()

plotoriginmonth
# Save this!
ggsave("~/unhcr_r_project/displacement/out/plot/plot-origin-month.png", plotoriginmonth, width=8, height=6,units="in", dpi=300)
#########################################################################
#########################################################################

ggsave("~/unhcr_r_project/displacement/out/plot/plot-month.png", plotmonth, width=8, height=6,units="in", dpi=300)


### Remove everything!!
rm(master.gov)
rm(master.govorigin)
rm(master.origin)
rm(master.govmonth)
rm(master.accomodationall)
rm(master.accomodation)
rm(master.accomodationmonth)
rm(master.accomodationallmonth)
rm(master.originmonth)
rm(master.originmonth)
rm(master.month)
rm(master.accomodationall)
rm(master.accomodationallk)
rm(master.accomodationallkr)
rm(master.accomodationallmonthk)

rm(plotaccomodation)
rm(plotaccomodationgov)
rm(plotaccomodationallgov)
rm(plotaccomodationallgovk)
rm(plotaccomodationallmonth)
rm(plotaccomodationallmonthk)
rm(plotaccomodationmonth)
rm(plotaccomodationmonthgov)
rm(master.govorigin)
rm(plotorigin)
rm(plotoriginmonth)
rm(plotgov)
rm(plotmonth)
rm(plotgovmonth)
rm(plotgovorigin)
rm(plotlocpop)
rm(plotlocpopgov)
rm(plotaccomodationall)
rm(format_si)
