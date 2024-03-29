
# look at each of the 3 Governorates in the North 
rm(masterloctype)
masterloctype <- aggregate(cbind( total, Master.Families, IDPs.in.Camps.or.transit.camps , School.Building ,Mosques.Holly.Shrines, Abandoned.public.buildings.under.construction , Collective.centres ,
                              Informal.settlements , Military.Camps , Unknown.or.other,
                              Hosted.or.Owned.Accomodation,
                              Rented.Accomodation,
                              Organised.site,
                              Improvised.site,
                              Squatted.schools,
                              Open.air                        
                              ) ~ Governorate+District+Place+Longitude+Latitude,
                       data = master, FUN = sum, na.rm = TRUE)

poptot <- sum(masterloctype$total)

masterkri <- subset(masterloctype, Governorate=="Dahuk" |
                                         Governorate=="Erbil" |
                                         Governorate=="Sulaymaniyah")
rm(mastersouth)
mastersouth <- subset(masterloctype, Governorate=="Anbar" |
                      Governorate=="Kirkuk" |
                      Governorate=="Ninewa"|
                        Governorate=="Baghdad"|
                        Governorate=="Najaf"|
                        Governorate=="Diyalah")


# get total indivdual for kri
poptotal <- sum(masterloctype$total)
popkri <- sum(masterkri$total)
popsouth <- sum(mastersouth$total)

est <-popsouth/(poptotal-popkri)
rm(master.accomodationsouth)
master.accomodationsouth <- melt(mastersouth, id=c(1), measure=c(8:15))
#master.accomodationall <- dcast(master.accomodation, Place + Governorate +  District ~ variable, sum)
#names(master.accomodationall)
master.accomodationsouth$variable <- factor(mastersouth.accomodationsouth$variable, levels = c(
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



rm(plotaccomodationsouthgov)
plotaccomodationsouthgov <- ggplot(data=master.accomodationsouth, aes(x=variable , y=value))+
  facet_wrap(  ~ Governorate, ncol=3)+
  geom_bar(stat="identity")+
  labs(x = "Accomodation type", y = "Total IDP Ind.")+
  scale_y_continuous(labels=format_si())+
  theme(strip.text.x = element_text(size = 6, angle = 90),strip.text.y = element_text(size = 4, angle = 90))+
  ggtitle("Total IDPs per Accomodation type for South (Main Gov: 77%)")+
  coord_flip()+
  theme_bw()

# Save this!
ggsave("~/unhcr_r_project/displacement/out/simulation/plot-accomodationallgov.png", plotaccomodationsouthgov, width=8, height=6,units="in", dpi=300)




########################################################################
rm(masterkri.accomodationall)
masterkri.accomodationall <- melt(masterkri, id=c(1), measure=c(8:15))

#levels(master.accomodation$variable)
## Change order
masterkri.accomodationall$variable <- factor(masterkri.accomodationall$variable, levels = c(
  "School.Building",
  "Informal.settlements",
  "Mosques.Holly.Shrines",
  "Abandoned.public.buildings.under.construction",
  "IDPs.in.Camps.or.transit.camps",
  "Unknown.or.other",
  "Military.Camps",
  "Collective.centres",
  "Rented.Hotel",
  "Rented.House",
  "With.HC.non.Relative",
  "With.Relative",
  "IDP.Owned.House"
))

masterkri.accomodationall.sum <- dcast(masterkri.accomodationall, Governorate ~ variable, sum)


##identify how many will be moved to camps under the 2 month action plan


## We need to load the existing camp description with capacity and current population
idpcamp <- read.csv("~/unhcr_r_project/displacement/data/idpcamp.csv")
idpcamp$Total.availability <- idpcamp$plancapacity - idpcamp$Individual

# sum per governorate
idpcamp.gov <- aggregate(Total.availability  ~ Governorate, data = idpcamp, FUN = sum, na.rm = TRUE)

#merging
rm(masterkri.accomodationall.merge)
masterkri.accomodationall.merge <- merge(x=masterkri.accomodationall.sum, y=idpcamp.gov, by="Governorate" )

rm(masterkri.accomodationall.sum)

##identify the balance  who remains in informal communal shelter etc -  as well as those living on their own private arrangements

masterkri.accomodationall.merge$available.after.school <- masterkri.accomodationall.merge$Total.availability - masterkri.accomodationall.merge$School.Building
masterkri.accomodationall.merge$available.after.inf.settle <- masterkri.accomodationall.merge$available.after.school - masterkri.accomodationall.merge$Informal.settlements
masterkri.accomodationall.merge$available.after.religious <- masterkri.accomodationall.merge$available.after.inf.settle - masterkri.accomodationall.merge$Mosques.Holly.Shrines
masterkri.accomodationall.merge$available.after.abandonned <- masterkri.accomodationall.merge$available.after.religious - masterkri.accomodationall.merge$Abandoned.public.buildings.under.construction
masterkri.accomodationall.merge$available.after.unknown <- masterkri.accomodationall.merge$available.after.abandonned - masterkri.accomodationall.merge$Unknown.or.other
masterkri.accomodationall.merge$available.after.othercommunal <- masterkri.accomodationall.merge$available.after.unknown - psum(masterkri.accomodationall.merge$IDPs.in.Camps.or.transit.camps,
                                                                                                            masterkri.accomodationall.merge$Military.Camps,
                                                                                                            masterkri.accomodationall.merge$Collective.centres,
                                                                                                            na.rm=TRUE)

masterkri.accomodationall.merge$Remaining <- masterkri.accomodationall.merge$available.after.othercommunal - psum(masterkri.accomodationall.merge$Rented.Hotel, 
                                                                                                             masterkri.accomodationall.merge$Rented.House,
                                                                                                             masterkri.accomodationall.merge$With.HC.non.Relative, 
                                                                                                             masterkri.accomodationall.merge$With.Relative, 
                                                                                                             masterkri.accomodationall.merge$IDP.Owned.House,
                                                                                                             na.rm=TRUE)
write.csv(masterkri.accomodationall.merge, file = "~/unhcr_r_project/displacement/out/data/siumlation.csv")
rm(masterkri.accomodationall.plot)
masterkri.accomodationall.plot <- melt(masterkri.accomodationall.merge, id=c(1), measure=c(10:16))


masterkri.accomodationall.plot$variable <- factor(masterkri.accomodationall.plot$variable, levels = c(
  "Remaining",
  "available.after.unknown",
  "available.after.abandonned",
  "available.after.religious",
  "available.after.inf.settle",
  "available.after.school",
  "Total.availability"
))

masterkri.accomodationall.plot1 <- dcast(masterkri.accomodationall.plot,  variable  ~ value, sum)
masterkri.accomodationall.plot1 <- aggregate(masterkri.accomodationall.plot, by=list(value,variable),FUN=sum, na.rm=TRUE)

## Code colors for balance
masterkri.accomodationall.plot[["sign"]] = ifelse(masterkri.accomodationall.plot[["value"]] >= 0, "positive", "negative")
## Code colors for available
#masterkri.accomodationall.plot[["sign"]] = ifelse(masterkri.accomodationall.plot["variable"] == 'available',"available", masterkri.accomodationall.plot$sign )


rm(plotrelocationall)
plotrelocationall <- ggplot(masterkri.accomodationall.plot, aes(x=variable , y=value))+
  geom_bar(stat="identity",)+
  labs(x = "Availability", y = "Total IDP persons")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Capacity Vs Needs in KRG plan")+   

  coord_flip()+
  theme_bw()
ggsave("~/unhcr_r_project/displacement/out/simulation/plot-relocationall.png", plotrelocationall, width=8, height=6,units="in", dpi=300)
rm(plotrelocationall)

rm(plotrelocationallgov)
plotrelocationallgov <- ggplot(masterkri.accomodationall.plot, aes(x=variable , y=value, fill = sign))+
  geom_bar(stat="identity",)+
  labs(x = "Availibility", y = "Total IDP persons")+
  scale_y_continuous(labels=format_si())+
  ggtitle("Capacity Vs Needs in KRG plan (by Governorate)")+ 
  scale_fill_manual(values = c("positive" = "darkblue", "negative" = "red"))+
  facet_wrap(  ~ Governorate, ncol=3)+
  coord_flip()+
  theme_bw()

ggsave("~/unhcr_r_project/displacement/out/simulation/plot-relocationallgov.png", plotrelocationallgov, width=8, height=6,units="in", dpi=300)
rm(plotrelocationallgov)


############ Checking classes for Abandonned building

rm(masterkri.accomodationall)

masterkri$class.abandoned.fixed <-as.factor(findCols(classIntervals(masterkri$Abandoned.public.buildings.under.construction, n=6, style="fixed",fixedBreaks=c(0, 50, 100, 250, 500, 2000, 90000))))

masterkri$class.abandoned.fixed <-revalue(masterkri$class.abandoned.fixed, c("1"="0-49", "2"="50-99", "3"="100-249", "4"="250-499", "5"="500-2000", "6"=">2000"))


rm(histo.abandonnedtotalclassfamily)
histo.abandonnedtotalclassfamily <- ggplot(masterkri, aes(x=as.factor(masterkri$class.abandoned.fixed)))+
  geom_histogram(binwidth=.5)+
  labs(x = "Population class size")+
  ggtitle("Number of abandonned sites per population size class")+
  theme_bw()
ggsave("~/unhcr_r_project/displacement/out/simulation/histogramm-abandonnedtotalclassfamily.png", histo.abandonnedtotalclassfamily, width=8, height=6,units="in", dpi=300)

rm(histo.abandonnedtotalclassfamilygov)
histo.abandonned totalclassfamilygov <- ggplot(masterkri, aes(x=as.factor(masterkri$class.abandoned.fixed))) +
  geom_histogram(binwidth=.5)+
  labs(x = "Population class size")+
  ggtitle("Number of abandonned sites per population size class")+
  facet_wrap(  ~ Governorate, ncol=3)+
  theme_bw()
ggsave("~/unhcr_r_project/displacement/out/simulation/histogramm-abandonnedtotalclassfamilygov.png", histo.abandonnedtotalclassfamilygov, width=8, height=6,units="in", dpi=300)



#########################################################################


#names(masterkri)
##and identify number of people / families currently living in what can be called communal centers / shelters etc

masterkri.accomodation <- melt(masterkri, id=c(1), measure=c(16:21))

#levels(master.accomodation$variable)
## Change order
masterkri.accomodation$variable <- factor(masterkri.accomodation$variable, levels = c(
  "Hosted.or.Owned.Accomodation",
  "Rented.Accomodation",
  "Organised.site",
  "Improvised.site",
  "Squatted.schools",
  "Open.air"
))

masterkri.accomodation.sum <- dcast(masterkri.accomodation, Governorate ~ variable, sum)

##identify how many will be moved to camps under the 2 month action plan


## We need to load the existing camp description with capacity and current population
#idpcamp <- read.csv("~/unhcr_r_project/displacement/data/idpcamp.csv")
#idpcamp$available <- idpcamp$plancapacity - idpcamp$Individual

# sum per governorate
#idpcamp.gov <- aggregate(available  ~ Governorate, data = idpcamp, FUN = sum, na.rm = TRUE)

#merging
rm(masterkri.accomodation.merge)
masterkri.accomodation.merge <- merge(x=masterkri.accomodation.sum, y=idpcamp.gov, by="Governorate" )
rm(masterkri.accomodation.sum)

##identify the balance  who remains in informal communal shelter etc -  as well as those living on their own private arrangements

masterkri.accomodation.merge$available.after.open <- masterkri.accomodation.merge$available - masterkri.accomodation.merge$Open.air
masterkri.accomodation.merge$available.after.school <- masterkri.accomodation.merge$available.after.open - masterkri.accomodation.merge$Squatted.schools
masterkri.accomodation.merge$available.after.impro <- masterkri.accomodation.merge$available.after.school - masterkri.accomodation.merge$Improvised.site


rm(masterkri.accomodation)
