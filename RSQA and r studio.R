#run this everytime when I start
setwd("~/Documents/All of my R") 



mydata <- read.csv("AAAAAAA/Results.csv")
View(mydata)
exp_count<-mydata%>%
  group_by(PARM_NM)%>%
  summarise(count = n())
exp_count
exp_count$count
ggplot(exp_count, aes(x=count)) + geom_histogram(binwidth = 100)
exp_count<-exp_count[exp_count$count > 50,]
exp_count<-exp_count[order(exp_count$count, decreasing=T),]
head(exp_count)
ChlorimuronEthyl<-mydata[mydata$PARM_NM=='Chlorimuron-ethyl, wf',]
ChlorimuronEthyl
f<-ggplot(ChlorimuronEthyl, aes(x=PARM_NM)) + geom_jitter()
ggplot(ChlorimuronEthyl, aes(x=PARM_NM, y=RESULT_VA)) + geom_jitter()
top_5_data<-mydata[mydata$PARM_NM%in%exp_count$PARM_NM[1:5],]
ggplot(top_5_data, aes(x=PARM_NM, y=RESULT_VA)) + geom_jitter() + facet_grid(.~PARM_NM)
library('rnaturalearth')
library('rnaturalearthdata')
library('ggspatial')
library('rgeos')

getwd()
sites<-read.csv("Sites.CSV")
head(sites)
head(top_5_data)
top_5_data_sites<-top_5_data%>%left_join(sites, by="SITE_NO")
world<-ne_countries(scale='medium', returnclass='sf')
world_map<-ggplot(data=world)+
  geom_sf()+theme_classic()
world_map <-ggplot(data=world) +
  geom_sf() + theme_classic() +
  labs(title="Map of PNSQA Sampling Sites", x="Longitude", y="Latitude",
      subtitle=paste0("A total of ", (length(unique(sites$SITE_NO))), " sites")) +
  geom_point(data = top_5_data_sites, aes(x = DEC_LONG_VA, y = DEC_LAT_VA,color=STATE_NM.x), size = 1, shape = 19)+
  coord_sf(xlim=c((min(sites$DEC_LONG_VA)),(max(sites$DEC_LONG_VA))),ylim =c((min(sites$DEC_LAT_VA)),(max(sites$DEC_LAT_VA))))
world_map 
library('ggpubr')
head(Results_graph)
Results_graph
pyrene_solids<-Results_graph[Results_graph$PARM_NM=="Pyrene, solids",]

#might delete this code
pyrene_solids$COUNTY_NM
pyrene_solids
counties<-c("Alameda County", 'Contra Costa County','Marin County', 'Monterey County', "Napa County", "San Benito County", "San Luis Obispo County", "San Mateo County", "Santa Barbara County", "Santa Clara County", "Santa Cruz County", "Solano County", "Sonoma County")
pyrene_solids_and_counties<-filter(pyrene_solids, COUNTY_NM%in%counties)
pyrene_solids_and_counties


#start of code for the dot chart graph
theme_set(theme_bw())
head(pyrene_solids_and_counties)
e <-ggplot(pyrene_solids_and_counties, aes(x=COUNTY_NM, y=RESULT_VA))

f<-e + geom_jitter(
  aes(shape=COUNTY_NM, color=COUNTY_NM),
  position = position_jitter(0.2),
  size=1.2)
f
g<-f + ggtitle("Max Conc. of Pyrene Solids") +
  xlab("County Name") + ylab("Max Conc./Site")
g
#retry dotpot
aaa<-ggplot(pyrene_solids, aes(x=COUNTY_NM, y=RESULT_VA, group=cond)) +
  geom_jitter()
aab<-aaa + geom_jitter(
  aes(shape=COUNTY_NM, color=COUNTY_NM),
  position=position_jitter(0.2),
  size=1.2)
aab
#graph that is not a dotplot, the graph that turned out though
plot<-ggplot(pyrene_solids, aes(COUNTY_NM, RESULT_VA, color = COUNTY_NM)) + geom_point()
plot
#repeating graph from data used in step 3

plot2<-ggplot(ChlorimuronEthyl, aes(COUNTY_NM, RESULT_VA, color =COUNTY_NM)) + geom_point()
plot2

#graph2 in step 31, map
merged_data<-pyrene_solids%>%left_join(Sites_graph, by="SITE_NO")
california<-ne_countries(scale='medium', returnclass = 'sf')
california_map<-ggplot(data=california) +
  geom_sf() + theme_classic() +
  labs(title='World Map', x='lat',y='long')
california_map
california_map2<-ggplot(data=california)+
  geom_sf() + theme_classic() +
  aes(title="Map of California Sampling Sites for Pyrene Solids", x="Longitude", y="Latitude",
      subtitle=paste0("A total of", (length(unique(merged_data$SITE_NO))),"sites")) + 
        geom_point(data=merged_data, aes(x="DEC_LONG_VA", y="DEC_LONG_VA"), size=1, shape=19) +
  coord_sf(xlim=c((min(Sites_graph$DEC_LONG_VA)),(max(Sites_graph$DEC_LONG_VA))), ylim=c((min(Sites_graph$DEC_LAT_VA)),(max(Sites_graph$DEC_LAT_VA))))

california_map2
      
