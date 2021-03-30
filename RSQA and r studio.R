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
pyrene_solids$COUNTY_NM
pyrene_solids
counties<-c("Alameda County", 'Contra Costa County','Marin County', 'Monterey County', "Napa County", "San Benito County", "San Luis Obispo County", "San Mateo County", "Santa Barbara County", "Santa Clara County", "Santa Cruz County", "Solano County", "Sonoma County")
filter(pyrene_solids, COUNTY_NM%in%counties)
CONC_PER_SITE<-pyrene_solids$RESULT_VA
CONC_PER_SITE
theme_set(theme_pubclean())
aaa<-ggplot(pyrene_solids, aes(x=counties, y=CONC_PER_SITE))
aaa + geom_jitter(
  aes(shape=counties, color=counties),
  position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8),
  size=1.2)+
  stats_summary(
    aes(color=counties),R
    fun.data="mean_sdl", fun.args = list(mult=1),
    geom='pointrange', size=0.4,
    position=position_dodge(0.8)) +
  scale_color_manual(values =c("#00AFBB", "#E7B800"))
ggplot(Results_graph, aes(counties, CONC_PER_SITE))+
       geom_jitter(aes(color=counties))+
  
  