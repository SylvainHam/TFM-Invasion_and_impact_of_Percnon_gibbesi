### ...# Read in ----


NPP.dataset  <- read.csv("C:/Users/pessa/OneDrive/Documentos/Marine Life/Articles/2021/Pessarrodona et al - Dataset paper/NPP Dataset.csv")%>%
  glimpse()
World <- readOGR("C:/Users/pessa/OneDrive/Documentos/Marine Life/Resources/Shapefiles/ne_10m_land")
World.land <- readOGR("C:/Users/pessa/OneDrive/Documentos/Marine Life/Resources/Shapefiles/Countries_WGS84")


############ DATA TIDYING & SELECTION ###################

#. Tidy data ----

## ... Check species names ----
species_list <- NPP.dataset %>% 
  dplyr::select(Species) %>%
  dplyr::distinct() %>% 
  #dplyr::pull((Species))%>% 
  glimpse()

## create a species list that u then upload to match taxon in WoRMS database which tells you which are current

#write.csv(species_list,"C:/Users/pessa/OneDrive/Documentos/PhD/Science!/Global kelp carbon/Model feed/species_list.csv")

## ... Check no taxa are in 2 fams ----

double.species<-NPP.dataset%>%
  dplyr::group_by(Genus, Species)%>%
  tally()%>%
  dplyr::group_by(Species)%>%
  tally()%>%
  glimpse() 

double.genus<-NPP.dataset%>%
  dplyr::group_by(Family, Genus)%>%
  tally()%>%
  dplyr::group_by(Genus)%>%
  tally()%>%
  glimpse()  # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species

double.family<-NPP.dataset%>%
  dplyr::group_by(Order, Family)%>%
  tally()%>%
  dplyr::group_by(Family)%>%
  tally()%>%
  glimpse()  # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species


double.order<-NPP.dataset%>%
  dplyr::group_by(Phyla,Order)%>%
  tally()%>%
  dplyr::group_by(Order)%>%
  tally()%>%
  glimpse()  # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species




## ... Check points are not on land ----
## Create an sf POINTS object
library(sf)
library(spData)
library(maptools)
library(maps)
library(tidyverse)
library(rgdal)

World.land <- readOGR("C:/Users/pessa/OneDrive/Documentos/Marine Life/Resources/Shapefiles/ne_10m_land")

## need to convert everything to a sf object otherwise wont work
#wrld <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
wrld<- st_as_sf(World.land)
class(wrld)

points <- NPP.dataset  %>% 
  dplyr::filter(!is.na(Longitude_decimal_degrees), !is.na(Latitude_decimal_degrees))%>% 
  dplyr::select(Longitude_decimal_degrees, Latitude_decimal_degrees)%>%  # Note that I reversed OP's ordering of lat/long
  glimpse() 

## need to convert everything to a sf object otherwise wont work
pts <- st_as_sf(points, coords=1:2, crs=4326)

## Find which points fall over land
ii <- !is.na(as.numeric(st_intersects(pts, wrld)))

## plot it 
plot(st_geometry(wrld))
plot(pts, col=1+ii, pch=16, add=TRUE) ## red means is on land

# problem is the resolution of the shapefile is too coarse, giving lots of false positives and also some of the datapoints were entered in EPSG:3857 (google)
#so we need to check them manually

## extract that info into a dataframe
data.on.land <- cbind(points,as.data.frame(ii))%>% 
  dplyr::select(Latitude_decimal_degrees,Longitude_decimal_degrees,ii)%>% 
  filter(ii==(TRUE))%>% 
  distinct(Latitude_decimal_degrees,Longitude_decimal_degrees,ii)%>% 
  glimpse()


#write.csv(data.on.land,"C:/Users/pessa/OneDrive/Documentos/PhD/Science!/Global kelp carbon/Dataset paper/data.on.land2.csv")

## ... Check double values ----
double.value.check <- NPP.dataset %>%
  group_by(Species,Latitude_decimal_degrees, Longitude_decimal_degrees,Avg_NPP_kg_C_m2_y)%>%
  tally()%>%
  dplyr::filter(n>1)%>% ## drop studies with non equal values
  glimpse()
## check now those studies with n greater than 1

# great they are all legit!

## ... Check extreme values ----

extreme.value.check <- NPP.dataset %>%
  dplyr::filter(!is.na(Avg_NPP_kg_C_m2_y))%>% 
  # dplyr::group_by(Species,Latitude_decimal_degrees, Longitude_decimal_degrees,) %>% 
  dplyr::summarize(quants = quantile(Avg_NPP_kg_C_m2_y, probs = c(0.1, 0.5, 0.9)))%>% #calculate 5, 50 and 95% quartiles)
  #dplyr::mutate(quants=quants*1000)%>% ## multiply by 1000 to make it more readable
  glimpse()

#quantiles are below 0.06 g and 1.1 kg

#### DATABASE NUMBERS ######

### ...# studies ----

no.studies.trusted<-NPP.dataset%>%
  dplyr::group_by(Reference)%>%
  tally()%>%glimpse()  # we have 229 studies


# number of sites and count how many obs they have
### ...# Sites ----

Site<-NPP.dataset%>%
  dplyr::group_by(Site,Latitude_decimal_degrees,Longitude_decimal_degrees)%>%
  
  tally() %>%glimpse()# we have 419 sites

#number of studies looking at multispecies 
### ...# Multispecies ----
multispecies<-NPP.dataset%>%
  dplyr::group_by(Multispecies)%>%
  tally()%>%glimpse()  # we have 580 entries out of 624 looking at single species 



#number of species/taxonomic entities (number of rows) and count how many obs they have


### ...# Species ----
species.trusted<-NPP.dataset%>%
  dplyr::group_by(Species)%>%
  tally()%>%glimpse()  # we have 242 rows with species 

### ...# Species per phyla ----
species.phyla<-NPP.dataset%>%
  dplyr::group_by(Species,Phyla)%>%
  tally()%>%
  dplyr::group_by(Phyla)%>%
  tally()%>%glimpse() 

species.phyla.graph=ggplot(data=species.phyla%>% dplyr::filter(!Phyla %in% c("")), 
                           aes(y=n, x=forcats::fct_reorder(Phyla, n, .fun=mean, .desc=F), fill=Phyla))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.y =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Number of species")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  #  scale_x_continuous(breaks=seq(0,175,25))+
  coord_flip()+
  xlab("")
species.phyla.graph

### ...# Family ----
family.trusted<-NPP.dataset%>%
  dplyr::group_by(Family)%>%
  tally()%>%glimpse()  # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species

## FIGURE 2 ----
### ...# Entries by order  ----
entries.per.order<-NPP.dataset%>%
  dplyr::group_by(Phyla,Order)%>%
  tally()%>%  glimpse() # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species



entries.per.order.graph=ggplot(data=entries.per.order%>% dplyr::filter(!Order %in% c("")), 
                               aes(y=n, x=forcats::fct_reorder(Order, n, .fun=mean, .desc=F), fill=Phyla))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.y =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Number of entries")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  coord_flip()+
  xlab("")
entries.per.order.graph

ggpubr::ggarrange(species.phyla.graph,entries.per.order.graph,common.legend = T, legend="bottom")


#number of phyla entities (number of rows) and count how many obs they have
### ...# Phyla ----
taxon<-NPP.dataset%>%
  dplyr::group_by(Phyla)%>%
  tally()%>%glimpse()  # we have 211 rows with species and most rows (1-193) have between 1-5 obs per species

taxon.freq<-taxon%>%
  dplyr::mutate(rel.freq=n/1261)%>%
  glimpse()

phyla.graph=ggplot(data=taxon.freq%>% dplyr::filter(!Phyla %in% c("Yellow")), 
                   aes(y=rel.freq, x=forcats::fct_reorder(Phyla, rel.freq, .fun=mean, .desc=T), fill=Phyla))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Number of species")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  #  scale_x_continuous(breaks=seq(0,175,25))+
  # coord_flip()+
  scale_y_continuous(labels = scales::percent)+
  xlab("")
phyla.graph



#number of phyla entities (number of rows) by depth

### ...# Vegetation category ----
vegetation.category<-NPP.dataset%>%
  dplyr::group_by(Vegetation_category,Reference)%>%
  tally()%>%
  dplyr::group_by(Vegetation_category)%>%
   tally()%>%
  glimpse() 

vegetation.category.freq<-vegetation.category%>%
  dplyr::mutate(rel.freq=n/251)%>%
  glimpse()

vegetation.graph=ggplot(data=vegetation.category.freq, 
                   aes(y=n, x=forcats::fct_reorder(Vegetation_category, n, .fun=mean, .desc=T), fill=Vegetation_category))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        #axis.text = element_text(size=13),
        #axis.title = element_text(size=14),
        legend.position= "none")+
  ylab("Number of studies")+
#  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=function(x) sub("&","&\n",x,fixed=TRUE))+ ## insert split after &
  xlab("")
vegetation.graph

### ...# Substrate category ----
Substrate<-NPP.dataset%>%
  dplyr::group_by(Substrate_category,Reference)%>%
  tally()%>%
  dplyr::group_by(Substrate_category)%>%
  tally()%>%
  glimpse() 


Substrate.category.freq<-Substrate%>%
  dplyr::mutate(rel.freq=n/240)%>%
  glimpse()

substrate.graph=ggplot(data=Substrate.category.freq, 
                        aes(y=n, x=forcats::fct_reorder(Substrate_category, n, .fun=mean, .desc=T), fill=Substrate_category))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        #axis.text = element_text(size=13),
        #axis.title = element_text(size=14),
        legend.position= "none")+
  ylab("Number of studies")+
  #  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=function(x) sub("&","&\n",x,fixed=TRUE))+ ## insert split after &
  xlab("")
substrate.graph


### ...# Mining method ----
prod.method.trusted<-  NPP.dataset%>%
  dplyr::group_by(Data_mining_method)%>%
  tally() %>%
  glimpse() #




### ...# Method ----
prod.method.trusted<-  NPP.dataset%>%
  dplyr::group_by(Prod_method_general)%>%
  tally() %>%
  glimpse() # we have 529 method obs, of which 91 and 407 are PR and BA respectively

### ...# Method.group ----
prod.method.group.trusted<-  NPP.dataset%>%
  dplyr::group_by(Prod_method_group,Reference)%>%
  tally()%>%
  dplyr::group_by(Prod_method_group)%>%
  tally()%>%
  glimpse()


prod.method.group.freq<-prod.method.group.trusted%>%
  dplyr::mutate(rel.freq=n/235)%>%
  glimpse()

prod.method.group.freq.graph=ggplot(data=prod.method.group.freq, 
                   aes(y=n, x=forcats::fct_reorder(Prod_method_group, n, .fun=mean, .desc=T), fill=Prod_method_group))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "none")+
  ylab("Number of studies")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  #  scale_x_continuous(breaks=seq(0,175,25))+
  # coord_flip()+
 # scale_y_continuous(labels = scales::percent)+
  xlab("")
prod.method.group.freq.graph


#number of studies looking at multispecies 
### ... Temporal duration by study ----
prod.duration<-  NPP.dataset%>%
  dplyr::mutate(Duration=End_year-Start_year)%>%
  # dplyr::filter(Duration>=3)%>% activate this if you  wanna know which studies
  dplyr::group_by(Duration, Reference)%>%
  tally() %>% 
 # dplyr::group_by(Duration)%>%
#  tally() %>% 
  glimpse()

prod.duration.freq<-prod.duration%>%
  dplyr::mutate(rel.freq=n/239)%>%
  dplyr::mutate(Duration=as.factor(Duration))%>%
  glimpse()


duration.graph=ggplot(data=prod.duration.freq, 
                   aes(y=n, x=Duration, fill=Duration))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "none",
        axis.text = element_text(size=14),
        axis.title = element_text(size=14))+
  ylab("Study duration")+
  # coord_flip()+
#  scale_y_continuous(labels = scales::percent)+
  xlab("")
duration.graph

### ... Time of study duration by study ----
time.of.study<-  NPP.dataset%>%
  # dplyr::filter(Duration>=3)%>% activate this if you  wanna know which studies
  dplyr::group_by(Seasons, Reference)%>%
  tally() %>% 
  dplyr::group_by(Seasons)%>%
  tally() %>% 
  glimpse()



### ... Temporal resolution ----

prod.resolution<-  NPP.dataset%>%
  dplyr::mutate(Duration=End_year-Start_year)%>%
  # dplyr::filter(Duration>=3)%>% activate this if you  wanna know which studies
  dplyr::group_by(Ann_sampling_freq)%>%
  tally() %>% 
  glimpse()






prod.resolution.groups<- NPP.dataset%>% 
  dplyr::mutate(Ann_sampling_freq=as.factor(Ann_sampling_freq))%>%
 
  glimpse()


prod.resolution.groups$Ann_sampling_freq <-  factor(prod.resolution.groups$Ann_sampling_freq,
                                                    levels = c("1", "2", "3", "4", "5", "6","7","8","9","10","11","12","24"))

## plot first to see all the genuses to se which have most data

ggplot(data=prod.resolution.groups)+
  geom_jitter(aes(x=Ann_sampling_freq, y=Avg_NPP_kg_C_m2_y), width=0.2, alpha=0.5,size=3)+
  facet_wrap(~Genus) +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("NPP")


ggplot(data=prod.resolution.groups%>%dplyr::filter(Genus %in% c("Turf assemblage", "Laminaria", "Ecklonia", "Ascophyllum","Fucus","Saccharina","Sargassum")))+
  geom_boxplot(aes(x=Ann_sampling_freq, y=Avg_NPP_kg_C_m2_y))+
  facet_wrap(~Genus, scales = "free") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom",
        axis.text= element_text(size=14),
        strip.text = element_text(size=14),
        axis.title= element_text(size=18))+
  xlab("Annual sampling frequency")+
  ylab("NPP (kg C m-2 y-1)")

### ...Entries by decade----
## plot number of entries by decade
Productivity.decade <- NPP.dataset%>% 
  filter(!is.na(Start_year))%>%
  droplevels()%>%
  mutate(decade = floor(Start_year/10)*10) %>% 
  group_by(decade)%>%
  tally() %>% 
  glimpse()

### ...Entries by year----
Productivity.year <- NPP.dataset%>% 
  filter(!is.na(Start_year))%>%
  droplevels()%>%
  group_by(Start_year)%>%
  tally() %>% 
  # ungroup() %>% 
  glimpse()


entries.graph=ggplot(data=Productivity.year, aes(y=n, x=Start_year))+
  geom_bar( stat="identity") +
  # geom_vline(xintercept=1973,  size=0.5)+
  #geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  #  annotate(geom="text", x=1970, y=50, label="Mann 1973",           color="red")+
  #  annotate(geom="text", x=1986, y=50, label="Mann 1982",           color="red")+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Database entries")+
  scale_x_continuous(breaks=seq(1965,2020,5))+
  xlab("")
entries.graph


Productivity.year.Phyla <- NPP.dataset%>% 
  filter(!is.na(Start_year))%>%
  droplevels()%>%
  group_by(Vegetation_category,Start_year)%>%
  tally() %>% 
   glimpse()


Productivity.year.Phyla$Vegetation_category <- factor(Productivity.year.Phyla$Vegetation_category, levels = c(
  "Rhodolith beds & Coralline algae", 
  "Floating Sargassum",
  "Free floating algae",
  "Brown algal beds",
  "Green algal beds",
  "Red algal beds & algal turfs",
  "Marine forest" 
  
))

## FIGURE 1 ----

entries.graph.Phyla=ggplot(data=Productivity.year.Phyla, aes(y=n, x=Start_year, fill=Vegetation_category))+
  geom_bar( stat="identity", position="stack") +
  # geom_vline(xintercept=1973,  size=0.5)+
  #geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  #  annotate(geom="text", x=1970, y=50, label="Mann 1973",           color="red")+
  #  annotate(geom="text", x=1986, y=50, label="Mann 1982",           color="red")+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "none")+
  ylab("Database entries")+
  scale_x_continuous(breaks=seq(1965,2020,5))+
  xlab("")
entries.graph.Phyla


### ...Studies by year----
Publication.year<- NPP.dataset%>%
  filter(!is.na(Start_year))%>%
  group_by(Reference)%>% 
  dplyr::summarize(average=mean(Avg_NPP_kg_C_m2_y))%>% 
  dplyr::mutate(Pub.year=stringr::str_extract(Reference, "[[:digit:]]+"))%>% ## extract all numbers 
  group_by(Pub.year)%>%
  tally() %>% 
  filter(!is.na(Pub.year))%>% 
  filter(Pub.year >1901)%>%
  ungroup()%>%
  mutate(Pub.year=as.integer(Pub.year))%>% 
  glimpse() 



pubs.graph=ggplot(data=Publication.year, aes(y=n, x=Pub.year))+
  geom_bar( stat="identity") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Number of publications")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  scale_x_continuous(breaks=seq(1965,2020,5))+
  scale_y_continuous(breaks=seq(0,10,2))+
  xlab("")
pubs.graph


Publication.year.Phyla<- NPP.dataset%>%
  filter(!is.na(Start_year))%>%
  group_by(Vegetation_category,Reference)%>% 
  dplyr::summarize(average=mean(Avg_NPP_kg_C_m2_y))%>% 
  dplyr::mutate(Pub.year=stringr::str_extract(Reference, "[[:digit:]]+"))%>% ## extract all numbers 
  group_by(Vegetation_category,Pub.year)%>%
  tally() %>% 
  filter(!is.na(Pub.year))%>% 
  filter(Pub.year >1901)%>%
  ungroup()%>%
  mutate(Pub.year=as.integer(Pub.year))%>% 
   glimpse()



Publication.year.Phyla$Vegetation_category <- factor(Publication.year.Phyla$Vegetation_category, levels = c(
  "Rhodolith beds & Coralline algae", 
  "Floating Sargassum",
  "Free floating algae",
  "Brown algal beds",
  "Green algal beds",
  "Red algal beds & algal turfs",
  "Marine forest" 
  
))

pubs.graph.Phyla=ggplot(data=Publication.year.Phyla, aes(y=n, x=Pub.year, fill=Vegetation_category))+
  geom_bar( stat="identity",position="stack") +
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.x =  element_blank(),
        plot.background = element_blank(),
        legend.position= "none")+
  ylab("Number of publications")+
  # geom_vline(xintercept=1972,  size=0.5)+
  #  geom_vline(xintercept=1982, linePhyla="dashed", size=0.5)+
  # annotate(geom="text", x=1975, y=7, label="Mann 1973", color="red")+
  #  annotate(geom="text", x=1979, y=6, label="Mann 1982", color="red")+
  scale_x_continuous(breaks=seq(1965,2020,5))+
  scale_y_continuous(breaks=seq(0,14,2))+
  xlab("")
pubs.graph.Phyla

library(ggpubr)
ggarrange(pubs.graph,entries.graph)
ggarrange(pubs.graph.Phyla,entries.graph.Phyla)
ggarrange(entries.graph,entries.graph.Phyla,pubs.graph,pubs.graph.Phyla)



Publication.year.after.mann<- Publication.year%>%
  filter(Pub.year >1972)%>%
  summarize(sum=sum(n))%>% 
  glimpse()

entries.year.after.mann<- Productivity.year%>%
  #  filter(Start_year>1972)%>%
  summarize(sum=sum(n))%>% 
  glimpse()


library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(ggpubr)
library(gridExtra)
library(grid)
####### when,what,where ----


lay <- rbind(c(1,2),
             c(3))

#where
grid.arrange(substrate.graph + theme(axis.text = element_text(size=13),axis.title = element_text(size=14),legend.position="none"), 
             vegetation.graph + theme(axis.text = element_text(size=13),axis.title = element_text(size=14),legend.position="none"),
             prod.method.group.freq.graph+ theme(axis.text = element_text(size=13),axis.title = element_text(size=14),legend.position="none",plot.title = element_text(size = 20))+ggtitle("How"),
             layout_matrix=lay,
             top = textGrob("Where",gp=gpar(fontsize=20))) 
#when
grid.arrange(duration.graph + theme(axis.text = element_text(size=13),axis.title = element_text(size=14),legend.position="none"), 
             vegetation.graph + theme(axis.text = element_text(size=13),axis.title = element_text(size=14),legend.position="none"),
             layout_matrix=lay,
             top = textGrob("When",gp=gpar(fontsize=20))) 





#FIGURE 3 -----
Macroalgae.sites <- dplyr::select(NPP.dataset, 
                                  Latitude_decimal_degrees, 
                                  Longitude_decimal_degrees, 
                                  Vegetation_category,
                                  Level,
                                  Avg_NPP_kg_C_m2_y, 
                                  Phyla, 
                                  Order,
                                  Genus, 
                                  Reference)%>% glimpse()

## plot of sites

## we are gonna make a bubble plot where the number of obs within an area is summed to make a bubble bigger or smaller

Macroalgae.sites <- filter(Macroalgae.sites, !is.na(Latitude_decimal_degrees)) %>% 
  mutate(lat2 = round(Latitude_decimal_degrees,0), # new coloumn with the latitude rounded to 1ºlatitude
         long2 = round(Longitude_decimal_degrees,0), 
         value = 1)  %>%# a dummy column we will use to sum 
  glimpse()

Macroalgae.sites.reference.count <- filter(Macroalgae.sites, !is.na(Latitude_decimal_degrees)) %>% 
  mutate(lat2 = round(Latitude_decimal_degrees,0), # new coloumn with the latitude rounded to 1ºlatitude
         long2 = round(Longitude_decimal_degrees,0)) %>% 
  group_by(lat2, long2,  Vegetation_category, Reference) %>% 
  tally()%>%
  dplyr::mutate(value=1) %>% 
  #group_by(lat2, long2,  Habitat, Reference) %>% 
  dplyr::summarise(number.studies = sum(value)) %>% 
  glimpse()


#create new dataframe counting the number of obs by each Phyla of habitat
Macroalgae.count <- Macroalgae.sites %>% group_by(lat2, long2,  Vegetation_category, Reference) %>% 
  dplyr::summarise(number.entries = sum(value)) %>% glimpse()
#join that data to the main dataframe
Macroalgae.sites<-   dplyr::left_join(Macroalgae.sites, Macroalgae.count)%>% 
 glimpse()


Macroalgae.sites.reference <-   dplyr::left_join(Macroalgae.sites, Macroalgae.sites.reference.count)%>%
   glimpse()


# plot by entries

ggplot() + 
  geom_hline(yintercept= c(-20.27,20.27),  colour="grey")+ ## insert tropics 
  geom_hline(yintercept= c(66.33,-66.37), linePhyla ="dashed", colour="grey")+ # insert  polar circle
   geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees, size=number.entries, colour=Vegetation_category)) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_size_continuous(range = c(2,5))+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")


# ... NPP by vegetation category ----
NPP.dataset.means <-NPP.dataset%>%
  dplyr::group_by(Vegetation_category)%>%
  dplyr::filter(!is.na(Avg_NPP_kg_C_m2_y))%>%
  dplyr::summarize(mean=mean(Avg_NPP_kg_C_m2_y),
                   sd=sd(Avg_NPP_kg_C_m2_y),
                   n=length(Avg_NPP_kg_C_m2_y),
                   se=sd/sqrt(n),
                   median=quantile(Avg_NPP_kg_C_m2_y, prob=0.5),
                   q25=quantile(Avg_NPP_kg_C_m2_y, prob=0.25),
                   q75=quantile(Avg_NPP_kg_C_m2_y, prob=0.75))%>%
  glimpse()


ggplot(data = NPP.dataset, aes(x = fct_reorder(Vegetation_category,Avg_NPP_kg_C_m2_y,
                                                   .fun = mean, .desc= F), 
                                   y = Avg_NPP_kg_C_m2_y, colour=Vegetation_category),
)+
  # geom_violin(trim=T,  colour= NA, scale= "width", aes(fill=Habitat_area_estimates),alpha=0.65)+
  geom_jitter(position=position_jitter(0.25),size=4.5, alpha=0.3)+
  geom_errorbar(data=NPP.dataset.means, aes(x=Vegetation_category, y=mean, 
                                                   ymax=mean+sd, ymin=mean-sd),
                position=position_dodge(0.05), width=0.0, colour="black")+
  geom_point(data=NPP.dataset.means, aes(x=Vegetation_category, y=mean), size=5.5, colour="black")+
  geom_point(data=NPP.dataset.means, aes(x=Vegetation_category, y=mean), size=5)+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major.y = element_blank(),#remove major-grid labels on x axis
        plot.background = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18),
        legend.position = "none")+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  coord_flip()+
  xlab("")

  

# ... Number of studies ----
#FIGURE 3A ----
ggplot() + 
  geom_hline(yintercept= c(-20.27,20.27),  colour="grey")+ ## insert tropics 
  geom_hline(yintercept= c(66.33,-66.37), linePhyla ="dashed", colour="grey")+ # insert  polar circle
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference, aes(x=long2, y=lat2, size=number.studies, colour=Vegetation_category)) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_size_continuous(range = c(3,9))+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.text = element_text(size=14),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")

#FIGURE 3B ----
## ... Sites by depth ----

ggplot() + 
  geom_hline(yintercept= c(-20.27,20.27),  colour="grey")+ ## insert tropics 
  geom_hline(yintercept= c(66.33,-66.37), linePhyla ="dashed", colour="grey")+ # insert  polar circle
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=NPP.dataset #%>% dplyr::filter(Depth_min_m < 15)
             ,
             aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,  colour=Depth_min_m), size=4,alpha=0.7) +
  # scale_colour_gradientn(colours=c("#5E85B8","#EDF0C0","#C13127"))+
  scale_colour_gradientn(colours=rainbow(7), limits=c(0,55))+
  #facet_wrap(~Habitat)+
  scale_size_continuous(range = c(2,5))+
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")


#FIGURE 5 ----
## ... Sites by method ----

ggplot() + 
  geom_hline(yintercept= c(-20.27,20.27),  colour="grey")+ ## insert tropics 
  geom_hline(yintercept= c(66.33,-66.37), linePhyla ="dashed", colour="grey")+ # insert  polar circle
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=NPP.dataset %>% dplyr::filter(Prod_method_general %in% c("BA", "PR"))
             ,
             aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,  colour=Prod_method_general), size=4,alpha=1) +
  facet_wrap(~Prod_method_general, ncol=1)+
  # scale_colour_gradientn(colours=c("#5E85B8","#EDF0C0","#C13127"))+
  # scale_colour_gradientn(colours=rainbow(7), limits=c(0,55))+
  #facet_wrap(~Habitat)+
  scale_size_continuous(range = c(2,5))+
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")



## ... Productivity in space ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="light grey", linePhyla="dashed", )+
  coord_equal() +
  # geom_point(data=Macroalgae.sites.reference, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,  colour=Avg_NPP_kg_C_m2_y, size=number.studies),  alpha=0.5) +
  geom_point(data=Macroalgae.sites, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,  colour=Avg_NPP_kg_C_m2_y), size=3.5,colour="black", alpha=1) +
  geom_point(data=Macroalgae.sites, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,  colour=Avg_NPP_kg_C_m2_y), size=3, alpha=1) +
  
  # scale_colour_gradientn(colours=rev(rainbow(4)), limits=c(0,5))+
  # scale_colour_gradientn(colours=c("#5E85B8","#D7E0A6", "#E4E9BA", "#EDF0C8", "#E4CAA6","#DDAB8B","#C13127"), limits=c(0,5))+
  #  scale_colour_gradientn(colours=c("#5E85B8","#EDF0C0","#C13127"))+
  # scale_colour_gradientn(colours=c("#4F60AC", "#22FF41", "#FAED23", "#FC4E07"), limits=c(0,5))+
  scale_colour_gradientn(colours=c("#00A8FC", "#00E510", "#FAED23","#F5833A","#D12027"), limits=c(0,5))+
  
  # scale_colour_gradientn(colours=c("#00AFBB", "#22FF41", "#E7B800", "#FC4E07"),limits=c(0,5))+
  scale_size_continuous(range = c(2,5))+
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")



## ... Productivity in space by habitat ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites,
             aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees, size=count, colour=Avg_NPP_kg_C_m2_y),alpha=0.5) +
  facet_wrap(~Vegetation_category)+
  scale_colour_gradientn(colours=rainbow(7), limits=c(0,5))+
  scale_size_continuous(range = c(2,5))+
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")


# ... Histogram of productivity ----
ggplot(data = NPP.dataset, aes(Avg_NPP_kg_C_m2_y))+
  geom_histogram(binwidth = 0.1)


# ... Histogram of productivity ----
ggplot(data = NPP.dataset, aes(sqrt(Avg_NPP_kg_C_m2_y)))+
  geom_histogram(binwidth = 0.1)

#... productivity by species ----
ggplot(data = NPP.dataset, aes(x = fct_reorder(Species,Avg_NPP_kg_C_m2_y,.fun = mean), y = Avg_NPP_kg_C_m2_y))+
  geom_point(alpha=0.5, aes(colour=Prod_method_general))+
  theme_bw()+
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))

#... productivity by genus ----


Genus.summary<-Rmisc::summarySE(NPP.dataset, 
                                measurevar="Avg_NPP_kg_C_m2_y", 
                                groupvars=c("Genus", 
                                            "Phyla"),
                                na.rm = TRUE)
Genus.summary


ggplot(data = NPP.dataset %>%dplyr::filter(Phyla %in% c("Brown")), 
       aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,.fun = mean), y = Avg_NPP_kg_C_m2_y))+
  geom_jitter(width= 0.2, alpha=0.5, aes(colour=Prod_method_general), size=2)+
  geom_errorbar(data = Genus.summary%>% dplyr::filter(Phyla %in% c("Brown")), aes(x = Genus, #add error bars, overriding the colour with black (hence the fill instead of colour in aes)
                                                                                 y=Avg_NPP_kg_C_m2_y, 
                                                                                 ymin=Avg_NPP_kg_C_m2_y-sd, ymax=Avg_NPP_kg_C_m2_y+sd),color = "black",width=.2, size=0.75)+
  geom_point(data = Genus.summary  %>% dplyr::filter(Phyla %in% c("Brown")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3.5, colour="black")+
  geom_point(data = Genus.summary  %>% dplyr::filter(Phyla %in% c("Brown")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3, colour="brown")+
  theme_bw()+
  facet_wrap(~Phyla, scales="free_x")+
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),  #remove minor-grid labels
        panel.grid.minor.y= element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size=14),
        axis.text.y = element_text( size=14),
        legend.position="bottom")+
  ylim(-0.25,5.5)+
  xlab("")+
  ylab("NPP (kg C m-2 y-1)")


ggplot(data = NPP.dataset %>%dplyr::filter(Phyla %in% c("Red")), 
       aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,.fun = mean), y = Avg_NPP_kg_C_m2_y))+
  geom_jitter(width= 0.2, alpha=0.5, aes(colour=Prod_method_general), size=2)+
  geom_errorbar(data = Genus.summary%>% dplyr::filter(Phyla %in% c("Red")), aes(x = Genus, #add error bars, overriding the colour with black (hence the fill instead of colour in aes)
                                                                               y=Avg_NPP_kg_C_m2_y, 
                                                                               ymin=Avg_NPP_kg_C_m2_y-sd, ymax=Avg_NPP_kg_C_m2_y+sd),color = "black",width=.2, size=0.75)+
  geom_point(data = Genus.summary  %>% dplyr::filter(Phyla %in% c("Red")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3.5, colour="black")+
  geom_point(data = Genus.summary  %>% dplyr::filter(Phyla %in% c("Red")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3, colour="red")+
  
  theme_bw()+
  facet_wrap(~Phyla, scales="free_x")+
  theme(panel.background = element_blank(), 
        panel.grid.major.x = element_blank(),  #remove minor-grid labels
        panel.grid.minor.y= element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size=14),
        axis.text.y = element_text( size=14),
        legend.position="bottom")+
  ylim(0,5)+
  xlab("")+
  ylab("NPP (kg C m-2 y-1)")

ggplot(data = NPP.dataset %>%dplyr::filter(Phyla %in% c("Red", "Brown"),
                                                  !Genus %in% c("Red algae","Understorey algae","Himanthalia, Bifurcaria", 
                                                                "Pterygophora, Laminaria,  Chondracanthus, Rhodymenia",
                                                                "Ballia, Callophyllis, Plocamium","Plocamium, Phacelocarpus")), 
       aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,.fun = mean), y = Avg_NPP_kg_C_m2_y))+
  geom_jitter(width= 0.2, alpha=0.5, aes(colour=Prod_method_general), size=2.5)+
  geom_errorbar(data = Genus.summary %>%dplyr::filter(Phyla %in% c("Red", "Brown"),!Genus %in% c("Red algae","Understorey algae","Himanthalia, Bifurcaria", 
                                                                                                "Pterygophora, Laminaria,  Chondracanthus, Rhodymenia",
                                                                                                "Ballia, Callophyllis, Plocamium","Plocamium, Phacelocarpus")), aes(x = Genus, #add error bars, overriding the colour with black (hence the fill instead of colour in aes)
                                                                                                                                                                    y=Avg_NPP_kg_C_m2_y, 
                                                                                                                                                                    ymin=Avg_NPP_kg_C_m2_y-sd, ymax=Avg_NPP_kg_C_m2_y+sd),color = "black",width=.2, size=0.75)+
  geom_point(data = Genus.summary   %>%dplyr::filter(Phyla %in% c("Red", "Brown"),  !Genus %in% c("Red algae","Understorey algae","Himanthalia, Bifurcaria", 
                                                                                                 "Pterygophora, Laminaria,  Chondracanthus, Rhodymenia",
                                                                                                 "Ballia, Callophyllis, Plocamium","Plocamium, Phacelocarpus")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3.5, colour="black")+
  geom_point(data = Genus.summary   %>%dplyr::filter(Phyla %in% c("Red", "Brown"), !Genus %in% c("Red algae","Understorey algae","Himanthalia, Bifurcaria", 
                                                                                                "Pterygophora, Laminaria,  Chondracanthus, Rhodymenia",
                                                                                                "Ballia, Callophyllis, Plocamium","Plocamium, Phacelocarpus")), aes(x = Genus, y=Avg_NPP_kg_C_m2_y), size=3, colour="red")+
  
  theme_bw()+
  facet_wrap(~Phyla, scales="free_y")+
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_blank(),  #remove minor-grid labels
        panel.grid.minor.y= element_blank(),
        plot.background = element_blank(),
        axis.text.x = element_text( size=10),
        axis.text.y = element_text( size=12),
        legend.position="bottom")+
  ylim(-0.25,5.5)+
  coord_flip()+
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))+
  xlab("")+
  ylab("NPP (kg C m-2 y-1)")

# ....productivity Sargassum ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference%>% dplyr::filter(Genus %in% c("Sargassum")), aes(x=long2, y=lat2, size=number.studies, colour=Avg_NPP_kg_C_m2_y), alpha=0.3) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_colour_gradientn(colours=rainbow(6),limits=c(0,5))+
  scale_size_continuous(limits=c(1,8),range = c(2,8) )+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size=20),
        plot.title.position = "panel")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")+
  ggtitle("Sargassum")

# ....productivity Laminaria ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference%>% dplyr::filter(Genus %in% c("Laminaria")), aes(x=long2, y=lat2, size=number.studies, colour=Avg_NPP_kg_C_m2_y),alpha=0.3) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_colour_gradientn(colours=rainbow(6),limits=c(0,5))+
  scale_size_continuous(limits=c(1,8),range = c(2,8) )+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size=20),
        plot.title.position = "panel")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")+
  ggtitle("Laminaria")

# ....productivity Laminariales ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference%>% dplyr::filter(Order %in% c("Laminariales")), aes(x=long2, y=lat2, size=number.studies, colour=Avg_NPP_kg_C_m2_y),alpha=0.35) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_colour_gradientn(colours=rainbow(6),limits=c(0,5))+
  scale_size_continuous(limits=c(1,8),range = c(2,8) )+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size=20),
        plot.title.position = "panel")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")+
  ggtitle("Laminariales")


# ....productivity Fucales ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference%>% dplyr::filter(Order %in% c("Fucales")), aes(x=long2, y=lat2, size=number.studies, colour=Avg_NPP_kg_C_m2_y),alpha=0.35) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_colour_gradientn(colours=rainbow(6),limits=c(0,5))+
  scale_size_continuous(limits=c(1,8),range = c(2,8) )+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size=20),
        plot.title.position = "panel")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")+
  ggtitle("Fucales")


# ....productivity Gelidiales & Gracilariales ----
ggplot() + 
  geom_polygon(data = World.land, aes(x=long, y=lat, group=group), fill="grey")+
  coord_equal() +
  geom_point(data=Macroalgae.sites.reference%>% dplyr::filter(Order %in% c("Gelidiales","Gracilariales")), aes(x=long2, y=lat2, size=number.studies, colour=Avg_NPP_kg_C_m2_y),alpha=0.35) +
  #geom_text(data=NPP.dataset, aes(x=Longitude_decimal_degrees, y=Latitude_decimal_degrees,label=Reference),hjust=0.5, vjust=0.5)+
  scale_colour_gradientn(colours=rainbow(6),limits=c(0,5))+
  scale_size_continuous(range = c(2,6))+ #set min and max values for count
  scale_x_continuous(breaks=seq(-180,180,45))+
  scale_y_continuous(breaks=seq(-90,90,30))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        panel.grid.major = element_blank(),
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Latitude (º)")+
  xlab("Longitude (º)")


#productivity by algae group
ggplot(data = NPP.dataset, aes(x = Phyla, y = Avg_NPP_kg_C_m2_y,fill=Phyla) )+
  geom_violin(trim=T, colour = NA)+
  geom_jitter(position=position_jitter(0.1), size=0.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", colour="red", size=2,  width=0.5)



#productivity by algae habitat
ggplot(data = NPP.dataset.site, aes(x = Habitat, 
                                   y = mean.Avg_NPP_kg_C_m2_y, colour=Habitat))+
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", colour="red", size=2,  width=0.5)




ggplot(data = NPP.dataset.site, aes(x = fct_reorder(Habitat,mean.Avg_NPP_kg_C_m2_y,
                                                   .fun = mean, .desc= T),
                                   y = mean.Avg_NPP_kg_C_m2_y,
                                   colour=Habitat))+
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", colour="red", size=2,  width=0.5)

#### histograms of producitivyt
hist(NPP.dataset$Avg_NPP_kg_C_m2_y)

ggplot(data=NPP.dataset, aes(x=Avg_NPP_kg_C_m2_y)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.05) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  ylab("Relative Frequency")+
  xlab("Average NPP (kg C · m-2 · y-1)")+
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())
############### dotplot by site

ggplot(NPP.dataset%>%filter(Avg_NPP_kg_C_m2_y >0), #<
       aes(x=fct_reorder(Site,Avg_NPP_kg_C_m2_y,
                         .fun = max, .desc= F), 
           y=Avg_NPP_kg_C_m2_y,
           colour=Order)) +
  geom_point() +   # Draw points
  scale_colour_manual(values=c("#878787","#878787","#878787","#878787","#878787","#878787","#878787","#878787",
                               "#878787","#0000F4","#878787","#878787","#878787","#FF0606","#878787","#878787",
                               "#878787","#878787"))+
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") 



ggplot(NPP.dataset%>%filter(Avg_NPP_kg_C_m2_y > 1), 
       aes(x=fct_reorder(Site,Avg_NPP_kg_C_m2_y,
                         .fun = max, .desc= F), 
           y=Avg_NPP_kg_C_m2_y,
           colour=Order)) +
  geom_point() +   # Draw points
  scale_colour_manual(values=c("#0000F4","#FF0606","#878787","#878787","#878787","#878787","#878787","#878787",
                               "#878787","#0000F4","#878787","#878787","#878787","#FF0606","#878787","#878787",
                               "#878787","#878787"))+
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(), 
        legend.position= "bottom")+
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") 

#productivity by latitude
prod.latitude= ggplot(data = NPP.dataset, aes(x = Latitude_decimal_degrees, y = Avg_NPP_kg_C_m2_y, 
                                                     colour=Order))+
  geom_point(inherit.aes = T, size=3)+
  scale_x_continuous(breaks=seq(-80,80,10))+
  scale_y_continuous(breaks=seq(-0,6,1))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("Longitude (º)")
prod.latitude

####FIGURE 4  ----
#productivity by latitude and method
prod.latitude.method= ggplot(data = NPP.dataset%>% filter(Prod_method_general %in% c("BA", "PR")),
                             aes(x = Latitude_decimal_degrees, 
                                 y = Avg_NPP_kg_C_m2_y, 
                                 colour=Prod_method_general))+
  geom_point(inherit.aes = T, size=3, alpha=0.5)+
  #  geom_text( aes(x=Latitude_decimal_degrees, y=Avg_NPP_kg_C_m2_y,label=Reference),hjust=0.5, vjust=0.5)+
  facet_wrap(.~Prod_method_general, ncol=1)+
  scale_x_continuous(breaks=seq(-80,80,10))+
  scale_y_continuous(breaks=seq(-0,6,1))+
  theme_bw()+
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("Longitude (º)")
prod.latitude.method




#productivity by depth
prod.depth= ggplot(data = NPP.dataset, aes(x = Avg_NPP_kg_C_m2_y, y = Depth_max_m, colour=Habitat) )+
  geom_point(inherit.aes = T, size=3)+
  theme_bw()+
  theme(panel.background = element_blank(),
        plot.background = element_blank())+
  ylab("Depth(m)")+
  xlab("Average NPP (kg C · m-2 · y-1)" )
prod.depth

#productivity by depth and method
prod.depth.method=ggplot(data = NPP.dataset2, aes(x = Avg_NPP_kg_C_m2_y, 
                                                         y = Depth_max_m, 
                                                         colour=Prod_method_general) )+
  facet_grid(.~Prod_method_general)+
  geom_point(inherit.aes = T, size=3)+
  theme_bw()+
  theme(panel.background = element_blank(),
        plot.background = element_blank())+
  ylab("Depth (m)")+
  xlab("Average NPP (kg C · m-2 · y-1)" )
prod.depth.method


ggarrange(prod.latitude, prod.depth,   ncol=2, nrow=1, 
          common.legend = T, legend= "bottom")

ggarrange(prod.latitude.method, prod.depth.method,   ncol=2, nrow=1, 
          common.legend = T, legend= "bottom")

#productivity by depth and by Phyla of algae, to show how prod attenuates in different Phylas of algae

#exclude blue and multiple algae, which have very few samples
NPP.dataset.Phyla <- filter(NPP.dataset, !is.na(Avg_NPP_kg_C_m2_y), NPP.dataset$Trusted_data != "N",
                          NPP.dataset$Phyla != "Multiple", NPP.dataset$Phyla != "Blue",NPP.dataset$Phyla != "") 

ggplot(data = NPP.dataset.Phyla, aes(x = Avg_NPP_kg_C_m2_y, y = Depth_max_m, colour=Phyla) )+
  geom_point(inherit.aes = T, size=3)+
  #scale_y_reverse()+
  facet_grid(.~Phyla)+
  theme_bw()+
  #scale_colour_manual(values =c ("#A3A500", "#00BA38", "#F8766D"))+
  theme(    panel.background = element_blank(), 
            #remove major-grid labels
            panel.grid.minor = element_blank(),  #remove minor-grid labels
            plot.background = element_blank(),
            legend.position = "bottom")


## productivity by algae group

#exclude blue and multiple algae, and multispecies assemblages
NPP.dataset.Phyla <- filter(NPP.dataset, !is.na(Avg_NPP_kg_C_m2_y), NPP.dataset$Trusted_data != "N",
                          NPP.dataset$Phyla != "Multiple", NPP.dataset$Phyla != "Blue",
                          NPP.dataset$Multispecies != "YES") 


#produce a column saying how many obs you have
Order.count <- NPP.dataset.Phyla   %>%     dplyr::group_by(Order) %>% 
  tidyr::drop_na(Avg_NPP_kg_C_m2_y) %>% tally()
Order.count <-   dplyr::left_join(NPP.dataset.Phyla, Order.count, by = "Order")#the ones with one observation 




#Plot of NPP by order

prod.order=ggplot(data = Order.count %>% filter(n > 2)%>% filter(Habitat2!= "Aquaculture"), 
                  aes(x = fct_reorder(Order,Avg_NPP_kg_C_m2_y,
                                      .fun = median, .desc=F), 
                      y = Avg_NPP_kg_C_m2_y, 
                      fill=Phyla))+
  scale_colour_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  scale_fill_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  geom_boxplot(outlier.size=0, outlier.alpha = 0)+
  geom_jitter(position=position_jitter(0.05), alpha=0.6)+
  stat_summary(fun=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(-0,6,1))+
  theme_bw()+
  theme(axis.text.x = element_text(size=11),
        #axis.text.y = element_text(size=14),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= c(0.8, 0.6))+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")+
  coord_flip()
prod.order



ggplot(data = NPP.dataset.Phyla, aes(x = fct_reorder(Order,Avg_NPP_kg_C_m2_y,
                                                   .fun = mean, .desc=T), 
                                   y = Avg_NPP_kg_C_m2_y, 
                                   fill=Phyla))+
  geom_violin(trim=T,  colour= NA, scale= "width", aes(fill=Phyla))+
  geom_jitter(position=position_jitter(0.1), size=1)+
  stat_summary(fun.y=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  scale_colour_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  scale_fill_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= c(0.8, 0.6))+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")



#Plot of NPP by family


Family.count <- NPP.dataset.Phyla   %>%     dplyr::group_by(Family) %>% 
  tidyr::drop_na(Avg_NPP_kg_C_m2_y) %>% tally()
Family.count <-   dplyr::left_join(NPP.dataset.Phyla, Family.count, by = "Family")#the ones with one observation 



prod.family= ggplot(data = Family.count %>% filter(n > 2), 
                    aes(x = fct_reorder(Family,Avg_NPP_kg_C_m2_y,
                                        .fun = mean, .desc=T), 
                        y = Avg_NPP_kg_C_m2_y, 
                        fill=Phyla))+
  scale_colour_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  scale_fill_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  geom_boxplot(outlier.size= 0)+
  geom_jitter(position=position_jitter(0.1), size=1)+
  stat_summary(fun=mean, geom="point", colour="red", size=1)+
  stat_summary(fun=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= c(0.8, 0.6))+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")
prod.family


ggplot(data = NPP.dataset.Phyla, aes(x = fct_reorder(Family,Avg_NPP_kg_C_m2_y,
                                                   .fun = mean, .desc=T), 
                                   y = Avg_NPP_kg_C_m2_y, 
                                   fill=Phyla))+
  geom_violin(trim=T,  colour= NA, scale= "width", aes(fill=Phyla))+
  geom_jitter(position=position_jitter(0.1), size=0.4)+
  stat_summary(fun.y=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  scale_colour_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  scale_fill_manual(values =c ("#AA7D0E", "#00BA38", "#F2302C"))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= c(0.8, 0.6))+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")

### Select brown only and look at genuses

NPP.dataset.brown <- filter(NPP.dataset.Phyla, Phyla == "Brown") 

Genus.count <- NPP.dataset.Phyla   %>%     dplyr::group_by(Genus) %>% 
  tidyr::drop_na(Avg_NPP_kg_C_m2_y) %>% tally()
Genus.count <-   dplyr::left_join(NPP.dataset.brown, Genus.count, by = "Genus")#the ones with one observation 



prod.brown= ggplot(data = Genus.count %>% filter(n > 2),
                   aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,
                                       .fun = mean, .desc=T), 
                       y = Avg_NPP_kg_C_m2_y,
                       fill=Phyla))+
  scale_fill_manual(values ="#AA7D0E")+
  geom_boxplot(outlier.size= 0)+
  geom_jitter(position=position_jitter(0.1), size=1)+
  stat_summary(fun=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= "none")+
  
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")
prod.brown



ggplot(data = NPP.dataset.brown, aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,
                                                    .fun = mean, .desc=T), 
                                    y = Avg_NPP_kg_C_m2_y, 
                                    fill=Family))+
  geom_violin(trim=T,  colour= NA, scale= "width", aes(fill=Family),alpha=0.65)+
  geom_jitter(position=position_jitter(0.1), size=0.4)+
  stat_summary(fun.y=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position= "bottom")+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")




### Select laminariales only and look at genuses
NPP.dataset.laminariales<- filter(NPP.dataset, Order == "Laminariales") 


prod.laminariales = ggplot(data = NPP.dataset.laminariales, aes(x = fct_reorder(Genus,Avg_NPP_kg_C_m2_y,
                                                                               .fun = mean, .desc=T), 
                                                               y = Avg_NPP_kg_C_m2_y, 
                                                               fill=Family))+
  geom_boxplot(outlier.size= 0)+
  geom_jitter(position=position_jitter(0.1), size=1)+
  stat_summary(fun.y=mean, geom="point", colour="red", size=1)+
  scale_y_continuous(breaks=seq(0,6,1))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.background = element_blank(),
        panel.grid.major.x = element_blank(),#remove major-grid labels on x axis
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.position = "bottom")+
  ylab("Average NPP (kg C · m-2 · y-1)")+
  xlab("")
prod.laminariales


ggarrange(prod.order, prod.family,   ncol=2, nrow=1, 
          common.legend = T, legend= "none")

ggarrange(prod.brown, prod.laminariales,   ncol=2, nrow=1, 
          common.legend = T, legend= "none")


ggarrange(prod.family, prod.brown,   ncol=2, nrow=1, 
          common.legend = T)


#productivity by method



ggplot(data = NPP.dataset, aes(x = Prod_method_general, 
                                      y = Avg_NPP_kg_C_m2_y, 
                                      colour=Prod_method_general))+
  geom_boxplot()+
  geom_jitter(position=position_jitter(0.35))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", colour="red", size=2,  width=0.5)

ggplot(data = NPP.dataset, aes(x = fct_reorder(Prod_method_general,Avg_NPP_kg_C_m2_y,
                                                      .fun = mean, .desc= T),
                                      y = Avg_NPP_kg_C_m2_y,
                                      colour=Prod_method_general))+
  geom_violin()+
  geom_jitter(position=position_jitter(0.15))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", colour="red", size=2,  width=0.5)


#### Methods comparison

test <- NPP.dataset [!(NPP.dataset$Prod_method == ""), ] %>%
  group_by( Reference, Prod_method) %>%
  tally()

multispecies<-NPP.dataset%>%
  dplyr::group_by(Multispecies)%>%
  tally() 

