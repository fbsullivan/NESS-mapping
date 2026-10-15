library(sf)
library(terra)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(exactextractr)

dem = rast('./rasters/DEM_30m.tif')
slope = terrain(dem,v="slope",unit="degrees",neighbors=8)
aspect = terrain(dem,v="aspect",unit="degrees",neighbors=8)
hillshade <- shade(slope, aspect, angle=45, direction=315)
roi = './shapefiles/NorthernNewEngland.shp'
NE = read_sf(roi)
NE = st_transform(NE,crs=crs(dem))

# need to make some modifications above to produce NESS_Sites file...linking was done in Excel initially!
ness_scan = read.csv("./datascan/snowsites/NOHRSC_ness-sites.csv")
ness_survey = read.csv("./datascan/snowsites/SURVEY_ness-sites.csv")[,c(1:18)] # available on request with permission from survey respondents
cso_data = read.csv("./datascan/snowsites/cso_2021-2025.csv")
all_stations = rbind(ness_scan,ness_survey,cso_data)

nesssf = st_as_sf(all_stations,coords=c("longitude","latitude"),crs=4326)
nesssf = st_transform(nesssf,crs=5070)
nesssf = st_filter(nesssf,NE)
nessvec = terra::extract(dem,nesssf,method="simple",bind=TRUE)

unique(nessvec$network) #20 networks

nessvec$yor = as.numeric(substr(nessvec$end_date,1,4))-as.numeric(substr(nessvec$start_date,1,4))+1
nessvec$peryear = nessvec$n_obs/nessvec$yor
nessvec$active = "yes"
nessvec$active[which(as.numeric(substr(nessvec$end_date,1,4))<2025)] = "no"
nessvec$group = "A"
nessvec$group[which(nessvec$peryear>1&nessvec$peryear<=3)] = "B"
nessvec$group[which(nessvec$peryear>3&nessvec$peryear<=6)] = "C"
nessvec$group[which(nessvec$peryear>6&nessvec$peryear<=12)] = "D"
nessvec$group[which(nessvec$peryear>12)] = "E"

elementct = nessvec %>% group_by(Station_Id) %>% summarize(n=n()) %>% as_tibble()

network_counts = nessvec %>% group_by(element,group) %>% summarize(n=n()) %>% as_tibble()
nessvec$element[match(elementct$Station_Id[which(elementct$n==1)],nessvec$Station_Id)]
pal = colorRampPalette(c("gray","pink","palevioletred","red","darkred"))(5)
f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=nessvec[which(nessvec$element=="snow_depth"&nessvec$isAutomated=="manual")]%>%arrange(group),size=0.85,aes(color=factor(group))) + 
  scale_color_manual(values=pal,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="count per year") + 
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/Figure1a.jpg',plot=f,dpi=300,height=5,width=5,units="in")

f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=nessvec[which(nessvec$element=="snow_depth"&nessvec$isAutomated=="automated"&nessvec$method=="camera")],size=0.85,aes(color="Camera")) + 
  geom_sf(data=nessvec[which(nessvec$element=="snow_depth"&nessvec$isAutomated=="automated"&nessvec$method!="camera")],size=0.85,aes(color="Snow Depth")) + 
  geom_sf(data=nessvec[which(nessvec$element=="swe"&nessvec$isAutomated=="automated")],size=0.85,aes(color="SWE")) +
  labs(color="automated \nstations")+scale_color_manual(values=c("Snow Depth"="red","SWE"="blue","Camera"="gray")) +
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/Figure1c.jpg',plot=f,dpi=300,height=5,width=5,units="in")


pal = colorRampPalette(c("gray","lightblue","cornflowerblue","blue","darkblue"))(5)
f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=nessvec[which(nessvec$element=="swe"&nessvec$isAutomated=="manual")]%>%arrange(group),size=0.85,aes(color=factor(group))) + 
  scale_color_manual(values=pal,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="count per year") + 
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/Figure1b.jpg',plot=f,dpi=300,height=5,width=5,units="in")

pal1 = colorRampPalette(c("gray","pink","red"))(5)
pal2 = colorRampPalette(c("gray","lightblue","blue"))(5)


nessvec$elvBins = cut(nessvec$elevation,breaks=c(seq(0,2000,100)))
nessvec$elvBins = floor(nessvec$elevation/100)*100
elvGroupData = nessvec %>% group_by(element,group,elvBins) %>% summarize(n=n()) %>% as_tibble()

f = ggplot(data=elvGroupData%>%arrange(group),aes(x=elvBins)) +
  geom_bar(aes(y=n,fill=group,group=element),data=~subset(.,element=="snow_depth"),position="stack",stat="identity") +
  scale_fill_manual(values=pal1,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="Snow Depth \nobs. per year") +
  new_scale_fill() +
  geom_bar(aes(y=n*-1,fill=group,group=element),data=~subset(.,element=="swe"),position="stack",stat="identity") +
  scale_fill_manual(values=pal2,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="SWE \nobs. per year") +
  geom_hline(yintercept=0,color="black")+ 
  scale_y_continuous(breaks=seq(-200,800,by=100),
                     labels=c(200,100,0,100,200,300,400,500,600,700,800),limits=c(-275,800))+
  
  labs(x="elevation [m]",y="n stations")+
  scale_x_continuous(breaks = seq(-50,1950,by=300),
                     labels=seq(0,2000,by=300)) +
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.85,0.65),
        legend.background=element_rect(fill="white",color=NA),
        legend.key.size=unit(0.8,"lines"),
        panel.grid = element_blank(),
        panel.border=element_rect(colour="black",fill=NA))+
  coord_flip()
ggsave('./datascan/figures/Figure1d.jpg',plot=f,dpi=300,height=5,width=5,units="in")

  
  
  
  