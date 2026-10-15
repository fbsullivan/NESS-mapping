library(sf)
library(terra)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(exactextractr)
source("./filtering/shapefiles_intersect.r")
source("./filtering/filter_tools.r")

# user inputs
writePlots = FALSE
saveFiltered = TRUE

# additional images
dem = rast('./rasters/DEM_30m.tif')
swe = rast('./rasters/Snow_Water_Days.tif')
d2p = rast('./rasters/PrimaryRoadDistance_30m.tif')
d2b = rast('./rasters/UnpavedRoadDistance_30m.tif')
huc8ws = vect(huc8)
huc8ws = project(huc8ws,"epsg:5070")

# filter parameters
# rasters:
# dem
demmin = 600 #Maine 400
demminME = 400
demmax = 2000
# slope
slopemax = 10
# swe days
swedaysmin = 12927 #75th percentile 18591.82, 50th percentile is 12927

# calculate slope and aspect, id states
slope = terrain(dem,v="slope",unit="degrees",neighbors=8)
aspect = terrain(dem,v="aspect",unit="degrees",neighbors=8)
gc() # free unused memory

# calculate masks
swe[swe<swedaysmin] = NA
if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=swe) + scale_fill_viridis_c(na.value="transparent") + labs(fill="SwS") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure2e_SwS.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}


dem[is.na(stateID)] = NA
dem[dem>demmax | dem<demminME] = NA
dem[dem<demmin & stateID=="Vermont"] = NA
dem[dem<demmin & stateID=="New Hampshire"] = NA
if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=dem) + scale_fill_viridis_c(na.value="transparent",option="cividis") + labs(fill="Elevation (m)") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure2c_elevation.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

gc() # free unused memory

slope[is.na(stateID)] = NA
slope[slope>slopemax] = NA
gc()
if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=slope) + scale_fill_viridis_c(na.value="transparent",option="cividis") + labs(fill="Slope (deg)") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure2d_slope.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

lands[stateID=="New Hampshire" & !(lands=="Federal"|lands=="State")] = NA
lands[stateID=="Vermont" & !(lands=="Federal"|lands=="State")] = NA
lands[wilderness==1] = NA
gc()
if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=lands) + scale_fill_viridis_d(na.value="transparent",na.translate=FALSE,option="cividis") + labs(fill="Status") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure2a_ownership.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

soil[is.na(stateID)] = NA
if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=as.factor(soil)) + scale_fill_viridis_d(na.value="transparent",na.translate=FALSE,labels=c("TRUE")) + labs(fill="Non-Histosols") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
         legend.position.inside=c(0.15,0.8),
         legend.background=element_rect(fill="white"),
         legend.key.size=unit(0.6,"lines"),
         legend.text = element_text(size=rel(0.85)),
         legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure2b_soil.jpg',plot=f,dpi=300,height=5,width=5,units="in")
  
}

# need swe to be aligned with other rasters
swe = resample(swe,dem,method="bilinear")
overlap = soil
combined = c(dem,slope,swe,soil,lands,aspect,watershed,stateID)
names(combined) = c("elevation","slope","sweDays","soil","owner","aspect","watershed","stateID")
overlap[anyNA(combined)] = NA
rm(dem,slope,swe,soil,lands)
gc()

if(writePlots){
  f = ggplot() + 
    geom_spatraster(data=as.factor(overlap)) + 
    scale_fill_manual(values=c("1"="black"),na.value="transparent",na.translate=FALSE,labels=c("TRUE"))  + labs(fill="Meets Criteria") +
    geom_sf(data=NE,fill=NA,color="darkgray") +
    theme_minimal() +
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave(paste0('./filtering/figures/Figure2f_intersection.jpg'),plot=f,dpi=300,height=5,width=5,units="in")
}

# calculating areas:
global(overlap,"notNA")$notNA*900/1000000
global(dem,"notNA")$notNA*900/1000000
global(slope,"notNA")$notNA*900/1000000
global(soil,"notNA")$notNA*900/1000000
global(lands,"notNA")$notNA*900/1000000
vto = overlap
vto[!(stateID=="Vermont")] = NA
nho = overlap
nho[!(stateID=="New Hampshire")] = NA
meo = overlap
meo[!(stateID=="Maine")] = NA
global(vto,"notNA")$notNA*900/1000000
global(nho,"notNA")$notNA*900/1000000
global(meo,"notNA")$notNA*900/1000000

fedo = overlap
fedo[!(lands=="Federal")] = NA
stateo = overlap
stateo[!(lands=="State")] = NA
othero = overlap
othero[!(lands=="Federal"|lands=="State")] = NA
global(fedo,"notNA")$notNA*900/1000000
global(stateo,"notNA")$notNA*900/1000000
global(othero,"notNA")$notNA*900/1000000

fedovt = vto
fedovt[!(lands=="Federal")] = NA
stateovt = vto
stateovt[!(lands=="State")] = NA
global(fedovt,"notNA")$notNA*900/1000000
global(stateovt,"notNA")$notNA*900/1000000


fedonh = nho
fedonh[!(lands=="Federal")] = NA
stateonh = nho
stateonh[!(lands=="State")] = NA
global(fedonh,"notNA")$notNA*900/1000000
global(stateonh,"notNA")$notNA*900/1000000

fedome = meo
fedome[!(lands=="Federal")] = NA
stateome = meo
stateome[!(lands=="State")] = NA
global(fedome,"notNA")$notNA*900/1000000
global(stateome,"notNA")$notNA*900/1000000



# convert to polygon shapefile
# make a covering grid for the shapefile intersection area
sfRast = rast(ext(overlap),res=1000,crs=crs(overlap))
#overlap[is.na(overlap)] = 0
sfRast = resample(overlap,sfRast,method="sum")
sfGrid = as.polygons(sfRast,aggregate=FALSE)

# filter our grid to only include polygons with >1 ha qualifying area
sfGrid$fracCov = sfGrid$layer*900/10000 # calculates percentage
sfGrid = sfGrid %>% filter(fracCov>1)
if(writePlots){
  f = ggplot() + 
    geom_sf(data=NE,fill=NA) +
    geom_sf(data=ness_select,aes(fill=fracCov),color=NA) + 
    scale_fill_viridis_c() +
    theme_minimal()+
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/InspectionGridFractionalCoverage.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

combined[anyNA(combined)] = NA
gc()

# calculate zonal statistics from combined raster
sfGrid$ID = seq(1,nrow(sfGrid))
sfGrid_attributes = exact_extract(subset(combined,c("elevation","sweDays","aspect")),st_as_sf(sfGrid),fun=c('mean','stdev'),append_cols="ID",progress=TRUE)
sfGrid_state = exact_extract(combined$stateID,st_as_sf(sfGrid),fun='majority',append_cols="ID",progress=TRUE)
sfGrid_state$state = levels(stateID)[[1]]$STATE_NAME[match(sfGrid_state$majority,levels(stateID)[[1]]$value)]
sfGrid_ws = exact_extract(combined$watershed,st_as_sf(sfGrid),fun='majority',append_cols="ID",progress=TRUE)
sfGrid_ws$name = levels(watershed)[[1]]$NAME[match(sfGrid_ws$majority,levels(watershed)[[1]]$value)]
sfGrid_lte = exact_extract(lte,st_as_sf(sfGrid),fun='count',append_cols="ID",progress=TRUE)
sfGrid_lte$lte.cov = sfGrid_lte$count*900/10000
sfGrid_pdist = exact_extract(d2p,st_as_sf(sfGrid),fun='min',append_cols="ID",progress=TRUE)
names(sfGrid_pdist)[2] = "primary_dist"
sfGrid_bdist = exact_extract(d2b,st_as_sf(sfGrid),fun='min',append_cols="ID",progress=TRUE)
names(sfGrid_bdist)[2] = "secondary_dist"
sfGrid_clusters = merge(sfGrid_state[,c(1,3)],sfGrid_attributes,by="ID")
sfGrid_clusters = merge(sfGrid_clusters,sfGrid_lte[,c(1,3)],by="ID")
sfGrid_clusters = merge(sfGrid_clusters,sfGrid_ws[,c(1,3)],by="ID")
sfGrid_clusters = merge(sfGrid_clusters,sfGrid_pdist,by="ID")
sfGrid_clusters = merge(sfGrid_clusters,sfGrid_bdist,by="ID")

# state-wise k-means clustering by elevation and sweDays mean, sd
states = split(sfGrid_clusters,sfGrid_clusters$state)
states = lapply(states,FUN = function(x){cbind(x,cluster=match(kmeans(x[c('mean.elevation','stdev.elevation','mean.sweDays')],centers=5)$cluster,
                                                               order(kmeans(x[c('mean.elevation','stdev.elevation','mean.sweDays')],centers=5)$centers[,"mean.elevation"])))})
sfGrid_clusters = unsplit(states,sfGrid_clusters$state)
sfGrid = merge(sfGrid,sfGrid_clusters,by="ID")
if(writePlots){
  f = ggplot() + 
    geom_sf(data=NE,fill=NA) +
    geom_sf(data=sfGrid,aes(fill=as.factor(cluster)),color=NA) + 
    scale_fill_viridis_d(name="K-Means Cluster",labels=c("low SwS, low elev.","mod. SwS, low elev.","mod. SwS, low-mod elev.","mod.-high Sws, mod. elev.","high SwS, high elev.")) +
    theme_minimal()+
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure3a.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

counts = sfGrid_clusters %>% group_by(name,cluster) %>% summarize(total_count=n())
wider = counts %>% pivot_wider(names_from=cluster,values_from=total_count)
names(wider)[2:6] = c("cluster_2","cluster_4","cluster_1","cluster_3","cluster_5")
names(wider)[1] = "NAME"
wider$cluster_count = rowSums(!is.na(wider[,c(2:6)]))
wider$total_count = rowSums(wider[,c(2:6)],na.rm=TRUE)

huc8ws = merge(huc8ws,wider,all.x=TRUE,by="NAME")
huc8ws = terra::intersect(huc8ws,vect(NE))
huc8ws = terra::aggregate(huc8ws,by="NAME")
if(writePlots){
  f = ggplot() + 
    geom_sf(data=NE,fill=NA) +
    geom_sf(data=huc8ws,aes(fill=mean_total_count),color="gray") + 
    scale_fill_viridis_c(name="No. Tiles",na.value="transparent") +
    theme_minimal()+
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/Figure3b.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

sfGrid$minDist = apply(as_tibble(sfGrid[,c(13,14)]),1,FUN=min,na.rm=TRUE)

huc8 = st_as_sf(huc8ws)

# these are calculated in watershed_meta
sfGrid$sweCount = huc8$sweCount[match(sfGrid$name,huc8$NAME)]
sfGrid$depthCount = huc8$depthCount[match(sfGrid$name,huc8$NAME)]
sfGrid$gageCount = huc8$gageCount[match(sfGrid$name,huc8$NAME)]

ness_select = sfGrid %>% slice_max(order_by = fracCov, by=c(name,cluster),with_ties=FALSE)


writeVector(sfGrid,'./shapefiles/InspectionGrid_1km_w-attributes.shp',filetype="ESRI Shapefile",overwrite=TRUE)
writeVector(ness_select,'./shapefiles/NESS_Reselect_1kmGrid_Unscreened.shp',filetype="ESRI Shapefile",overwrite=TRUE)
#writeRaster(overlap,'./rasters/FinalOverlap.t')

ness_select = vect('./shapefiles/NESS_Reselect_1kmGrid_Screened.shp')
if(writePlots){
  f = ggplot() + 
    geom_sf(data=NE,fill=NA) +
    geom_sf(data=ness_select,aes(fill=as.factor(cluster)),color=NA) + 
    scale_fill_viridis_d(name="K-Means Cluster",labels=c("low SwS, low elev.","mod. SwS, low elev.","mod. SwS, low-mod elev.","mod.-high Sws, mod. elev.","high SwS, high elev.")) +
    theme_minimal()+
    theme(legend.position="inside",
          legend.position.inside=c(0.15,0.8),
          legend.background=element_rect(fill="white"),
          legend.key.size=unit(0.6,"lines"),
          legend.text = element_text(size=rel(0.85)),
          legend.title = element_text(size=rel(0.85)))
  ggsave('./filtering/figures/CandidatesMap.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}


# reviewer requested overlap of suitable sites and station data
# we will need station data from sites_analysis_and_figures.r
# we will need 1 km tiles
library(sf)
library(terra)
library(purrr)
library(dplyr)
library(ggplot2)
library(tidyterra)
library(exactextractr)
library(ggnewscale)

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

sfGrid = vect('./shapefiles/InspectionGrid_1km_w-attributes.shp')

spOverlap = extract(sfGrid,nessvec)
inside = which(!is.na(spOverlap[,2]))
Overlap = nessvec[inside]

network_counts = Overlap %>% group_by(element,group) %>% summarize(n=n()) %>% as_tibble()
pal = colorRampPalette(c("gray","pink","palevioletred","red","darkred"))(5)
f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=Overlap[which(Overlap$element=="snow_depth"&Overlap$isAutomated=="manual")]%>%arrange(group),size=0.85,aes(color=factor(group))) + 
  scale_color_manual(values=pal,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="count per year") + 
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/FigureS1a.jpg',plot=f,dpi=300,height=5,width=5,units="in")

pal = colorRampPalette(c("gray","lightblue","cornflowerblue","blue","darkblue"))(5)
f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=Overlap[which(Overlap$element=="swe"&Overlap$isAutomated=="manual")]%>%arrange(group),size=0.85,aes(color=factor(group))) + 
  scale_color_manual(values=pal,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="count per year") + 
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/FigureS1b.jpg',plot=f,dpi=300,height=5,width=5,units="in")

f = ggplot() + 
  geom_sf(data=NE,fill=NA) +
  geom_sf(data=Overlap[which(Overlap$element=="snow_depth"&Overlap$isAutomated=="automated"&Overlap$method=="camera")],size=0.85,aes(color="Camera")) + 
  geom_sf(data=Overlap[which(Overlap$element=="snow_depth"&Overlap$isAutomated=="automated"&Overlap$method!="camera")],size=0.85,aes(color="Snow Depth")) + 
#  geom_sf(data=Overlap[which(Overlap$element=="swe"&Overlap$isAutomated=="automated")],size=0.85,aes(color="SWE")) +
  labs(color="automated \nstations")+scale_color_manual(values=c("Snow Depth"="red","Camera"="gray")) +
  theme_minimal()+
  theme(legend.position="inside",
        legend.position.inside=c(0.15,0.8),
        legend.background=element_rect(fill="white"))+
  guides(color = guide_legend(override.aes = list(size = 1.5)))
ggsave('./datascan/figures/FigureS1c.jpg',plot=f,dpi=300,height=5,width=5,units="in")

pal1 = colorRampPalette(c("gray","pink","red"))(5)
pal2 = colorRampPalette(c("gray","lightblue","blue"))(5)


Overlap$elvBins = cut(Overlap$elevation,breaks=c(seq(0,2000,100)))
Overlap$elvBins = floor(Overlap$elevation/100)*100
elvGroupData = Overlap %>% group_by(element,group,elvBins) %>% summarize(n=n()) %>% as_tibble()

f = ggplot(data=elvGroupData%>%arrange(group),aes(x=elvBins)) +
  geom_bar(aes(y=n,fill=group,group=element),data=~subset(.,element=="snow_depth"),position="stack",stat="identity") +
  scale_fill_manual(values=pal1,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="Snow Depth \nobs. per year") +
  new_scale_fill() +
  geom_bar(aes(y=n*-1,fill=group,group=element),data=~subset(.,element=="swe"),position="stack",stat="identity") +
  scale_fill_manual(values=pal2,labels=c("1",">1\u20133",">3\u20136",">6\u201312",">12"),name="SWE \nobs. per year") +
  geom_hline(yintercept=0,color="black")+ 
  scale_y_continuous(breaks=seq(-50,250,by=25),
                     labels=c(50,25,0,25,50,75,100,125,150,175,200,225,250),limits=c(-50,275))+
  
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
ggsave('./datascan/figures/FigureS1d.jpg',plot=f,dpi=300,height=5,width=5,units="in")
