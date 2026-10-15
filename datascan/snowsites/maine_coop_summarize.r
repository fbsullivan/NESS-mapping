library(curl)
library(dplyr)

#generate list of survey ID, description, lat, lon for data retrieved outside of nohrsc
#maine coop
dat = read.csv('./datascan/snowsites/Maine_Snow_Survey_Data.csv')

dat = subset(dat,SURVEY_YEAR>2020)
write.csv(dat,'./datascan/snowsites/maine_coop_post-2001.csv',row.names=FALSE)

maine_stations = 
  dat %>%
    group_by(SITE_ID) %>%
    filter(SURVEY == max(SURVEY))

maine_stations = merge(maine_stations,count(dat,SITE_ID),by="SITE_ID")

maine_stations = as.data.frame(maine_stations)
names(maine_stations)[1] = "survey_id"

# other sites, retrieved from web sources
maine_stations = maine_stations[-which(maine_stations$SOURCE=="NEW HAMPSHIRE DEPARTMENT OF ENVIRONMENTAL SERVICES"),]
other_stations = read.csv('./datascan/snowsites/other_sites.csv')
maine_stations$ORGANIZATION = "Maine Cooperative Snow Survey"
all_stations = rbind(other_stations,maine_stations[c(1,5,15,22,23,19,20)])

# read nws stations list to link with ness sites to handle redundancies and identify sources
nws_stations = read.csv('./datascan/snowsites/nws_stations_list.csv')
nws_stations = nws_stations[,c(1,3,7,37,38)]
names(nws_stations)[c(2,4,5)]=c("SITE_NAME","LONGITUDE","LATITUDE")
nws_stations$network = lapply(nws_stations$network,function(x){strsplit(x,"_")[[1]][2]})

# convert to shapefile, link by distance to nws station
nws = st_as_sf(nws_stations,coords=c("LONGITUDE","LATITUDE"),crs=4326)
ness = st_as_sf(all_stations,coords=c("LONGITUDE","LATITUDE"),crs=4326)
nws = st_transform(nws,5070)
ness = st_transform(ness,5070)
nearest = st_nearest_feature(ness,nws)
ness$distance = st_distance(ness,nws[nearest,],by_element=TRUE)
ness$Station_Id = nws$Station_Id[nearest]
ness$nws_description = nws$SITE_NAME[nearest]
ness$network = nws$network[nearest]
ness = ness %>% st_drop_geometry()
ness$distance = as.numeric(gsub("[m]","",ness$distance))
ness$SITE_NAME = gsub(",","",ness$SITE_NAME)
ness$SOURCE = gsub(",","",ness$SOURCE)
ness$nws_description = gsub(",","",ness$nws_description)
ness$network = as.character(ness$network)
write.csv(ness,"./datascan/snowsites/ness_nws-linked.csv",quote=TRUE)

#checked in excel
ness = read.csv('./datascan/snowsites/ness_nws-linked.csv')
ness = merge(ness[,c(1:8)],all_stations[,c(1,3,4,5,6,7)],by=c('survey_id','n','ORGANIZATION','SOURCE'),all.x=TRUE)
ness = merge(ness,maine_stations[,c(1,15,18,19,21,22)],all.x=TRUE)
ness = ness[,c(1:10)]
nohrsc_sf = read.csv('./datascan/snowsites/nohrsc_sites_summary_post-2021.csv')
nohrsc_sf$network = lapply(nohrsc_sf$network,function(x){as.character(strsplit(x,"_")[[1]][2])})
nohrsc_sf$network = as.character(nohrsc_sf$network)


# merge the data sets and do some clean up
ness = merge(nohrsc_sf,ness,by='Station_Id',all=TRUE)
ness$latitude[which(is.na(ness$latitude))] = ness$LATITUDE[which(is.na(ness$latitude))]
ness$longitude[which(is.na(ness$longitude))] = ness$LONGITUDE[which(is.na(ness$longitude))]
ness$network[which(is.na(ness$network))] = ness$ORGANIZATION[which(is.na(ness$network))]
#ness$name[which(is.na(ness$name))] = ness$SITE_NAME[which(is.na(ness$name))]
ness$network[which(!is.na(ness$ORGANIZATION)&(ness$network=="DCP"))] = ness$ORGANIZATION[which(!is.na(ness$ORGANIZATION)&(ness$network=="DCP"))]
ness$n_obs = apply(ness[,c('n.x','n.y')],FUN=function(x){max(x,na.rm=TRUE)},MARGIN = 1)

write.csv(ness[c(1,2,3,4,5,6,7,9,11,16,18,20,21,23,25)],"./datascan/snowsites/NOHRSC_ness-sites.csv",row.names=FALSE)