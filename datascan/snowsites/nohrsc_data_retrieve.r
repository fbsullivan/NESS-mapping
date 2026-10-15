library(sf)
library(curl)
library(dplyr)

getSnowData = function(region,day,utc){
  sd_link = paste("https://www.nohrsc.noaa.gov/nsa/discussions_text/",region,"/snowdepth/",substr(day,1,6),"/snowdepth_",day,utc,"_m.txt",sep="")
  swe_link = paste("https://www.nohrsc.noaa.gov/nsa/discussions_text/",region,"/swe/",substr(day,1,6),"/swe_",day,utc,"_m.txt",sep="")
  sd = try(read.csv(url(sd_link),sep="|",skip=1),silent=TRUE)
  swe = try(read.csv(url(swe_link),sep="|",skip=1),silent=TRUE)

  if((!is.null(nrow(sd))&!is.null(nrow(swe)))){
    return(rbind(sd,swe))
  }
  
  if((is.null(nrow(sd)))|is.null((nrow(swe)))){
    if(is.null(nrow(sd))){
      return(swe)
    }else{
      return(sd)
    }
  }
  
  if((is.null(nrow(sd))&is.null(nrow(swe)))){
    return(print(paste("No snow data for",day,utc,sep=" ")))
  }
  
}

days = seq(as.Date("2021-01-01"),as.Date("2026-02-16"),by="day")
#days = seq(as.Date("2004-01-02"),as.Date("2006-12-31"),by="day") # change dates, pre December 2006 no lat-lon
#we're interested in last 5 years of observations between january and april
days = days[which(substr(days,6,7)=="01"|substr(days,6,7)=="02"|substr(days,6,7)=="03"|substr(days,6,7)=="04")]
fdays = gsub("-","",days)
ts = c("00","06","12","18")
regions = c("Northeast","Southern_Great_Lakes","Allegheny_Front")

# format of data file url:
# https://www.nohrsc.noaa.gov/nsa/discussions_text/Northeast/snowdepth/202102/snowdepth_2021020906_e.txt
i = 0
for(d in fdays){
  for(t in ts){
    for(r in regions){
      i = i+1
      if(i==1){
        dat = getSnowData(r,d,t)
      }else{
        tmp = try(getSnowData(r,d,t))
        dat = rbind(dat,tmp)
      }
      
    }
  }
  dat = unique(dat) # cleans out duplicate observations from stations that are included in multiple releases
}

write.csv(dat,"nohrsc_data_jan1-apr30-2021-2026.csv")

sd = subset(dat,Physical_Element=="snowdepth")
swe = subset(dat,Physical_Element=="swe")

# pull together station id, name, lat, lon, element, first and last report
sd = sd[seq(nrow(sd),1),]
swe = swe[seq(nrow(swe),1),]

swe_summary = data.frame("Station_Id"=unique(swe$Station_Id))
swe_summary$name = swe[match(swe_summary$Station_Id,swe$Station_Id),"Name"]
swe_summary$latitude = swe[match(swe_summary$Station_Id,swe$Station_Id),"Latitude"]
swe_summary$longitude = swe[match(swe_summary$Station_Id,swe$Station_Id),"Longitude"]
swe_summary$element = "swe"
swe_summary$start_date = swe[seq(nrow(swe),1),][match(swe_summary$Station_Id,swe$Station_Id),"DateTime_Report.UTC."]
swe_summary$end_date = swe[match(swe_summary$Station_Id,rev(swe$Station_Id)),"DateTime_Report.UTC."]
n_obs = swe %>% count(Station_Id)
swe_summary = merge(swe_summary,n_obs,by="Station_Id")

sd_summary = data.frame("Station_Id"=unique(sd$Station_Id))
sd_summary$name = sd[match(sd_summary$Station_Id,sd$Station_Id),"Name"]
sd_summary$latitude = sd[match(sd_summary$Station_Id,sd$Station_Id),"Latitude"]
sd_summary$longitude = sd[match(sd_summary$Station_Id,sd$Station_Id),"Longitude"]
sd_summary$element = "snow_depth"
sd_summary$start_date = sd[seq(nrow(sd),1),][match(sd_summary$Station_Id,sd$Station_Id),"DateTime_Report.UTC."]
sd_summary$end_date = sd[match(sd_summary$Station_Id,rev(sd$Station_Id)),"DateTime_Report.UTC."]
n_obs = sd %>% count(Station_Id)
sd_summary = merge(sd_summary,n_obs,by="Station_Id")

nohrsc = rbind(swe_summary,sd_summary)

# retrieve network-linked NWS station IDs (IOWA STATE MESONET)
# new hampshire
asos = read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NH_ASOS.txt','r'))
coop = read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NH_COOP.txt','r'))
coco = read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NH_COCORAHS.txt','r'))
dcp = read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NH_DCP.txt','r'))

# maine
asos = rbind(asos,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/ME_ASOS.txt','r')))
coop = rbind(coop,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/ME_COOP.txt','r')))
coco = rbind(coco,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/ME_COCORAHS.txt','r')))
dcp = rbind(dcp,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/ME_DCP.txt','r')))

# vermont
asos = rbind(asos,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/VT_ASOS.txt','r')))
coop = rbind(coop,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/VT_COOP.txt','r')))
coco = rbind(coco,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/VT_COCORAHS.txt','r')))
dcp = rbind(dcp,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/VT_DCP.txt','r')))

# new york
asos = rbind(asos,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NY_ASOS.txt','r')))
coop = rbind(coop,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NY_COOP.txt','r')))
coco = rbind(coco,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NY_COCORAHS.txt','r')))
dcp = rbind(dcp,read.csv(curl('https://mesonet.agron.iastate.edu/api/1/network/NY_DCP.txt','r')))

# link asos, coop, coco, and dcp station ids to nohrsc stations report
all_stations = rbind(asos,coop,coco,dcp)

# some stations are duplicates
station_ct = count(nohrsc_sf,Station_Id)
duplicate_stations = station_ct$Station_Id[which(station_ct$n>1)]
full_duplicates = all_stations[all_stations$Station_Id %in% duplicate_stations,]
# this exercise determined that all duplicate stations are ASOS or COOP that are also reported as DCP
# so we'll filter the station list to only have the first time the station is listed
filtered_stations =
  all_stations %>% group_by(Station_Id) %>%
  filter(row_number()==1)

# merge filtered stations list to link NOHRSC stations with existing networks
names(filtered_stations)[c(1,3,6,37,38)] = c("Station_Id","description","list_elev","list_lon","list_lat")
nohrsc = merge(nohrsc,filtered_stations[c(1,3,4,7,8,12,37,38)],by="Station_Id",all.x=TRUE)

# convert to shapefile
nohrsc_points = st_as_sf(nohrsc,coords=c("longitude","latitude"),crs=4326)
region = read_sf("NortheastStates.shp")
regionwgs84 = st_transform(region,4326)
regionwgs84 = st_union(regionwgs84)
nohrsc_sf = nohrsc[lengths(st_within(nohrsc_points,regionwgs84))>0,]
# nohrsc_sfshp = st_filter(nohrsc_points,regionwgs84,.predicate=st_within)
# plot(regionwgs84)
# plot(nohrsc_sfshp,add=TRUE)

write.csv(nohrsc_sf,'./datascan/snowsites/nohrsc_sites_summary_post-2021.csv',row.names=FALSE)
#write.csv(nohrsc,'nohrsc_sites_summary-pre-2007.csv')
write.csv(filtered_stations,'./datascan/snowsites/nws_stations_list.csv',row.names=FALSE)

