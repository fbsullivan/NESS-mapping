library(dataRetrieval)
library(sf)
library(exactextractr)

active = read_waterdata_ts_meta(last_modified="P5Y",parameter_name="Discharge",properties=c("monitoring_location_id","computation_period_identifier"))
active = unique(active$monitoring_location_id)

nh = read_waterdata_monitoring_location(state_name="New Hampshire",site_type="Stream",properties=c("monitoring_location_id"))

vt = read_waterdata_monitoring_location(state_name="Vermont",site_type="Stream",properties=c("monitoring_location_id"))

me = read_waterdata_monitoring_location(state_name="Maine",site_type="Stream",properties=c("monitoring_location_id"))

nne = dplyr::bind_rows(nh,vt,me) |> 
  dplyr::mutate(active_gage = dplyr::case_when(monitoring_location_id %in% active~"active",TRUE~"inactive"))

nne_active = nne |> dplyr::filter(active_gage=="active")
write_sf(nne_active,"./shapefiles/active_gages_5y.shp")

huc8 = read_sf("./shapefiles/HUC10NorthernStates.shp")
huc8 = st_transform(huc10,crs=5070)
ness = read.csv("./datascan/results/NESS_Sites.csv")
nesssf = st_as_sf(ness,coords=c("longitude","latitude"),crs=4269)
nesssf = st_transform(nesssf,crs=5070)
nesssf = st_filter(nesssf,NE)
nne_active = st_transform(nne_active,crs=5070)

if(writePlots){
  f = ggplot() + 
    geom_sf(data=NE,fill=NA) +
    geom_sf(data=huc10,fill=NA) +
    geom_sf(data=nne_active,color="black") +
    geom_sf(data=nesssf[which(nesssf$element=="snow_depth"|nesssf$element=="swe"),],aes(shape=isAutomated,color=element)) + 
    scale_color_manual(name="Element",values=c("darkblue","lightblue")) +
    scale_shape(name="Measurement Type") +
    theme_minimal()
  ggsave('./datascan/figures/DataScan_SWE-Depth-Gauges.jpg',plot=f,dpi=300,height=5,width=5,units="in")
}

# zonal statistics, count depth, swe, and gauge sites within watersheds
huc8$sweCount = lengths(st_intersects(huc8,nesssf[which(nesssf$element=="swe"),]))
huc8$depthCount = lengths(st_intersects(huc8,nesssf[which(nesssf$element=="snow_depth"),]))
huc8$gageCount = lengths(st_intersects(huc8,nne_active))

write_sf(huc8,"./shapefiles/nne_huc8_w-counts.shp")