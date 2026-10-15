library(dataRetrieval)
library(sf)
library(rgee)

active = read_waterdata_ts_meta(last_modified="P5Y",parameter_name="Discharge",properties=c("monitoring_location_id","computation_period_identifier"))
active = unique(active$monitoring_location_id)

nh = read_waterdata_monitoring_location(state_name="New Hampshire",site_type="Stream",properties=c("monitoring_location_id"))

vt = read_waterdata_monitoring_location(state_name="Vermont",site_type="Stream",properties=c("monitoring_location_id"))

me = read_waterdata_monitoring_location(state_name="Maine",site_type="Stream",properties=c("monitoring_location_id"))

nne = dplyr::bind_rows(nh,vt,me) |> 
  dplyr::mutate(active_gage = dplyr::case_when(monitoring_location_id %in% active~"active",TRUE~"inactive"))

nne_active = nne |> dplyr::filter(active_gage=="active")
write_sf(nne_active,"./shapefiles/active_gages_5y.shp")

huc10 = read_sf("./shapefiles/HUC10NorthernStates.shp")
winn = dplyr::filter(huc10,ObjectID==403)[2,]

ee_Initialize(user="fsulliva@gmail.com",drive=TRUE)
ee_roi = sf_as_ee(winn)

# dem
dem = ee$ImageCollection("USGS/3DEP/10m_collection")$filterBounds(ee_roi)
proj = dem$first()$projection()
demImage = dem$mosaic()$select('elevation')$setDefaultProjection(proj)$clip(ee_roi)

slope = ee$Terrain$slope(demImage)$clip(ee_roi)

# prism
prism = ee$ImageCollection("OREGONSTATE/PRISM/ANm")$
  filterBounds(ee_roi)$
  filter(ee$Filter$date('2023-12-01','2024-01-01'))$select('ppt')
ppt = prism$first()$clip(ee_roi)

# snodas
swe1 = ee$ImageCollection('projects/earthengine-legacy/assets/projects/climate-engine/snodas/daily')$
  filterBounds(ee_roi)$
  filterDate('2023-12-15')$
  select('SWE')
swe2 = ee$ImageCollection('projects/earthengine-legacy/assets/projects/climate-engine/snodas/daily')$
  filterBounds(ee_roi)$
  filterDate('2023-12-22')$
  select('SWE')
swediff = swe2$first()$subtract(swe1$first())$clip(ee_roi)

task_img4 = ee_image_to_drive(
  image = swediff,
  folder = "GEE_Images",
  description = "DelSWE",
  crs = "EPSG:4326",
  scale = swediff$projection()$nominalScale(),
  region = ee_roi$geometry()
)
task_img4$start()
ee_monitoring(task_img4)
