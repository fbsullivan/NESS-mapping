library(rgee)
library(sf)

roi = './shapefiles/NorthernNewEngland.shp'
NE = read_sf(roi)
NE_union = st_union(NE)

# start up GEE and pass ROI
ee_Initialize(user="fsulliva@gmail.com",drive=TRUE,gcs=TRUE,project="ness-483617")
ee_roi = sf_as_ee(st_convex_hull(NE_union))

dem = ee$ImageCollection(demCol)$filterBounds(ee_roi)
proj = dem$first()$projection()
demImage = dem$mosaic()$select('elevation')$setDefaultProjection(proj)$clip(ee_roi)
aspect = ee$Terrain$aspect(demImage)$clip(ee_roi)
slope = ee$Terrain$slope(demImage)$clip(ee_roi)

expdem = ee_image_to_asset(
  image = demImage,
  description = "NE DEM",
  assetId = paste0('users/fsulliva/NNE_DEM'),
  overwrite = TRUE,
  region = ee_roi,
  maxPixels = 4000000000
)
expdem$start()
ee_monitoring(expdem)

expslope = ee_image_to_asset(
  image = slope,
  description = "NE Slope",
  assetId = paste0('users/fsulliva/NNE_Slope'),
  overwrite = TRUE,
  region = ee_roi,
  maxPixels = 4000000000
)
expslope$start()
ee_monitoring(expslope)

expasp = ee_image_to_asset(
  image = aspect,
  description = "NE Aspect",
  assetId = paste0('users/fsulliva/NNE_Aspect'),
  overwrite = TRUE,
  region = ee_roi,
  maxPixels = 4000000000
)
expasp$start()
ee_monitoring(expasp)


toLocal = ee_image_to_gcs(
  image = demImage,
  description = "NE DEM",
  bucket = "data_throughway",
  region = ee_roi,
  scale = 30,
  crs = 'EPSG:5070',
  maxPixels = 4000000000
)
toLocal$start()
ee_monitoring(toLocal)

ee_gcs_to_local(toLocal,dsn='./rasters/DEM_30m.tiff')
