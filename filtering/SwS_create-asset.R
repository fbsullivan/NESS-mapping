library(rgee)
library(sf)

roi = './shapefiles/NorthernNewEngland.shp'
NE = read_sf(roi)
NE_union = st_union(NE)

# start up GEE and pass ROI
ee_Initialize(user="fsulliva@gmail.com",drive=TRUE,gcs=TRUE,project="ness-483617")
ee_roi = sf_as_ee(st_convex_hull(NE_union))

sweDays = ee_utils_pyfunc(function(winterYear){
  y = ee$Number(winterYear)
  y2 = y$add(1)
  start_date = ee$Date$fromYMD(y,10,1)
  end_date = ee$Date$fromYMD(y2,4,30)
  allDays = swe$filterDate(start_date,end_date)$toBands()
  
  # to calculated pixel-wise sums, all bands need same name
  bds = allDays$bandNames()
  l = bds$map(ee_utils_pyfunc(function(n){ return(allDays$select(c(n))$rename('b1'))}))
  allDays = ee$ImageCollection$fromImages(l)
  
  sweDays = allDays$reduce(reducer=ee$Reducer$sum())
  return(sweDays)
})

sy = 2012
ey = 2022
years = ee$List$sequence(sy,ey)
bands = ee$List$sequence(0,212,1)
swe = ee$ImageCollection("projects/ness-483617/assets/UofA_SWE")$select(bands)
sweDaysYears = ee$ImageCollection(years$map(sweDays))
#sweDaysMean = sweDaysYears$reduce(reducer=ee$Reducer$mean())$setDefaultProjection(swe$first()$projection())$clip(ee_roi)
sweDaysMean = sweDaysYears$reduce(reducer=ee$Reducer$mean())$setDefaultProjection(proj)$clip(ee_roi)
sweDaysMean = sweDaysMean$rename('sweDays')

exp = ee_image_to_asset(
  image = sweDaysMean,
  description = "SWE Days Product",
  assetId = paste0('users/fsulliva/UA_SWEdays'),
  overwrite = TRUE,
  region = ee_roi,
  maxPixels = 4000000000
)
exp$start()
ee_monitoring(exp)


toLocal = ee_image_to_gcs(
  image = sweDaysMean,
  description = "NE SWE Days",
  bucket = "data_throughway",
  region = ee_roi,
  crs = 'EPSG:5070',
  scale = 500,
  maxPixels = 4000000000
)
toLocal$start()
ee_monitoring(toLocal)

ee_gcs_to_local(toLocal,dsn='./rasters/Snow_Water_Days.tiff')
