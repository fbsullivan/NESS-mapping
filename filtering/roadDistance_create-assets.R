library(rgee)
library(sf)

roi = './shapefiles/NorthernNewEngland.shp'
NE = read_sf(roi)
NE_union = st_union(NE)

# start up GEE and pass ROI
ee_Initialize(user="fsulliva@gmail.com",drive=TRUE,gcs=TRUE,project="ness-483617")
ee_roi = sf_as_ee(st_convex_hull(NE_union))
roadsCol = "projects/sat-io/open-datasets/TIGER/2025/Roads"

# TIGER Roads
roads = ee$FeatureCollection(roadsCol)$
  filterBounds(ee_roi)

primary = roads$filter(ee$Filter$Or(
  ee$Filter$eq('MTFCC','S1100'),
  ee$Filter$eq('MTFCC','S1200'),
  ee$Filter$eq('MTFCC','S1400')))

unpaved = roads$filter(ee$Filter(ee$Filter$eq('MTFCC','S1500')))

d2pr = primary$distance(5000)
d2pr = d2pr$rename('primary')
d2br = unpaved$distance(5000)
d2br = d2br$rename('unpaved')

toLocal = ee_image_to_gcs(
  image = d2pr,
  description = "Distance_to_Paved_Road",
  bucket = "data_throughway",
  region = ee_roi,
  scale = 30,
  crs = 'EPSG:5070',
  maxPixels = 4000000000
)
toLocal$start()
ee_monitoring(toLocal)

ee_gcs_to_local(toLocal,dsn='./rasters/PrimaryRoadDistance_30m.tiff')

toLocal = ee_image_to_gcs(
  image = d2br,
  description = "Distance_to_Unpaved_Road",
  bucket = "data_throughway",
  region = ee_roi,
  scale = 30,
  crs = 'EPSG:5070',
  maxPixels = 4000000000
)
toLocal$start()
ee_monitoring(toLocal)

ee_gcs_to_local(toLocal,dsn='./rasters/UnpavedRoadDistance_30m.tiff')
