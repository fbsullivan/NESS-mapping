# extra unused rgee code from spatial_filtering.r script

# filterImage = filterImage$updateMask(demImage$gte(demmin))$
#   updateMask(demImage$lte(demmax))$
#   updateMask(slope$lte(slopemax))
# filterImage = filterImage$unmask()
# 
# # we'll export this binary image so we can access it and operate on it more quickly
exp = ee_image_to_asset(
  image = filterImage,
  description = "DEM Filter",
  assetId = paste0('projects/ness-483617/assets','/demBinary'),
  overwrite = TRUE,
  scale = proj$nominalScale(),
  region = ee_roi,
  maxPixels = 4000000000
)
exp$start()
# ee_monitoring(exp)


# filterImage = ee$Image$constant(1)$setDefaultProjection(proj2)$clip(ee_roi)
# filterImage = filterImage$updateMask(sweDaysMean$gte(swedaysmin))
# filterImage = filterImage$unmask()
# 
# exp2 = ee_image_to_asset(
#   image = filterImage,
#   description = "SWE Filter",
#   assetId = paste0('projects/ness-483617/assets','/sweBinary'),
#   overwrite = TRUE,
#   scale = proj$nominalScale(),
#   region = ee_roi,
#   maxPixels = 4000000000
# )
# exp2$start()
# # ee_monitoring(exp2)

# d2r = roads$distance(buff)
# d2pr = primary$distance(buff)
# d2br = d2r$subtract(d2pr)
# d2br = d2br$updateMask(d2br$eq(0))
# 
# calculate filterImage
# filterImage = ee$Image$constant(1)$setDefaultProjection(proj)$clip(ee_roi)
# filterImage = filterImage$clip(ee_roi)$updateMask(paintedmain$eq(1))
# filterImage = filterImage$unmask(2) # to identify unpaved access areas with non-zero value
# filterImage = filterImage$clip(ee_roi)$updateMask(paintedroads$eq(1))


# exp3 = ee_image_to_asset(
#   image = d2r,
#   description = "Distance to All Roads",
#   assetId = paste0('projects/ness-483617/assets','/Distance2Roads'),
#   overwrite = TRUE,
#   scale = proj$nominalScale(),
#   region = ee_roi,
#   maxPixels = 4000000000
# )
# exp3$start()
# # ee_monitoring(exp2)
# 
# exp4 = ee_image_to_asset(
#   image = d2pr,
#   description = "Distance to Primary Roads",
#   assetId = paste0('projects/ness-483617/assets','/Distance2PrimaryRoads'),
#   overwrite = TRUE,
#   scale = proj$nominalScale(),
#   region = ee_roi,
#   maxPixels = 4000000000
# )
# exp4$start()





watersheds = ee$FeatureCollection('USGS/WBD/2017/HUC10')$filterBounds(ee_roi)

filterWS = watersheds$map(function(f){
  reduced = filterImage$reduceRegion(reducer=ee$Reducer$mean(),geometry=f$geometry(),maxPixels=50000000)
  return(f$set(reduced))
})

filteredWS = filterWS$filter(ee$Filter$gte('constant',0))

rGrid = ee_as_sf(x=filterGrid,dsn='./shapefiles/GEE_NESS-Filter.shp',via='drive',container='GEE_Tables',quiet=TRUE)








# # iterate over geometry ROIs as Google Earth Engine Object
# for(p in 1:nrow(h10)){
#   
#   watershed = h10[p,]
#   print(paste(watershed$name, Sys.time()))
#   ee_roi = sf_as_ee(watershed)
#   
#   # updateMask for main roads and output
#   filterImageMain = filterImage$clip(ee_roi)$updateMask(paintedmain$eq(1))
#   filterPoly = filterImageMain$reduceToVectors(
#     scale=demImage$projection()$nominalScale(),
#     geometry=ee_roi,
#     maxPixels=100000000
#   )
#   filterSF = ee_as_sf(filterPoly,
#                       via="getInfo",
#                       maxFeatures=35000)
#   
#   geeMainroads = rbind(geeMainroads,st_transform(filterSF,crs=st_crs(watershed)))
#   print(paste('Accumulated features:',nrow(geeMainroads)))
#   
#   # updateMask for dirt roads and output
#   filterImageDirt = filterImage$clip(ee_roi)$updateMask(painteddirt$eq(1))
#   filterPoly = filterImageDirt$reduceToVectors(
#     scale=demImage$projection()$nominalScale(),
#     geometry=ee_roi,
#     maxPixels=100000000
#   )
#   filterSF = ee_as_sf(filterPoly,
#                       via="getInfo",
#                       maxFeatures=35000)
#   
#   geeUnpaved = rbind(geeUnpaved,st_transform(filterSF,crs=st_crs(watershed)))
#   print(paste('Accumulated features:',nrow(geeUnpaved)))
# 
# }
# 
# st_write(geeMainroads,'./filtering/results/DEM600-2000_MainRoadsMax500_SlopeMax10_SCDMin80.shp')
# st_write(geeUnpaved,'./filtering/results/DEM600-2000_UnpavedRoadsMax500_SlopeMax10_SCDMin80.shp')



#extra stuff
demviz = list(max=2000,min=0,palette = c("#000000","#5AAD5A","#A9AD84","#FFFFFF"))
aspectviz = list(min=0,max=360,palette = c("#FFFFFF","#000000"))
slopeviz = list(min=0,max=60,palette = c("#FFFFFF","#000000"))

Map$centerObject(ee_roi)
Map$setZoom(7)
Map$addLayer(
 eeObject = aspect,
 visParams = aspectviz,
 name = "Aspect"
 ) +
Map$addLayer(
 eeObject = demImage,
 visParams =  demviz,
 name = "DEM"
 ) +
Map$addLayer(
 eeObject = slope,
 visParams = slopeviz,
 name = "Slope"
 ) +
Map$addLegend(
 demviz,
 name='Elevation',
 'topleft'
 ) +
Map$addLegend(
 aspectviz,
 name='Aspect',
 'bottomright'
 ) +
Map$addLegend(
 slopeviz,
 name='Slope',
 'bottomright'
 )

snowviz = list(min=100,max=25000,palette = c("#000000","#FFFFFF"))
Map$centerObject(ee_roi)
Map$setZoom(7)
Map$addLayer(
  eeObject = sd2$clip(ee_roi),
  visParams = snowviz,
  name = "Mean SWE Days"
  ) +   Map$addLegend(
  snowviz,
  name='Snow Water Days',
  'bottomright'
  )

snowviz = list(min=0,max=5000,palette = c("#000000","#FFFFFF"))
Map$centerObject(ee_roi)
Map$setZoom(7)
Map$addLayer(
  eeObject = d2br$clip(ee_roi),
  visParams = snowviz,
  name = "Distance to Unpaved Road"
) +   Map$addLegend(
  snowviz,
  name='Distance',
  'bottomright'
) + Map$addLayer(
  eeObject = d2pr$clip(ee_roi),
  visParams = snowviz,
  name = "Distance to Primary Road"
)


# 
# roadviz = list(min=-1,max=1,palette = c("#000000","#FFFFFF","#FFA500"))
# Map$centerObject(ee_roi)
# Map$setZoom(7)
# Map$addLayer(
#   eeObject = dirtonly,
#   roadviz,
#   name = "RoadAccess"
# )
# 
# styled1 = primary$style(color="#000000",width=1)
# styled2 = unpaved$style(color="#FFA500",width=1)
# Map$addLayer(styled1) +
#  Map$addLayer(styled2)
# 
# viz = list(styleProperty="constant",min=0,max=1,color=c("#000000","#FFA500"))
# styledGrid = filterGrid$style(viz)
# Map$addLayer(styledGrid)


