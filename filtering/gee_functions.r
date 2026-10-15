shape_intersect = function(shapefiles_list=NULL){

  soils = map(soils,read_sf) %>% bind_rows %>% filter(component_ %in% componentName_)
  lands = map(lands,read_sf) %>% bind_rows %>% filter(AGNCY_TYPE %in% AGNCY_TYPEName) %>% select("AGNCY_TYPE")
  soils = st_make_valid(st_transform(soils,st_crs(lands)))
  lands = st_make_valid(lands)
  soils = st_difference(NE_union,st_union(soils))
  
  intersection = st_intersection(soils,st_union(lands))
  intersection = st_transform(intersection,5070) # use projection that is in meters, CONUS Albers
}

make_sieve = function(shapefile,cell_size,points=FALSE){
  vectorized = terra::vect(shapefile)

  if(points==TRUE){
    inspectionGrid = terra::rast(ext(vectorized),res=30,crs=crs(vectorized))
    inspectionGrid = terra::rasterize(vectorized,inspectionGrid,cover=TRUE)
    inspectionGrid = ifel(!is.na(inspectionGrid)==1,1,NA)
    vecGrid = terra::as.points(inspectionGrid,na.rm=TRUE)
  }else{
    inspectionGrid = terra::rast(ext(vectorized),res=cell_size,crs=crs(vectorized))
    inspectionGrid = terra::rasterize(vectorized,inspectionGrid,field=1,fun="min")
    vecGrid = terra::as.polygons(inspectionGrid,aggregate=FALSE)
  }
  sfCast = sf::st_as_sf(vecGrid)
  sfCast$ID = seq(1,nrow(sfCast))
  return(sfCast)
}

run_sieve = function(sievegrid,filters = NULL){
  
}

make_bounds = function(coordinatesList,res){
  xPlus = coordinatesList[,1]+res/2
  yPlus = coordinatesList[,2]+res/2
  xMinus = coordinatesList[,1]-res/2
  yMinus = coordinatesList[,2]-res/2
  bounds = cbind(xMinus,yPlus,xPlus,yPlus,xPlus,yMinus,xMinus,yMinus,xMinus,yPlus)
  return(bounds)
}

bboxPoly = function(x,res){
  #p = st_geometry(x)
  #b = st_buffer(p,res)
  pg = st_as_sfc(st_bbox(x))
  #x$geometry = pg
  return(pg)
}
pointsToPoly = function(pointsSF,res){
  pointsgeom = st_geometry(pointsSF)
  buffered = st_buffer(pointsgeom,res/2)
  #polygeom = buffered %>% map(bboxPoly,.progress=TRUE) %>% reduce(vctrs::vec_c)
  #pointsSF$geometry = polygeom
  #pointsSF = st_sf(as.data.frame(pointsSF)[,1:(ncol(sfCast)-1)],polygeom)
  #pointsSF = pointsSF %>% pmap(bboxPoly, res=res/2, .progress=TRUE)
  return(polygeom)
}
