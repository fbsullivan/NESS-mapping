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
  #sfCast = sf::st_as_sf(vecGrid)
  #sfCast$ID = seq(1,nrow(sfCast))
  vecGrid$ID = seq(1,nrow(vecGrid))
  return(vecGrid)
}

