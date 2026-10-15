library(sf)
library(terra)
library(tidyterra)
library(purrr)
library(dplyr)
library(ggplot2)

# user inputs
writePlots = TRUE
saveFiltered = TRUE

# borrowing DEM footprint
demIm = './rasters/DEM_30m.tif'
dem = rast(demIm)
roi = './shapefiles/NorthernNewEngland.shp'
NE = read_sf(roi)
NE = st_transform(NE,crs=crs(dem))

# shapefiles
lands = list.files('./shapefiles/LandOwnership',pattern='*.shp',full.names=TRUE) #c('./shapefiles/LandOwnership/VTCL.shp','./shapefiles/LandOwnership/NHCPL.shp','./shapefiles/LandOwnership/MCL.shp')
wilderness = './shapefiles/LandOwnership/S_USA.BdyDesg_LSRS_WildernessStatus.shp'
lte4g = list.files('./shapefiles/4G/',pattern='*.shp',full.names=TRUE)
soils = list.files('./shapefiles/Soils/',pattern='*tax-order.shp',full.names=TRUE) #c('./shapefiles/Soils/VT_tax-order.shp','./shapefiles/Soils/NH_tax-order.shp','./shapefiles/Soils/ME_tax-order.shp')
states = './shapefiles/NorthernNewEngland.shp'
huc8 = './shapefiles/Watersheds_Huc8.shp'

shapefiles = list(lands,wilderness,lte4g,soils,states,huc8)

# attribute filters:
filterFlag = c(FALSE,FALSE,FALSE,TRUE,FALSE,FALSE)
filterAttribute = c('AGNCY_TYPE',NA,NA,'component_','STATE_NAME','NAME')
filterTrait = list(NA,NA,NA,c('Histosols'),NA,NA)
filterType = c(NA,NA,NA,"not",NA,NA) # and/not

# output files:
landfile = paste('./rasters/LandOwnership.tif',sep='_')
wildfile = paste('./rasters/Wilderness.tif',sep='_')
ltefile = './rasters/LTE4G_Coverage.tif'
soilsfile = './rasters/DrySoils.tif'
statesfile = './rasters/StateID.tif'
huc8file = './rasters/HUC8.tif'
rasterfiles = c(landfile,wildfile,ltefile,soilsfile,statesfile,huc8file)

# image var names:
imagenames = c('lands','wilderness','lte','soil','stateID','watershed')

# filter and rasterize
# start with calculating intersection of local shapefiles
for(f in 1:length(rasterfiles)){
  if(!file.exists(rasterfiles[f])){
    print(paste("compiling",rasterfiles[f],"from shapefiles"))
    if(filterFlag[f]){
      if(filterType[f]=="and"){
        data = map(unlist(shapefiles[f]),read_sf) %>% bind_rows %>% filter(get(filterAttribute[f]) %in% unlist(filterTrait[f]))
      }
      if(filterType[f]=="not"){
        data = map(unlist(shapefiles[f]),read_sf) %>% bind_rows %>% filter(!(get(filterAttribute[f]) %in% unlist(filterTrait[f])))
      }
    }else{
      data = map(unlist(shapefiles[f]),read_sf) %>% bind_rows
    }
    
    # rasterize the shapefile
    if(!is.na(unlist(filterAttribute[f]))&is.na(filterType[f])){
      assign(imagenames[f],
             rasterize(vect(st_transform(st_make_valid(data),crs=crs(dem))),
                rast(ext(dem),res=res(dem),crs=crs(dem)),
                field=filterAttribute[f]))
      print(paste("exporting",rasterfiles[f]))
      writeRaster(get(imagenames[f]),rasterfiles[f],overwrite=TRUE)
      assign(imagenames[f],rast(rasterfiles[f]))
    }else{
      assign(imagenames[f],
             rasterize(vect(st_transform(data,crs=crs(dem))),
                rast(ext(dem),res=res(dem),crs=crs(dem)),
                field=1,fun="min"))
      print(paste("exporting",rasterfiles[f]))
      writeRaster(get(imagenames[f]),rasterfiles[f],overwrite=TRUE)
      assign(imagenames[f],rast(rasterfiles[f]))
    }
    gc()

    if(writePlots){
      imout = c(imagenames[f],filterAttribute[f],filterType[f],unlist(filterTrait[f]))
      p = ggplot() + 
        geom_spatraster(data=as.factor(get(imagenames[f]))) + 
        scale_fill_viridis_d(na.value="transparent") +
        geom_sf(data=NE,fill=NA,color="darkgray") +
        theme_minimal()
      ggsave(paste('./filtering/figures/',paste(imout[!is.na(imout)],collapse='_'),'.jpg',sep='_'),plot=p,dpi=300,height=5,width=5,units="in")
    }
    
    
  }else{
    # read in the file
    print(paste(rasterfiles[f],"exists! reading to memory"))
    assign(imagenames[f],rast(rasterfiles[f]))
  }
}

print('shapefiles rasterized and loaded')