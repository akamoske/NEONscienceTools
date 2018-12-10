#' Intersect a shapefile with kml files and return the kml files as a shapefile, as well as the files
#' that were intersected
#' 
#' This function reads in a list of kml files, converts them into simplified spatial polygons, reads
#' in a shapefile of location data (points or polygons), and then finds out what kml files these 
#' location data reside in.
#' 
#' This is super useful because NEON data flight lines and tiles only come as KML files and sometimes
#' we do not want to process the entire batch of data - we only need a tile or two of data based on
#' where our field data is located.
#' 
#' This function lets us use that field data and find only the tiles we need to process, thus 
#' cutting down on our processing time immensely.
#' 
#' This function will simplify complex kml files - it will turn them into polygons based on the 
#' greatest extent in each direction. This should be used for intersection purposes only.
#' 
#' ----------USAGE----------
#' 
#' # set your working directory
#' setwd("some directory")
#' 
#' # run the function - files paths need to look like this
#' serc.tiles <- kml.shp.intersectR(kml.files.path = "./NEON_Flight_Boundary_KML/SERC2017",
#'                                  shp.file.path = "./Processed_Shapefiles",
#'                                  shp.file.name = "SERC2017_SoilChem_20181203",
#'                                  epsg = 32618)
#'                          
#' # lets make sure that everything was returned correctly - we can plot the shapefiles and look at the list of 
#' # kml files that were returned
#' plot(serc.tiles$laz.tiles.shp)
#' plot(serc.tiles$loc.data.shp, add = TRUE)
#' serc.tiles$laz.tiles
#' 
#' # the list of kml files is returned as a pattern so that it can be used with the list.files function to
#' # return the files that you need to use
#' list.files("./NEON_Flight_Boundary_KML/SERC2017", pattern = serc.tiles$laz.tiles)
#' 
#' @param kml.files.path file path to the kml files you want to process
#' @param shp.file.path file path to the shape file you want to intersect with the kml files
#' @param shp.file.name name of the shapefile
#' @param epsg epsg code for the projection you want the data to be in
#' @return A list with three parts: a shapefile of the combined kml files, a shapefile of the location data, a list of kml files that were the location data intersected with
#' @export

kml.shp.intersectR <- function(kml.files.path, shp.file.path, shp.file.name, epsg) {
  
  #load needed libraries
  library(rgeos)
  library(rgdal)
  library(raster)
  
  #first we need to create a function that runs of just one file
  kmlshpR <- function(kml.file, epsg) {
    
    #pull the name of the file so we can save it as an attribute
    file.name <- tools::file_path_sans_ext(basename(kml.file))
    
    #get the layer names and read in the kml file
    kml.layer <- ogrListLayers(dsn = kml.file)
    kml.shp <- readOGR(dsn = kml.file, layer = kml.layer, verbose = TRUE)
    
    #project the kml file
    crs.proj <- base::paste0("+init=epsg:", epsg)
    kml.proj <- sp::spTransform(kml.shp, crs.proj)
    
    #add the file name as an attribute to the shapefile
    kml.proj$name <- file.name
    
    #lets convert this kml file into a polygon
    e <- extent(kml.proj)
    x.max <- e@xmax
    y.max <- e@ymax
    x.min <- e@xmin
    y.min <- e@ymin
    
    #lets put this all together now
    kml.square <- base::matrix(c(x.min, y.max, #NW corner
                                 x.max, y.max, #NE corner
                                 x.max, y.min, #SE corner
                                 x.min, y.min, #SW corner
                                 x.min, y.max), #NW corner - close polygon
                               ncol = 2,
                               byrow = TRUE) 
    
    #lets turn this data into a spatial polygon so that we can map it
    kml.laz <- sp::Polygon(kml.square)
    crs.proj <- base::paste0("+init=epsg:", epsg)
    kml.laz.sp <- sp::SpatialPolygons(list(Polygons(list(kml.laz), ID = file.name)),
                                          proj4string = CRS(crs.proj))
    
    #lets add the file name as an attribute
    kml.laz.sp$name <- file.name
    
    #return the shapefile
    return(kml.laz.sp)
    
  }
  
  #make a list of all the kml files
  kml.files <- list.files(kml.files.path, pattern = ".kml$", full.names = TRUE)
  
  #lets only have the files larger than 500kb, this is a check because sometimes there are empty kml files
  kml.files <- kml.files[sapply(kml.files, base::file.size) > 500]
  
  #lets apply the above function to all the kml files in this list
  shp.files <- lapply(kml.files, function(x) kmlshpR(x, epsg))
  
  #lets merge all the shapefiles into one shapefile
  shp.files.merged <- base::do.call(raster::bind, shp.files)
  
  #lets read in our plots shapefile and find out what tiles intersect with it
  shp.data <- rgdal::readOGR(shp.file.path, shp.file.name)
  crs.proj <- base::paste0("+init=epsg:", epsg)
  shp.data.proj <- sp::spTransform(shp.data, crs.proj)
  
  #lets intersect these
  laz.tiles <- raster::intersect(shp.data.proj, shp.files.merged)
                      
  #lets find the number of columns
  col.num <- ncol(laz.tiles)
  
  #lets save the laz tile names so we can moves some files around
  laz.names <- raster::unique(laz.tiles@data[,col.num])
  
  #convert list of file names to pattern
  file.pattern <- paste(laz.names, collapse = "|")
  
  #return the final data
  final.data <- list("laz.tiles.shp" = shp.files.merged, "loc.data.shp" = shp.data.proj, "laz.tiles" = file.pattern)
  return(final.data)
}
  


