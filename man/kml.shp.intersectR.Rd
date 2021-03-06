% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmlshpintersectr.R
\name{kml.shp.intersectR}
\alias{kml.shp.intersectR}
\title{Intersect a shapefile with kml files and return the kml files as a shapefile, as well as the files
that were intersected}
\usage{
kml.shp.intersectR(kml.files.path, shp.file.path, shp.file.name, epsg)
}
\arguments{
\item{kml.files.path}{file path to the kml files you want to process}

\item{shp.file.path}{file path to the shape file you want to intersect with the kml files}

\item{shp.file.name}{name of the shapefile}

\item{epsg}{epsg code for the projection you want the data to be in}
}
\value{
A list with three parts: a shapefile of the combined kml files, a shapefile of the location data, a list of kml files that were the location data intersected with
}
\description{
This function reads in a list of kml files, converts them into simplified spatial polygons, reads
in a shapefile of location data (points or polygons), and then finds out what kml files these 
location data reside in.
}
\details{
This is super useful because NEON data flight lines and tiles only come as KML files and sometimes
we do not want to process the entire batch of data - we only need a tile or two of data based on
where our field data is located.

This function lets us use that field data and find only the tiles we need to process, thus 
cutting down on our processing time immensely.

This function will simplify complex kml files - it will turn them into polygons based on the 
greatest extent in each direction. This should be used for intersection purposes only.

----------USAGE----------

# set your working directory
setwd("some directory")

# run the function - files paths need to look like this
serc.tiles <- kml.shp.intersectR(kml.files.path = "./NEON_Flight_Boundary_KML/SERC2017",
                                 shp.file.path = "./Processed_Shapefiles",
                                 shp.file.name = "SERC2017_SoilChem_20181203",
                                 epsg = 32618)
                         
# lets make sure that everything was returned correctly - we can plot the shapefiles and look at the list of 
# kml files that were returned
plot(serc.tiles$laz.tiles.shp)
plot(serc.tiles$loc.data.shp, add = TRUE)
serc.tiles$laz.tiles

# the list of kml files is returned as a pattern so that it can be used with the list.files function to
# return the files that you need to use
list.files("./NEON_Flight_Boundary_KML/SERC2017", pattern = serc.tiles$laz.tiles)
}
