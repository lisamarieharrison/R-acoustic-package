setwd("C:/Users/Lisa/Documents/phd/southern ocean/R/maps")
load("lcc_projected.Rdata")
load("lcc_projected_hires.Rdata")
load("longlat_unprojected.Rdata")
library(rgdal)


## basic projected map
plot(ptopo, addfun = function() plot(pcm, add = TRUE))

## llgridlines needs an sp object
sp_obj <- as(extent(ptopo), "SpatialPolygons")
prj <- "+proj=lcc +lon_0=180 +lat_0=-60 +lat_1=-75 +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 +over"
projection(sp_obj) <- prj

llgridlines(sp_obj, easts = seq(120, 2))

## interactive zoom:
# zoom(ptopo, addfun = function() plot(pcm, add = TRUE))
##some complaining from R Studio around here.     
## interactive save extent
e <- drawExtent()

sp_obj <- as(e, "SpatialPolygons")
projection(sp_obj) <- prj

llgridlines(sp_obj, side = "NE")

scalebar(20, xy = click(), type = "bar", label = "20km")

## local example
plot(e, type = "n", axes = FALSE, xlab = "", ylab = "")
plot(ptopo, add = TRUE)
plot(pcm, add = TRUE)
sp_obj <- as(e, "SpatialPolygons")
projection(sp_obj) <- prj
op <- par(xpd = NA)
llgridlines(sp_obj, side = "WS")
par(op)

scalebar(100000, xy = click(), type = "bar", label = "100km")
