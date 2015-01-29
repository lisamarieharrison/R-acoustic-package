###required packages
library(RNetCDF)
setwd("C:/Users/Lisa/Documents/phd/southern ocean/R/maps/")

track <- read.csv("C:/Users/Lisa/Desktop/KAOS/kaos_cruise_track.csv", header = T)

bathy_fname <- "GEBCO_2014_2D_62.0_-68.0_66.0_-65.0.nc"

#load bathy data
nc <- open.nc(bathy_fname)
print.nc(nc)
tmp <- read.nc(nc)
z <- array(tmp$elevation, dim=c(length(tmp$lon), length(tmp$lat)))
xran <- tmp$x_range
yran <- tmp$y_range
zran <- tmp$z_range
lon <- seq(min(tmp$lon), max(tmp$lon), length.out = length(tmp$elevation))
lat <- seq(min(tmp$lat), max(tmp$lat), length.out = length(tmp$elevation))


###Plot
#make palette
ocean.pal <- colorRampPalette(
  c("#00061E", "#000728", "#000932", "#002650", 
    "#00426E", "#005E8C", "#007AAA", "#0096C8", "#22A9C2", "#45BCBB", 
    "#67CFB5", "#8AE2AE", "#ACF6A8", "#BCF8B9")
)

land.pal <- colorRampPalette(
  c("#336600", "#F3CA89", "#D9A627", 
    "#A49019", "#9F7B0D", "#996600", "#B27676", "#C2B0B0", "#E5E5E5", 
    "#FFFFFF")
)

#turn cruise track into matrix
cruise <- matrix(0, nrow = nrow(z), ncol = ncol(z))
for (i in 1:nrow(track)) {
  w <- which.min(abs(tmp$lon - track$Longitude[i]))
  x <- which.min(abs(tmp$lat - track$Latitude[i]))
  cruise[w, x] <- 1
}


zbreaks <- seq(-4000, 1200, by=10)
cols <-c(ocean.pal(sum(zbreaks<=0)-1), land.pal(sum(zbreaks>0)))


layout(matrix(1:2, 1,2), widths=c(6,1), heights=c(6))

par(mar=c(4,4,1,1), ps=10)
image(tmp$lon, tmp$lat, z=z, col=cols, breaks=zbreaks, useRaster=TRUE, xlab = "longitude",
      ylab = "latitude", cex.lab = 1.5, cex.axis = 1.5, xlim = c(62.5, 65), ylim = c(-68, -66))

#add cruise track over top
image(tmp$lon, tmp$lat, cruise, add = T, col = c(NA, "black"), xlim = c(62.5, 65), ylim = c(-68, -66))


par(mar=c(2,0,1,4))
image(x=1, y=zbreaks, z=matrix(zbreaks, 1, length(zbreaks)), col=cols, breaks=zbreaks, useRaster=TRUE, xlab="", ylab="", axes=FALSE)
axis(4, at=seq(-4000, 7000, 1000), las=2, cex.axis = 1.5)
mtext("elevation (m)", side=4, line=3, cex = 1.5)
box()







