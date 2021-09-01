install.packages("tidycensus")
install.packages("RColorBrewer")
install.packages("tmap")
install.packages("sf")
install.packages("data.table")
install.packages("leaflet")
install.packages("raster")
install.packages("adehabitatHR")
install.packages("rgeos")
install.packages("maptools")

library(tidyverse)
library(tidycensus)
library(sf)
library(tmap)
library(RColorBrewer)
library(leaflet)
library(data.table)
library(raster) 
library(adehabitatHR)
library(maptools)

crimes <- read_csv("crimes.csv")
beats <- read_csv("PoliceBeatDec2012.csv")
Beat <- st_read("geo_export_87315a10-c9f9-4642-80ea-d542f7e50f47.shp")
bounds <- read_csv("CommAreas.csv")
County <- st_read("geo_export_88b12b3e-52c4-4579-a788-01e1c308be00.shp")
crimes <- crimes[,c(1,5,8, 15,16)]

head(crimes)

pip <- over(County,crimes)
pip <- st_join(House.Points, OA.Census, join = st_within)

glimpse(crimes)
glimpse(bounds)

crimepts <- na.omit(crimes)

Pts <- st_as_sf(crimepts, coords = c("X COORDINATE","Y COORDINATE"), crs = 26916)
plot(Pts)
Pts.sp <- sf:::as_Spatial(Pts)

crimes2<-  st_read("chi.crimes.shp")
crimes3 <- na.omit(crimes2)
crimes.spatial <- sf:::as_Spatial(crimes3)
kde.output <- kernelUD(crimes.spatial, h="href", grid = 1000)
plot(kde.output)

head(crimes)

class(Pts.sp)
glimpse(Pts.sp)

Crimes.dens <- readShapePoints(crimes2)
plot(kde.output)


st_crs(County)  
st_crs(homicides.new)

CRS.new <- st_crs(homicides)
County.new <- st_transform(County, CRS.new)

CRS.new <- st_crs(County)
homicides.new <- st_transform(homicides, CRS.new)

tmap_mode("plot")
tmap_mode("view")

#ask warren
tm_shape(homicides.new) + tm_dots(size = 0.05, col="red" )+ tm_shape(County) + tm_borders(alpha=.5) 

plot(kde.output)
kde <- raster(kde.output)

tm_shape(kde.output2) + tm_raster("ud") + tm_shape(County) + tm_borders(alpha=.5) 
tm_shape(kde.output2) + tm_raster("ud") + tm_shape(Beat) + tm_borders(alpha=.4) 


#s1 is only homecide
s1<-subset(crimes, crimes$`PRIMARY DESCRIPTION`== "HOMICIDE")
subset.csv <- write_csv(s1)
write.csv(s1, "homicides.updated.csv")


s1 <- read_csv("homicides.new.csv")
s2<-na.omit(s1)
head(s1)
homicides.new <- st_as_sf(s2, coords = c("LONGITUDE","LATITUDE"), crs = 4236)
crimes.spatial2 <- sf:::as_Spatial(homicides.new)
kde.output2 <- kernelUD(crimes.spatial2, h="href", grid = 2000)
plot(kde.output2)