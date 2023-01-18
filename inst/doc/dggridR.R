## ---- fig.width=5, fig.height=5, results='hide', warning=FALSE, error=FALSE, message=FALSE, echo=FALSE, fig.align='center'----
# Generate cover picture
library(dggridR)
library(ggplot2)

# Generate grids of various sizes
hgrids <- lapply(3:5, function(res) dgconstruct(res=res))
hgrids <- lapply(hgrids, function(dggs) dgearthgrid(dggs))
hgrids <- lapply(hgrids, function(x) st_wrap_dateline(x, options = c("WRAPDATELINE=YES","DATELINEOFFSET=10"), quiet = TRUE))

countries <- map_data("world")

# Crop generate dgrids to areas of interest
bounds = st_bbox(c(xmin = -90, xmax = 75, ymin = -90, ymax = 90), crs = st_crs(4326))
hgrids[[1]] = hgrids[[1]] %>% st_make_valid() %>% st_filter(st_as_sfc(bounds), .predicate=st_within)
bounds = st_bbox(c(xmin = 20, xmax = 145, ymin = -90, ymax = 90), crs = st_crs(4326))
hgrids[[2]] = hgrids[[2]] %>% st_make_valid() %>% st_filter(st_as_sfc(bounds), .predicate=st_within)
bounds = st_bbox(c(xmin = 90, xmax = 215, ymin = -90, ymax = 90), crs = st_crs(4326))
hgrids[[3]] = hgrids[[3]] %>% st_make_valid() %>% st_filter(st_as_sfc(bounds), .predicate=st_within)

ggplot() +
    geom_polygon(data=countries,  aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    scale_fill_gradient(low="blue", high="red")+
    geom_sf(data=hgrids[[1]], fill=NA, color="#1B9E77")+
    geom_sf(data=hgrids[[2]], fill=NA, color="#D95F02")+
    geom_sf(data=hgrids[[3]], fill=NA, color="#7570B3")+
    # coord_sf(crs="+proj=ortho +lat_0=0 +lon_0=90")+
    xlab('')+ylab('')+
    theme(axis.ticks.x=element_blank())+
    theme(axis.ticks.y=element_blank())+
    theme(axis.text.x=element_blank())+
    theme(axis.text.y=element_blank())

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE---------------
#Include libraries
library(dggridR)
library(dplyr)

#Construct a global grid with cells approximately 1000 miles across
dggs          <- dgconstruct(spacing=1000, metric=FALSE, resround='down')

#Load included test data set
data(dgquakes)

#Get the corresponding grid cells for each earthquake epicenter (lat-long pair)
dgquakes$cell <- dgGEO_to_SEQNUM(dggs,dgquakes$lon,dgquakes$lat)$seqnum

#Converting SEQNUM to GEO gives the center coordinates of the cells
cellcenters   <- dgSEQNUM_to_GEO(dggs,dgquakes$cell)

#Get the number of earthquakes in each cell
quakecounts   <- dgquakes %>% group_by(cell) %>% summarise(count=n())

#Get the grid cell boundaries for cells which had quakes
grid          <- dgcellstogrid(dggs,quakecounts$cell)

#Update the grid cells' properties to include the number of earthquakes
#in each cell
grid          <- merge(grid,quakecounts,by.x="seqnum",by.y="cell")

#Make adjustments so the output is more visually interesting
grid$count    <- log(grid$count)
cutoff        <- quantile(grid$count,0.9)
grid          <- grid %>% mutate(count=ifelse(count>cutoff,cutoff,count))

#Get polygons for each country of the world
countries <- map_data("world")

## ---- fig.width=6, fig.height=4-----------------------------------------------
#Plot everything on a flat map

# Handle cells that cross 180 degrees
wrapped_grid = st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)

ggplot() +
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_sf     (data=wrapped_grid, aes(fill=count), color=alpha("white", 0.4)) +
    geom_point  (aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) +
    scale_fill_gradient(low="blue", high="red")

## ---- fig.width=6, fig.height=6, echo=FALSE, results='hide'-------------------
#Replot on a spherical projection
ggplot() +
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_sf     (data=grid, aes(fill=count), color=alpha("white", 0.4)) +
    geom_point  (aes(x=cellcenters$lon_deg, y=cellcenters$lat_deg)) +
    scale_fill_gradient(low="blue", high="red") +
    # coord_sf(crs="+proj=ortho +lat_0=20 +lon_0=90")+
    xlab('')+ylab('')+
    theme(axis.ticks.x=element_blank())+
    theme(axis.ticks.y=element_blank())+
    theme(axis.text.x=element_blank())+
    theme(axis.text.y=element_blank())+
    ggtitle('Your data could look like this')

## ---- eval=FALSE--------------------------------------------------------------
#  library(sf)
#  
#  #Get the grid cell boundaries for the whole Earth using this dggs in a form
#  #suitable for printing to a KML file
#  grid <- dgearthgrid(dggs)
#  
#  #Update the grid cells' properties to include the number of earthquakes
#  #in each cell
#  grid$count <- merge(grid, quakecounts, by.x="seqnum", by.y="cell", all.x=TRUE)
#  
#  #Write out the grid
#  st_write(grid, "quakes_per_cell.kml", layer="quakes", driver="KML")

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE---------------
#Include libraries
library(dggridR)
library(dplyr)

N <- 100    #How many cells to sample

#Distribute the points uniformly on a sphere using equations from
#http://mathworld.wolfram.com/SpherePointPicking.html
u     <- runif(N)
v     <- runif(N)
theta <- 2*pi*u      * 180/pi
phi   <- acos(2*v-1) * 180/pi
lon   <- theta-180
lat   <- phi-90

df    <- data.frame(lat=lat,lon=lon)

#Construct a global grid in which every hexagonal cell has an area of
#100,000 miles^2. You could, of course, choose a much smaller value, but these
#will show up when I map them later.

#Note: Cells can only have certain areas, the `dgconstruct()` function below
#will tell you which area is closest to the one you want. You can also round
#up or down.

#Note: 12 cells are actually pentagons with an area 5/6 that of the hexagons
#But, with millions and millions of hexes, you are unlikely to choose one
#Future versions of the package will make it easier to reject the pentagons
dggs    <- dgconstruct(area=100000, metric=FALSE, resround='nearest')

#Get the corresponding grid cells for each randomly chosen lat-long
df$cell <- dgGEO_to_SEQNUM(dggs,df$lon,df$lat)$seqnum

#Get the hexes for each of these cells
gridfilename <- dgcellstogrid(dggs,df$cell)

## ---- fig.width=6, fig.height=4-----------------------------------------------
#Get the grid in a more convenient format
grid <- dgcellstogrid(dggs,df$cell)
grid <- st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)

#Get polygons for each country of the world
countries <- map_data("world")

#Plot everything on a flat map
p <- ggplot() +
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_sf(data=grid, fill=alpha("green", alpha=0.4), color=alpha("white", alpha=0.4))
p

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE---------------
#Include libraries
library(dggridR)
library(dplyr)

N <- 100    #How many cells to sample

#Construct a global grid in which every hexagonal cell has an area of
#100,000 miles^2. You could, of course, choose a much smaller value, but these
#will show up when I map them later.

#Note: Cells can only have certain areas, the `dgconstruct()` function below
#will tell you which area is closest to the one you want. You can also round
#up or down.

#Note: 12 cells are actually pentagons with an area 5/6 that of the hexagons
#But, with millions and millions of hexes, you are unlikely to choose one
#Future versions of the package will make it easier to reject the pentagons
dggs    <- dgconstruct(area=100000, metric=FALSE, resround='nearest')

maxcell <- dgmaxcell(dggs)                     #Get maximum cell id
cells   <- sample(1:maxcell, N, replace=FALSE) #Choose random cells
grid    <- dgcellstogrid(dggs,cells)           #Get grid

## ---- fig.width=6, fig.height=4-----------------------------------------------
#Get the grid in a more convenient format
grid <- dgcellstogrid(dggs,df$cell)
grid <- st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)

#Get polygons for each country of the world
countries <- map_data("world")

#Plot everything on a flat map
p <- ggplot() +
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_sf(data=grid, fill=alpha("green", 0.4), color=alpha("white", 0.4))
p

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE---------------
library(dggridR)
#Generate a global grid whose cells are ~100,000 miles^2
dggs         <- dgconstruct(area=100000, metric=FALSE, resround='nearest')
#Save the cells to a KML file for use in other software
gridfilename <- dgearthgrid(dggs,savegrid=tempfile())

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE, fig.align='center', fig.width=5, fig.height=5----
library(dggridR)

#Generate a dggs specifying an intercell spacing of ~25 miles
dggs      <- dgconstruct(spacing=100, metric=FALSE, resround='nearest')

#Read in the South Africa's borders from the shapefile
sa_border <- st_read(dg_shpfname_south_africa(), layer="ZAF_adm0")
st_crs(sa_border) = 4326

#Get a grid covering South Africa
sa_grid   <- dgshptogrid(dggs, dg_shpfname_south_africa())

#Plot South Africa's borders and the associated grid
p <- ggplot() +
    geom_sf(data=sa_border, fill=NA, color="black")   +
    geom_sf(data=sa_grid, fill=alpha("blue", 0.4), color=alpha("white", 0.4))
p

## ---- results='hide', warning=FALSE, error=FALSE, message=FALSE, fig.align='center', fig.width=6, fig.height=4, echo=FALSE----

lat <- c(90,-90,26.57,-26.57,26.57,-26.57,26.57,-26.57,26.57,-26.57,26.57,-26.57)
lon <- c(0,0,0,36,72,108,144,180,216,252,288,324)

dggs  <- dgconstruct(area=100000, metric=FALSE, resround='nearest')
cells <- dgGEO_to_SEQNUM(dggs,lon,lat)$seqnum
grid  <- dgcellstogrid(dggs,cells) #Get grid
grid  <- st_wrap_dateline(grid, options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), quiet = TRUE)

#Get polygons for each country of the world
countries <- map_data("world")

#Plot everything on a flat map
p <- ggplot() +
    geom_polygon(data=countries, aes(x=long, y=lat, group=group), fill=NA, color="black")   +
    geom_sf(data=grid, fill=alpha("purple", 0.6), color=alpha("white", 0.4))
p

