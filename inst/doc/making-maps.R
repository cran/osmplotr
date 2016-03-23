## ----load, message=FALSE-------------------------------------------------
library (maptools)
library (osmplotr)

## ---- echo=FALSE, message=FALSE------------------------------------------
require (devtools)
setwd ("../..")
load_all ("osmplotr")
setwd ("./osmplotr/vignettes")

## ---- echo=FALSE, message=FALSE------------------------------------------
# Combining (dat_B, dat_BC) and (dat_H, dat_HP) requires removing the repeated
# objects
indx <- which (!london$dat_BR$id %in% london$dat_BNR$id)
dat_B <- spRbind (london$dat_BR [indx,], london$dat_BNR)
indx <- which (!london$dat_H$id %in% london$dat_HP$id)
dat_H <- spRbind (london$dat_H [indx,], london$dat_HP)
dat_T <- london$dat_T

## ------------------------------------------------------------------------
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))

## ---- eval=FALSE---------------------------------------------------------
#  dat_B <- extract_osm_objects (key="building", bbox=bbox)$obj

## ----map1, eval=FALSE----------------------------------------------------
#  plot_osm_basemap (bbox=bbox, bg="gray20", file="map1.png")

## ---- echo=FALSE---------------------------------------------------------
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  add_osm_objects (dat_B, col="gray40")

## ----map2, eval=TRUE, echo=FALSE-----------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map2.png", width=200)
add_osm_objects (dat_B, col="gray40")
graphics.off ()

## ------------------------------------------------------------------------
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  dat_B <- extract_osm_objects (key="building", bbox=bbox)$obj
#  dat_H <- extract_osm_objects (key="highway", bbox=bbox)$obj
#  dat_T <- extract_osm_objects (key="natural", value="tree", bbox=bbox)$obj

## ------------------------------------------------------------------------
class (dat_B)
class (dat_H)
class (dat_T)

## ------------------------------------------------------------------------
length (dat_B)
length (dat_H)
length (dat_T)

## ------------------------------------------------------------------------
bbox <- slot (dat_B, "bbox")

## ----map3, eval=TRUE-----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map3.png", width=200)
add_osm_objects (dat_B, col="gray40")
add_osm_objects (dat_H, col="gray70")
graphics.off ()

## ----map4, eval=TRUE-----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map4.png", width=200)
add_osm_objects (dat_B, col="gray40", border="orange", lwd=0.2)
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  dat_BR <- extract_osm_objects (key="building", value="residential",
#                                 bbox=bbox)$obj
#  dat_HP <- extract_osm_objects (key="highway", value="primary", bbox=bbox)$obj

## ---- echo=FALSE---------------------------------------------------------
dat_BR <- london$dat_BR
dat_HP <- london$dat_HP

## ------------------------------------------------------------------------
length (dat_BR)

## ----map5, eval=TRUE-----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map5.png", width=200)
add_osm_objects (dat_BR, col="gray40")
add_osm_objects (dat_HP, col="gray70")
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  dat_H <- extract_osm_objects (key="highway", value="!primary", bbox=bbox)$obj

## ---- echo=FALSE---------------------------------------------------------
dat_H <- london$dat_H

## ----map6, eval=TRUE-----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map6.png", width=200)
add_osm_objects (dat_H, col="gray50")
add_osm_objects (dat_HP, col="gray80")
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  dat_BNR <- extract_osm_objects (key="building", value="!residential",
#                                  bbox=bbox)$obj

## ---- echo=FALSE---------------------------------------------------------
dat_BNR <- london$dat_BNR

## ----map7, eval=TRUE-----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map7.png", width=200)
add_osm_objects (dat_BR, col="gray80")
add_osm_objects (dat_BNR, col="gray40")
graphics.off ()

## ---- eval=FALSE---------------------------------------------------------
#  extra_pairs <- c ("name", "Royal.Festival.Hall")
#  dat_RFH <- extract_osm_objects (key="building", extra_pairs=extra_pairs,
#                                  bbox=bbox)$obj

## ---- eval=FALSE---------------------------------------------------------
#  extra_pairs <- list (c ("addr:street", "Stamford.St"),
#                       c ("addr:housenumber", "150"))
#  dat_ST <- extract_osm_objects (key="building", extra_pairs=extra_pairs,
#                                  bbox=bbox)$obj

## ---- echo=FALSE---------------------------------------------------------
dat_RFH <- london$dat_RFH
dat_ST <- london$dat_ST

## ----map8, eval=TRUE-----------------------------------------------------
bbox <- get_bbox (c (-0.118, 51.504, -0.110, 51.507))
plot_osm_basemap (bbox=bbox, bg="gray95", file="map8.png", width=200)
add_osm_objects (dat_H, col="gray80")
add_osm_objects (dat_HP, col="gray60", lwd=4)
add_osm_objects (dat_RFH, col="orange", border="red", lwd=3)
add_osm_objects (dat_ST, col="skyblue", border="blue", lwd=3)
graphics.off ()

## ------------------------------------------------------------------------
osm_structures ()

## ------------------------------------------------------------------------
osm_structures()$value [1:4]

## ------------------------------------------------------------------------
struct_types <- c ("amenity", "building", "grass", "highway", "natural", "park")
osm_structures (struct_types, col_scheme="light")

## ------------------------------------------------------------------------
names (london)

## ------------------------------------------------------------------------
struct_types <- c ("highway", "highway", "building", "building", "building",
                 "amenity", "grass", "park", "natural")
structures <- osm_structures (structures=struct_types, col_scheme="dark")
structures$value [1] <- "!primary"
structures$value [2] <- "primary"
structures$suffix [2] <- "HP"
structures$value [3] <- "!residential"
structures$value [4] <- "residential"
structures$value [5] <- "commercial"
structures$suffix [3] <- "BNR"
structures$suffix [4] <- "BR"
structures$suffix [5] <- "BC"
structures

## ---- eval=FALSE---------------------------------------------------------
#  extra_pairs <- c ("name", "Royal.Festival.Hall")
#  london$dat_RFH <- extract_osm_objects (key="building", extra_pairs=extra_pairs,
#                                  bbox=bbox)$obj
#  extra_pairs <- list (c ("addr:street", "Stamford.St"),
#                       c ("addr:housenumber", "150"))
#  london$dat_ST <- extract_osm_objects (key="building", extra_pairs=extra_pairs,
#                                  bbox=bbox)$obj

## ----map9, eval=TRUE-----------------------------------------------------
osm_data <- make_osm_map (osm_data=london, structures=structures, 
                          dat_prefix="dat_", file="map9.png", width=200)
graphics.off ()

## ----map10, eval=TRUE----------------------------------------------------
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
osm_data <- make_osm_map (osm_data=london, structures=structures, 
                          dat_prefix="dat_", bbox=bbox, file="map10.png",
                          width=200)
graphics.off ()

## ---- echo=FALSE---------------------------------------------------------
indx <- which (!london$dat_BR$id %in% london$dat_BNR$id)
dat_B <- spRbind (london$dat_BR [indx,], london$dat_BNR)

## ----map11, eval=TRUE----------------------------------------------------
pts <- sp::SpatialPoints (cbind (c (-0.115, -0.125, -0.125, -0.115),
                             c (51.510, 51.510, 51.516, 51.516)))
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
plot_osm_basemap (bbox=bbox, bg="gray20", file="map11.png", width=200)
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40",
                   colmat=FALSE) 
graphics.off ()

## ----map12, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map12.png", width=200)
add_osm_groups (dat_B, pts, col="orange", col_extra="gray40", colmat=FALSE,
                   boundary=0)
graphics.off ()

## ----map13, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map13.png", width=200)
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
graphics.off ()

## ------------------------------------------------------------------------
pts <- sp::SpatialPoints (cbind (c (-0.117, -0.122, -0.122, -0.117),
                                 c (51.512, 51.512, 51.518, 51.518)))

## ----map14, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map14.png", width=200)
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
col_park_in <- rgb (50, 255, 50, maxColorValue=255)
col_park_out <- rgb (50, 155, 50, maxColorValue=255)
add_osm_groups (london$dat_P, groups=pts, col=col_park_in, 
                   col_extra=col_park_out, colmat=FALSE, boundary=0)
graphics.off ()

## ----map15, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map15.png", width=200)
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
add_osm_objects (london$dat_P, col=col_park_out)
col_park_in <- rgb (50, 255, 50, maxColorValue=255)
col_park_out <- rgb (50, 155, 50, maxColorValue=255)
add_osm_groups (london$dat_P, groups=pts, col=col_park_in, 
                   col_extra=col_park_out, colmat=FALSE, boundary=0)
graphics.off ()

## ----map16, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray95", file="map16.png", width=200)
add_osm_groups (dat_B, groups=pts, col="gray40", col_extra="gray85",
                   colmat=FALSE, boundary=1)
add_osm_groups (dat_H, groups=pts, col="gray20", col_extra="gray70",
                   colmat=FALSE, boundary=0)
add_osm_groups (dat_HP, groups=pts, col="gray10", col_extra="white",
                   colmat=FALSE, boundary=0)
graphics.off ()

## ---- fig.width=4--------------------------------------------------------
plot.new ()
cmat <- colour_mat (plot=TRUE)
graphics.off ()

## ---- echo=FALSE---------------------------------------------------------
set.seed (2)

## ------------------------------------------------------------------------
ngroups <- 12
x <- bbox [1,1] + runif (ngroups) * diff (bbox [1,])
y <- bbox [2,1] + runif (ngroups) * diff (bbox [2,])
groups <- cbind (x, y)
groups <- apply (groups, 1, function (i) 
              sp::SpatialPoints (matrix (i, nrow=1, ncol=2)))
# Then create small rectangles around each pts
groups <- lapply (groups, function (i)
               {
                   x <- sp::coordinates (i) [1] + c (-0.002, 0.002, 0.002,
                                                     -0.002)
                   y <- sp::coordinates (i) [2] + c (-0.002, -0.002, 0.002,
                                                     0.002)
                   sp::SpatialPoints (cbind (x, y))
               })

## ----map17, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map17.png", width=200)
add_osm_groups (dat_B, groups=groups, col_extra=NA, make_hull=FALSE,
                   colmat=TRUE, lwd=3)
graphics.off ()

## ----map18, eval=TRUE----------------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20", file="map18.png", width=200)
add_osm_groups (dat_B, groups=groups, col_extra=NA, make_hull=FALSE,
                   colmat=TRUE, rotate=90, lwd=3)
graphics.off ()

## ---- echo=FALSE---------------------------------------------------------
structures <- c ("amenity", "grass", "park", "natural")
structs <- osm_structures (structures=structures, col_scheme="dark")
col_G <- structs$cols [structs$structure == "grass"]
col_A <- structs$cols [structs$structure == "amenity"]

groups <- list (london$highways1, london$highways2, london$highways3)
library (RColorBrewer)
cols_B <- brewer.pal (4, "Set2") [2:4] # first colour is green
# First plot buildings:
st_full <- osm_structures ()
col_extra_B <- st_full$cols [which (st_full$structure == "building")]

# Then darken colours for highways
cols <- 0.6 * col2rgb (cols_B)
# convert to hex:
cols_H <- rep (NA, ncol (cols))
for (i in seq (ncol (cols)))
{
    s <- sprintf ('%X', round (cols [,i]))
    cols_H [i] <- paste0 ("#", s [1], s [2], s [3])
}
col_extra_H <- "gray20"

## ---- echo=FALSE, eval=TRUE----------------------------------------------
plot_osm_basemap (bbox=bbox, bg="gray20")
plot_osm_basemap (bbox=bbox, bg="gray20", file="map19.png", width=200)
add_osm_objects (london$dat_P, col=col_G)
add_osm_objects (london$dat_G, col=col_G)
add_osm_objects (london$dat_N, col=col_G)
add_osm_objects (london$dat_A, col=col_A)
add_osm_groups (london$dat_BNR, groups=groups, boundary=0,
                   col_extra=col_extra_B, colmat=FALSE, col=cols_B)
add_osm_groups (london$dat_BR, groups=groups, boundary=0,
                   col_extra=col_extra_B, colmat=FALSE, col=cols_B)

add_osm_groups (london$dat_H, groups=groups, boundary=0,
                   col_extra=col_extra_H, colmat=FALSE, col=cols_H)
add_osm_groups (london$dat_HP, groups=groups, boundary=0,
                   col_extra=col_extra_H, colmat=FALSE, col=cols_H)
graphics.off ()

## ------------------------------------------------------------------------
bbox <- get_bbox (c(-0.15,51.5,-0.1,51.52)) 

## ---- eval=FALSE---------------------------------------------------------
#  highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
#                 "Fleet.St", "Aldwych")
#  highways1 <- highways2polygon (highways=highways, bbox=bbox)

## ------------------------------------------------------------------------
## Warning in connect_highways(ways): Cycle unable to be extended through all
## ways

## ---- echo=FALSE---------------------------------------------------------
highways1 <- london$highways1

## ------------------------------------------------------------------------
class (highways1)
head (sp::coordinates (highways1))
dim (sp::coordinates (highways1))

## ---- eval=FALSE---------------------------------------------------------
#  highways <- c ("Queen.s.Walk", "Blackfriars", "Waterloo", "The.Cut")
#  highways2 <- highways2polygon (highways=highways, bbox=bbox)
#  highways <- c ("Regent.St", "Oxford.St", "Shaftesbury")
#  highways3 <- highways2polygon (highways=highways, bbox=bbox)

## ---- eval=FALSE---------------------------------------------------------
#  groups <- list (highways1, highways2, highways3)

## ---- echo=FALSE---------------------------------------------------------
groups <- list (london$highways1, london$highways2, london$highways3)

## ---- eval=TRUE----------------------------------------------------------
structures <- c ("amenity", "grass", "park", "natural")
structs <- osm_structures (structures=structures, col_scheme="dark")
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52)) # back to smaller bbox
junk <- make_osm_map (filename="map20.png", bbox=bbox, 
                      osm_data=london, structures=structs, width=200)
graphics.off ()

## ---- message=FALSE------------------------------------------------------
require (RColorBrewer)

## ------------------------------------------------------------------------
cols <- RColorBrewer::brewer.pal (4, "Set1") [2:4] # first colour is green
# darken colours for highways by first converting to rgb
cols_rgb <- 0.6 * col2rgb (cols)
# convert to hex using `sprintf ('%X',...)`
cols_dark <- rep (NA, ncol (cols_rgb))
for (i in seq (ncol (cols_rgb)))
{
    s <- sprintf ('%X', round (cols_rgb [,i]))
    cols_dark [i] <- paste0 ("#", s [1], s [2], s [3])
}
cols
cols_dark

## ------------------------------------------------------------------------
st_all <- osm_structures ()
col_extra_B <- st_all$cols [which (st_all$structure == "building")]
col_extra_H <- "gray20"

## ---- eval=FALSE---------------------------------------------------------
#  add_osm_groups (london$dat_BNR, groups=groups, boundary=0,
#                     col_extra=col_extra_B, colmat=FALSE, col=cols)
#  add_osm_groups (london$dat_BR, groups=groups, boundary=0,
#                     col_extra=col_extra_B, colmat=FALSE, col=cols)
#  
#  add_osm_groups (london$dat_H, groups=groups, boundary=0,
#                     col_extra=col_extra_H, colmat=FALSE, col=cols_dark)
#  add_osm_groups (london$dat_HP, groups=groups, boundary=0,
#                     col_extra=col_extra_H, colmat=FALSE, col=cols_dark)
#  graphics.off ()

## ----highways2polygon, fig.width=4, message=FALSE, eval=TRUE-------------
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
               "Fleet.St", "Aldwych")
bbox <- get_bbox (c(-0.15,51.5,-0.1,51.52)) # need larger bbox
highway_list <- highways2polygon (highways=highways, bbox=bbox, plot=TRUE)

