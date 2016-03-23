This `R` package is designed to produce visually impressive graphical plots of OpenStreetMap (OSM) data. A particular feature of `osmplotr` is the ability to highlight selected areas using user-definable graphical styles which contrast with the remaining background of a map, like this:

![map](map16.png)

This ability a not currently facilitated by any other `R` package. This map, and all of the following examples, displays results for a small portion of central London, U.K.

1. Introduction
===============

Before demonstrating how particular regions may be selected and highlighted, this vignette describes how `osmplotr` may be used to generate visually customised plots of OSM data. `osmplotr` downloads data directly from the [overpass API](https://overpass-api.de). The following simple steps produce the subsequent map:

``` r
library (maptools)
library (osmplotr)
```

1.  Specify the bounding box for the desired region

``` r
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
```

1.  Download the desired data---in this case, all building perimeters.

``` r
dat_B <- extract_osm_objects (key="building", bbox=bbox)$obj
```

1.  Initiate an `osm_basemap` with desired background (`bg`) colour

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map1.png")
```

1.  Add desired plotting objects in the desired colour.

``` r
add_osm_objects (dat_B, col="gray40")
```

![map2](map2.png)

1.  When finished, close graphics device

``` r
graphics.off ()
```

Additional capabilities of `osmplotr` are described in the following sections.

2. Combining multiple OSM objects
---------------------------------

As illustrated above, OSM data for particular types of objects can be downloaded using `extract_osm_objects`.

``` r
dat_B <- extract_osm_objects (key="building", bbox=bbox)$obj
dat_H <- extract_osm_objects (key="highway", bbox=bbox)$obj
dat_T <- extract_osm_objects (key="natural", value="tree", bbox=bbox)$obj
```

The `extract_osm_objects` function returns a list with `warn` containing any warnings generated during download, and `obj` containing the spatial (`sp`) `data.frame` objects of appropriate types.

``` r
class (dat_B)
```

    ## [1] "SpatialPolygonsDataFrame"
    ## attr(,"package")
    ## [1] "sp"

``` r
class (dat_H)
```

    ## [1] "SpatialLinesDataFrame"
    ## attr(,"package")
    ## [1] "sp"

``` r
class (dat_T)
```

    ## [1] "SpatialPointsDataFrame"
    ## attr(,"package")
    ## [1] "sp"

The `SpatialPolygonsDataFrame`, `SpatialLinesDataFrame`, and `SpatialPointsDataFrame` of London buildings, highways, and trees respectively contain

``` r
length (dat_B)
```

    ## [1] 2178

``` r
length (dat_H)
```

    ## [1] 2139

``` r
length (dat_T)
```

    ## [1] 1310

... 2,178 building polygons, 2,139 highway lines, and 1,310 trees.

As illustrated above, plotting maps requires first making a basemap with a specified background colour. The basemap also defines the dimensions of the plot, which are scaled by default in proportion to the latitudinal and longitudinal range of the objects to be plotted. This default scaling may be overridden by passing any other desired longitudinal and latitudinal limits to the `bbox` argument of `plot_osm_basemap`. Objects are overlaid on basemaps using `add_osm_objects`.

``` r
bbox <- slot (dat_B, "bbox")
```

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map3.png")
add_osm_objects (dat_B, col="gray40")
add_osm_objects (dat_H, col="gray70")
graphics.off ()
```

![map3](map3.png)

Other graphical parameters can also be passed to `add_osm_objects`, such as border colours or line widths and types. For example,

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map4.png")
add_osm_objects (dat_B, col="gray40", border="orange", lwd=0.2)
graphics.off ()
```

![map4](map4.png)

The `osmplotr` package is intended to produce high quality graphical output written to particular graphic devices such as `png` or `jpeg` (see `?png` for a list of possible devices). Map production generally involves the sequential addition of objects, for which the graphic device must remain open. It is important once finished a map to close the device with `dev.off` or `graphics.off`.

3. OSM Structures
-----------------

OSM structures are identified through `key-value` pairs. The preceding calls to `extract_osm_objects` did not specify any values, and so returned all objects matching the `key`, regardless of value. Particular values can also be requested:

``` r
dat_BR <- extract_osm_objects (key="building", value="residential",
                               bbox=bbox)$obj
dat_HP <- extract_osm_objects (key="highway", value="primary", bbox=bbox)$obj
```

``` r
length (dat_BR)
```

    ## [1] 40

There are only 40 buildings in this part of central London marked as 'residential'.

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map5.png")
add_osm_objects (dat_BR, col="gray40")
add_osm_objects (dat_HP, col="gray70")
graphics.off ()
```

![map5](map5.png)

### 3a. Negation

Values can also be negated through the prefix `!`---for example, non-primary highways can be extracted, allowing primary highways to be overlaid in a different colour.

``` r
dat_H <- extract_osm_objects (key="highway", value="!primary", bbox=bbox)$obj
```

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map6.png")
add_osm_objects (dat_H, col="gray50")
add_osm_objects (dat_HP, col="gray80")
graphics.off ()
```

![map6](map6.png)

Or non-residential buildings can be extracted.

``` r
dat_BNR <- extract_osm_objects (key="building", value="!residential",
                                bbox=bbox)$obj
```

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map7.png")
add_osm_objects (dat_BR, col="gray80")
add_osm_objects (dat_BNR, col="gray40")
graphics.off ()
```

![map7](map7.png)

### 3b. Additional `key-value` pairs

`extract_osm_objects` accepts an additional argument `extra_pairs` through which additional OSM `key-value` pairs can be passed to the [overpass API](https://overpass-api.de). For example, the polygon of a particular building can be extracted by passing its name:

``` r
extra_pairs <- c ("name", "Royal.Festival.Hall")
dat_RFH <- extract_osm_objects (key="building", extra_pairs=extra_pairs, 
                                bbox=bbox)$obj
```

Or a street address can be given:

``` r
extra_pairs <- list (c ("addr:street", "Stamford.St"),
                     c ("addr:housenumber", "150"))
dat_ST <- extract_osm_objects (key="building", extra_pairs=extra_pairs, 
                                bbox=bbox)$obj
```

As mentioned, additional graphics arguments can be passed to `add_osm_objects`, as illustrated in the following.

``` r
bbox <- get_bbox (c (-0.118, 51.504, -0.110, 51.507))
plot_osm_basemap (bbox=bbox, bg="gray95", file="map8.png", width=480)
add_osm_objects (dat_H, col="gray80")
add_osm_objects (dat_HP, col="gray60", lwd=4)
add_osm_objects (dat_RFH, col="orange", border="red", lwd=3)
add_osm_objects (dat_ST, col="skyblue", border="blue", lwd=3)
graphics.off ()
```

![map8](map8.png)

Note that `add_osm_objects` calls `polypath` to fill polygons, and this function can only fill polygons with solid colours, as described in `?polypath`.

4. Automating map production
----------------------------

Production of complete maps overlaying various type of OSM objects is facilitated with `make_osm_map`. The structure of a map is defined by `osm_structures`, which returns a `data.frame` containing OSM `key-value` pairs and associated colours.

``` r
osm_structures ()
```

    ##     structure      key value suffix      cols
    ## 1    building building           BU #646464FF
    ## 2     amenity  amenity            A #787878FF
    ## 3    waterway waterway            W #646478FF
    ## 4       grass  landuse grass      G #64A064FF
    ## 5     natural  natural            N #647864FF
    ## 6        park  leisure  park      P #647864FF
    ## 7     highway  highway            H #000000FF
    ## 8    boundary boundary           BO #C8C8C8FF
    ## 9        tree  natural  tree      T #64A064FF
    ## 10 background                          gray20

`osm_structures` recognises many common structures and converts them into `key-value` pairs which can be submitted to the [overpass API](https://overpass-api.de). Many structures are identified by keys only, in which cases the values are empty strings.

``` r
osm_structures()$value [1:4]
```

    ## [1] ""      ""      ""      "grass"

The last row of `osm_structures` exists only to define the background colour of the map. Objects in maps are overlaid on the plot accoording to the order of rows in `osm_structures` (with the exception that `background` is plotted first). This order can be readily changed or restricted simply by submitting structures in a desired order.

``` r
struct_types <- c ("amenity", "building", "grass", "highway", "natural", "park")
osm_structures (struct_types, col_scheme="light")
```

    ##    structure      key value suffix      cols
    ## 1    amenity  amenity            A #DCDCDCFF
    ## 2   building building            B #C8C8C8FF
    ## 3      grass  landuse grass      G #C8FFC8FF
    ## 4    highway  highway            H #969696FF
    ## 5    natural  natural            N #C8DCC8FF
    ## 6       park  leisure  park      P #C8DCC8FF
    ## 7 background                          gray95

In addition to `osm_structures`, one of the arguments which may be passed to `make_osm_map` is `osm_data`. If NULL (default), then all data passed in the `structures` argument are downloaded and returned after making the map. Any data that have already been downloaded may be passed (as a list) as `osm_data`. Each item of this list must be named by combining the given `dat_prefix` with the suffix given in `osm_structures`. Any additional data present in `osm_structures` yet not in `osm_data` will be downloaded, appended to `osm_data` and returned from `make_osm_map`.

`osmplotr` includes example data for a small area of central London, U.K. ---see `?london`

``` r
names (london)
```

    ##  [1] "dat_H"     "dat_HP"    "dat_BNR"   "dat_BR"    "dat_BC"   
    ##  [6] "dat_A"     "dat_G"     "dat_P"     "dat_N"     "dat_T"    
    ## [11] "dat_RFH"   "dat_ST"    "highways1" "highways2" "highways3"

The `osm_structures` describing these data were obtained by modifying a call to `osm_structures` as follows:

``` r
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
```

    ##     structure      key        value suffix      cols
    ## 1     highway  highway     !primary      H #000000FF
    ## 2     highway  highway      primary     HP #000000FF
    ## 3    building building !residential    BNR #646464FF
    ## 4    building building  residential     BR #646464FF
    ## 5    building building   commercial     BC #646464FF
    ## 6     amenity  amenity                   A #787878FF
    ## 7       grass  landuse        grass      G #64A064FF
    ## 8        park  leisure         park      P #647864FF
    ## 9     natural  natural                   N #647864FF
    ## 10 background                                 gray20

The `london` data also include polygons for the two particular buildings shown above, which were extracted as follows.

``` r
extra_pairs <- c ("name", "Royal.Festival.Hall")
london$dat_RFH <- extract_osm_objects (key="building", extra_pairs=extra_pairs, 
                                bbox=bbox)$obj
extra_pairs <- list (c ("addr:street", "Stamford.St"),
                     c ("addr:housenumber", "150"))
london$dat_ST <- extract_osm_objects (key="building", extra_pairs=extra_pairs, 
                                bbox=bbox)$obj
```

Having appropriately modified a data frame of `osm_structures`, a corresponding map may then be produced through submitting these structures, and any accompanying data, to `make_osm_map`:

``` r
osm_data <- make_osm_map (osm_data=london, structures=structures, 
                          dat_prefix="dat_", file="map9.png")
graphics.off ()
```

![map9](map9.png)

Because no bounding box was passed to `make_osm_map`, a bounding box is extracted as the **largest** box spanning all objects in `osm_data`. These objects include the highways which extend notably further to the north and west than the actual bounding box used to extract these highways. Passing the previous bounding box to the same call gives:

``` r
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
osm_data <- make_osm_map (osm_data=london, structures=structures, 
                          dat_prefix="dat_", bbox=bbox, file="map10.png")
graphics.off ()
```

![map10](map10.png)

### 4.1 A note on rivers

OSM objects are extracted by the `osmar` package which does not currently handle the extraction of rivers in a failsafe way. Rivers are generally defined by `(key,value)=('waterway','riverbank')`, with relations returned as an OSM `multipolygon`. These `multipolygon` objects are, however, **not** extracted by `osmar` when they extend beyond a requested `bbox` (and there is no way to know in advance whether or not that may be the case), preventing rivers from being included in plots. This problem will be addressed in future releases of `osmplotr`.

5. Highlighting particular areas
--------------------------------

One of the primary aims of `osmplotr` is to offer a convenient means to highlight particular regions within a city simply by applying different colours to the same OSM structures. The routine which enables this is `add_osm_groups`, the two primary arguments to which are `obj`, which defines the OSM structure to be used for plotting the regions (for example, `dat_B` defining the previous buildings), and `groups` which is a list of SpatialPoints objects defining the desired regions.

The simplest way of defining a region is with `click_map`, which enables a map to be clicked and returns a set of `SpatialPoints` corresponding to the clicks. (`click_map` finishes when the same point is clicked twice.) This set of points, or a set of points generated through any other means, can then be passed as the `groups` argument to `add_osm_groups`. The following illustrates an area defined by manually entering coordinates of bounding points.

``` r
pts <- sp::SpatialPoints (cbind (c (-0.115, -0.125, -0.125, -0.115),
                             c (51.510, 51.510, 51.516, 51.516)))
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52))
plot_osm_basemap (bbox=bbox, bg="gray20", file="map11.png")
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40",
                   colmat=FALSE) 
graphics.off ()
```

![map11](map11.png)

### 5.1 Inclusive, exclusive, and bisected polygons

The highlighted region of the previous map is irregular because inclusion for each polygon within a group is defined by mean coordinates. `add_osm_groups` has a `boundary` argument which defines whether objects should be assigned to groups inclusively (`boundary>0`) or exclusively (`boundary<0`), or whether they should be precisely bisected by a group boundary (`boundary=0`). The two options in addition to the above default of `boundary=-1` produce the following maps.

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map12.png")
add_osm_groups (dat_B, pts, col="orange", col_extra="gray40", colmat=FALSE,
                   boundary=0)
graphics.off ()
```

![map12](map12.png)

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map13.png")
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
graphics.off ()
```

![map13](map13.png)

The ability to combine inclusive and bisected polygons is particularly useful when selected areas partially contain large polygons such as parks. The following map is created with buildings plotting *inclusively* within the group, and parks bisected by the boundary.

``` r
pts <- sp::SpatialPoints (cbind (c (-0.117, -0.122, -0.122, -0.117),
                                 c (51.512, 51.512, 51.518, 51.518)))
```

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map14.png")
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
col_park_in <- rgb (50, 255, 50, maxColorValue=255)
col_park_out <- rgb (50, 155, 50, maxColorValue=255)
add_osm_groups (london$dat_P, groups=pts, col=col_park_in, 
                   col_extra=col_park_out, colmat=FALSE, boundary=0)
graphics.off ()
```

![map14](map14.png)

Bisection allocates points either to within or beyond a given boundary, with resultant polygons generally separated by a visible gap between locations at which the polygons are defined. Because plotting is progressively overlaid, such gaps can nevertheless be avoided simply by initially plotting underlying layers prior to grouping objects:

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map15.png")
add_osm_groups (dat_B, groups=pts, col="orange", col_extra="gray40", 
                   colmat=FALSE, boundary=1)
add_osm_objects (london$dat_P, col=col_park_out)
col_park_in <- rgb (50, 255, 50, maxColorValue=255)
col_park_out <- rgb (50, 155, 50, maxColorValue=255)
add_osm_groups (london$dat_P, groups=pts, col=col_park_in, 
                   col_extra=col_park_out, colmat=FALSE, boundary=0)
graphics.off ()
```

![map15](map15.png)

### 5.2 Dark-on-Light Highlights

A particularly effective way to highlight particular areas is through using dark colours upon otherwise light coloured maps.

``` r
plot_osm_basemap (bbox=bbox, bg="gray95", file="map16.png")
add_osm_groups (dat_B, groups=pts, col="gray40", col_extra="gray85",
                   colmat=FALSE, boundary=1)
add_osm_groups (dat_H, groups=pts, col="gray20", col_extra="gray70",
                   colmat=FALSE, boundary=0)
add_osm_groups (dat_HP, groups=pts, col="gray10", col_extra="white",
                   colmat=FALSE, boundary=0)
graphics.off ()
```

![map16](map16.png)

### 5.3 The Colour Matrix: Colouring Several Regions

Individual studies of particular regions are almost always based on *representative* data sampled at some particular set of points. Presuming such sample points to represent the underlying sample structure, they are commonly subject to clustering analyses of some form or other, resuling in a spatial partition between clusters (whether potentially overlapping or not). In such cases, every location within a given plot will be considered to belong to some particular group(s), yet the plot must distinguish these groups by colour alone. Beyond a handful of groups, manually devising an appropriate colour scheme may become difficult.

`osmplotr` offers a convenient way to allocate systematiclly distinct colours to spatially distinct groups with the `colour_mat` function.

``` r
plot.new ()
cmat <- colour_mat (plot=TRUE)
```

![](making-maps_files/figure-markdown_github/unnamed-chunk-30-1.png)<!-- -->

``` r
graphics.off ()
```

This function accepts a vector of 4 or more colours assinged to each corner of a rectangular grid of defined size(s). The interior of the grid is then filled through interpolation. The default colours are `rainbow (4)` (or red, green, violet, blue), as illustrated above.

Regional groups may be coloured using `colour_mat` by setting `colmat=TRUE` in `add_osm_groups` and by submitting a desired vector of colours. A `colour_mat` may be illustrated by plotting inclusive groups defined by random points. `add_osm_groups` first discerns which components (polygons or lines) lie *within* groups, then if `col_extra=NULL` (or `NA`), all points are assigned to the nearest group. This nevertheless requires the initial groups to be of non-zero size, and so the following lines initially select random points and then extend them by creating small rectangles around each.

``` r
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
```

The `ngroups` can then simply be plotted as follows.

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map17.png")
add_osm_groups (dat_B, groups=groups, col_extra=NA, make_hull=FALSE,
                   colmat=TRUE, lwd=3)
graphics.off ()
```

![map17](map17.png)

The `colour_mat` function has an option to rotate the colour space. This may also be passed directly to `add_osm_groups` enabling, for example, the colours in the previous plot to be rotated by 90 degrees.

``` r
plot_osm_basemap (bbox=bbox, bg="gray20", file="map18.png")
add_osm_groups (dat_B, groups=groups, col_extra=NA, make_hull=FALSE,
                   colmat=TRUE, rotate=90, lwd=3)
graphics.off ()
```

![map18](map18.png)

6 Bounding areas within named highways
--------------------------------------

The function `highways2polygon` takes a list of OSM highway names and a bounding box, and returns the boundary of a polygon encircling the named highways. This can be used to highlight selected regions simply by naming the highways which encircle them, producing maps which look like this:

![map19](map19.png)

Note that this map was generated with the `london` data provided with `osmplotr`. The full polygons surrounding the named areas can be seen by repeating the extraction of data with a larger bounding box:

``` r
bbox <- get_bbox (c(-0.15,51.5,-0.1,51.52)) 
```

### 6.1 The highways2polygon function

The primary function enabling the delination of groups like the above is `highways2polygon`, an example of which is the orange area above, the boundary of which was obtained from:

``` r
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
               "Fleet.St", "Aldwych")
highways1 <- highways2polygon (highways=highways, bbox=bbox)
```

``` r
## Warning in connect_highways(ways): Cycle unable to be extended through all
## ways
```

The reason for the warning will be explored further below. In the meantime, note that,

``` r
class (highways1)
```

    ## [1] "SpatialPoints"
    ## attr(,"package")
    ## [1] "sp"

``` r
head (sp::coordinates (highways1))
```

    ##              x        y
    ## 474 -0.1050311 51.51721
    ## 147 -0.1050755 51.51722
    ## 146 -0.1052826 51.51729
    ## 145 -0.1054163 51.51734
    ## 144 -0.1055184 51.51737
    ## 143 -0.1057503 51.51745

``` r
dim (sp::coordinates (highways1))
```

    ## [1] 177   2

``` r
highways <- c ("Queen.s.Walk", "Blackfriars", "Waterloo", "The.Cut")
highways2 <- highways2polygon (highways=highways, bbox=bbox)
highways <- c ("Regent.St", "Oxford.St", "Shaftesbury")
highways3 <- highways2polygon (highways=highways, bbox=bbox)
```

Multiple regions may be highlighted simply by passing a list of bounding polygons (each of class `SpatialPoints`) to `add_osm_groups`.

``` r
groups <- list (highways1, highways2, highways3)
```

Highlighted groups are then added as above using `add_osm_groups`. These are overlaid on top of a basemap. The following maps highlight the selected regions with both buildings and highways, while the remaining structures are plotted on the initial basemap with the following lines. (Note that `make_osm_map` returns `osm_data` with any additional structures not previously present added; in the following case, these data are not altered.)

``` r
structures <- c ("amenity", "grass", "park", "natural")
structs <- osm_structures (structures=structures, col_scheme="dark")
bbox <- get_bbox (c(-0.13,51.5,-0.11,51.52)) # back to smaller bbox
junk <- make_osm_map (filename="map20.png", bbox=bbox, 
                      osm_data=london, structures=structs)
graphics.off ()
```

![map20](map20.png)

The desired areas are then highlighted in the following lines using colours from `RColorBrewer`. These lines also demonstrate how particular colours may be lightened or darkened for use as highlights.

``` r
require (RColorBrewer)
```

``` r
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
```

    ## [1] "#377EB8" "#4DAF4A" "#984EA3"

``` r
cols_dark
```

    ## [1] "#214C6E" "#2E692C" "#5B2F62"

Then define non-highlighted colours for highways and buildings

``` r
st_all <- osm_structures ()
col_extra_B <- st_all$cols [which (st_all$structure == "building")]
col_extra_H <- "gray20"
```

And finally add groups to plot using these colours, resulting in the plot shown at the start of this section.

``` r
add_osm_groups (london$dat_BNR, groups=groups, boundary=0,
                   col_extra=col_extra_B, colmat=FALSE, col=cols)
add_osm_groups (london$dat_BR, groups=groups, boundary=0,
                   col_extra=col_extra_B, colmat=FALSE, col=cols)

add_osm_groups (london$dat_H, groups=groups, boundary=0,
                   col_extra=col_extra_H, colmat=FALSE, col=cols_dark)
add_osm_groups (london$dat_HP, groups=groups, boundary=0,
                   col_extra=col_extra_H, colmat=FALSE, col=cols_dark)
graphics.off ()
```

The extraction of bounding polygons from named highways is not failsafe, as demonstrated by the above error message. To understand why it may not work, it is usefull to examine `highways2polygons` in more detail, as follows.

### 6.2 highways2polygon in detail

`highways2polygon` finds a sequential polygon that circularly connects the named highways. Cases where no circular connection is possible generate an error message. The function which actually connects the given highways into a circular sequence is `connect_highways`, which, as explained in `?connect_highways`,

> Takes a list of OpenStreetMap highways returned by `extract_highways` and sequentially connects closest nodes of adjacent highways until the set of highways connects to form a cycle.

`connect_highways` proceeds through the three stages of,

1.  Adding intersection nodes to junctions of ways where these don't already exist

2.  Filling a connectivity matrix between the listed highways and extracting the **longest** cycle connecting them all

3.  Inserting extra connections between highways until the length of the longest cycle is equal to `length (highways)`.

However, even once the highways are connected, the individual components of each highway may not necessarily connect in a continuous manner to complete the cycle. The final task, completed within the `highways2polygons` routine, is thus ensuring that the components of each individual highway actually connect, through sequentially connecting the closest pair of components until a shortest path is possible between the two components which connect with other highways.

This procedure can not be guaranteed failsafe owing both to the inherently unpredictable nature of OpenStreetMap, as well as to the unknown relationships between named highways. To enable problematic cases to be examined and hopefully resolved, `highways2polygons` has a `plot` option:

``` r
highways <- c ("Kingsway", "Holborn", "Farringdon.St", "Strand",
               "Fleet.St", "Aldwych")
bbox <- get_bbox (c(-0.15,51.5,-0.1,51.52)) # need larger bbox
highway_list <- highways2polygon (highways=highways, bbox=bbox, plot=TRUE)
```

    ## Downloading OSM data ...
    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===========                                                      |  17%
      |                                                                       
      |======================                                           |  33%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |===========================================                      |  67%

    ## Warning in extract_highways(highway_names = highways, bbox = bbox): No
    ## valid data for name=(Fleet.St)

    ## 
      |                                                                       
      |======================================================           |  83%
      |                                                                       
      |=================================================================| 100%
    ## Failed to download all data, trying again (#1/20) ...
    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |===========                                                      |  17%
      |                                                                       
      |======================                                           |  33%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |===========================================                      |  67%
      |                                                                       
      |======================================================           |  83%
      |                                                                       
      |=================================================================| 100%

    ## Warning in connect_highways(ways): Cycle unable to be extended through all
    ## ways

![](making-maps_files/figure-markdown_github/highways2polygon-1.png)<!-- --> The plot depicts each highway in a different colour, along with numbers at start and end points of each segements. This plot reveals in this case that highway\#6 ('Aldwych') is actually nested within two components of highway\#4 ('Strand'). `connect_highways` searches for the shortest path connecting all named highways, and since 'Strand' connects to both highways\#1 and \#5, the shortest path excludes \#6. This exclusion of one of the named components generates the warning message.
