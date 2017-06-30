## ----load, message = FALSE, eval = FALSE---------------------------------
#  library (osmplotr)

## ---- echo = FALSE, message = FALSE--------------------------------------
devtools::load_all ("../.", export_all = FALSE)
map_dpi <- 72 # dpi res for all maps

## ------------------------------------------------------------------------
bbox <- get_bbox (c(-0.13, 51.51, -0.11, 51.52))

## ---- echo = FALSE, message = FALSE--------------------------------------
dat_B <- rbind (london$dat_BR, london$dat_BNR)
dat_H <- rbind (london$dat_H, london$dat_HP)
dat_HP <- london$dat_HP

## ---- eval = FALSE-------------------------------------------------------
#  dat_B <- extract_osm_objects (key = 'building', bbox = bbox)

## ------------------------------------------------------------------------
dat_B <- rbind (london$dat_BNR, london$dat_BR)

## ----map1, eval = FALSE--------------------------------------------------
#  pts <- cbind (c (-0.115, -0.125, -0.125, -0.115),
#                c (51.513, 51.513, 51.517, 51.517))
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = pts, cols = 'orange', bg = 'gray40')
#  print_osm_map (map)

## ----map1-print, echo = FALSE--------------------------------------------
pts <- cbind (c (-0.115, -0.125, -0.125, -0.115),
              c (51.513, 51.513, 51.517, 51.517))
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = pts, cols = 'orange', bg = 'gray40')
print_osm_map (map, filename = 'map_b1.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map2, eval = FALSE--------------------------------------------------
#  pts2 <- cbind (c (-0.111, -0.1145, -0.1145, -0.111),
#                 c (51.517, 51.517, 51.519, 51.519))
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2),
#                         cols = c ('orange', 'tomato'), bg = 'gray40')
#  print_osm_map (map)

## ----map2-print, echo = FALSE--------------------------------------------
pts2 <- cbind (c (-0.111, -0.1145, -0.1145, -0.111),
               c (51.517, 51.517, 51.519, 51.519))
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2),
                       cols = c ('orange', 'tomato'), bg = 'gray40')
print_osm_map (map, filename = 'map_b2.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map3, eval = FALSE--------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2),
#                         cols = c ('orange', 'tomato'))
#  print_osm_map (map)

## ----map3-print, echo = FALSE--------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2),
                       cols = c ('orange', 'tomato'))
print_osm_map (map, filename = 'map_b3.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map5, eval = FALSE--------------------------------------------------
#  pts <- rbind (pts, c (-0.12, 51.515))
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = pts, cols = 'orange', bg = 'gray40')
#  print_osm_map (map)

## ----map5-print, echo = FALSE--------------------------------------------
pts <- rbind (pts, c (-0.12, 51.515))
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = pts, cols = 'orange', bg = 'gray40')
print_osm_map (map, filename = 'map_b5.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map6, eval = FALSE--------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
#                         cols = c ('orange', 'tomato'), bg = 'gray40',
#                         boundary = 1)
#  print_osm_map (map)

## ----map6-print, echo = FALSE--------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
                       cols = c ('orange', 'tomato'), bg = 'gray40',
                       boundary = 1)
print_osm_map (map, filename = 'map_b6.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map7, eval = FALSE--------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
#                         cols = c ('orange', 'tomato'), bg = 'gray40',
#                         boundary = 0)
#  print_osm_map (map)

## ----map7-print, echo = FALSE--------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
                       cols = c ('orange', 'tomato'), bg = 'gray40',
                       boundary = 0)
print_osm_map (map, filename = 'map_b7.png', width = 600,
               units = 'px', dpi = map_dpi)

## ---- eval = FALSE-------------------------------------------------------
#  dat_P <- extract_osm_objects (key = 'park', bbox = bbox)

## ------------------------------------------------------------------------
osm_structures (structure = 'park')

## ---- echo = FALSE-------------------------------------------------------
dat_P <- london$dat_P

## ----map8, eval = FALSE--------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
#                         cols = c ('orange', 'tomato'), bg = 'gray40',
#                         boundary = 0)
#  col_park_in <- rgb (50, 255, 50, maxColorValue = 255)
#  col_park_out <- rgb (50, 155, 50, maxColorValue = 255)
#  map <- add_osm_groups (map, dat_P, groups = list (pts, pts2),
#                         cols = rep (col_park_in, 2), bg = col_park_out,
#                         boundary = 0)
#  print_osm_map (map)

## ----map8-print, echo = FALSE--------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
                       cols = c ('orange', 'tomato'), bg = 'gray40',
                       boundary = 0)
col_park_in <- rgb (50, 255, 50, maxColorValue = 255)
col_park_out <- rgb (50, 155, 50, maxColorValue = 255)
map <- add_osm_groups (map, dat_P, groups = list (pts, pts2),
                       cols = rep (col_park_in, 2), bg = col_park_out,
                       boundary = 0)
print_osm_map (map, filename = 'map_b8.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map9, eval = FALSE--------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_objects (map, dat_P, col = col_park_out)
#  map <- add_osm_groups (map, dat_P, groups = list (pts, pts2),
#                         cols = rep (col_park_in, 2), bg = col_park_out,
#                         boundary = 0)
#  map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
#                         cols = c ('orange', 'tomato'), bg = 'gray40',
#                         boundary = 0)
#  print_osm_map (map)

## ----map9-print, echo = FALSE--------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_objects (map, dat_P, col = col_park_out)
map <- add_osm_groups (map, dat_P, groups = list (pts, pts2),
                       cols = rep (col_park_in, 2), bg = col_park_out,
                       boundary = 0)
map <- add_osm_groups (map, dat_B, groups = list (pts, pts2), make_hull = TRUE,
                       cols = c ('orange', 'tomato'), bg = 'gray40',
                       boundary = 0)
print_osm_map (map, filename = 'map_b9.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map10, eval = FALSE-------------------------------------------------
#  # darken colours by aboud 20%
#  cols_adj <- adjust_colours (c ('orange', 'tomato'), adj = -0.2)
#  map <- add_osm_groups (map, london$dat_HP, groups = list (pts, pts2),
#                         make_hull = TRUE, cols = cols_adj,
#                         bg = adjust_colours ('gray40', adj = -0.4),
#                         boundary = 1, size = 2)
#  map <- add_osm_groups (map, london$dat_H, groups = list (pts, pts2),
#                         make_hull = TRUE, cols = cols_adj,
#                         bg = adjust_colours ('gray40', adj = -0.2),
#                         boundary = 1, size = 1)
#  print_osm_map (map)

## ----map10-print, echo = FALSE-------------------------------------------
# darken colours by aboud 20%
cols_adj <- adjust_colours (c ('orange', 'tomato'), adj = -0.2)
map <- add_osm_groups (map, london$dat_HP, groups = list (pts, pts2),
                       make_hull = TRUE, cols = cols_adj,
                       bg = adjust_colours ('gray40', adj = -0.4),
                       boundary = 1, size = 2)
map <- add_osm_groups (map, london$dat_H, groups = list (pts, pts2),
                       make_hull = TRUE, cols = cols_adj,
                       bg = adjust_colours ('gray40', adj = -0.2),
                       boundary = 1, size = 1)
print_osm_map (map, filename = 'map_b10.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map11, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray95')
#  map <- add_osm_groups (map, dat_B, groups = pts, cols = 'gray40', bg = 'gray85',
#                     boundary = 1)
#  map <- add_osm_groups (map, dat_H, groups = pts, cols = 'gray20', bg = 'gray70',
#                     boundary = 0)
#  map <- add_osm_groups (map, dat_HP, groups = pts, cols = 'gray10', bg = 'white',
#                     boundary = 0, size = 1)
#  print_osm_map (map)

## ----map11-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray95')
map <- add_osm_groups (map, dat_B, groups = pts, cols = 'gray40', bg = 'gray85',
                   boundary = 1)
map <- add_osm_groups (map, dat_H, groups = pts, cols = 'gray20', bg = 'gray70',
                   boundary = 0)
map <- add_osm_groups (map, dat_HP, groups = pts, cols = 'gray10', bg = 'white',
                   boundary = 0, size = 1)
print_osm_map (map, filename = 'map_b11.png', width = 600,
               units = 'px', dpi = map_dpi)

## ------------------------------------------------------------------------
set.seed (2)
ngroups <- 12
x <- bbox [1, 1] + runif (ngroups) * diff (bbox [1, ])
y <- bbox [2, 1] + runif (ngroups) * diff (bbox [2, ])
groups <- as.list (data.frame (t (cbind (x, y))))

## ----map12, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray95')
#  map <- add_osm_groups (map, dat_B, groups = groups,
#                         cols = rainbow (length (groups)))
#  print_osm_map (map)

## ----map12-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray95')
map <- add_osm_groups (map, dat_B, groups = groups,
                       cols = rainbow (length (groups)))
print_osm_map (map, filename = 'map_b12.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map13, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray95')
#  map <- add_osm_groups (map, dat_B, groups = groups, border_width = 2,
#                         cols = heat.colors (length (groups)))
#  print_osm_map (map)

## ----map13-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray95')
map <- add_osm_groups (map, dat_B, groups = groups, border_width = 2,
                       cols = heat.colors (length (groups)))
print_osm_map (map, filename = 'map_b13.png', width = 600,
               units = 'px', dpi = map_dpi)

## ---- eval = FALSE-------------------------------------------------------
#  cmat <- colour_mat (rainbow (4), plot = TRUE)

## ---- fig.width = 4, echo = FALSE----------------------------------------
plot.new ()
cmat <- colour_mat (rainbow (4), plot = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  cmat <- colour_mat (rainbow (4), n = c(4, 8), rotate = 90, plot = TRUE)

## ---- fig.width = 4, echo = FALSE----------------------------------------
plot.new ()
cmat <- colour_mat (rainbow (4), n = c(4, 8), rotate = 90, plot = TRUE)

## ----map14, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray95')
#  map <- add_osm_groups (map, dat_B, groups = groups, border_width = 2,
#                         colmat = TRUE,
#                         cols = c('red', 'green', 'yellow', 'blue'), rotate = 180)
#  print_osm_map (map)

## ----map14-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray95')
map <- add_osm_groups (map, dat_B, groups = groups, border_width = 2,
                       colmat = TRUE,
                       cols = c('red', 'green', 'yellow', 'blue'), rotate = 180)
print_osm_map (map, filename = 'map_b14.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----connect-highways, eval = FALSE--------------------------------------
#  highways <- c ('Monmouth.St', 'Short.?s.Gardens', 'Endell.St', 'Long.Acre',
#                 'Upper.Saint.Martin')
#  highways1 <- connect_highways (highways = highways, bbox = bbox)
#  highways <- c ('Endell.St', 'High.Holborn', 'Drury.Lane', 'Long.Acre')
#  highways2 <- connect_highways (highways = highways, bbox = bbox)
#  highways <- c ('Drury.Lane', 'High.Holborn', 'Kingsway', 'Great.Queen.St')
#  highways3 <- connect_highways (highways = highways, bbox = bbox)
#  highways <- c ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
#                 'Fleet.St', 'Aldwych')
#  highways4 <- connect_highways (highways = highways, bbox = bbox)

## ---- echo = FALSE-------------------------------------------------------
load (system.file ('extdata', 'hwys.rda', package = 'osmplotr'))
highways1 <- hwys [[1]]
highways2 <- hwys [[2]]
highways3 <- hwys [[3]]
con_h <- function (hwy)
{
    hwy <- osmplotr:::connect_single_ways (hwy)
    hwy <- osmplotr:::get_highway_cycle (hwy)
    conmat <- osmplotr:::get_conmat (hwy)
    cycles <- try (ggm::fundCycles (conmat), TRUE)
    cyc <- cycles [[which.max (sapply (cycles, nrow))]]
    osmplotr:::sps_through_cycle (hwy, cyc)
}
highways1 <- con_h (highways1)
highways2 <- con_h (highways2)
highways3 <- con_h (highways3)

## ------------------------------------------------------------------------
class (highways1); nrow (highways1); nrow (highways2); nrow (highways3)

## ---- echo = TRUE--------------------------------------------------------
groups <- list (highways1, highways2, highways3)
cols_B <- c ('red', 'orange', 'tomato') # for the 3 groups
cols_H <- adjust_colours (cols_B, -0.2)
bg_B <- 'gray40'
bg_H <- 'gray60'

## ----map15, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_objects (map, dat_P, col = col_park_out)
#  map <- add_osm_groups (map, dat_B, groups = groups, boundary = 1,
#                     bg = bg_B, cols = cols_B)
#  map <- add_osm_groups (map, dat_H, groups = groups, boundary = 1,
#                     bg = bg_H, cols = cols_H)
#  map <- add_osm_groups (map, dat_HP, groups = groups, boundary = 0,
#                     cols = cols_H, bg = bg_H, size = 1)
#  print_osm_map (map)

## ----map15-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_objects (map, dat_P, col = col_park_out)
map <- add_osm_groups (map, dat_B, groups = groups, boundary = 1,
                   cols = cols_B, bg = bg_B)
map <- add_osm_groups (map, dat_H, groups = groups, boundary = 0,
                   cols = cols_H, bg = bg_H)
map <- add_osm_groups (map, dat_HP, groups = groups, boundary = 0,
                   cols = cols_H, bg = bg_H, size = 1)
print_osm_map (map, filename = 'map_b15.png', width = 600,
               units = 'px', dpi = map_dpi)

## ------------------------------------------------------------------------
n <- 5
x <- seq (bbox [1, 1], bbox [1, 2], length.out = n)
y <- seq (bbox [2, 1], bbox [2, 2], length.out = n)
dat <- data.frame (
    x = as.vector (array (x, dim = c(n, n))),
    y = as.vector (t (array (y, dim = c(n, n)))),
    z = x * y
    )
head (dat)

## ----map16, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
#  print_osm_map (map)

## ----map16-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
print_osm_map (map, filename = 'map_b16.png', width = 600,
               units = 'px', dpi = map_dpi)

## ---- eval = TRUE--------------------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_HP, dat = dat, cols = heat.colors (30))
map <- add_osm_surface (map, dat_H, dat = dat, cols = heat.colors (30))

## ----map17, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
#  cols_adj <- adjust_colours (heat.colors (30), -0.2)
#  map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj, size = 1.5)
#  map <- add_osm_objects (map, dat_P, col = rgb (0.1, 0.3, 0.1))
#  map <- add_osm_objects (map, dat_H, col = 'gray60')
#  print_osm_map (map)

## ----map17-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
cols_adj <- adjust_colours (heat.colors (30), -0.2)
map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj, size = 1.5)
map <- add_osm_objects (map, dat_P, col = rgb (0.1, 0.3, 0.1))
map <- add_osm_objects (map, dat_H, col = 'gray60')
print_osm_map (map, filename = 'map_b17.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map18, eval = FALSE-------------------------------------------------
#  map <- add_colourbar (map, cols = terrain.colors (100), zlims = range (dat$z))
#  print_osm_map (map)

## ----map18-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
cols_adj <- adjust_colours (heat.colors (30), -0.2)
map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj, size = 1.5)
map <- add_osm_objects (map, dat_P, col = rgb (0.1, 0.3, 0.1))
map <- add_osm_objects (map, dat_H, col = 'gray60')
map <- add_colourbar (map, cols = terrain.colors (100), zlims = range (dat$z))
print_osm_map (map, filename = 'map_b18.png', width = 600,
               units = 'px', dpi = map_dpi)

## ----map19, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
#  cols_adj <- adjust_colours (heat.colors (30), -0.2)
#  map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj, size = 1.5)
#  map <- add_colourbar (map, cols = heat.colors (100), zlims = range (dat$z),
#                        alpha = 0.9, vertical = FALSE,
#                        barwidth = c(0.1, 0.12), barlength = c(0.5, 0.9),
#                        text_col = "blue", fontsize = 5, fontface = 3,
#                        fontfamily = "Times")
#  map <- add_axes (map, colour = "blue", fontsize = 5, fontface = 3,
#                   fontfamily = "Times")
#  print_osm_map (map)

## ----map19-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30))
cols_adj <- adjust_colours (heat.colors (30), -0.2)
map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj, size = 1.5)
map <- add_colourbar (map, cols = heat.colors (100), zlims = range (dat$z),
                      alpha = 0.9, vertical = FALSE,
                      barwidth = c(0.1, 0.12), barlength = c(0.5, 0.9),
                      text_col = "blue", fontsize = 5, fontface = 3,
                      fontfamily = "Times")
map <- add_axes (map, colour = "blue", fontsize = 5, fontface = 3,
                 fontfamily = "Times")
print_osm_map (map, filename = 'map_b19.png', width = 600,
               units = 'px', dpi = map_dpi)

## ------------------------------------------------------------------------
d <- sqrt ( (dat$x - mean (dat$x)) ^ 2 + (dat$y - mean (dat$y)) ^ 2)
range (d)

## ----map20, eval = FALSE-------------------------------------------------
#  dat <- dat [which (d < 0.01), ]
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30),
#                          bg = "gray40")
#  cols_adj <- adjust_colours (heat.colors (30), -0.2)
#  map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj,
#                          size = c (1.5, 0.5), bg = "gray70")
#  print_osm_map (map)

## ----map20-print, echo = FALSE-------------------------------------------
dat <- dat [which (d < 0.01), ]
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_B, dat = dat, cols = heat.colors (30),
                        bg = "gray40")
cols_adj <- adjust_colours (heat.colors (30), -0.2)
map <- add_osm_surface (map, dat_HP, dat = dat, cols = cols_adj,
                        size = c (1.5, 0.5), bg = "gray70")
print_osm_map (map, filename = 'map_b20.png', width = 600,
               units = 'px', dpi = map_dpi)

## ---- eval = FALSE-------------------------------------------------------
#  dat_T <- extract_osm_objects (key = 'tree', bbox = bbox)

## ---- echo = FALSE-------------------------------------------------------
dat_T <- london$dat_T

## ----map21, eval = FALSE-------------------------------------------------
#  map <- osm_basemap (bbox = bbox, bg = 'gray20')
#  map <- add_osm_surface (map, dat_HP, dat = dat, cols = terrain.colors (30),
#                          size = c (1.5, 0.5), bg = "gray70")
#  map <- add_osm_surface (map, dat_H, dat = dat, cols = terrain.colors (30),
#                          size = c (1, 0.5), bg = "gray70")
#  map <- add_osm_surface (map, dat_T, dat = dat, cols = heat.colors (30),
#                                bg = "lawngreen", size = c(3, 2), shape = c(8, 1))
#  print_osm_map (map)

## ----map21-print, echo = FALSE-------------------------------------------
map <- osm_basemap (bbox = bbox, bg = 'gray20')
map <- add_osm_surface (map, dat_HP, dat = dat, cols = terrain.colors (30),
                        size = c (1.5, 0.5), bg = "gray70")
map <- add_osm_surface (map, dat_H, dat = dat, cols = terrain.colors (30),
                        size = c (1, 0.5), bg = "gray70")
map <- add_osm_surface (map, dat_T, dat = dat, cols = heat.colors (30),
                              bg = "lawngreen", size = c(3, 2), shape = c(8, 1))
print_osm_map (map, filename = 'map_b21.png', width = 600,
               units = 'px', dpi = map_dpi)

