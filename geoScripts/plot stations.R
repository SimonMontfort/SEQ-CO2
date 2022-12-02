library(sf)
library(tmap)

stat <- st_read("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/ch.bfe.ladestellen-elektromobilitaet_de.json")

# projection
proj <- 21781
proj <- 4326

# same projection for all objects
stat <- st_transform(stat, proj)

stat_grid = st_make_grid(stat, c(.08, .08), what = "polygons", square = FALSE)

stat <- st_transform(stat, proj)

# To sf and add grid ID
stat_grid = st_sf(stat_grid) %>%
  # add grid ID
  mutate(grid_id = 1:length(lengths(stat_grid)))

# count number of points in each grid
# https://gis.stackexchange.com/questions/323698/counting-points-in-polygons-with-sf-package-of-r
stat_grid$n_colli = lengths(st_intersects(stat_grid, stat))

# remove grid without value of 0 (i.e. no points in side that grid)
stat_count <-  filter(stat_grid, n_colli > 0)

tmap_mode("plot")

map_stat_count = tm_shape(stat_count) +
  tm_fill(
    col = "n_colli",
    palette = "Reds",
    style = "cont",
    title = "Number of collisions",
    id = "grid_id",
    showNA = FALSE,
    alpha = 0.6,
    popup.vars = c(
      "Number of collisions: " = "n_colli"
    ),
    popup.format = list(
      n_colli = list(format = "f", digits = 0)
    )
  ) +
  tm_borders(col = "grey40", lwd = 0.7)

map_stat_count
