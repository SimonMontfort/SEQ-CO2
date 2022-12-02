rm(list = ls())

library(sf)
library(tmap)
# library(geojsonsf)
library(RJSONIO)
library(dplyr)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(tmap)
library(lwgeom)
library(splancs)
library(shp2graph)

# read data
cant <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Boundaries_LV95/swissTLMRegio_KANTONSGEBIET_LV95.shp")
road <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Product_LV95/Transportation/swissTLMRegio_Road.shp")
junc <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Product_LV95/Transportation/swissTLMRegio_Junctions.shp")
stat <- st_read("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/ch.bfe.ladestellen-elektromobilitaet_de.json")

# projection
proj <- 21781

# same projection for all objects
cant <- st_transform(cant, proj)
road <- st_transform(road, proj)
junc <- st_transform(junc, proj)
stat <- st_transform(stat, proj)

# keep cantons in CH
cant <- cant[cant$ICC == "CH",]
# Switzerland as one poly
ch <- st_union(cant)

# create a variable keep with the index of stat for those that should be kept
int <- unlist(lapply(st_intersects(stat, ch), length))
keep <- 1:length(int)*int
keep <- keep[keep != 0]

# keep stations in CH
stat <- stat[keep,]
stat

st_snap_points = function(x, y, max_dist = 1000) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}


# # for testing subset to BS:
# int <- st_intersects(road, cant[cant$NAME == "Basel-Stadt",])
# road <- road[unlist(lapply(int, length)) !=0,]
# 
# # for testing subset to BS:
# int <- st_intersects(stat, cant[cant$NAME == "Basel-Stadt",])
# stat <- stat[unlist(lapply(int, length)) !=0,]

# snap stations to the road
stat_sn <- st_snap_points(stat, road, max_dist = 100)

tm_shape(road) + tm_lines() + tm_shape(stat_sn) + tm_dots("blue", size = .1) 

# start and stop
road <- road %>%
  mutate(start_original = lwgeom::st_startpoint(road),
         end_original = lwgeom::st_endpoint(road),
         length_original = st_length(road))

############## calculate the shortest distance along the road

# ids are unique
any(duplicated(road$OBJECTID))

library(nngeo)
# (i) roads 
# snap charging stations to roads
stat_sn <- st_snap(stat_sn, road, tol=1e-10000000000)
# split roads with stations
road_split <- st_collection_extract(st_split(road, st_buffer(stat_sn, dist = 1e-10)), "LINESTRING")
# name splitted roads
road_split$test <- ifelse(duplicated(road_split$OBJECTID), "test", "")
# check
tm_shape(road_split) + tm_lines() + tm_shape(stat_sn) + tm_dots("blue", size = .1) + 
  tm_shape(st_collection_extract(lwgeom::st_split(road, stat_sn),"LINESTRING")) + tm_lines("red")+ 
  tm_shape(stat) + tm_dots("red")

# (ii) respondents 
# to test, create 1500 random points within BS 
n <- 100
resp <- st_sample(cant[cant$NAME == "Basel-Stadt",], n)
resp <- read_xlsx("/Users/simon/Downloads/VoteJune21_June 12, 2021_00.50.xlsx")
resp <- resp[-1,]
resp <- resp[!is.na(resp$LocationLongitude) & !is.na(resp$LocationLatitude),]
resp$LocationLongitude <- as.numeric(resp$LocationLongitude)
resp$LocationLatitude <- as.numeric(resp$LocationLatitude)
resp <- resp %>% as_tibble() %>% st_as_sf(., coords = c("LocationLongitude", "LocationLatitude")) %>% st_set_crs(., proj)

# snap stations to the road
resp_sn <- st_snap_points(resp, road, max_dist = 1000000)
# snap their points to the closest road 
resp_sn <- st_snap(resp_sn, road_split, tol=1e-10000000000)
# union between stations and junctions
road_split_2 <- st_collection_extract(st_split(road_split, st_buffer(resp_sn, dist = 1e-10)), "LINESTRING")
# name splitted roads
road_split_2$char <- ifelse(duplicated(road_split_2$OBJECTID), as.character(1:n), "")
# check
tmap_mode('view')
tm_shape(road_split) + tm_lines() + tm_shape(stat_sn) + tm_dots("blue", size = .1) + 
  tm_shape(st_collection_extract(lwgeom::st_split(road, stat_sn),"LINESTRING")) + tm_lines("red")+ 
  tm_shape(stat) + tm_dots("red") +
  tm_shape(resp) + tm_dots("black") + tm_shape(resp_sn) + tm_dots("orange")

# check which ones they intersect, should all be three:
l <- st_intersects(st_buffer(resp_sn, dist = 1e-10), road_split_2)
any(unlist(lapply(l, length)) == 3) 

# graph is connected. Each element in road_split_2 is connected to at least one other element in road_split_2.
l <- st_touches(road_split_2, road_split_2)
any(unlist(lapply(l, length)) >=1) 

# calculate start and end point and create edge list
road_split_2 <- road_split_2 %>% 
  mutate(start = st_startpoint(.),
         end = st_endpoint(.),
         length = st_length(.),
         id = 1:nrow(.))


spld <- as_Spatial(road)

network <- nt.connect(spld)

stations_ <- points2network(ntdata = network, pointsxy = st_coordinates(stat_sn), approach = 1,
                           ELComputed = T)

Respondent_ <- points2network(ntdata = network, pointsxy = st_coordinates(resp_sn), approach = 1,
                           ELComputed = T)

CoorespondIDs = stations_[[3]]
# Generate an 'igraph' object by weighting this graph with edge length
ig <- nel2igraph(stations_[[1]], stations_[[2]], weight = stations_[[6]]$length_original)
Stat_ <- as.numeric(CoorespondIDs)

# Respondent
CoorespondIDs3 = Respondent_[[3]]
ig3 <- nel2igraph(Respondent_[[1]], Respondent_[[2]], weight = Respondent_[[8]])
Res_ <- as.numeric(CoorespondIDs3)
plot(ig3,
     vertex.size = ifelse(V(ig3) %in% c(55,103), 5, 1),
     vertex.color = ifelse(V(ig3) %in% c(55,103), "blue", "red"),
     vertex.label = NA)

spm1 <- shortest.paths(ig3, v = c(unique(Res_)), to = c(unique(Stat_)), weight = get.edge.attribute(ig3,
                                                                               name = "weight"))

rownames(spm1) <- as.character(unique(Res_))
colnames(spm1) <- as.character(unique(Stat_))
spm1_df <- as.data.frame(spm1)

rownames(spm1_df[which.min(spm1_df$`55`),])

column_min<- apply(spm1_df, 1, FUN = min)

spth<- get.shortest.paths(ig3,228,98)

spm1[min(spm1),]

save.image(file = "/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoScripts/shortestPath.RData")


