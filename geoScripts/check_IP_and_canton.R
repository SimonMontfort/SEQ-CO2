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
muni <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/plz_verzeichnis_v2/plz_verzeichnis_v2.shp")
resp <- read_xlsx("/Users/simon/Downloads/VoteJune21_June 12, 2021_00.50.xlsx")

# projection
proj <- 21781
proj_lon_lat <- 4326

# same projection for all objects
cant <- st_transform(cant, proj)
road <- st_transform(road, proj)
junc <- st_transform(junc, proj)
stat <- st_transform(stat, proj)
muni <- st_transform(muni, proj)

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

# respondents 
resp <- resp[-1,]
resp <- resp[!is.na(resp$LocationLongitude) & !is.na(resp$LocationLatitude),]
resp$LocationLongitude <- as.numeric(resp$LocationLongitude)
resp$LocationLatitude <- as.numeric(resp$LocationLatitude)

# use lon lat as coordinates, assign lon lat crs and then transform to Swiss projection 
resp <- resp %>% 
  as_tibble() %>% 
  st_as_sf(., coords = c("LocationLongitude", "LocationLatitude")) %>% 
  st_set_crs(., proj_lon_lat) %>% 
  st_transform(., proj)

# create a variable keep with the index of respondent for those that should be kept
resp <- resp[!is.na(resp$QID22),]
int <- unlist(lapply(st_intersects(resp, ch), length))
keep <- 1:length(int)*int
keep <- keep[keep != 0]
# keep respondents in CH
resp <- resp[keep,]
resp

# which respondent geo-location lies within municipality?
resp_in_muni <- st_within(resp, muni)
# for get PLZ  for the nth line in municipality with which resp intersects
pstlz <- muni$postleitzah[unlist(resp_in_muni)]
# how respondents have the same PLZ as they indicated in the survey
sum(as.numeric(resp$QID22) == as.numeric(pstlz))
# totally 170 --> not usable
