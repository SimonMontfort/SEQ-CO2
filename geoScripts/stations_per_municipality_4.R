##################
## load packages
##################
rm(list = ls())

library(sf)
library(tmap)
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
library(readxl)
library(geojsonsf)
library(units)

##################
## load and prepare data
##################

# read data
cant <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Boundaries_LV95/swissTLMRegio_KANTONSGEBIET_LV95.shp")
# road <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Product_LV95/Transportation/swissTLMRegio_Road.shp")
# junc <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/swissTLMRegio/swissTLMRegio_Product_LV95/Transportation/swissTLMRegio_Junctions.shp")
stat <- st_read("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/ch.bfe.ladestellen-elektromobilitaet_de.json")
muni <- st_read("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/plz_verzeichnis_v2/plz_verzeichnis_v2.shp")
resp <- read_xlsx("/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/DatenUmfrage/VoteJune21_June 17, 2021_14.06.xlsx")

# subset to full completes
resp <- resp[resp$QID6 == "Ja",] # only respondents who are eligible to vote
# full completes
resp[resp$Status == "IP Address",]
# subset missing values for conjoint (using the conjoint attribute values shown to the respondent)
resp <- resp[rowSums(is.na(resp[,c("QID430_cjp1",	"QID430_cjp2",	"QID431_cjp1",	"QID431_cjp2", "QID392_cjp1",	"QID392_cjp2", "QID393_cjp1", "QID393_cjp2",
                                   "QID98_1", "QID98_2", "QID270_1", "QID270_2", "QID282_1",	"QID282_2", "QID281_1", "QID281_2", 
                                   "QID389_1", "QID389_2", "QID415_1", "QID415_2", "QID412_1",	"QID412_2", "QID414_1", "QID414_2")])) == 0,]
# inspect good completes
unique(resp$gc) # all 1 or NA
resp <- resp[!is.na(resp$gc),]
nrow(resp)

geojsonsf::geojson_sf("https://data.geo.admin.ch/ch.bfe.ladestellen-elektromobilitaet/data/ch.bfe.ladestellen-elektromobilitaet_de.json")

# projection
proj <- 21781
proj_lon_lat <- 4326

# same projection for all objects
cant <- st_transform(cant, proj)
# road <- st_transform(road, proj)
# junc <- st_transform(junc, proj)
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

# # respondents 
resp <- resp[-1,]

##################
## number of charging stations for electric vehicles
##################
# remove empty polygons
muni <- muni %>% filter(!st_is_empty(.))
# which municipalities contain which stations?
muni_contain_stat <- st_intersects(muni, stat)
# length of elements within list is the number of stations in that municipality
no_of_stations_ev <- unlist(lapply(muni_contain_stat, length))
# bind PLZ to station
muni$no_of_stations_ev <- no_of_stations_ev

tmap_mode("view")
tm_shape(muni) + tm_polygons(col = "no_of_stations_ev") +
  tm_shape(stat) + tm_dots(size = .1)

## calculate the ratio of the number of station
# muni <- muni %>% 
#   group_by(postleitzah) %>% 
#   mutate(no_of_stations_ev = sum(no_of_stations_ev),
#          ) %>% 
#   ungroup() %>% 
#   mutate(area = set_units(st_area(muni), km^2),
#          ratio_ev_to_muni_area = no_of_stations_ev/area)

muni <- muni %>% 
  mutate(area = set_units(st_area(geometry), km^2)) %>% 
  group_by(postleitzah) %>% 
  mutate(
    no_of_stations_ev = sum(no_of_stations_ev),
    area = sum(area)) %>% 
  # filter(duplicated(postleitzah)) %>% 
  arrange(postleitzah) %>% 
  ungroup() %>% 
  mutate(ratio_ev_to_muni_area = no_of_stations_ev/area)

# 1042, 1184, 1682
tmap_mode("view")
tm_shape(muni) + tm_polygons(col = "ratio_ev_to_muni_area") +
  tm_shape(stat) + tm_dots(size = .1)

# new variable with name plz for postleitzahl
resp$plz <- as.numeric(resp$QID22)
# how many NAs do we have? 980 respondents left
table(is.na(resp$plz))
# ratio:
table(is.na(resp$plz))[2]/table(is.na(resp$plz))[1] # we miss 24% 


# ##################
# ## number of public transport stations
# ##################
# stat_pt <- sf::st_read(dsn = "/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/geoDaten/publiTransport/HaltestellenOeV_LV95.gdb", layer = "Betriebspunkt")
# stat_pt <- st_transform(stat_pt, proj)
# # which municipalities contain which stations?
# muni_contain_stat <- st_intersects(muni, stat_pt)
# # length of elements within list is the number of stations in that municipality
# no_of_stations_pt <- unlist(lapply(muni_contain_stat, length))
# # bind PLZ to station
# muni$no_of_stations_pt <- no_of_stations_pt
# 
# ## calculate the ratio of the number of station
# muni <- muni %>% 
#   mutate(ratio_pt_to_muni_area = no_of_stations_pt/area)
# 
# # inspect NAs
# sum(is.na(no_of_stations_ev))
# sum(is.na(no_of_stations_pt))

muni_merg <- muni %>% 
  as.data.frame(.) %>% 
  select(
    no_of_stations_ev, 
    # no_of_stations_pt, 
    # ratio_pt_to_muni_area, 
    ratio_ev_to_muni_area,
    postleitzah
    ) %>% 
  mutate(postleitzah = as.numeric(postleitzah)) %>% 
  # group_by(postleitzah) %>% 
  # mutate(no_of_stations_ev = sum(no_of_stations_ev),
         # no_of_stations_pt = sum(no_of_stations_pt)
         # ) %>% 
  group_by(postleitzah) %>% 
  slice(1)

# muni_merg <- muni_merg[distinct()]
##################
## join dfs
##################
# join together
nrow(resp)
resp_merg <- left_join(resp, muni_merg, by = c("plz" = "postleitzah"))
nrow(resp_merg)
# invalid_plzs <- unique(resp_merg$plz[!resp_merg$plz %in% muni$postleitzah]) # drops PLZ 2021 3001 8000 1200 1111 3000 4000 1100 6000 --> are all invalid PLZ, i.e. wrong answers
# sum(resp_merg$plz %in% invalid_plzs) # due to 59 invalid PLZs that respondents indicated, the number of stations is NA for them
# look at result
resp_merg$no_of_stations_ev
resp_merg$ratio_ev_to_muni_area
resp_merg$no_of_stations_pt



sum(is.na(resp_merg$no_of_stations_ev))
sum(is.na(resp_merg$no_of_stations_pt))

dat <- resp_merg
save(dat, file = "/Volumes/Transcend/Uni/doktorat/Umfrage CO2 Gesetz/DatenUmfrage/dat_with_stations_3.RData")

