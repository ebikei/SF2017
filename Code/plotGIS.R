x=c('tidyverse', 'reshape2', 'readxl', 'sf', 'USAboundaries')
lapply(x,require,character.only=T)

state_names <- c("california")
co_names <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")
CA<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)

# get COUNTY data for a given state
counties_spec <- us_counties(resolution = "low", states=state_names) %>% # use list of state(s) here
  filter(name %in% co_names) %>% # filter to just the counties we want
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels

# get range of lat/longs from counties for mapping and river function
mapRange1 <- c(range(st_coordinates(counties_spec)[,1]),range(st_coordinates(counties_spec)[,2]))

# Make a quick Map:

ggplot() + 
  geom_sf(data=CA, color = "gray30", lwd=2, fill=NA) +
  geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4) +
  #geom_label_repel(data=counties_spec, aes(x=lon, y=lat, label=name)) +
  coord_sf(xlim = mapRange1[c(1:2)], ylim = mapRange1[c(3:4)]) +
  theme_bw()








# get STATE data



