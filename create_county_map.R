library(tigris)
library(tidyverse)
library(urbnmapr)
library(urbnthemes)
library(sf)

set_urbn_defaults("map")
options(tigris_use_cache = T)
# 
# my_pen <- get_urbn_map(sf = T) %>% 
#   filter(state_name == "Pennsylvania") %>% 
#   mutate(type = "State")

my_pen_counties <- counties(state = "Pennsylvania") %>% 
  mutate(type = "County") %>% 
  slice(24)

my_pen_tract <- tracts(state = 42, county = 85, year = 2010) %>% 
  mutate(type = "Census Tract")


my_plot <-   ggplot() + 
  geom_sf(my_pen_counties, color = palette_urbn_cyan[6], mapping = aes(), fill = NA, size = 3.5) +
  geom_sf(my_pen_tract, color = palette_urbn_cyan[2], mapping = aes(), fill = NA, size = 1) +
  #geom_sf(my_pen, color = palette_urbn_yellow[4], mapping = aes(), fill = NA, size = 1, alpha = .3) +
  #geom_sf(bind_rows(my_pen_counties, my_pen_tract), mapping = aes(color = type), fill = NA, size = 1, alpha = .3) + 
  scale_color_manual(values = c(palette_urbn_cyan[2] , 
                                palette_urbn_cyan[6] ))+
  geom_sf(my_pen_counties, color = palette_urbn_cyan[2], mapping = aes(), fill = NA, size = 1) +
  labs(color = NULL) + 
  theme(legend.text = element_text(size = 15))

ggsave("www/images/pennsylvania_county.png", plot = my_plot) 


my_zctas <- zctas(year = 2019)

pen_zips_sf <- my_zctas %>% 
  #st_transform("EPSG:2163") %>%
  st_intersection(my_pen_counties) 

pen_zips <- pen_zips_sf %>%
  st_drop_geometry()

my_plot_zctas <- my_zctas %>% 
  filter(GEOID10 %in% pen_zips$GEOID10) %>% 
  ggplot() +
  geom_sf(mapping = aes(), fill= NA, color = palette_urbn_cyan[2], size = 1.3) + 
  geom_sf(my_pen_counties, mapping = aes(), fill = NA, color = palette_urbn_cyan[6], size = 1.3)

ggsave("www/images/zcta_plot_small.png", plot = my_plot_zctas)
