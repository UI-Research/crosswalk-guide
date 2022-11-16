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


my_zctas <- zctas(year = 2019) %>% 
  mutate(zcta_area = st_area(.))

pen_zips_sf <- my_zctas %>% 
  #st_transform("EPSG:2163") %>%
  st_intersection(my_pen_counties) %>% 
  mutate(int_area = st_area(.), 
         perc = as.numeric(int_area)/as.numeric(zcta_area))

pen_zips <- pen_zips_sf %>%
  st_drop_geometry()

pen_zips_filtered <- pen_zips_sf %>% 
  filter( perc > .01)

my_plot_zctas <- my_zctas %>% 
  filter(GEOID10 %in% pen_zips_filtered$GEOID10) %>% 
  ggplot() +
  geom_sf(mapping = aes(), fill= NA, color = palette_urbn_cyan[2], size = 1.3) + 
  geom_sf(my_pen_counties, mapping = aes(), fill = NA, color = palette_urbn_cyan[6], size = 1.3) 

ggsave("www/images/zcta_plot_small.png", plot = my_plot_zctas)

pen_zips_filtered_2 <- filter(pen_zips_filtered, perc < .99)

my_zcta_filtered <- filter(my_zctas, GEOID10 %in% pen_zips_filtered_2$GEOID10)

my_plot_zctas_2 <- 
 
  my_zctas %>% 
  filter(GEOID10 %in% pen_zips_filtered$GEOID10) %>% 
  ggplot() +
  geom_sf(my_zcta_filtered, mapping = aes(), fill= palette_urbn_gray[1], color = palette_urbn_cyan[2], size = 1.3) + 
  geom_sf(mapping = aes(), fill= NA, color = palette_urbn_cyan[1], size = .8) + 
  geom_sf(pen_zips_filtered_2, mapping = aes(), fill = palette_urbn_cyan[1], color = NA) + 
  geom_sf(my_zcta_filtered, mapping = aes(), fill= NA, color = palette_urbn_cyan[2], size = 1.3) + 
  geom_sf(my_pen_counties, mapping = aes(), fill = NA, color = palette_urbn_cyan[6], size = 1.3) 

ggsave("www/images/zcta_plot_small_w_int.png", plot = my_plot_zctas_2)
