library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)

options(scipen=999)
options(tigris_class = "sf")

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}


CTA_stops <- st_read("https://data.cityofchicago.org/download/4qtv-9w43/application%2Fxml") %>%
  select(-Description)%>%
  st_transform('EPSG:26916')

CTA_buffer <-  rbind(
  st_buffer(CTA_stops, 2640) %>%
    mutate(Legend = "Buffer") %>%
    dplyr::select(Legend),
  st_union(st_buffer(CTA_stops, 2640)) %>%
    st_sf() %>%
    mutate(Legend = "Unioned Buffer"))

ggplot() +
  geom_sf(data=CTA_buffer) +
  geom_sf(data=CTA_stops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "CTAstops Buffer Zones") +
  mapTheme()
