#setup
rm(list=ls())

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

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

?round

palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

#Chicago boundaries

CTA_boundary <-st_read("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=KML") %>%
  select(-Name, -Description)%>%
  st_transform('EPSG:26916')

vars00 <- c("P001001","P006002","PCT025050","PCT025009","P053001","H056001","P092001")
tracts00 <-  
  get_decennial(geography = "tract", vars00, 
                year=2000, state='IL', county='Cook', geometry=T) %>%  
  st_transform('EPSG:26916')
View(tracts00)

#Clip the boundary 
clip00 <- 
  st_intersection(CTA_boundary,tracts00) %>%
  dplyr::select(GEOID,value,variable) 
plot(clip00)
View(clip00)

#Convert data to the wide format, rename the variables and recreate new variables
tracts00<- 
  clip00 %>% 
  spread(variable, value)%>% 
  rename (
    TotalPop = P001001,
    Whites = P006002,
    FemaleBachelors = PCT025050,
    MaleBachelors = PCT025009,
    MedHHInc = P053001,
    MedRent = H056001,
    TotalPoverty = P092001)%>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2000") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)
View(tracts00)


# ---- Year 2017 tracts -----
#get and clip 2017 data 
vars17 <- c("B25026_001E","B02001_002E","B15001_050E",
            "B15001_009E","B19013_001E","B25058_001E",
            "B06012_002E")
tracts17 <-  
  get_acs(geography = "tract", vars17, 
          year=2017, state='IL', county='Cook', geometry=T) %>%
  st_transform('EPSG:26916')
glimpse(tracts17)

#Clip the boundary 
clip17 <- 
  st_intersection(tracts17,CTA_boundary) %>%
  dplyr::select(GEOID,variable,estimate) 
plot(clip17)

#Convert data to the wide format, rename the variables and recreate new variables
tracts17<- 
  clip17 %>% 
  spread(variable, estimate)%>% 
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)%>% 
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 
View(tracts17)

#Binding

allTracts <- rbind(tracts00,tracts17)
dim(tracts00)
dim(tracts17)
dim(allTracts)
View(allTracts)

#Transit Data
tractsboundary <-st_union(tracts17)

CTA_stops <- st_read("https://data.cityofchicago.org/download/4qtv-9w43/application%2Fxml") %>%
  select(-Description)%>%
  st_transform('EPSG:26916')%>%
  CTA_stops[tractsboundary,]

plot(CTA_stops)

CTA_buffer <-  rbind(
  st_buffer(CTA_stops, 800) %>%
    mutate(Legend = "Buffer") %>%
    dplyr::select(Legend),
  st_union(st_buffer(CTA_stops, 800)) %>%
    st_sf() %>%
    mutate(Legend = "Unioned Buffer"))

ggplot() +
  geom_sf(data=tracts00)+
  geom_sf(data=CTA_buffer) +
  geom_sf(data=CTA_stops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "CTAstops Buffer Zones") +
  mapTheme()

buffer <- filter(CTA_buffer, Legend=="Unioned Buffer")

#TOD indicators
selectCentroids <-
  st_centroid(tracts00)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts00, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2000", MedRent * 1.42, MedRent)) 

#TOD indicators maps

#Time/Space
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts17))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Rent
ggplot() +
  geom_sf(data = st_union(tracts17)) +
  geom_sf(data = allTracts.group, aes(fill=q5(MedRent.inf)))+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Median Rent\n(Quintile Breaks)") +
    facet_wrap(~year)+
  geom_sf(data = buffer, colour = "red", fill = "transparent")+
  labs(title = "Chicago 2000-2017 Mediant Rent", subtitle = "Real Dollars. The red border indicates areas within 1/2 mile to transit stations. ") +
  mapTheme() + theme(plot.title = element_text(size=22))

#White
ggplot() +
  geom_sf(data = st_union(tracts17)) +
  geom_sf(data = allTracts.group, aes(fill=q5(pctWhite)))+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctWhite",rnd=FALSE),
                    name = "Percentage of White\n(Quintile Breaks)") +
  facet_wrap(~year)+
  geom_sf(data = buffer, colour = "red", fill = "transparent")+
  labs(title = "Chicago 2000-2017 Percentage of WHite", subtitle = "The red border indicates areas within 1/2 mile to transit stations. ") +
  mapTheme() + theme(plot.title = element_text(size=22))

#Bachelor Degrees
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts00))+
  geom_sf(data = st_union(tracts17))+
  geom_sf(aes(fill = q5(pctBachelors)))  +
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBachelors"),
                    name = "Percent of Bachelors\n(Quintile Breaks)") +
  facet_wrap(~year)+
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  labs(title = "Percent of Bachelor Degrees 2000-2017", subtitle = "The red border indicates areas within 1/2 mile to transit stations. ") +
  mapTheme() + theme(plot.title = element_text(size=22))

#Poverty
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts00))+
  geom_sf(data = st_union(tracts17))+
  geom_sf(aes(fill = q5(pctPoverty)))  +
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctPoverty"),
                    name = "Percent of Poverty\n(Quintile Breaks)") +
  facet_wrap(~year)+
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  labs(title = "Percent of Poverty 2000-2017", subtitle = "The red border indicates areas within 1/2 mile to transit stations. ") +
  mapTheme() + theme(plot.title = element_text(size=22))

#Table

