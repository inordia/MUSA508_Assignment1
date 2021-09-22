#setup
rm(list=ls())

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(mapview)



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

palette5 <- c("#ffffcc","#a1dab4","#41b6c4","#2c7fb8","#253494")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]],
                                  c(.01,.2,.4,.6,.8), na.rm=T), digits = 3))
  }
}


q5 <- function(variable) {as.factor(ntile(variable, 5))}

#Chicago boundaries

CTA_boundary <-st_read("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=KML") %>%
  st_transform('EPSG:26916')
CTA_boundary_buffer <- st_union(CTA_boundary)

vars00 <- c("P001001","P006002","PCT025050","PCT025009","P053001","H056001","P092001")
tracts00 <-  
  get_decennial(geography = "tract", vars00, 
                year=2000, state='IL', county='Cook', geometry=T) %>%  
  st_transform('EPSG:26916')


#Convert data to the wide format, rename the variables and recreate new variables
tracts00<- tracts00%>%spread(variable, value)%>% 
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
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty,-NAME)
View(tracts00)

#Clip the boundary 
tracts00 <- st_intersection(tracts00,CTA_boundary_buffer)


# ---- Year 2017 tracts -----

vars17 <- c("B25026_001E","B02001_002E","B15001_050E",
            "B15001_009E","B19013_001E","B25058_001E",
            "B06012_002E")
tracts17 <-  
  get_acs(geography = "tract", vars17, 
          year=2017, state='IL', county='Cook', geometry=T) %>%
  st_transform('EPSG:26916')
#glimpse(tracts17)

#Clip the boundary 
clip17 <- 
  st_intersection(tracts17,CTA_boundary_buffer) %>%
  dplyr::select(GEOID,variable,estimate) 
#plot(clip17)

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

#Binding

allTracts <- rbind(tracts00,tracts17)
dim(tracts00)
dim(tracts17)
dim(allTracts)
View(allTracts)

#Transit Data


CTA_stops <- st_read("https://data.cityofchicago.org/download/4qtv-9w43/application%2Fxml") %>%
  select(-Description)%>%
  st_transform('EPSG:26916')

CTA_stops<- CTA_stops[CTA_boundary_buffer,]

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

#TOD Indicators Table

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T))

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "TOD Inditators, Chicago",
           general = "\n")

#TOD Indicators Plots

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")

#sub-markets

Loop <- c (17031320100,	
           17031320200,
           17031320300,
           17031320400,	
           17031320500,	
           17031320600)

Downtown <- subset(allTracts.group, allTracts$GEOID==Loop)%>%
  mutate(submarket = "downtown")

#Graduated Symbol Maps

new_CTAstops <- CTA_stops
new_CTAstops <- st_join(CTA_stops, allTracts.group, join = st_intersects) %>%
  na.omit()

#population Graduated Symbol Map
ggplot()+
  geom_sf(data = allTracts,
          fill = "antiquewhite1", color = "grey75")+
  geom_sf(data = new_CTAstops %>%
            st_centroid(), 
          shape = 21,
          aes(
            size = TotalPop,
            fill = TotalPop
          ),alpha = 1, show.legend = "point") +
  scale_size_continuous(
    range = c(1,6),
    breaks = c(0, 1644, 2934, 5172, 18841),
    labels = qBr(new_CTAstops, "TotalPop"),
    name = "Total Population")+
  scale_fill_stepsn(
    colors = palette5,
    breaks = c(0, 1644, 2934, 5172, 18841),
    guide = FALSE)+
  labs(title = "Graduated Symbol Map of Population", subtitle = "within 1/2 mi of stations\n 2000-2017") +
  facet_wrap(~year)+
  mapTheme()+
  guides(size = guide_legend(override.aes = list(fill = palette5)))


#Rent Graduated Symbol Map
ggplot()+
  geom_sf(data = allTracts,
          fill = "antiquewhite1", color = "grey75")+
  geom_sf(data = new_CTAstops %>%
            st_centroid(), 
          shape = 21,
          aes(
            size = MedRent,
            fill = MedRent
          ),alpha = 1, show.legend = "point") +
  scale_size_continuous(
    range = c(1,6),
    breaks = c(0, 680, 864, 1406, 2261),
    labels = qBr(new_CTAstops, "MedRent"),
    name = "Median Rent")+
  scale_fill_stepsn(
    colors = palette5,
    breaks = c(0, 680, 864, 1406, 2261),
    guide = FALSE)+
  labs(title = "Graduated Symbol Map of Median Rent", subtitle = "within 1/2 mi of stations\n 2000-2017") +
  facet_wrap(~year)+
  mapTheme()+
  guides(size = guide_legend(override.aes = list(fill = palette5)))

#Rent and Distance to Subway Station

multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
  {
    #create a list of distances that we'll iterate through to create each ring
    distances <- seq(0, maxDistance, interval)
    #we'll start with the second value in that list - the first is '0'
    distancesCounter <- 2
    #total number of rings we're going to create
    numberOfRings <- floor(maxDistance / interval)
    #a counter of number of rings
    numberOfRingsCounter <- 1
    #initialize an otuput data frame (that is not an sf)
    allRings <- data.frame()
    
    #while number of rings  counteris less than the specified nubmer of rings
    while (numberOfRingsCounter <= numberOfRings) 
    {
      #if we're interested in a negative buffer and this is the first buffer
      #(ie. not distance = '0' in the distances list)
      if(distances[distancesCounter] < 0 & distancesCounter == 2)
      {
        #buffer the input by the first distance
        buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
        #different that buffer from the input polygon to get the first ring
        buffer1_ <- st_difference(inputPolygon, buffer1)
        #cast this sf as a polygon geometry type
        thisRing <- st_cast(buffer1_, "POLYGON")
        #take the last column which is 'geometry'
        thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
        #add a new field, 'distance' so we know how far the distance is for a give ring
        thisRing$distance <- distances[distancesCounter]
      }
      
      
      #otherwise, if this is the second or more ring (and a negative buffer)
      else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
      {
        #buffer by a specific distance
        buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
        #create the next smallest buffer
        buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
        #This can then be used to difference out a buffer running from 660 to 1320
        #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
        #bc the area after 660ft in buffer2 = NA.
        thisRing <- st_difference(buffer2,buffer1)
        #cast as apolygon
        thisRing <- st_cast(thisRing, "POLYGON")
        #get the last field
        thisRing <- as.data.frame(thisRing$geometry)
        #create the distance field
        thisRing$distance <- distances[distancesCounter]
      }
      
      #Otherwise, if its a positive buffer
      else 
      {
        #Create a positive buffer
        buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
        #create a positive buffer that is one distance smaller. So if its the first buffer
        #distance, buffer1_ will = 0. 
        buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
        #difference the two buffers
        thisRing <- st_difference(buffer1,buffer1_)
        #cast as a polygon
        thisRing <- st_cast(thisRing, "POLYGON")
        #geometry column as a data frame
        thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
        #add teh distance
        thisRing$distance <- distances[distancesCounter]
      }  
      
      #rbind this ring to the rest of the rings
      allRings <- rbind(allRings, thisRing)
      #iterate the distance counter
      distancesCounter <- distancesCounter + 1
      #iterate the number of rings counter
      numberOfRingsCounter <- numberOfRingsCounter + 1
    }
    
    #convert the allRings data frame to an sf data frame
    allRings <- st_as_sf(allRings)
  }

  
allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(buffer, 8000, 800)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  na.omit()%>%
  mutate(distance = distance /1609.34)

Summary_allTracts.rings <-
  st_drop_geometry(allTracts.rings) %>%
  group_by(year, distance) %>%
  summarize(Rent = mean(MedRent, na.rm = T))

ggplot(data=Summary_allTracts.rings,aes(x=distance,y=Rent,color=year))+
  geom_line(size=2)+
  labs(title = "Rent as a function of distance to subway stations",subtitle = "Census tracts")+
  xlab("Miles")+
  ylab("Average rent")


#Crime Data
library(crimedata)
crime_2009 <- get_crime_data(
  years = 2009, 
  cities = c("Chicago"), 
  type = "core"
) %>%
  filter(offense_type == "destruction/damage/vandalism of property (except arson)")

crime_2017 <- get_crime_data(
  years = 2017, 
  cities = c("Chicago"), 
  type = "core"
) %>%
  filter(offense_type == "destruction/damage/vandalism of property (except arson)")

crime0917 <- rbind(crime_2009,crime_2017)%>%
  st_as_sf(coords = c("longitude","latitude"), crs=st_crs(4326),agr="constant") %>%
  st_transform(st_crs(allTracts.group))

tracts_crime <- st_join(allTracts.group, crime0917, join = st_intersects) 
crime_count <- count(as_tibble(tracts_crime), GEOID)
tracts_crimecount <- left_join(crime_count, tracts_crime) %>%
  rename("Crimecount" = n) %>%
  st_as_sf()

tracts_crimecount <- tracts_crimecount %>%
  dplyr::select(-offense_type, -offense_group, -offense_against, -date_single,
                -location_type, -location_category)
#Crime Map
ggplot(tracts_crimecount[CTA_boundary,])+
  geom_sf(data = tracts17, fill = "antiquewhite1", color = "grey75")+
  geom_sf(aes(fill = q5(Crimecount))) +
  geom_sf(data = buffer, fill = "transparent", color = "red") +
  scale_fill_manual(values = palette5,
                    labels = qBr(tracts_crimecount, "Crimecount"),
                    name = "Crime count\n(Quintile Breaks)") +
  labs(title = "Crime count 2009-2017", subtitle = "The red border denotes areas close to rail stations") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

#Crime Plot
tracts_crime.summary <- 
  st_drop_geometry(tracts_crimecount) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T),
            Crime = mean(Crimecount, na.rm = T))

tracts_crime.summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T)%>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value)%>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "TOD Inditators and Crime, Chicago",
           general = "\n")

tracts_crime.summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")
