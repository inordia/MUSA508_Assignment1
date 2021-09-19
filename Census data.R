#---- Set Up ----

# Load Libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
options(scipen=999)
options(tigris_class = "sf")
#Census API Key
census_api_key("744a6c627b7cc93310688f5ce408189636167475", install = TRUE)
#Get Chicago boundary
CTA_boundary <-st_read("https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=KML") %>%
  select(-Description)%>%
  st_transform('EPSG:26916')
View(CTA_boundary)


Chicago_boundary <- st_read("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=KML")%>%
  select(-Name, -Description)%>%
  st_transform('EPSG:26916')

Chicago00 <- st_intersection(Chicago_boundary, tracts00)

ggplot() +
  geom_sf(data = Chicago00, aes(fill = value)) +
  theme(plot.title = element_text(size=22))

# ---- Year 2000 tracts -----
vars00 <- c("P001001","P006002","PCT025050","PCT025009","P053001","H056001","P092001")
tracts00 <-  
  get_decennial(geography = "tract", vars00, 
          year=2000, state='IL', county='Cook', geometry=T) %>%  
  st_transform('EPSG:26916')
View(tracts00)

#Clip the boundary 

clip00 <- 
  st_intersection(tracts00, CTA_boundary) %>%
  dplyr::select(value,variable)

#Convert data to the wide format, rename the variables and recreate new variables
tracts00<- 
  clip00 %>% 
  dplyr::select(-Selection_Type)%>% 
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
  st_intersection(tracts17, CTA_boundary) %>%
  dplyr::select(variable,estimate) 

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

# --- Combining 09 and 17 data ----

allTracts <- rbind(tracts00,tracts17)
dim(tracts00)
dim(tracts17)
dim(allTracts)
View(allTracts)
