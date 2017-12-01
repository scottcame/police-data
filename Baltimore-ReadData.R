library(readr)
library(tidyverse)
library(stringr)
library(lubridate)
library(rgdal)
library(rgeos)
library(broom)
library(tidycensus)
library(memoise)
library(ggmap)

CallsForService <- read_csv('/opt/data/police-data/baltimore/911_Calls_for_Service.csv') %>%
  mutate(coords=str_replace_all(location, '\\(|\\)|[ ]', '')) %>%
  select(-location) %>%
  separate(coords, c('Latitude', 'Longitude'), sep=',', convert=TRUE, fill='right') %>%
  filter(!is.na(Latitude)) %>%
  mutate(callDateTime=parse_date_time(callDateTime, 'mdY H:M:s p', tz='America/New_York')) %>%
  mutate(CallDate=as_date(callDateTime), CallHour=hour(callDateTime)) %>%
  rename(CallDateTime=callDateTime) %>%
  filter(!is.na(CallDateTime))

getPart1Arrests <- function() {
  Part1Incidents <- read_csv('/opt/data/police-data/baltimore/BPD_Part_1_Victim_Based_Crime_Data.csv', col_types=cols()) %>%
    mutate(Neighborhood=case_when(
      .$Neighborhood=='Carroll - Camden Industri' ~ 'Carroll - Camden Industrial Area',
      .$Neighborhood=='Coldstream Homestead Mont' ~ 'Coldstream Homestead Montebello',
      .$Neighborhood=='Concerned Citizens Of For' ~ 'Concerned Citizens Of Forest Park',
      .$Neighborhood=='Coppin Heights/Ash-Co-Eas' ~ 'Coppin Heights/Ash-Co-East',
      .$Neighborhood=='Curtis Bay Industrial Are' ~ 'Curtis Bay Industrial Area',
      .$Neighborhood=='Locust Point Industrial A' ~ 'Locust Point Industrial Area',
      .$Neighborhood=='Middle Branch/Reedbird Pa' ~ 'Middle Branch/Reedbird Parks',
      .$Neighborhood=='North Roland Park/Poplar' ~ 'North Roland Park/Poplar Hill',
      .$Neighborhood=='Northwest Community Actio' ~ 'Northwest Community Action',
      .$Neighborhood=='Orangeville Industrial Ar' ~ 'Orangeville Industrial Area',
      .$Neighborhood=='Patterson Park Neighborho' ~ 'Patterson Park Neighborhood',
      .$Neighborhood=='Penrose/Fayette Street Ou' ~ 'Penrose/Fayette Street Outreach',
      .$Neighborhood=='Rosemont Homeowners/Tenan' ~ 'Rosemont Homeowners/Tenants',
      .$Neighborhood=='Spring Garden Industrial' ~ 'Spring Garden Industrial Area',
      .$Neighborhood=='Stonewood-Pentwood-Winsto' ~ 'Stonewood-Pentwood-Winston',
      .$Neighborhood=='Washington Village/Pigtow' ~ 'Washington Village/Pigtown',
      TRUE ~ Neighborhood
    )) %>%
    mutate(Type=case_when(
      .$Description=='HOMICIDE' ~ 'Murder',
      .$Description=='SHOOTING' ~ 'Shooting',
      .$Description=='BURGLARY' ~ 'Burglary',
      .$Description=='AGG. ASSAULT' ~ 'Aggravated Assault',
      .$Description=='AUTO THEFT' ~ 'Motor Vehicle Theft',
      .$Description=='ARSON' ~ 'Arson',
      .$Description=='RAPE' ~ 'Rape',
      startsWith(.$Description, 'ROBBERY') ~ 'Robbery',
      startsWith(.$Description, 'LARCENY') ~ 'Larceny',
      grepl(x=.$Description, pattern='ASSAULT') ~ 'Other Assault'
    )) %>%
    mutate(ViolentCrime=Type %in% c('Murder', 'Aggravated Assault', 'Rape', 'Robbery', 'Shooting')) %>%
    mutate(coords=str_replace_all(`Location 1`, '\\(|\\)|[ ]', '')) %>%
    select(-`Location 1`) %>%
    separate(coords, c('Latitude', 'Longitude'), sep=',', convert=TRUE) %>%
    filter(!is.na(Latitude)) %>%
    mutate(ArrestDate=as_date(CrimeDate, format='%m/%d/%Y')) %>%
    rename(ArrestTime=CrimeTime) %>%
    mutate(ArrestTime=case_when(
      str_length(.$ArrestTime)<3 ~ as.character(NA),
      str_length(.$ArrestTime)==3 ~ paste0('0', str_sub(.$ArrestTime, 1, 1), ':', str_sub(.$ArrestTime, 2, 3)),
      str_length(.$ArrestTime)==4 ~ paste0(str_sub(.$ArrestTime, 1, 2), ':', str_sub(.$ArrestTime, 3, 4)),
      .$ArrestTime %in% c('00:00:00','00:01:00') ~ as.character(NA),
      TRUE ~ .$ArrestTime
    )) %>%
    mutate(ArrestHour=as.integer(str_sub(ArrestTime, 1, 2)))
  Part1Incidents
}

Arrests <- getPart1Arrests()

minDate <- max(min(Arrests$ArrestDate), min(CallsForService$CallDate))
minDateS <- format(minDate, '%b %Y')
maxDate <- min(max(Arrests$ArrestDate), max(CallsForService$CallDate))
maxDateS <- format(maxDate, '%b %Y')

Arrests <- Arrests %>% filter(ArrestDate >= minDate) %>% filter(ArrestDate <= maxDate)
CallsForService <- CallsForService %>% filter(CallDate >= minDate) %>% filter(CallDate <= maxDate)

get_acs_mem <- memoize(get_acs)

acsData2015 <- get_acs_mem(geography='tract', state='MD', county='Baltimore City', output='wide',
                       variables=c('B01003_001E',
                                   'B19013_001E',
                                   'B01002_001E',
                                   'B01001A_001E',
                                   'C18120_001E',
                                   'C18120_003E',
                                   'B25105_001E',
                                   'B06008_003E')) %>%
  select(GEOID,
         Population=B01003_001E,
         WhitePopulation=B01001A_001E,
         MedianHouseholdIncome=B19013_001E,
         MedianAge=B01002_001E,
         MedianHousingCosts=B25105_001E,
         LaborForce=C18120_001E,
         Employed=C18120_003E,
         Married=B06008_003E) %>%
  mutate(PercentWhite=WhitePopulation/Population,
         UnemploymentRate=(LaborForce-Employed)/LaborForce,
         PercentMarried=Married/Population)

acsData2010 <- get_acs_mem(geography='tract', state='MD', county='Baltimore City', output='wide',
                           variables=c('B01003_001E'), endyear=2010) %>%
  select(GEOID, Population=B01003_001E)

CountySPDF <- readOGR('/opt/data/Shapefiles/cb_2015_us_county_500k/', 'cb_2015_us_county_500k', verbose=FALSE) %>%
  subset(COUNTYFP=='510' & STATEFP=='24')
CountySDF <- suppressMessages(tidy(CountySPDF))

CensusBlockSPDF <- readOGR('/opt/data/Shapefiles/cb_2016_24_tract_500k/', 'cb_2016_24_tract_500k', verbose=FALSE) %>%
  subset(COUNTYFP=='510')

CensusBlockSDF <- suppressMessages(tidy(CensusBlockSPDF))
CensusBlockDf <- CensusBlockSPDF@data
CensusBlockDf$id <- rownames(CensusBlockDf)

ArrestLocations <- SpatialPoints(cbind(Arrests$Longitude, Arrests$Latitude), proj4string=CRS('+proj=longlat')) %>%
  spTransform(proj4string(CensusBlockSPDF))

Arrests <- Arrests %>% bind_cols(over(ArrestLocations, CensusBlockSPDF) %>%
                                   select(CensusTract=GEOID) %>% mutate_if(is.factor, as.character))

CallsForServiceLocations <- SpatialPoints(cbind(CallsForService$Longitude, CallsForService$Latitude), proj4string=CRS('+proj=longlat')) %>%
  spTransform(proj4string(CensusBlockSPDF))

CallsForService <- CallsForService %>% bind_cols(over(CallsForServiceLocations, CensusBlockSPDF) %>%
                                                   select(CensusTract=GEOID) %>% mutate_if(is.factor, as.character))

CensusBlockDf <- CensusBlockDf %>%
  mutate_if(is.factor, as.character) %>%
  inner_join(acsData2015, by='GEOID') %>%
  inner_join(Arrests %>% group_by(CensusTract) %>% summarize(Arrests=n(), ViolentCrimeArrests=sum(ViolentCrime)), by=c('GEOID'='CensusTract')) %>%
  inner_join(CallsForService %>% group_by(CensusTract) %>% summarize(CallsForService=n()), by=c('GEOID'='CensusTract')) %>%
  mutate(ArrestsPerCapita=Arrests/Population, ViolentCrimeArrestsPerCapita=ViolentCrimeArrests/Population, CallsForServicePerCapita=CallsForService/Population,
         ArrestsPerCallForService=Arrests/CallsForService)

TractPriorityDf <- CallsForService %>%
  filter(!is.na(CensusTract)) %>%
  mutate(UrgentCalls=priority %in% c('Emergency', 'High'), NonUrgentCalls=priority=='Low') %>%
  select(CensusTract, UrgentCalls, NonUrgentCalls) %>%
  group_by(CensusTract) %>%
  summarize_all(sum, na.rm=TRUE) %>%
  mutate(UrgentCallRatio=UrgentCalls/NonUrgentCalls) %>%
  select(CensusTract, UrgentCallRatio)

CensusBlockDf <- CensusBlockDf %>% inner_join(TractPriorityDf, by=c('GEOID'='CensusTract'))

landUseSPDF <- readOGR('/opt/data/police-data/baltimore/shapefiles/Baci2013/OVERLAYS/LULC/2010', 'bacilu10', verbose=FALSE) %>%
  spTransform(proj4string(CensusBlockSPDF))
landUseSDF <- tidy(landUseSPDF)
landUseDF <- landUseSPDF@data
landUseDF$id <- rownames(landUseDF)
landUseDF <- landUseDF %>% mutate_if(is.factor, as.character)

landUseCodes <- c(
  '11', 'Low-density residential',
  '12', 'Medium-density residential',
  '13', 'High-density residential',
  '14', 'Commercial',
  '15', 'Industrial',
  '16', 'Institutional',
  '17', 'Extractive',
  '18', 'Open urban land',
  '21', 'Cropland',
  '22', 'Pasture',
  '23', 'Orchards/vineyards/horticulture',
  '24', 'Feeding operations',
  '25', 'Row and garden crops',
  '41', 'Deciduous forest',
  '42', 'Evergreen forest',
  '43', 'Mixed forest',
  '44', 'Brush',
  '50', 'Water',
  '60', 'Wetlands',
  '70', 'Barren land',
  '71', 'Beaches',
  '72', 'Bare exposed rock',
  '73', 'Bare ground',
  '80', 'Transportation'
) %>% matrix(byrow=TRUE, ncol=2) %>% as_data_frame() %>% rename(LU_CODE=V1, LandUseDescription=V2) %>%
  mutate(LandUseCategory=case_when(.$LU_CODE %in% c('11','12','13','14','15','16') ~ .$LandUseDescription, TRUE ~ 'Other'))

landUseDF <- inner_join(landUseDF, landUseCodes, by='LU_CODE')
landUseSDF <- inner_join(landUseSDF, landUseDF, by='id')

zoningSPDF <- readOGR('/opt/data/police-data/baltimore/shapefiles/Baci2013/OVERLAYS/GenZoning', 'BaciGenzone', verbose=FALSE)
zoningSDF <- tidy(zoningSPDF)
zoningDF <- zoningSPDF@data
zoningDF$id <- rownames(zoningDF)
zoningDF <- zoningDF %>% mutate_if(is.factor, as.character)
zoningSDF <- inner_join(zoningSDF, zoningDF, by='id')

p4s <- paste0('+proj=aea +lon_1=', min(CensusBlockSDF$long), ' +lon_2=', max(CensusBlockSDF$long),
              ' +lat_1=', min(CensusBlockSDF$lat), ' +lat_2=', max(CensusBlockSDF$lat),
              ' +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs')

CensusBlockLandUseSPDF <- gIntersection(CensusBlockSPDF, landUseSPDF, byid=c(TRUE, TRUE)) %>%
  spTransform(CRS(p4s))

areas <- gArea(CensusBlockLandUseSPDF, byid=TRUE)

CensusBlockLandUseDf <- map_df(CensusBlockLandUseSPDF@polygons, function(polygon) {
  id <- polygon@ID
  area <- areas[id]
  id <- str_split(id, ' ') %>% unlist()
  tibble(CensusBlockID=id[1], LandUseID=id[2], Area=area)
}) %>%
  inner_join(landUseDF %>% select(id, LandUseCategory), by=c('LandUseID'='id')) %>%
  mutate(LandUseCategory=case_when(
    .$LandUseCategory=='High-density residential' ~ 'HDR',
    .$LandUseCategory=='Medium-density residential' ~ 'MDR',
    .$LandUseCategory=='Low-density residential' ~ 'LDR',
    TRUE ~ .$LandUseCategory
  )) %>% spread(LandUseCategory, Area, fill=0) %>%
  select(-LandUseID) %>%
  group_by(CensusBlockID) %>%
  summarize_all(sum) %>%
  mutate(NHDR=LDR+MDR,
         TotalArea=Commercial+HDR+NHDR+Industrial+Institutional+Other,
         ResidentialPercentage=(NHDR+HDR)/TotalArea,
         HDRPercentageOfResidential=HDR/(HDR+NHDR))

CensusBlockDf <- CensusBlockDf %>% inner_join(CensusBlockLandUseDf, by=c('id'='CensusBlockID'))
CensusBlockSDF <- CensusBlockSDF %>% inner_join(CensusBlockDf, by='id')

NeighborhoodSPDF <- readOGR('/opt/data/police-data/baltimore/shapefiles/Neighborhoods/', 'Neighborhoods', verbose=FALSE) %>%
  spTransform(proj4string(CensusBlockSPDF))

NeighborhoodCentroids <-
  coordinates(NeighborhoodSPDF) %>% as_data_frame() %>%
  rename(CentroidLongitude=V1, CentroidLatitude=V2)

NeighborhoodSDF <- suppressMessages(tidy(NeighborhoodSPDF))
NeighborhoodDf <- NeighborhoodSPDF@data
NeighborhoodDf$id <- rownames(NeighborhoodDf)

NeighborhoodDf <- NeighborhoodDf %>%
  mutate_if(is.factor, as.character) %>%
  bind_cols(NeighborhoodCentroids) %>%
  inner_join(Arrests %>% group_by(Neighborhood) %>% summarize(Arrests=n()), by=c('Name'='Neighborhood')) %>%
  mutate(ArrestsPerCapita=Arrests/Population)

NeighborhoodSDF <- NeighborhoodSDF %>% inner_join(NeighborhoodDf, by='id')

Arrests <- Arrests %>% bind_cols(over(ArrestLocations, NeighborhoodSPDF) %>%
                                   select(GeocodedNeighborhood=Name) %>% mutate_if(is.factor, as.character))

CallsForService <- CallsForService %>% bind_cols(over(CallsForServiceLocations, NeighborhoodSPDF) %>%
                                   select(GeocodedNeighborhood=Name) %>% mutate_if(is.factor, as.character))

NeighborhoodDf <- NeighborhoodDf %>%
  inner_join(CallsForService %>% group_by(GeocodedNeighborhood) %>% summarize(CallsForService=n()), by=c('Name'='GeocodedNeighborhood')) %>%
  mutate(CallsForServicePerCapita=CallsForService/Population, ArrestsPerCallForService=Arrests/CallsForService)

NeighborhoodSDF <- NeighborhoodSDF %>%
  inner_join(NeighborhoodDf %>% select(id, CallsForService, CallsForServicePerCapita, ArrestsPerCallForService), by='id')

TouchingNeighborhoods <- gTouches(NeighborhoodSPDF, byid=TRUE) %>% as_tibble() %>%
  mutate(n1=as.character(as.integer(rownames(.))-1)) %>%
  gather(key='n2', value='value', -n1) %>% filter(value) %>% select(-value) %>%
  inner_join(NeighborhoodDf %>% select(id, Name), by=c('n1'='id')) %>% select(-n1) %>% rename(n1=Name) %>%
  inner_join(NeighborhoodDf %>% select(id, Name), by=c('n2'='id')) %>% select(-n2) %>% rename(n2=Name)

MismatchedArrests <- Arrests %>%
  filter(Neighborhood != GeocodedNeighborhood) %>% select(Neighborhood, GeocodedNeighborhood) %>%
  mutate(GeocodedAdjacent=map2_lgl(Neighborhood, GeocodedNeighborhood, function(Neighborhood, GeocodedNeighborhood) {
      neighbors <- TouchingNeighborhoods %>% filter(n1==Neighborhood)
      GeocodedNeighborhood %in% neighbors$n2
    }))

DowntownGoogleMap <- get_map(location=c(lon=-76.613, lat=39.285), zoom=15)
DowntownCensusTract <- CensusBlockDf %>% filter(GEOID=='24510040100')
DowntownArrests <- Arrests %>% filter(Neighborhood %in% c('Downtown', 'Inner Harbor', 'Downtown West'))
BPDhq <- tibble(Latitude=39.290219, Longitude=-76.607758)

if (exists('allObjects')) rm(allObjects)
objectNames <- ls()
map(objectNames, function(objectName) {
  get(objectName)
}) %>%
  set_names(objectNames) %>%
  saveRDS('allObjects.rds')

