# Experimental / scratch-pad R code

# Used to keep track of things that I don't want to lose, but are not part of any published visualization or notebook
# Code may or may not run as things evolve...

library(tidyverse)
library(readr)
library(stringr)
library(sp)
library(rgdal)
library(rgeos)
library(ggmap)
library(memoise)
library(lubridate)
library(XML)
library(tidycensus)
library(scales)
library(rvest)
library(ggrepel)
library(ggthemes)

getArrests <- function() {
  chargeCodeXml <- read_xml('/opt/data/police-data/baltimore/cjiscode.xml')
  chargeCodes <- chargeCodeXml %>% xml_find_all('//cjischarge') %>%
    as_list() %>%
    map_df(function(l) {
      map(l, function(ll){ll[[1]]}) %>% data.frame(stringsAsFactors=FALSE) %>% as_tibble()
    })
  
  chargeCodes <- xmlToDataFrame(getNodeSet(xmlParse('/opt/data/police-data/baltimore/cjiscode.xml'), '//cjischarge'), stringsAsFactors=FALSE) %>%
    select(CJISCode=cjisclass, Description=describe35, Felony=felony, contains('date')) %>%
    mutate(Felony=Felony=='Y', CJISCode=trimws(CJISCode)) %>% distinct() %>%
    group_by(CJISCode) %>% mutate(n=n()) %>% filter(n==1 | date_end == '') %>%
    ungroup() %>%
    bind_rows(c('2A0700', 'ATT-ROBBERY', TRUE,
                '3 5010', 'THEFT:LESS $500 VALUE', FALSE,
                '6 5599', 'LITTER/DUMP:UNDER 100 LBS', FALSE,
                '3 5060', 'EXTORTION:VALUE $500. PLUS', TRUE,
                '2 2030', 'FIREARM-POSS W/FEL CONV', TRUE,
                '2C0230', 'CON-NARC - MANUF/DIST LG AMT', FALSE,
                '2 0900', 'MURDER-FIRST DEGREE', TRUE,
                '3 4145', 'CRED CARD PERSONATN + $500', FALSE,
                '2 0231', 'NARC POSS W/INTENT- LG AMT', TRUE,
                '1 0333', 'CAUSE LITTER/DUMP: L/T 100 LBS', FALSE,
                '1 0468', 'Counterfeiting--Counterfeiting United States currency', TRUE,
                '3 5210', 'FIREARM: ALTER ETC ID NUMBER', FALSE) %>% matrix(ncol=3, byrow=TRUE) %>% as_tibble() %>%
                rename(CJISCode=V1, Description=V2, Felony=V3) %>% mutate(Felony=as.logical(Felony))) %>%
    mutate(Murder=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='MURDER|HOMICIDE|MANSLAUGHTER') &
             !grepl(x=Description, pattern='ATT|ACCESSORY'),
           Rape=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='RAPE') &
             !grepl(x=Description, pattern='ATT'),
           Robbery=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='ROBBERY|CARJACK') &
             !grepl(x=Description, pattern='ACCESS|GRAVE'),
           Burglary=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='BURGL|B&E') &
             !grepl(x=Description, pattern='ACCESSORY'),
           Assault=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='ASSAULT') &
             !grepl(x=Description, pattern='WPN|WEAPON'),
           Arson=grepl(x=CJISCode, pattern='. .+') &
             grepl(x=Description, pattern='ARSON') &
             !grepl(x=Description, pattern='THREAT|ATT')) %>%
    mutate(Type=case_when(
      .$Murder ~ 'Murder',
      .$Rape ~ 'Rape',
      .$Robbery ~ 'Robbery',
      .$Assault ~ 'Assault',
      .$Burglary ~ 'Burglary',
      .$Arson ~ 'Arson'
    )) %>%
    select(CJISCode, Description, Felony, Murder, Rape, Robbery, Burglary, Assault, Arson, Type)
  
  Arrests <- read_csv('/opt/data/police-data/baltimore/BPD_Arrests.csv',
                      col_types=cols(
                        Arrest = col_integer(),
                        Age = col_integer(),
                        Sex = col_character(),
                        Race = col_character(),
                        ArrestDate = col_character(),
                        ArrestTime = col_character(),
                        ArrestLocation = col_character(),
                        IncidentOffense = col_character(),
                        IncidentLocation = col_character(),
                        Charge = col_character(),
                        ChargeDescription = col_character(),
                        District = col_character(),
                        Post = col_integer(),
                        Neighborhood = col_character(),
                        `Location 1` = col_character()
                      )) %>%
    mutate(coords=str_replace_all(`Location 1`, '\\(|\\)|[ ]', '')) %>%
    select(-`Location 1`) %>%
    separate(coords, c('Latitude', 'Longitude'), sep=',', convert=TRUE) %>%
    filter(!is.na(Latitude)) %>%
    mutate(ArrestDate=as_date(ArrestDate, format='%m/%d/%Y')) %>%
    mutate(Charge=ifelse(Charge=='1 1301', '1 1300', Charge)) %>%
    left_join(chargeCodes, by=c('Charge'='CJISCode'))
  Arrests
}

Arrests <- getPart1Arrests()

mdCensusBlock <- readOGR('/opt/data/Shapefiles/cb_2016_24_tract_500k/', 'cb_2016_24_tract_500k')
county <- readOGR('/opt/data/Shapefiles/cb_2015_us_county_500k/', 'cb_2015_us_county_500k')

points <- SpatialPoints(cbind(Arrests$Longitude, Arrests$Latitude), proj4string=CRS('+proj=longlat')) %>%
  spTransform(proj4string(mdCensusBlock))
overlay <- over(points, mdCensusBlock) %>% select(CensusTract=GEOID, County=COUNTYFP) %>%
  mutate_if(is.factor, as.character)

Arrests <- bind_cols(Arrests, overlay) %>%
  filter(County=='510')

mdCensusBlockDf <- fortify(mdCensusBlock)
mdCensusBlockData <- mdCensusBlock@data
mdCensusBlockData$id <- rownames(mdCensusBlockData)
mdCensusBlockData <- mdCensusBlockData %>% mutate_if(is.factor, as.character) %>% filter(COUNTYFP=='510')
mdCensusBlockDf <- mdCensusBlockDf %>% inner_join(mdCensusBlockData, by='id')

get_acs_mem <- memoize(get_acs)

acsData <- get_acs_mem(geography='tract', state='MD', county='Baltimore City', output='wide',
                   variables=c('B01003_001E',
                               'B19013_001E',
                               'B01002_001E',
                               'B01001A_001E',
                               'C18120_001E',
                               'C18120_003E',
                               'B06008_003E')) %>%
  select(GEOID,
         TotalPopulation=B01003_001E,
         WhitePopulation=B01001A_001E,
         MedianHouseholdIncome=B19013_001E,
         MedianAge=B01002_001E,
         LaborForce=C18120_001E,
         Employed=C18120_003E,
         Married=B06008_003E) %>%
  mutate(PercentWhite=WhitePopulation/TotalPopulation,
         UnemploymentRate=(LaborForce-Employed)/LaborForce,
         PercentMarried=Married/TotalPopulation)

CensusBlockArrestSummary <- Arrests %>%
  group_by(CensusTract) %>%
  summarize(Arrests=n()) %>%
  left_join(acsData, by=c('CensusTract'='GEOID')) %>%
  mutate(ArrestsPerCapita=Arrests/TotalPopulation, id=row_number())

model <- lm(data=CensusBlockArrestSummary, formula=ArrestsPerCapita ~ PercentWhite + MedianAge + PercentMarried + MedianHouseholdIncome)
summary(model)
FittedArrestsPerCapita=fitted(model)
fit <- tibble(FittedArrestsPerCapita, id=names(FittedArrestsPerCapita)) %>% mutate(id=as.integer(id))

CensusBlockArrestSummary <- CensusBlockArrestSummary %>% left_join(fit, by='id') %>% select(-id) %>%
  mutate(ArrestsPerCapitaResidual=ArrestsPerCapita-FittedArrestsPerCapita)

CensusBlockArrestSummaryPolygons <- CensusBlockArrestSummary %>% right_join(mdCensusBlockDf, by=c('CensusTract'='GEOID'))

baltimoreCountyBoundary <- subset(county, COUNTYFP=='510')
baltimoreCountyBoundaryDf <- fortify(baltimoreCountyBoundary)

baltimore11 <- get_map('Baltimore', zoom=11)
baltimore12 <- get_map('Baltimore', zoom=12)
baltimore13 <- get_map('Baltimore', zoom=13)
baltimore14 <- get_map('Baltimore', zoom=14)
baltimore15 <- get_map('Baltimore', zoom=15)

ggplot(data=mdCensusBlockDf) +
  geom_point(data=Arrests, mapping=aes(x=Longitude, y=Latitude)) +
  geom_path(aes(x=long, y=lat, group=group)) + coord_map()

minDate <- min(Arrests$ArrestDate)
minDateS <- format(minDate, '%b %Y')
maxDate <- max(Arrests$ArrestDate)
maxDateS <- format(maxDate, '%b %Y')

ggmap(baltimore12) +
  stat_density2d(data=Arrests, mapping=aes(x=Longitude, y=Latitude, fill=..level..), alpha=0.2, geom='polygon', bins=50, na.rm=TRUE) +
  scale_fill_continuous(limits=c(0,600), low='#ffdfdc', high='#e95059', breaks=seq(0, 600, by=200)) +
  geom_point(data=Arrests %>% filter(Type=='Murder'), mapping=aes(x=Longitude, y=Latitude, color=Type), color='blue', alpha=.3, size=1) +
  geom_path(data=baltimoreCountyBoundaryDf, mapping=aes(x=long, y=lat), na.rm=TRUE) +
  theme_void() + theme(legend.position='bottom') +
  labs(fill='Arrests', title=paste0('Baltimore PD Arrests, ', minDateS, ' - ', maxDateS), subtitle='Murders indicated by blue dots')

ggmap(baltimore12) +
  stat_density2d(data=Arrests, mapping=aes(x=Longitude, y=Latitude, fill=..level..), alpha=0.2, geom='polygon', bins=50, na.rm=TRUE) +
  scale_fill_continuous(limits=c(0,600), low='#ffdfdc', high='#e95059', breaks=seq(0, 600, by=200)) +
  geom_point(data=Arrests %>% filter(Type=='Shooting'), mapping=aes(x=Longitude, y=Latitude, color=Type), color='blue', alpha=.3, size=1) +
  geom_path(data=baltimoreCountyBoundaryDf, mapping=aes(x=long, y=lat), na.rm=TRUE) +
  theme_void() + theme(legend.position='bottom') +
  labs(fill='Arrests', title=paste0('Baltimore PD Arrests, ', minDateS, ' - ', maxDateS), subtitle='Shootings indicated by blue dots')

ArrestHourSummary <- Arrests %>% filter(year(ArrestDate)==2015) %>% group_by(ArrestDate, ArrestHour) %>% summarize(n=n()) %>%
  ungroup() %>%
  mutate(n=case_when(.$n > 100 ~ as.integer(NA), TRUE ~ .$n)) %>% arrange(ArrestDate, ArrestHour)

ravens <- read_html('https://en.wikipedia.org/wiki/2015_Baltimore_Ravens_season') %>%
  html_nodes(xpath="//h3/span[@id='Regular_season']/../following-sibling::table[1]") %>%
  html_table(fill=TRUE) %>% .[[1]] %>% .[,1:3] %>% as_tibble() %>%
  filter(Date != 'Bye') %>%
  mutate(Date=as_date(paste0(Date, ', ', ifelse(startsWith(Date, 'January'), '2016', '2015')), format='%B %d, %Y')) %>%
  mutate(HomeGame=!startsWith(Opponent, 'at')) %>%
  separate(Opponent, paste0('X', 1:4), fill='left') %>%
  mutate_at(vars(starts_with('X')), function(v) {ifelse(is.na(v) | grepl(x=v, pattern='^at$'), '', v)}) %>%
  unite(OpponentCity, matches('X[1-3]'), sep=' ') %>% rename(OpponentTeam=X4) %>%
  filter(year(Date)==2015)

ArrestDateSummary <- ArrestHourSummary %>%
  group_by(ArrestDate) %>% summarize(n=sum(n)) %>% arrange(ArrestDate) %>%
  left_join(ravens, by=c('ArrestDate'='Date'))

ggplot(ArrestDateSummary) +
  geom_path(aes(x=ArrestDate, y=n)) +
  geom_point(data=filter(ArrestDateSummary, HomeGame), mapping=aes(x=ArrestDate, y=n), color='#9370DB') +
  geom_text(data=filter(ArrestDateSummary, HomeGame), mapping=aes(x=ArrestDate, y=200, label=OpponentTeam),
            color='#9370DB', size=3, angle=90, hjust=0) +
  scale_x_date(date_breaks='1 months', date_labels='%b-%e', limits=as_date(c('2015-01-01', '2015-12-31'))) +
  labs(x=NULL, y=NULL)

ggplot(ArrestHourSummary) +
  geom_tile(aes(x=ArrestDate, y=ArrestHour, fill=n)) +
  scale_fill_gradient(low = "white",high = "steelblue") + scale_y_discrete(limits=0:23) +
  scale_x_date(date_breaks='1 months', date_labels='%b-%e') +
  theme(axis.ticks.y=element_blank(), panel.background=element_blank()) +
  labs(x=NULL, y=NULL, fill='Arrests/hour', title='Arrests by Time of Day, each day in 2015')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=ArrestsPerCapita)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90', breaks=seq(0, 3, by=.5), limits=c(0, 3)) +
  coord_map() + theme_void() +
  labs(fill='Arrests/Capita',
       title=paste0('Baltimore PD Arrests, ', minDateS, ' - ', maxDateS), subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=PercentWhite)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90', breaks=seq(0, 1, by=.25), limits=c(0, 1), labels=percent) +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Percent of Tract Population that is White', subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=MedianAge)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90') +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Median Age of Tract Population', subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=UnemploymentRate)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90', labels=percent) +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Unemployment Rate', subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=MedianHouseholdIncome)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90', labels=dollar) +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Median Household Income', subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=PercentMarried)) +
  geom_path(na.rm=TRUE) +
  scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90', labels=percent) +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Percentage of Population Currently Married', subtitle='By Census Tract')

ggplot(CensusBlockArrestSummaryPolygons, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(mapping=aes(fill=ArrestsPerCapitaResidual)) +
  geom_path(na.rm=TRUE) +
  #scale_fill_continuous(low='#ffffff', high='#e95059', na.value='grey90') +
  scale_fill_gradient2(low="#0099F7", high="#F11712", midpoint=0, na.value='grey90') +
  coord_map() + theme_void() +
  labs(fill=NULL,
       title='Analysis of Regression Residuals', subtitle='By Census Tract')

DowntownTractArrests <- Arrests %>% filter(CensusTract=='24510040100')
hq <- tibble(Latitude=39.290219, Longitude=-76.607758)

ggmap(baltimore15) +
  geom_density2d(data=DowntownTractArrests, mapping=aes(x=Longitude, y=Latitude),
                 bins=10, na.rm=TRUE) +
  geom_point(data=hq, mapping=aes(x=Longitude, y=Latitude), size=5, color='red') +
  theme_void() +
  labs(title='Arrest Density Map in Anomalous Downtown Tract', subtitle='Red Dot = BPD HQ and Central District HQ')

landUse <- readOGR('/opt/data/police-data/baltimore/shapefiles/landuse/', 'landuse')
landUse <- spTransform(landUse, proj4string(mdCensusBlock))
landUseData <- landUse@data
landUseData$ID <- rownames(landUseData)

downtownTract <- subset(mdCensusBlock, GEOID=='24510040100')
landUse <- gIntersection(landUse, downtownTract, byid=c(TRUE, FALSE))
landUseDf <- fortify(landUse) %>%
  inner_join(landUseData, by=c('id'='ID')) %>%
  mutate(
    LandUse=case_when(
      grepl(x=.$LU_2008, pattern='.+Commercial/.+Residential') ~ 'Dual (High Intensity)',
      grepl(x=.$LU_2008, pattern='.+Residential') ~ 'Residential',
      grepl(x=.$LU_2008, pattern='.+Commercial') ~ 'Commercial',
      TRUE ~ 'Other'
    )
  )

ggmap(baltimore15) +
  geom_polygon(data=landUseDf, mapping=aes(x=long, y=lat, group=group, fill=LandUse), alpha=.4) +
  theme_void() + theme(legend.position='bottom') +
  labs(fill='Land Use:', title='Land Use in Anomalous Downtown Census Tract', subtitle='Source: City of Baltimore')

ggplot(data=CensusBlockArrestSummary, mapping=aes(y=Arrests, x=TotalPopulation)) + geom_point() + geom_smooth(method='lm') +
  geom_label_repel(data=CensusBlockArrestSummary %>% filter(CensusTract=='24510040100'), mapping=aes(label=CensusTract),
                   box.padding=unit(20, 'mm'), label.padding=unit(2, 'mm'), force=10) +
  labs(x='Census Tract Population', y='Arrests in Tract',
       title='Relationship of Arrests to Population', subtitle='By Census Tract') + theme_economist()
  
ggplot() + geom_polygon(data=mdCensusBlockDf, mapping=aes(x=long, y=lat, group=group), fill='lightblue') +
  geom_path(data=mdCensusBlockDf, mapping=aes(x=long, y=lat, group=group), color='red') +
  geom_path(data=NeighborhoodSDF, mapping=aes(x=long, y=lat, group=group), color='grey50') + coord_map()


zoningSPDF <- readOGR('/opt/data/police-data/baltimore/shapefiles/Baci2013/OVERLAYS/GenZoning', 'BaciGenzone', verbose=FALSE)
zoningSDF <- tidy(zoningSPDF)
zoningDF <- zoningSPDF@data
zoningDF$id <- rownames(zoningDF)
zoningDF <- zoningDF %>% mutate_if(is.factor, as.character)
zoningSDF <- inner_join(zoningSDF, zoningDF, by='id')
ggplot(data=zoningSDF, mapping=aes(x=long, y=lat, group=group)) + geom_path() +
  geom_polygon(mapping=aes(fill=GENZONE)) +
  coord_equal() + theme_void() + labs(fill='Zoning Type')

landUseSPDF <- readOGR('/opt/data/police-data/baltimore/shapefiles/Baci2013/OVERLAYS/LULC/2010', 'bacilu10', verbose=FALSE)
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

ggplot(data=landUseSDF, mapping=aes(x=long, y=lat, group=group)) + geom_path() +
  geom_polygon(mapping=aes(fill=LandUseCategory)) +
  scale_fill_brewer(type='qual', palette='Accent') +
  coord_equal() + theme_void() + labs(fill='Land Use Type')

DowntownGoogleMap <- get_map(location=c(lon=-76.613, lat=39.285), zoom=15)

ggmap(DowntownGoogleMap) +
  geom_path(data=CensusBlockSDF %>% filter(GEOID=='24510040100'), mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(data=NeighborhoodSDF %>% filter(Name %in% c('Downtown', 'Inner Harbor', 'Downtown West')),
               mapping=aes(x=long, y=lat, group=group, fill=Name), alpha=.35) +
  labs(fill='Neighborhood')

ggmap(DowntownGoogleMap) +
  geom_density2d(data=DowntownArrests, mapping=aes(x=Longitude, y=Latitude), bins=20, na.rm=TRUE, alpha=.5)
  #stat_density2d(data=DowntownArrests, mapping=aes(x=Longitude, y=Latitude, fill=..level..), alpha=0.2, geom='polygon', bins=50, na.rm=TRUE)
  

Arrests %>% filter(ArrestHour %in% c(0,1,2)) %>% group_by(Type) %>% summarize(n=n()) %>% mutate(pct=n/sum(n), pcts=percent(pct)) %>%
  ggplot() +
  geom_bar(mapping=aes(x=reorder(Type, n), y=n), stat='identity') + coord_flip() +
  stat_summary(fun.y = identity, geom="text", aes(x=reorder(Type, n), y=n, label=pcts), hjust = -.25) +
  scale_y_continuous(labels=comma, limits=c(0,10000)) +
  theme_economist() +
  labs(x='Crime Type', y='Incident Count', title='Distribution of Arrests by Crime Type',
       subtitle=paste0('Downtown Baltimore Neighborhoods, ', minDateS, ' - ', maxDateS),
       caption=paste0('n=', comma(nrow(.))))

CensusBlockDf %>% mutate(PercentViolentCrime=ViolentCrimeArrests/Arrests) %>%
  ggplot() +
  geom_point(aes(x=MedianHouseholdIncome, y=MedianHousingCosts, size=ArrestsPerCapita, color=PercentViolentCrime)) +
  scale_color_gradient(low = "#c6dbef", high = "#08306b") +
  theme_economist()

CensusBlockDf %>% mutate(PercentViolentCrime=ViolentCrimeArrests/Arrests) %>%
  ggplot() +
  geom_point(aes(x=MedianHouseholdIncome, y=ResidentialPercentage, size=ArrestsPerCapita, color=PercentViolentCrime)) +
  scale_color_gradient(low = "#c6dbef", high = "#08306b", labels=percent) +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=dollar) +
  labs(
    y='% of Tract Area Classified as Residential',
    x='Median Household Income',
    color='% Violent Crime',
    size='Arrests/Capita',
    title='Census Tract Arrests Per Capita, by Land Use and Median Household Income',
    subtitle=paste0('Baltimore City, ', minDateS, ' - ', maxDateS)
  ) +
  theme_economist() +
  theme(legend.text=element_text(size=10), legend.position='bottom')

CensusBlockDf %>% mutate(PercentViolentCrime=ViolentCrimeArrests/Arrests, PopDensity=Population/(TotalArea*3.86102e-7)) %>%
  ggplot() +
  geom_point(aes(x=MedianHouseholdIncome, y=PopDensity, size=ArrestsPerCapita, color=PercentViolentCrime)) +
  scale_color_gradient(low = "#c6dbef", high = "#08306b") +
  lims(y=c(0, 40000)) +
  theme_economist()

CensusBlockDf %>% mutate(PercentViolentCrime=ViolentCrimeArrests/Arrests, PopDensity=Population/(TotalArea*3.86102e-7)) %>%
  ggplot() +
  geom_point(aes(x=PercentWhite, y=ResidentialPercentage, size=ArrestsPerCapita, color=PercentViolentCrime)) +
  scale_color_gradient(low = "#c6dbef", high = "#08306b") +
  theme_economist()

CensusBlockDf %>% mutate(PercentViolentCrime=ViolentCrimeArrests/Arrests) %>%
  ggplot() +
  geom_point(aes(x=PercentWhite, y=ResidentialPercentage, size=ArrestsPerCapita, color=PercentViolentCrime)) +
  scale_color_gradient(low = "#c6dbef", high = "#08306b", labels=percent) +
  scale_y_continuous(labels=percent) +
  scale_x_continuous(labels=percent) +
  labs(
    y='% of Tract Area Classified as Residential',
    x='% of Tract Population that is White',
    color='% Violent Crime',
    size='Arrests/Capita',
    title='Census Tract Arrests Per Capita, by Land Use and Race',
    subtitle=paste0('Baltimore City, ', minDateS, ' - ', maxDateS)
  ) +
  theme_economist() +
  theme(legend.text=element_text(size=10), legend.position='bottom')


  
  
  
  
