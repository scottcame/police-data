# Loads Part I arrest data (created in Baltimore-ReadData.R) into a dimensional database
# Model for the database resides in the SQL Power Architect artifact in the db directory
# Mondrian schema for the resulting database resides in the mondrian directory

library(tidyverse)
library(RMySQL)
library(openxlsx)

connection <- dbConnect(MySQL(), host="localhost", dbname="ojbc_analytics_police_data", username="root")

UNKNOWN_ID_VALUE <- 99999
UNKNOWN_DESCRIPTION_VALUE <- 'Unknown'

PremiseTypeCategory <- read.xlsx('PremiseTypeConversion.xlsx')

PremiseType <- Arrests %>%
  select(PremiseTypeDescription=Premise) %>%
  distinct() %>%
  filter(!is.na(PremiseTypeDescription)) %>%
  arrange(PremiseTypeDescription) %>%
  mutate(PremiseTypeID=row_number()) %>%
  inner_join(PremiseTypeCategory, by='PremiseTypeDescription') %>%
  mutate(PremiseTypeID=ifelse(PremiseTypeDescription=='UNKNOWN', UNKNOWN_ID_VALUE, PremiseTypeID))

WeaponType <- Arrests %>%
  select(WeaponTypeDescription=Weapon) %>%
  distinct() %>%
  filter(!is.na(WeaponTypeDescription)) %>%
  arrange(WeaponTypeDescription) %>%
  mutate(WeaponTypeID=row_number()) %>%
  bind_rows(
    tibble(WeaponTypeID=UNKNOWN_ID_VALUE,
           WeaponTypeDescription=UNKNOWN_DESCRIPTION_VALUE)
  )

DistrictType <- Arrests %>%
  select(DistrictTypeDescription=District) %>%
  distinct() %>%
  filter(!is.na(DistrictTypeDescription)) %>%
  arrange(DistrictTypeDescription) %>%
  mutate(DistrictTypeID=row_number()) %>%
  bind_rows(
    tibble(DistrictTypeID=UNKNOWN_ID_VALUE,
           DistrictTypeDescription=UNKNOWN_DESCRIPTION_VALUE)
  )

NeighborhoodType <- Arrests %>%
  select(NeighborhoodTypeDescription=Neighborhood) %>%
  distinct() %>%
  filter(!is.na(NeighborhoodTypeDescription)) %>%
  arrange(NeighborhoodTypeDescription) %>%
  mutate(NeighborhoodTypeID=row_number()) %>%
  bind_rows(
    tibble(NeighborhoodTypeID=UNKNOWN_ID_VALUE,
           NeighborhoodTypeDescription=UNKNOWN_DESCRIPTION_VALUE)
  )

IncidentType <- Arrests %>%
  select(IncidentTypeDescription=Type) %>%
  distinct() %>%
  filter(!is.na(IncidentTypeDescription)) %>%
  arrange(IncidentTypeDescription) %>%
  mutate(IncidentTypeID=row_number()) %>%
  bind_rows(
    tibble(IncidentTypeID=UNKNOWN_ID_VALUE,
           IncidentTypeDescription=UNKNOWN_DESCRIPTION_VALUE)
  )

HourType <- tibble(
  HourTypeID=as.integer(0:23),
  HourTypeDescription=c('Midnight', paste0(1:11, ':00'), 'Noon', paste0(13:23, ':00')),
  HourTypeCategory=c(rep('Early Morning', 6), rep('Morning', 6), rep('Afternoon', 6), rep('Evening', 6)),
  HourTypeCategorySort=c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6))
)

buildDateDimensionTable <- function(minDate, maxDate, datesToExclude=as_date(x = integer(0)),
                                    unknownCodeTableValue=UNKNOWN_ID_VALUE) {
  minDate <- as_date(minDate)
  maxDate <- as_date(maxDate)
  writeLines(paste0("Building date dimension, earliest date=", minDate, ", latestDate=", maxDate))
  DateDf <- tibble(CalendarDate=seq(minDate, maxDate, by="days")) %>%
    mutate(DateTypeID=as.integer(format(CalendarDate, "%Y%m%d")),
           Year=year(CalendarDate),
           YearLabel=as.character(Year),
           CalendarQuarter=quarter(CalendarDate),
           Month=month(CalendarDate),
           MonthName=as.character(month(CalendarDate, label=TRUE, abbr=FALSE)),
           FullMonth=format(CalendarDate, paste0(Year, "-", Month)),
           Day=day(CalendarDate),
           DayOfWeek=as.character(wday(CalendarDate, label=TRUE, abbr=FALSE)),
           DayOfWeekSort=wday(CalendarDate),
           DateMMDDYYYY=format(CalendarDate, "%m%d%Y")
    ) %>%
    bind_rows(tibble(CalendarDate=as_date("1899-01-01"),
                     DateTypeID=unknownCodeTableValue,
                     Year=0,
                     YearLabel='Unk',
                     CalendarQuarter=0,
                     Month=0,
                     MonthName='Unknown',
                     FullMonth='Unknown',
                     Day=0,
                     DayOfWeek='Unknown',
                     DayOfWeekSort=0,
                     DateMMDDYYYY='Unknown'))
  DateDf <- DateDf %>% filter(!(CalendarDate %in% datesToExclude))
  writeLines(paste0("Adding ", nrow(DateDf), " new dates to the Date dimension"))
  DateDf
}

maxDate <- max(Arrests$ArrestDate)
minDate <- min(Arrests$ArrestDate)

DateType <- buildDateDimensionTable(minDate, maxDate)

Incident <- Arrests %>%
  left_join(PremiseType, by=c('Premise'='PremiseTypeDescription')) %>%
  mutate(PremiseTypeID=ifelse(is.na(PremiseTypeID), UNKNOWN_ID_VALUE, PremiseTypeID)) %>%
  left_join(WeaponType, by=c('Weapon'='WeaponTypeDescription')) %>%
  mutate(WeaponTypeID=ifelse(is.na(WeaponTypeID), UNKNOWN_ID_VALUE, WeaponTypeID)) %>%
  left_join(DistrictType, by=c('District'='DistrictTypeDescription')) %>%
  mutate(DistrictTypeID=ifelse(is.na(DistrictTypeID), UNKNOWN_ID_VALUE, DistrictTypeID)) %>%
  left_join(NeighborhoodType, by=c('Neighborhood'='NeighborhoodTypeDescription')) %>%
  mutate(NeighborhoodTypeID=ifelse(is.na(NeighborhoodTypeID), UNKNOWN_ID_VALUE, NeighborhoodTypeID)) %>%
  left_join(IncidentType, by=c('Type'='IncidentTypeDescription')) %>%
  mutate(IncidentTypeID=ifelse(is.na(IncidentTypeID), UNKNOWN_ID_VALUE, IncidentTypeID)) %>%
  mutate(HourTypeID=ifelse(is.na(ArrestHour), UNKNOWN_ID_VALUE, ArrestHour)) %>%
  mutate(DateTypeID=as.integer(format(ArrestDate, '%Y%m%d'))) %>%
  mutate(IncidentID=row_number()) %>%
  select(IncidentID,
         DateTypeID,
         HourTypeID,
         IncidentTypeID,
         PremiseTypeID,
         WeaponTypeID,
         DistrictTypeID,
         NeighborhoodTypeID)

writeTable <- function(name, df, colsToExclude=character(0)) {
  dbClearResult(dbSendStatement(connection, paste0('truncate ', name)))
  dbWriteTable(connection, name, df %>% select(-one_of(colsToExclude)), append=TRUE, row.names=FALSE)
}

dbClearResult(dbSendStatement(connection, paste0('set foreign_key_checks=0')))
writeTable('PremiseType', PremiseType)
writeTable('WeaponType', WeaponType)
writeTable('IncidentType', IncidentType)
writeTable('DistrictType', DistrictType)
writeTable('NeighborhoodType', NeighborhoodType)
writeTable('HourType', HourType)
writeTable('DateType', DateType)
writeTable('Incident', Incident)

