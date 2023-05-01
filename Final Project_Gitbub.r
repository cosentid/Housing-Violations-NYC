library(rvest)
library("sf")
library(data.table)
library(tidyverse)
library(ggplot2)
library(jsonlite)
library(tidycensus)
library(geojsonio)
library(dplyr)

housing.violations = 
  fromJSON("https://data.cityofnewyork.us/resource/wvxf-dwi5.json?$limit=5000")
glimpse(housing.violations)

#change into good dates
housing.violations$goodDates = as.Date(housing.violations$currentstatusdate)
glimpse(housing.violations)

#get the most recent violation date for 200 days (will use more days once finalized code)
(mostRecentViolation = max(housing.violations$goodDates))
recent.housing.violations = housing.violations %>% filter(goodDates >= mostRecentViolation -4000)
glimpse(recent.housing.violations)


##clean up and factor
recent.housing.violations$violationstatus <-as.factor(recent.housing.violations$violationstatus)
recent.housing.violations$rentimpairing = as.factor(recent.housing.violations$rentimpairing)
recent.housing.violations$boro = as.factor(recent.housing.violations$boro)
recent.housing.violations$boroid = as.factor(recent.housing.violations$boroid)

#ZIP codes of NYC
locationOfMyFiles = "/Folder/ZIP_CODE_040114"
NYCmap = st_read(locationOfMyFiles, layer = "ZIP_CODE_040114")
plot(st_geometry(NYCmap))
NYCMapZip = NYCmap %>%
  mutate(zip = as.character(ZIPCODE))

#joining ZIP and violations
violations.map = inner_join(NYCMapZip, recent.housing.violations)
glimpse(violations.map)
#map number open violations REVIEW
plot(violations.map["rentimpairing"])

#Get census data for population#Get census data for populationTRUE
census_api_key("key", install=TRUE, overwrite=T)
censusData = load_variables(2018, "acs5", cache=T)
readRenviron("~/.Renviron")

populationData = get_acs(geography = "zcta",
                         variables = 'B01003_001',
                         geometry = FALSE)

populationData$zip = sub("ZCTA5 ", "", populationData$NAME)

populationData = rename(populationData, population = estimate)

violations.map = inner_join(violations.map, populationData)

#download poverty data
vars = c(poverty = 'B17001_002')
povertyData = get_acs(geography = "zcta",
                      variables = vars,
                      geometry = FALSE)

povertyData = povertyData %>% rename(povertyCount = estimate)

povertyData$zip = sub("ZCTA5 ", "", povertyData$NAME)

mainDataSet = inner_join(violations.map, povertyData, by = "zip")

mainDataSet$PovertyRate = (mainDataSet$povertyCount / mainDataSet$population)
glimpse(mainDataSet)

#remove columns we don't need
mainDataSet = select(mainDataSet, -c(latitude, longitude, apartment, certifieddate))

mainDataSet = select(mainDataSet, -c(lowhousenumber, highhousenumber, streetname, nta))

#logistic regressions

#logistic regression of rent impairing against violation status (open or closed)

ggplot(violations.map, aes(x = violationstatus, fill = rentimpairing)) + geom_bar()
(prop <- with(mainDataSet, table(rentimpairing, violationstatus)) %>% prop.table())
model1 <- glm(mainDataSet$rentimpairing ~ mainDataSet$violationstatus, family = "binomial")
summary(model1)

#logistic regression of rent impairing against poverty rate
model2 <- glm(mainDataSet$rentimpairing ~ mainDataSet$PovertyRate, family = binomial)
summary(model2)

#adding ethnicity data
vars = c('B02001_002')
ethnicityData = get_acs(geography = "zcta", variables = vars, geometry = FALSE)
ethnicityData$zip = sub("ZCTA5 ", "", ethnicityData$NAME)
ethnicityData = ethnicityData %>% rename(WhiteAlone = estimate)

#inner join mainDataSet and ethnicity data
mainDataSet = inner_join(mainDataSet, ethnicityData, by = 'zip')

#percentage white alone
mainDataSet$PctWhiteAlone = (mainDataSet$WhiteAlone / mainDataSet$population)

#logistic regression of rent impairing against white alone
(prop2 <- with(mainDataSet, table(rentimpairing, PctWhiteAlone) %>% prop.table()))
model3 <- glm(mainDataSet$rentimpairing~mainDataSet$PctWhiteAlone, family = binomial)
summary(model3)

#logistic regression of rent impairing against violation status, poverty rate and percentage white alone
model4 <- glm(mainDataSet$rentimpairing ~ mainDataSet$violationstatus + mainDataSet$PovertyRate + mainDataSet$PctWhiteAlone, family = 'binomial')
summary(model4)

