#### Bumble Bee Abundance Data and Flower Survey Data Basic Setup Script ####

#### Purpose ####

#Basic code that wrangles Ogilvie & CaraDonna long-term bee monitoring and flower survey data
#Details are annotated in the code below.
#The data.frame(s) created can be exported as .csv or used in other scripts.


#### Data Overview ####

#Data are from the Ogilvie & CaraDonna Long-term Bumble Bee Monitoring Project at the Rocky Mountain Biological Laboratory, Gothic, CO, USA
#Data files associated with this project can be found on the Open Science Framework: https://osf.io/zq36h/
#For an overview of data collection methods, see: 
#Ogilvie, J. E., & CaraDonna, P. J. (2022). 
#The shifting importance of abiotic and biotic factors across the life cycles of wild pollinators. 
#Journal of Animal Ecology, 91, 2412–2423. 
#https://doi.org/10.1111/1365-2656.13825


#### Packages ####
library(tidyverse)

#### Run climate script ####
#source("bombus.climate.base.script.00.R") #pull out and merge the snowmelt data here so we can easily calculate "days since snowmelt" rather than only aribitraty week numbers

#### Read in bee abundance data and floral abundance data ####

bee.data.raw <- read_csv("https://osf.io/hmgfe/download") #through 2023; updated 2024
flwtrans <- read_csv("https://osf.io/r4ngx/download") #through 2023; pdated 2024
flwtrans2015 <- read_csv("https://osf.io/k342u/download") #only 2015; updated 2022; 2015 data were collected slightly differently


#### Bumble Bee Data Manipulation ####

#i. clean up the data
bee.data <- bee.data.raw  %>%
  dplyr::mutate(species = stringr::str_replace(species, "rufocinctus.dark", "rufocinctus"),
                species = stringr::str_replace(species, "rufocinctus.orange", "rufocinctus")) %>%
  dplyr::filter(caste %in% c("q", "w", "m", "old.q"),
                behaviour %in% c("frg", "frg+ns", "ns", "ft", "rob.prim","rob.sec","rob.base"),
                species %in% c("appositus", "bifarius", "californicus", "flavifrons", "nevadensis", "mixtus", "occidentalis", "rufocinctus", "insularis"),
                site.code %in% c("AW", "CC", "FP", "GS", "RH", "SP")) %>%
  dplyr::mutate(caste = stringr::str_replace(caste, "old.q", "q")) %>%
  dplyr::select(site:resource.coll) %>%
  as_tibble()

#### Floral Resources Data Manipulation ####

#1. summarize floral data so we have columns: (i) flowers per meter and (ii) total flowers per habitat type 
#then, take mean of flower count at each site, habitat, week, etc, for each transect section

flw.summary <- flwtrans %>%
  dplyr::group_by(habitat, site, site.code, year, week, doy, plant.species, transect.section) %>%
  dplyr::summarize(flw.count.mean = mean(flower.count), 
                   flw.count.sum = sum(flw.count.mean), 
                   flw.count.mean.per.meter = (sum(flw.count.mean/(20*0.5)))) #divide by the transect size 20 x 0.5 m for all years but 2015 since the transects are 20 meters

flw.summary.2015 <- flwtrans2015 %>%
  dplyr::filter(transect.section != "habitat") %>% #remove the "habitat" value for transect
  dplyr::group_by(habitat, site, site.code, year, week, doy, plant.species, transect.section) %>%
  dplyr::summarize(flw.count.mean = mean(flower.count), 
                   flw.count.sum = sum(flw.count.mean), 
                   flw.count.mean.per.meter = (sum(flw.count.mean/(5*0.5)))) #divide transect size 5 x 0.5 for 2015

#merge/join flower years
#remove species not visited by bumble bees
#keep only the six core sites (extra sites only from a few censuses in 2015)

flw.summary.all.years <- dplyr::bind_rows(flw.summary, flw.summary.2015) %>% 
  dplyr::filter(!plant.species %in% 
                  c("Androsace.septentrionalis",
                    "Artemisia.tridentata", 
                    "Boechera.stricta", 
                    "Descurainia.incana", 
                    "Epilobium.sp", 
                    "Galium.septentrionalis", 
                    "Galium.septentrionale",
                    "Oligosporus.dracunculus", 
                    "Polygonum.douglasii", 
                    "Tragopogon.dubius"), 
                site.code %in% c("AW", "CC", "FP", "GS", "RH", "SP"))

#cleanup and summarize flower data

flw.data <- flw.summary.all.years %>%
  dplyr::filter(plant.species != "0") %>% #remove "0" values in plant.species column
  dplyr::group_by(year, site, site.code, plant.species, week) %>% #NOTE, we are not using habitat information here
  dplyr::summarize(flw.per.meter.sum.week = sum(flw.count.mean.per.meter),
                   flw.count.sum.week = sum(flw.count.sum))

#### Join Bumble Bee Data and Floral Data and Snowmelt date data

bee.flw.data <- bee.data %>%
  dplyr::left_join(flw.data)

#if using billy barr climate data:

#bee.flw.data <- bee.data %>%
#  dplyr::left_join(flw.data)) %>%
#  dplyr::left_join(barr.snow.data, by = "year") %>% 
#  dplyr::mutate(days.since.snowmelt = doy - snowmelt.doy.barr, study.year = (year - 2014)) #join/merge with snowmelt data, and scale emergence data values


#### Double check the data.frame to make sure nothing unusual has happened!
print(bee.flw.data) 



