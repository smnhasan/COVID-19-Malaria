library(MASS)
require(foreign)
require(ggplot2)
require(maptools)
library(car)


#Data Management

setwd('E:\\ResearchProject\\Jamal Sir\\COVID_Malaria\\')
mal_case <- read.csv("Malaria_Cases_miss.csv")
mal_case_2017 <- subset(mal_case, mal_case$Period == "2017")

mal_death <- read.csv("Malaria_Deaths_miss.csv")
mal_death_2017 <- subset(mal_death, mal_death$Period == "2017")

COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22 <- subset(COVID22, COVID22$date == "1/5/2022")
COVID22

mal_case_death <- merge(mal_death_2017, mal_case_2017, by=c("Location"))

mal_covid_case_death <- merge(mal_case_death, COVID22, by=c("Location"))

GHSI <- read.csv("GHSI.csv")
GHSI[GHSI == 0] <- NA

WGI <- read.csv("WGI.csv")
WGI[WGI == 0] <- NA

Obesity <- read.csv("Obesity.csv")
gdp <- read.csv("GDP_PC.csv")
loc <- read.csv("countries.csv")


#Merge all data with GHSI and WGI

finaldt1 <- merge(GHSI, WGI,  by="location")

finaldt2 <- merge(finaldt1, Obesity,  by="location")

finaldt3 <- merge(finaldt2, gdp,  by="location")

finaldt4 <- merge(finaldt3, loc,  by="location")



finaldt4$Location <- finaldt4$location 

fdata <- merge(finaldt4, mal_covid_case_death, by="Location")

fdata$Case_Value[is.na(fdata$Case_Value)] <- 0

fdata$Case_Value_m <- fdata$Case_Value/1000000
fdata$Death_Value_m <- fdata$Death_Value/1000000
fdata$total_tests_per_thousand_m <- fdata$total_tests_per_thousand/1000

#Only Key Variables
model.3nb <- glm.nb(fdata$total_deaths_per_million ~ fdata$Case_Value_m 
                    + fdata$aged_65_older 
                    + fdata$total_tests_per_thousand_m, data = fdata)


summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))



options(scipen = 999) ## To disable scientific notation
#All variables
model.3nb <- glm.nb(fdata$total_deaths_per_million ~  fdata$Case_Value_m  
                    +fdata$aged_65_older 
                    + fdata$total_tests_per_thousand_m
                    + fdata$total_vaccinations_per_hundred
                    + fdata$GHSI
                    + fdata$Obesity_rate
                    + fdata$GDP
                    + fdata$population_density
                    + fdata$stringency_index
                    + fdata$longitude
                    + fdata$latitude
                    ,data = fdata)

vif(model.3nb)
summary(model.3nb)

exp(model.3nb$coefficients)
exp(confint(model.3nb))

#+ fdata$diabetes_prevalence

 
  
cor.test(mal_covid_case_death$Case_Value_m , mal_covid_case_death$Death_Value_m, method = c("pearson"), use = "complete.obs")
cor.test(mal_covid_case_death$total_cases_per_million , mal_covid_case_death$total_deaths_per_million, method = c("pearson"), use = "complete.obs")


cor.test(mal_covid_case_death$total_cases_per_million , mal_covid_case_death$Case_Value_m, method = c("pearson"), use = "complete.obs")
cor.test(mal_covid_case_death$total_deaths_per_million , mal_covid_case_death$Case_Value_m, method = c("pearson"), use = "complete.obs")

cor.test(mal_covid_case_death$total_cases_per_million , mal_covid_case_death$Death_Value_m, method = c("pearson"), use = "complete.obs")
cor.test(mal_covid_case_death$total_deaths_per_million , mal_covid_case_death$Death_Value_m, method = c("pearson"), use = "complete.obs")




####MAP Cases


setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                     COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                     COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID22$cfrlog <- COVID22$total_cases_per_million

worldgovt <- dplyr::select(COVID22, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "COVID-19 Cases/M") +
  plain
y <- plot(worldCFR)
y


#MAP Malaria Cases

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log
setwd('E:\\ResearchProject\\Jamal Sir\\COVID_Malaria\\')
mal_case <- read.csv("Malaria_Cases.csv")
mal_case$Location
mal_case2017 <- subset(mal_case, mal_case$Period == "2017")
mal_case2017$Location
mal_case2017$mcases <- mal_case2017$Case_Value/1000000

worldgovt <- dplyr::select(mal_case2017, region = Location, mcases = mcases, "CC" =  Case_SpatialDimValueCode)
head(worldgovt)
worldgovt$region
diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$mcases))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldmcases <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Malaria Cases/M") +
  plain
x <- plot(worldmcases)
x





####MAP Deaths


setwd('E:\\ResearchProject\\Jamal Sir\\Vaccination\\Update2')
COVID22 <- read.csv("owid-covid-data - Copy.csv")
COVID22$date

COVID22 <- subset(COVID22, COVID22$date == "1/5/2022") #5/29/2021
#Remove World and International information
COVID22<-COVID22[!(COVID22$iso_code=="OWID_AFR" | COVID22$iso_code=="OWID_ASI" | COVID22$iso_code=="OWID_EUR" | COVID22$iso_code=="OWID_INT" | 
                     COVID22$iso_code=="OWID_KOS" | COVID22$iso_code=="OWID_NAM" | COVID22$iso_code=="OWID_CYN" | COVID22$iso_code=="OWID_SAM" |
                     COVID22$iso_code=="OWID_WRL" | COVID22$iso_code == "OWID_EUN" | COVID22$iso_code == "OWID_OCE"),]



library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log

COVID22$cfrlog <- COVID22$total_deaths_per_million

worldgovt <- dplyr::select(COVID22, region = location, cfrlog = cfrlog, "CC" =  iso_code)
head(worldgovt)

diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$cfrlog))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldCFR <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "COVID-19 Deaths/M") +
  plain
a <- plot(worldCFR)
a


#MAP Malaria deaths

library(dplyr)
library(stringr)
library(ggplot2)
library(maps)

options(scipen = 999) ## To disable scientific notation
world <- map_data("world")
worldplot <- ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)
worldplot

#log
setwd('E:\\ResearchProject\\Jamal Sir\\COVID_Malaria\\')
mal_deaths <- read.csv("Malaria_Deaths.csv")
mal_deaths2017 <- subset(mal_deaths, mal_case$Period == "2017")
mal_deaths2017$mdeaths <- mal_deaths2017$Death_Value/1000000

worldgovt <- dplyr::select(mal_deaths2017, region = Location, mdeaths = mdeaths, "CC" =  Death_SpatialDimValueCode)
head(worldgovt)
worldgovt$region
diff <- setdiff(world$region, worldgovt$region)

## Clean the dataset accordingly
worldgovt <- worldgovt %>% 
  mutate(region = recode(str_trim(region), "United States" = "USA",
                         "United Kingdom" = "UK",
                         "Korea (Rep.)" = "South Korea",
                         "Congo (Dem. Rep.)" = "Democratic Republic of the Congo",
                         "Congo (Rep.)" = "Republic of Congo")) %>%
  ## Editing the "North Korea" entry is a little trickier for some reason
  mutate(region = case_when((CC == "PRK") ~ "North Korea",
                            TRUE ~ as.character(.$region)))

worldSubset <- inner_join(world, worldgovt, by = "region")

## Make the HDI numeric
worldgovt$CFR <- as.numeric(as.character(worldgovt$mdeaths))
worldSubset <- inner_join(world, worldgovt, by = "region")
head(worldSubset)


## Let's ditch many of the unnecessary elements
plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

worldmdeaths <- ggplot(data = worldSubset, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = CFR)) +
  scale_fill_distiller(palette ="Reds", direction = 1) + # or direction=1
  ggtitle("") + labs(fill = "Malaria Deaths/M") +
  plain
b <- plot(worldmdeaths)
b


tiff("MapCase.tiff", units="in", width=6, height=4, res=300)
gridExtra::grid.arrange(x,y)
dev.off()

tiff("MapDeath.tiff", units="in", width=6, height=4, res=300)
gridExtra::grid.arrange(b,a)
dev.off()

tiff("MapMCaseCdeath.tiff", units="in", width=6, height=4, res=300)
gridExtra::grid.arrange(x,a)
dev.off()

tiff("MapMDeathCcase.tiff", units="in", width=6, height=4, res=300)
gridExtra::grid.arrange(b,y)
dev.off()







