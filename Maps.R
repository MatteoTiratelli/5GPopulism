library(tidyverse)
library(cowplot)
library(tmaptools)
library(sf)

EU19 <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/5GPopulism/main/2019%20EU%20Election%20Results.csv")
EU19$Brexit <- as.numeric(substr(EU19$Brexit,1,nchar(EU19$Brexit)-1))

Regions <- st_read("https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_4.geojson")
Regions$nuts118nm <- c("North East", "North West", "Yorkshire and the Humber", "East Midlands",
                       "West Midlands", "East", "London", "South East", "South West", "Wales",
                       "Scotland","Northern Ireland")

EU19 %>%
  drop_na(Region) %>%
  group_by(Region) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate)) -> BrexitByRegion

merged <- merge(Regions, BrexitByRegion, by.x = "nuts118nm", by.y='Region', all.y = TRUE)
merged <- st_as_sf(merged)

ggplot() + 
  geom_sf(data = merged, aes(fill=BrexitSupport)) + 
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple", limits = c(0, 0.4),
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Brexit Party support") +
  labs(title = "Figure 1: Support for the Brexit Party in the 2019 European Elections by region",
       caption = "Source: House of Commons elections data") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

# 5G attacks

PhoneMastAttacks <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/5GPopulism/main/Annex%20-%20Phone%20mast%20and%20telecoms%20infrastructure%20incident%20data.csv")
PhoneMastAttacks$MONTH <- substr(PhoneMastAttacks$DATE, 4, 5)
PhoneMastAttacks$DATE <- as.Date(PhoneMastAttacks$DATE, format = "%d/%m/%Y")

PhoneMastAttacks %>%
  group_by(MONTH) %>%
  count() %>%
  ggplot(data = ., aes(x = as.numeric(MONTH), y = n)) +
  geom_line() + 
  scale_x_continuous(breaks = c(3,5,7,9,11), labels = c("March", "May", "July","September","November")) +
  scale_y_continuous(limits = c(0,100)) +
  ylab(NULL) + xlab(NULL) +
  theme_classic() +
  labs(title = "Figure 2: The number of attacks on communications infrastructure in 2020", 
       caption = "Source: Ofcom data released via FOI 1046318") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))


PhoneMastAttacks %>%
  group_by(COUNTY) %>%
  count() -> Attacks

Attacks %>%
  mutate(Region = ifelse(COUNTY %in% c("Bedfordshire","Cambridgeshire", "Essex","Hertfordshire"), "East",
                         ifelse(COUNTY %in% c("Cheshire","Greater Manchester", "Lancashire","Merseyside"), "North West",
                                ifelse(COUNTY %in% c("City of Edinburgh", "Dundee City", "Fife","Glasgow City","South Ayrshire","West Glamorgan"), "Scotland",
                                       ifelse(COUNTY %in% c("County Durham"), "North East",
                                              ifelse(COUNTY %in% c("Derbyshire","Northamptonshire","Nottinghamshire"), "East Midlands",
                                                     ifelse(COUNTY %in% c("Devon"), "South West",
                                                            ifelse(COUNTY %in% c("Gwent","Powys"), "Wales",
                                                                   ifelse(COUNTY %in% c("Hampshire","Kent","West Sussex","Buckinghamshire"), "South East",
                                                                          ifelse(COUNTY %in% c("London"), "London",
                                                                                 ifelse(COUNTY %in% c("Somerset"), "South West",
                                                                                        ifelse(COUNTY %in% c("South Yorkshire","West Yorkshire"), "Yorkshire and the Humber",
                                                                                               ifelse(COUNTY %in% c("Tyne & Wear", "Tyne and Wear"), "North East",
                                                                                                      ifelse(COUNTY %in% c("Warwickshire","West Midlands"), "West Midlands", NA)))))))))))))) -> Attacks

Attacks %>%
  drop_na(Region) %>%
  group_by(Region) %>%
  summarize(n = sum(n)) -> Attacks

merged <- merge(Regions, Attacks, by.x = "nuts118nm", by.y='Region', all.y = TRUE)
merged <- st_as_sf(merged)

ggplot() + 
  geom_sf(data = merged, aes(fill=n)) + 
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple",
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Phone mast attacks") +
  labs(title = "Figure 3: Attacks on communications infrastructure in 2020 by region", 
       caption = "Source: Ofcom data released via FOI 1046318") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

EU19 %>% #Merseyside
  filter(la_code %in% c("E08000011","E08000012","E08000013","E08000014","E08000015")) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate))

EU19 %>% #West Yorkshire
  filter(la_code %in% c("E08000032","E08000033","E08000034","E08000035","E08000036")) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate))

EU19 %>% #West Midlands
  filter(la_code %in% c("E08000025","E08000026","E08000027","E08000028","E08000029","E08000030","E08000031")) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate))

EU19 %>%
  mutate(Nation = substr(la_code,1,1)) %>%
  group_by(Nation) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate))
