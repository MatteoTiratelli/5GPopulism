library(tidyverse)
library(cowplot)
library(tmaptools)
library(sf)

# 5G attacks

PhoneMastAttacks <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/5GPopulism/main/Annex%20-%20Phone%20mast%20and%20telecoms%20infrastructure%20incident%20data.csv")
PhoneMastAttacks$MONTH <- substr(PhoneMastAttacks$DATE, 4, 5)
PhoneMastAttacks$DATE <- as.Date(PhoneMastAttacks$DATE, format = "%d/%m/%Y")

PhoneMastAttacks %>%
  group_by(MONTH) %>%
  count() %>%
  ggplot(data = ., aes(x = as.numeric(MONTH), y = n)) +
  geom_line() + geom_label(aes(x=4.85, y=100, label="Thomas Cowan viral video released")) +
  scale_x_continuous(breaks = c(3,5,7,9,11), labels = c("March", "May", "July","September","November")) +
  scale_y_continuous(limits = c(0,100)) +
  ylab("Number of attacks on communications infrastructure") + xlab(NULL) +
  theme_classic() + 
  geom_vline(xintercept = (3+(12/31)), linetype = 'dashed')

PhoneMastAttacks %>%
  group_by(COUNTY) %>%
  count() -> Attacks

Counties <- Attacks$COUNTY
Counties <- paste(Counties, ", UK", sep="")
Geocoded <- geocode_OSM(Counties)
Geocoded$COUNTY <- str_sub(Geocoded$query, 1, -5)
Geocoded <- Geocoded[,c(2,3,8)]

PhoneMastAttacks <- merge(PhoneMastAttacks, Geocoded, by = "COUNTY")
Attacks <- merge(Attacks, Geocoded, by = "COUNTY")


# Brexit map

AdministrativeCounties <- st_read("https://opendata.arcgis.com/datasets/b216b4c8a4e74f6fb692a1785255d777_0.geojson") # Counties and Unitary Authorities (December 2019) Boundaries UK BUC
AdministrativeCounties %>%
  filter(substr(ctyua19cd,1,1) != 'N') -> AdministrativeCounties

EU19 <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/5GPopulism/main/2019%20EU%20Election%20Results.csv")
EU19lookup <- read_csv("https://opendata.arcgis.com/datasets/1512e57c5aa34d049569750083765eba_0.csv") # Local Authority District to County (December 2019) Lookup in England

EU19 <- merge(EU19, EU19lookup, by.x = 'la_code', by.y = 'LAD19CD', all = TRUE)
EU19$CTY19CD <- ifelse(EU19$la_code %in% sf$ctyua19cd, EU19$la_code, EU19$CTY19CD)
EU19$Brexit <- as.numeric(substr(EU19$Brexit,1,nchar(EU19$Brexit)-1))

EU19 %>%
  drop_na(CTY19CD) %>%
  group_by(CTY19CD) %>%
  summarize(BrexitSupport = (sum((Brexit/100)*`Total votes`)/sum(`Total votes`)),
            Population = sum(Electorate)) -> BrexitByCounty

merged <- merge(AdministrativeCounties, BrexitByCounty, by.x = "ctyua19cd", by.y='CTY19CD', all = TRUE)
merged <- st_as_sf(merged)

ggplot() + 
  geom_sf(data = merged, aes(fill=BrexitSupport)) + 
  #geom_jitter(data = PhoneMastAttacks[!PhoneMastAttacks$COUNTY %in% c("Antrim", "Armagh"),], 
  #           aes(x = lon, y = lat), alpha = 0.75, colour = 'black') +
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple", limits = c(0, 0.55),
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Brexit Party support") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0)) -> Main

LondonCD <- c('E09000007','E09000001','E09000012','E09000013','E09000014','E09000019','E09000020','E09000022',
              'E09000023','E09000025','E09000028','E09000030','E09000032','E09000033','E09000002','E09000003',
              'E09000004','E09000005','E09000006','E09000008','E09000009','E09000010','E09000011','E09000015',
              'E09000016','E09000017','E09000018','E09000021','E09000024','E09000026','E09000027','E09000029',
              'E09000031')

ggplot() + 
  geom_sf(data = merged[merged$ctyua19cd %in% LondonCD,], aes(fill=BrexitSupport)) + 
  #geom_jitter(data = PhoneMastAttacks[!PhoneMastAttacks$COUNTY %in% c("Antrim", "Armagh"),], 
  #            aes(x = lon, y = lat), alpha = 0.75, colour = 'black') +
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple", limits = c(0, 0.55),
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Brexit Party support") +
  ggtitle("London") +
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0)) -> London

ggdraw(Main) +
  draw_plot(London, width = 0.2, height = 0.2, 
            x = 0.18, y = 0.26) -> BrexitMap


# Regions

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
  #geom_jitter(data = PhoneMastAttacks[!PhoneMastAttacks$COUNTY %in% c("Antrim", "Armagh"),], 
  #           aes(x = lon, y = lat), alpha = 0.75, colour = 'black') +
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple", limits = c(0, 0.55),
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Brexit Party support") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0))

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
  #geom_jitter(data = PhoneMastAttacks[!PhoneMastAttacks$COUNTY %in% c("Antrim", "Armagh"),], 
  #           aes(x = lon, y = lat), alpha = 0.75, colour = 'black') +
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple",
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Phone mast attacks") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0))

# Not working... Probably need a manual lookup table or some way of summarising

CeremonialCounties <- st_read("https://opendata.arcgis.com/datasets/5b60fac33976436ab900e05eb1a33216_12.csv") # Boundary-Line ceremonial counties NOT WORKING

merged <- as.data.frame(merge(CeremonialCounties, Attacks, by.x = "ctyua19cd", by.y='COUNTY', all = TRUE))
merged <- st_as_sf(merged)

ggplot() + 
  geom_sf(data = merged, aes(fill=BrexitSupport)) + 
  theme_void() + coord_sf() +
  scale_fill_gradient(low = "yellow", high = "purple", limits = c(0, 0.55),
                      space = "Lab", na.value = "grey50", guide = "colourbar",
                      aesthetics = "fill", name = "Brexit Party support") +
  theme(plot.title.position = "plot",
        plot.caption.position =  "plot",
        plot.caption = element_text(hjust = 0)) -> Main

