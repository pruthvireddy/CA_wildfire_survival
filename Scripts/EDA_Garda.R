library(tidyverse)
library(leaflet)
library(stringr)
library(zoo)

CO_2021 <- read.csv("daily_42101_2021_CO.csv")
View(CO_2021)

CO_2021_CA <- CO_2021 %>% 
  filter(State.Name == "California")

CO_2021 %>% 
  distinct(County.Name)

#Monitors 2021

monitor_2021 <- read.csv("annual_conc_by_monitor_2021.csv")
View(monitor_2021)

monitor_parameters <- monitor_2021 %>% 
  distinct(Parameter.Name)

monitor_locations <- monitor_2021 %>% 
  distinct(Latitude, Longitude)

View(monitor_locations)

map_monitors <- leaflet(data = monitor_locations) %>% 
  addTiles() %>% 
  addMarkers(lng=monitor_locations$Longitude, 
             lat=monitor_locations$Latitude)

map_monitors

monitor_2021 %>% 
  group_by(Parameter.Name) %>% 
  summarize(n_row = n()) %>% 
  arrange(-n_row)

#CBSA 2020

CBSA_2020 <- read.csv("annual_aqi_by_cbsa_2020.csv")

CBSA_2020 <- CBSA_2020 %>% 
  separate(CBSA, c("City", "State"), ", ", remove=FALSE)

View(CBSA_2020)

top25_cbsa_unhealthy <- CBSA_2020 %>% 
  mutate(prop_unhealthy_days = Unhealthy.Days/Days.with.AQI,
         CA = ifelse(State == "CA", 1, 0)) %>% 
  arrange(-prop_unhealthy_days) %>% 
  head(25) 
  
top25_cbsa_unhealthy %>% 
  ggplot(aes(x=reorder(CBSA,prop_unhealthy_days), 
             y=prop_unhealthy_days, 
             fill=factor(CA))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("darkgrey", "red")) + 
  coord_flip() +
  labs(title="Top 25 core-based statistical areas (CBSAs) \n by unhealthy days in 2020",
       x ="CBSA", 
       y = "Proportion of unhealthy days") + 
  theme(legend.position = "none")
  
#Daily AQI by CBSA 2020

daily_aqi_cbsa_20 <- read.csv("daily_aqi_by_cbsa_2020.csv")
View(daily_aqi_cbsa_20)

daily_aqi_cbsa_20 %>% 
  distinct(CBSA) 

NY_SF_daily_aqi_20 <- daily_aqi_cbsa_20 %>% 
  filter(CBSA == "New York-Newark-Jersey City, NY-NJ-PA" |
         CBSA == "San Francisco-Oakland-Hayward, CA")

View(NY_SF_daily_aqi_20)

##Bay Area CBSA vs New York Area CBSA-Daily AQI

NY_SF_AQI <- NY_SF_daily_aqi_20 %>% 
  group_by(CBSA) %>% 
  mutate(AQI = AQI,
         AQI_3day = rollmean(AQI, 3, fill = NA),
         AQI_7day = rollmean(AQI, 7, fill = NA),
         AQI_14day = rollmean(AQI, 14, fill = NA)) %>% 
  ungroup()

View(NY_SF_AQI)

NY_SF_AQI %>% 
  ggplot(aes(x=as.Date(Date), y=AQI_7day)) +
  geom_line(aes(color=CBSA)) +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = "2020 AQI in New York and San Francisco",
       x = "Date",
       y = "7-day rolling AQI average") +
  theme(legend.position = "bottom")

#Daily AQI county 2020

daily_aqi_county_20 <- read.csv("daily_aqi_by_county_2020.csv")
View(daily_aqi_county_20)

cali_counties_aqi_20 <- daily_aqi_county_20 %>% 
  filter(State.Code == 4)

cali_counties_aqi_20 %>% 
  group_by(county.Name) %>% 
  summarize(n_rows = n()) %>% 
  arrange(-n_rows)

#Annual AQI by county map

annual_AQI_county_20 <- read.csv("annual_aqi_by_county_2020.csv")
View(annual_AQI_county_20)

annual_AQI_county_20 <- annual_AQI_county_20 %>% 
  mutate(State = tolower(State),
         County = tolower(County)) %>% 
  rename(region = State,
         subregion = County)

us_counties <- map_data("county")
counties_AQI_2020 <- us_counties %>% 
  left_join(annual_AQI_county_20, by= c("region", "subregion"))

View(counties_AQI_2020)

AQI_county_map <- counties_AQI_2020 %>% 
  ggplot(aes(x=long,y=lat,group=group, fill=Median.AQI)) +
  geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 45, lat1 = 55) +
  scale_fill_gradient(name = "Median AQI",
                      low = "yellow",
                      high = "red")+
  ggtitle("Median AQI by county in 2020")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="right",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank())

AQI_county_map

#CO_2020

CO_2020 <- read.csv("CO_2020.csv")
View(CO_2020)

CO_2020 %>% 
  filter(CBSA.Name == "San Francisco-Oakland-Hayward, CA")
