library("tidyverse")
library("lubridate")

daily_aqi_2020 <- read.csv("/Users/hamidah/Desktop/hackathon/daily_aqi_by_county_2020.csv")
daily_aqi_2020_cali <- daily_aqi_2020 %>% 
  filter(State.Name=="California") 

count_category_2020 <- daily_aqi_2020_cali %>% 
  group_by(Date) %>% 
  count(Category) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Category = ordered(Category,
                            levels = c("Good", "Moderate",
                                       "Unhealthy for Sensitive Groups",
                                       "Unhealthy",
                                       "Very Unhealthy",
                                       "Hazardous")))

ggplot(count_category, aes(fill=Category, y=n, x=Date)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("number of counties") +
  xlab("date") +
  ggtitle("Counties in California by 2020 AQI Category")


daily_aqi_2019 <- read.csv("/Users/hamidah/Desktop/hackathon/daily_aqi_by_county_2019.csv")
daily_aqi_2019 <- daily_aqi_2019 %>% 
  filter(State.Name=="California") %>%
  select(Date, county.Name, AQI, Category)

count_category_2019 <- daily_aqi_2019_cali %>% 
  group_by(Date) %>% 
  count(Category) %>%
  mutate(Date = ymd(Date)) %>%
  mutate(Category = ordered(Category,
                            levels = c("Good", "Moderate",
                                       "Unhealthy for Sensitive Groups",
                                       "Unhealthy",
                                       "Very Unhealthy",
                                       "Hazardous")))

ggplot(count_category_2019, aes(fill=Category, y=n, x=Date)) + 
  geom_bar(position="stack", stat="identity") +
  ylab("number of counties") +
  xlab("date") +
  ggtitle("Counties in California by 2019 AQI Category")

# ------------------------------------------------------------------------------

daily_aqi_2020_cali_wf <- daily_aqi_2020_cali %>% 
  mutate(fire_name = 
         ifelse(county.Name %in% c("Glenn", "Mendocino", "Lake", "Tehama", "Trinity", "Shasta"),"August",
                ifelse(county.Name %in% c("Santa Clara", "Alameda", "Contra Costa", "San Joaquin", "Merced", "Stanislaus"),"SCU Lightning",
                       ifelse(county.Name %in% c("Fresno", "Madera"),"Creek",
                              ifelse(county.Name %in% c("Colusa", "Napa", "Sonoma", "Solano", "Yolo"), "LNU Lightning",
                                     ifelse(county.Name %in% c("Plumas", "Butte"), "North", 
                                            ifelse(county.Name %in% c("Tulare"), "SQF",
                                                   ifelse(county.Name %in% c("Siskiyou", "Del Norte"), "Slater",
                                                          ifelse(county.Name %in% c("Humboldt"), "Red Salmon",
                                                                 ifelse(county.Name %in% c("Monterey"), "Dolan",
                                                                        ifelse(county.Name %in% c("Los Angeles"), "Bobcat",""))))))))))) %>%
  mutate(start_date = ifelse(fire_name == "August", "2020-08-16",
                             ifelse(fire_name == "SCU Lightning", "2020-08-16",
                                    ifelse(fire_name == "Creek", "2020-09-04",
                                           ifelse(fire_name == "LNU Lightning", "2020-08-17",
                                                  ifelse(fire_name == "North", "2020-08-17", 
                                                         ifelse(fire_name == "SQF", "2020-08-19",
                                                                ifelse(fire_name == "Slater", "2020-09-07",
                                                                       ifelse(fire_name == "Red Salmon", "2020-07-26",
                                                                              ifelse(fire_name == "Dolan", "2020-08-18",
                                                                                     ifelse(fire_name == "Bobcat", "2020-09-06",""))))))))))) %>%
  mutate(contain_date = ifelse(fire_name == "August", "2020-11-12",
                             ifelse(fire_name == "SCU Lightning", "2020-10-01",
                                    ifelse(fire_name == "Creek", "2020-12-24",
                                           ifelse(fire_name == "LNU Lightning", "2020-10-02",
                                                  ifelse(fire_name == "North", "2020-12-03", 
                                                         ifelse(fire_name == "SQF", "2021-01-05",
                                                                ifelse(fire_name == "Slater", "2020-11-16",
                                                                       ifelse(fire_name == "Red Salmon", "2020-11-17",
                                                                              ifelse(fire_name == "Dolan", "2020-12-31",
                                                                                     ifelse(fire_name == "Bobcat", "2020-12-18",""))))))))))) %>%
  mutate(start_date = ymd(start_date)) %>%
  mutate(day_diff = round(difftime(Date, start_date, units = "days"))) %>%
  filter(day_diff>0) %>%
  filter(fire_name!="") %>%
  mutate(area_affected = ifelse(fire_name == "August", 1032648,
                               ifelse(fire_name == "SCU Lightning", 396624,
                                      ifelse(fire_name == "Creek", 379895,
                                             ifelse(fire_name == "LNU Lightning", 363220,
                                                    ifelse(fire_name == "North", 318935, 
                                                           ifelse(fire_name == "SQF", 174178,
                                                                  ifelse(fire_name == "Slater", 166127,
                                                                         ifelse(fire_name == "Red Salmon", 144698,
                                                                                ifelse(fire_name == "Dolan", 124924,
                                                                                       ifelse(fire_name == "Bobcat", 115997,"")))))))))))
  

daily_aqi_2020_cali_wf_good <- daily_aqi_2020_cali_wf %>%
  mutate(good = ifelse(Category == "Good", 1, 0)) %>%
  filter(good == 1) %>%
  group_by(county.Name) %>%
  mutate(min_good = min(day_diff)) %>%
  ungroup() %>%
  filter(day_diff == min_good)
  
# ------------------------------------------------------------------------------

daily_aqi_2020_cali_wf %>%
  mutate(Category = ordered(Category, levels = c("Good", "Moderate",
                                                 "Unhealthy for Sensitive Groups",
                                                 "Unhealthy",
                                                 "Very Unhealthy",
                                                 "Hazardous"))) %>%
  ggplot(aes(fill=Category, x=day_diff)) +
  geom_bar(position="fill") +
  xlab("Day after wildfire") +
  ylab("Proportion of counties")
  


# ------------------------------------------------------------------------------
##survival analysis 7 days rolling average
daily_aqi_2020_cali_wf2 <-
  daily_aqi_2020_cali_wf %>% 
  group_by(county.Name) %>%
  mutate(roll_7days = zoo::rollmean(AQI,7,fill = NA))%>%
  mutate(good = ifelse(roll_7days < 50,1,0)) %>%
  ungroup()

daily_aqi_2020_cali_wf2_good <- daily_aqi_2020_cali_wf2 %>%
  group_by(county.Name) %>%
  mutate(max_sum = max(good, na.rm = TRUE))

survival_data_1 <- daily_aqi_2020_cali_wf2_good %>% 
  filter(good == 1) %>%
  group_by(county.Name) %>%
  mutate(last_obs = min(day_diff)) %>%
  ungroup() %>%
  filter(day_diff == last_obs)

survival_data_0 <- daily_aqi_2020_cali_wf2_good %>% 
  filter(max_sum == 0) %>%
  group_by(county.Name) %>%
  mutate(last_obs = max(day_diff)) %>%
  filter(day_diff == last_obs)

survival_data <- rbind(survival_data_1, survival_data_0)
survival_data <- survival_data %>%
  mutate(contain_date = ymd(contain_date)) %>%
  mutate(contain_day = round(difftime(contain_date, start_date, units = "days"))) %>%
  group_by(fire_name) %>%
  mutate(number_county = n())

county_population <- read.csv("/Users/hamidah/Desktop/hackathon/california_county_pop.csv")
survival_data_comp <- left_join(survival_data, county_population, by = "county.Name")

annual_aqi_by_county_2019 <- read.csv("/Users/hamidah/Desktop/hackathon/annual_aqi_by_county_2019.csv")
annual_aqi_by_county_2019 <- annual_aqi_by_county_2019 %>% filter(State=="California")
survival_data_comp <- left_join(survival_data_comp, annual_aqi_by_county_2019, by = c("county.Name" = "County"))

survival_data_comp <- survival_data_comp %>% 
  mutate(unhealthy_2019 = Unhealthy.for.Sensitive.Groups.Days + Unhealthy.Days + Very.Unhealthy.Days + Hazardous.Days)

library(survival)
km <- with(survival_data, Surv(day_diff, max_sum))
km_fit <- survfit(Surv(day_diff, max_sum) ~ 1, data=survival_data)
autoplot(km_fit, ylab="Survival Probability", xlab = "Days after wildfire") 
summary(km_fit, times = c(1,30,60,90*(1:10)))


poverty <- read.csv("/Users/hamidah/Desktop/hackathon/POVERTY_CA_2019.csv", sep = ";")
survival_data_comp <- left_join(survival_data_comp, poverty, by = c("county.Name" = "Name"))


cox <- coxph(Surv(day_diff, max_sum) ~ 
               contain_day +  
               number_county + 
               Density +
               unhealthy_2019 +
               Percent
             , data = survival_data_comp)
summary(cox)

View(survival_data_comp)

hist(as.numeric(survival_data_comp$last_obs), main = "Histogram of numbers of recovery day", xlab = "number of days")

survival_data_comp <- survival_data_comp %>% 
  mutate(poverty_rate = Percent, 
         day_recover = as.numeric(last_obs),
         population_density = Density,
         day_until_containment = contain_day)

model1 <- glm(day_recover ~
      unhealthy_2019, family="poisson", data=survival_data_comp)
model2 <- glm(day_recover ~
                day_until_containment, family="poisson", data=survival_data_comp)
model3 <- glm(day_recover ~
                population_density, family="poisson", data=survival_data_comp)
model4 <- glm(day_recover ~
                unhealthy_2019 + day_until_containment + population_density + poverty_rate, family="poisson", data=survival_data_comp)
library(stargazer)

stargazer(model1, model2, model3, model4, type = "text", 
          out = "regression.html",#we use html output to match our planned R Markdown output
          title = "Regression of Recovery Day")
