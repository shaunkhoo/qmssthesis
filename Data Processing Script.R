#############################
###### Data Processing ######
#############################

library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
library(caret)
library(sf)
library(Amelia)
library(Zelig)

# importing the data

subwaystns <- read_csv("finaldata/subwaystns.csv") %>%
  select(faredata_name, stop_lat, stop_lon) %>%
  distinct(faredata_name, .keep_all = TRUE)
subwaystns_raw <- read_csv("finaldata/subwaystns.csv")
farecard <- read_csv("finaldata/farecard.csv")
weather <- read_csv("finaldata/weather.csv")
cbtrips <- read_csv('finaldata/cbtrips.csv')
cbstns <- read_csv('finaldata/cbstns.csv')
bicycle_routes <- read_sf(dsn = "finaldata/bikelanes", layer = "geo_export_7a6dd778-b7bc-4265-8438-f31830f4180f")

# generating a unique list of stations-dates to insure against missing data

all_dates <- as.data.frame(seq.Date(as.Date("2012-05-26"), as.Date("2014-05-30"), 7))
all_stations <- unlist(distinct(farecard, station), use.names = FALSE)
for (station in all_stations) {
  df <- cbind(station, all_dates)
  if (station == all_stations[1]) {
    interim_df <- df
  } else {
    interim_df <- bind_rows(interim_df, df)
  }
}
colnames(interim_df) <- c("station", "date")

# checking for missing entries in farecard data

missing_data <- interim_df %>%
  left_join(weather) %>%
  left_join(farecard) %>%
  filter(is.na(full_fare) & date != as.Date("2013-04-20"))

subwaystns_remove <- missing_data %>%
  group_by(station) %>%
  summarise(count = n()) %>%
  filter(count > 15) %>% # keeps only stations with 1-3 missing entries
  select(station) %>%
  unlist(use.names =  FALSE)

data <- interim_df %>%
  filter(!(station %in% subwaystns_remove)) %>%
  left_join(weather) %>%
  left_join(farecard)

# generating citibike stations count

calc_cbstns <- function(station, sel_date) {
  
  # return a logical vector of which Citi Bike stations are within 400m of the subway station
  idx <- geosphere::distHaversine(subwaystns[subwaystns$faredata_name == station, c(3,2)], cbstns[,c(4,3)]) <= 400
  
  # if none, return 0 for everything
  if (sum(idx) == 0) {
    stn_count <- 0
    trips_start <- 0
    trips_end <- 0
  }
  
  # if there is at least 1, find which Citi Bike station(s) are nearby
  nearby_cbstns <- cbstns$stn_id[idx]
  
  # return the total start and end counts for these Citi Bike station(s)
  all_trips <- cbtrips %>%
    filter(date <= sel_date) %>%
    filter(stn_id %in% nearby_cbstns) %>%
    top_n(1, date)

  # if there are no observations from the Citi Bike station for that date, then return 0 for everything
  if (nrow(all_trips) == 0) {
    stn_count <- 0
    trips_start <- 0
    trips_end <- 0
  }
  
  # otherwise, sum the start and end counts
  if (nrow(all_trips) > 0) {
    stn_count <- nrow(distinct(all_trips, stn_id))
    trips_start <- sum(all_trips$start_counts)
    trips_end <- sum(all_trips$end_counts)
  }
  
  # return the data as a vector
  return(c(stn_count, trips_start, trips_end))
}

# initializing the column vectors
data$cbstn_count <- 999999
data$cbtrips_start <- 999999
data$cbtrips_end <- 999999

# running a loop through the whole dataset
for (i in 1:nrow(data)) {
  if (data$date[i] < as.Date("2013-05-31")) {
    data$cbstn_count[i] <- 0
    data$cbtrips_start[i] <- 0
    data$cbtrips_end[i] <- 0
    if (i %% 1000 == 0) {print(i)}
    next
  }
  output <- calc_cbstns(data$station[i], data$date[i])
  data$cbstn_count[i] <- output[1]
  data$cbtrips_start[i] <- output[2]
  data$cbtrips_end[i] <- output[3]
  if (i %% 1000 == 0) {print (i)}
}

# generating bicycle route measurements by year

bike_routes_2012 <- bicycle_routes %>%
  filter(year(date_instd) <= 2012) %>%
  st_transform(4326) %>%
  st_combine()

bike_routes_2013 <- bicycle_routes %>%
  filter(year(date_instd) <= 2013) %>%
  st_transform(4326) %>%
  st_combine()

bike_routes_2014 <- bicycle_routes %>%
  filter(date_instd < as.Date("2014-06-01")) %>%
  st_transform(4326) %>%
  st_combine()

subwaystns_sf <- st_as_sf(subwaystns, coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
  st_transform(crs = 3857)
subwaystns_circles <- subwaystns_sf %>%
  st_buffer(dist = 1200) %>% #1.2km
  st_transform(crs = 4326)

calc_bikelane <- function(station, year) {
  
  # retrieve the station-specific geometry
  geometry <- subwaystns_circles %>%
    filter(faredata_name == station) %>%
    .[1,]
  
  # check to prevent errors
  if (nrow(geometry) != 1) {
    print(station)
    stop()
  }
  
  # calculating bicycle lane distance by year
  if (year == 2012) {
    dist <- as.numeric(sum(st_length(suppressMessages(st_intersection(st_geometry(geometry), st_geometry(bike_routes_2012))))))
  } else if (year == 2013) {
    dist <- as.numeric(sum(st_length(suppressMessages(st_intersection(st_geometry(geometry), st_geometry(bike_routes_2013))))))
  } else {
    dist <- as.numeric(sum(st_length(suppressMessages(st_intersection(st_geometry(geometry), st_geometry(bike_routes_2014))))))
  }
  
  # returning the results
  return(dist)
}

subways_bikelane <- data %>%
  mutate(year = year(date)) %>%
  distinct(station, year) %>%
  mutate(bikelane = purrr::map2(station, year, calc_bikelane))

data2 <- data %>%
  left_join(subways_bikelane) %>%
  select(-year) %>%
  mutate(bikelane = round(unlist(bikelane)/1000, 2))

# finalizing the dataset

data_interim <- data2 %>%
  gather(key = 'group', value = 'ridership', full_fare, reduced_fare) %>%
  mutate(lnridership = log(ridership+1),
         group = ifelse(group == 'full_fare', 1, 0),
         bikeopen = ifelse(date >= as.Date("2013-06-01"), 1, 0),
         borough = subwaystns_raw$borough[match(station, subwaystns_raw$faredata_name)]) %>%
  group_by(station) %>%
  mutate(bikeopen = ifelse(any(cbstn_count > 0), bikeopen, 0)) %>%
  ungroup() %>%
  select(-ridership, -cbtrips_start, -cbtrips_end)
  
saveRDS(data_interim, "data/data_interim.rds")

# demonstrating there are some problems with the data

data_interim <- readRDS("data/data_interim.rds") 

missingdata_example <- data_interim %>%
  filter(station == "42ND STREET/TIMES SQUARE") %>%
  mutate(group = ifelse(group == 1, "Full Fare", "Reduced Fare"))

missingdata_plot <- ggplot(missingdata_example) +
  geom_point(aes(x = date, y = lnridership,
                 color = factor(group))) +
  geom_vline(xintercept = as.Date("2013-04-20"),
             color = "black", size = 1, alpha = .4) +
  geom_vline(xintercept = as.Date("2012-10-27"),
             color = "black", size = 1, alpha = .2) +
  geom_vline(xintercept = as.Date("2014-02-15"),
             color = "black", size = 1, alpha = .2) +
  geom_vline(xintercept = as.Date("2014-04-12"),
             color = "black", size = 1, alpha = .2) +
  labs(x = "Date",
       y = "Ridership (logged)",
       color = "Farecard Type") +
  scale_color_manual(values = c("Full Fare" = "#32d0b4",
                                "Reduced Fare" = "#d434ad")) +
  scale_x_date(date_labels = "%b %Y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15,
                                  hjust = 0),
        panel.grid.minor = element_blank())
missingdata_plot
saveRDS(missingdata_plot, "finaldata/missingdata_plot.rds")

# removing outliers and imputing missing data

data_amelia <- data_interim %>%
  mutate(lnridership = ifelse(date == as.Date("2012-10-27"), NA, lnridership)) %>%
  mutate(lnridership = ifelse(date == as.Date("2014-02-15"), NA, lnridership)) %>%
  mutate(lnridership = ifelse(date == as.Date("2014-04-12"), NA, lnridership)) 
  
sum(is.na(data_amelia)) # checking total number of NAs

output <- amelia(data_amelia, m = 5, # using AMELIA II's implementation of multiple imputation,
                 ts = "date", polytime = 0, # polytime = 0 means fixed effects
                 cs = "station", intercs = TRUE, # intercs needed to define cross-sectional variable
                 noms = "group", # identifying distinction between treatment and control
                 idvars = "borough") # removing all other categorical variables that don't matter

for (i in 1:5) {
  input <- output$imputations[[i]]$lnridership
  if (i == 1) {
    total <- input
  } else {
    total <- total + input
  }
}

data_final <- data_amelia %>%
  mutate(lnridership = total / 5) # taking the average of all 5 imputations

data_final %>%
  filter(station == sample(station, 1)) %>%
  ggplot() +
  geom_point(aes(x = date, y = lnridership)) +
  geom_vline(xintercept = as.Date("2013-04-20"),
             color = "red", size = 1, alpha = .5) +
  geom_vline(xintercept = as.Date("2012-10-27"),
             color = "orange", size = 1, alpha = .5) +
  geom_vline(xintercept = as.Date("2014-02-15"),
             color = "orange", size = 1, alpha = .5) +
  geom_vline(xintercept = as.Date("2014-04-12"),
             color = "orange", size = 1, alpha = .5) +
  labs(x = "Date",
       y = "Ridership (logged)",
       title = paste("Station Ridership")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15,
                                  hjust = 0),
        panel.grid.minor = element_blank())

sum(is.na(data_final)) # check to ensure imputation was done correctly

saveRDS(data_final, "finaldata/df.rds")

# running the fixed effect regressions

df <- readRDS("finaldata/df.rds")

model1_1 <- lm(lnridership ~ bikeopen:group:cbstn_count + bikelane
               + apptemp_low + precip_intensity + precip_accumulation
               + factor(station) + factor(date), data = df)
summary(model1_1)

model1_2 <- lm(lnridership ~ bikeopen:group:cbstn_count + bikelane
               + factor(station) + factor(date), data = df)
summary(model1_2)

model1_3 <- lm(lnridership ~ bikeopen:group:cbstn_count
               + factor(station) + factor(date), data = df)
summary(model1_3)

saveRDS(model1_1, "finaldata/model1_1.rds")
saveRDS(model1_2, "finaldata/model1_2.rds")
saveRDS(model1_3, "finaldata/model1_3.rds")

df_manhattan <- df %>%
  filter(borough == "Manhattan")
df_brooklyn <- df %>%
  filter(borough == "Brooklyn")

model2_1 <- lm(lnridership ~ bikeopen:group:cbstn_count + bikelane
               + apptemp_low + precip_intensity + precip_accumulation
               + factor(station) + factor(date), data = df_manhattan)
summary(model2_1)

model2_2 <- lm(lnridership ~ bikeopen:group:cbstn_count + bikelane
               + apptemp_low + precip_intensity + precip_accumulation
               + factor(station) + factor(date), data = df_brooklyn)
summary(model2_2)

saveRDS(model2_1, "finaldata/model2_1.rds")
saveRDS(model2_2, "finaldata/model2_2.rds")
