################################
###### Data Visualization ######
################################

library(ggmap)
library(dplyr)
library(readr)
      
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

# subway data visualization

subwayplot <- df %>%
  select(date, group, borough, lnridership) %>% 
  mutate(group = ifelse(group == 1, "Treatment", "Control")) %>%
  group_by(date, borough) %>%
  summarise(ridership = sum(exp(lnridership))) %>%
  ggplot() +
  labs(x = "Date",
       y = "Ridership (natural log)",
       color = "Borough") +
  scale_color_manual(values = c("Brooklyn" = "#64c916",
                                "Manhattan" = "#e5734d",
                                "Queens" = "#a34ddf",
                                "Bronx" = "#4dc5df",
                                "Staten Island" = "#dee750")) +
  geom_smooth(aes(x = date, y = ridership, color = borough)) +
  geom_point(aes(x = date, y = ridership, color = borough), size = 0.5, alpha = 0.4) +
  scale_x_date(date_labels = "%b %Y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 15,
                                  hjust = 0))

subwayplot
saveRDS(subwayplot, "finaldata/subwayridership_borough.rds")

nyc_lng <- -73.957042
nyc_lat <- 40.729728
nyc_map <- get_stamenmap(bbox = c(left = nyc_lng - 0.13, 
                                  bottom = nyc_lat - 0.22, 
                                  right = nyc_lng + 0.25,
                                  top = nyc_lat + 0.25),
                         zoom = 12, maptype = "toner-lite", crop = TRUE, messaging = FALSE,
                         urlonly = FALSE, color = "bw", force = FALSE)

subwaystns_sf <- subwaystns_raw %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = 4326)

subwayplot2 <- ggmap(nyc_map) +
  geom_sf(aes(fill = borough), data = subwaystns_sf,
          shape = 21, color =  "black", 
          size = 1.2, stroke = .1,
          inherit.aes = FALSE) +
  theme_minimal() +
  scale_fill_manual(values = c("Brooklyn" = "#64c916",
                                "Manhattan" = "#e5734d",
                                "Queens" = "#a34ddf",
                                "Bronx" = "#4dc5df",
                                "Staten Island" = "#dee750")) +
  labs(fill = "Borough") +
  theme(plot.title = element_text(size = 15,
                                  hjust = 0.5),
        legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

subwayplot2
saveRDS(subwayplot2, "finaldata/subway_map.rds")

# bicycle routes visualization

bikeroutes_byyear <- bicycle_routes %>%
  mutate(Year = lubridate::year(date_instd)) %>%
  group_by(Year) %>%
  summarise(geom = st_combine(geometry)) %>%
  ungroup() %>%
  filter(Year < 2019) %>%
  mutate(Group = cut(Year, 
                     breaks = c(1890, 2000, 2005, 2010, 2015, 2020),
                     labels = c("Pre 2000s",
                                "2000 to 2004",
                                "2005 to 2009",
                                "2010 to 2014",
                                "2015 to 2018"))) %>%
  group_by(Group) %>%
  summarise(geom = st_combine(geom))

bikerouteplot1 <- ggmap(nyc_map) +
  geom_sf(data = bikeroutes_byyear, 
          aes(color = Group,
              fill = Group),
          inherit.aes = FALSE) +
  scale_color_brewer(palette = "YlOrRd") +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(color = "Installed Date",
       fill = "Installed Date") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15,
                                  hjust = 0.5),
        legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank())

bikerouteplot1
saveRDS(bikerouteplot1, "finaldata/bikelane_map.rds")

bikeroutes_byyear2 <- bicycle_routes %>%
  mutate(Year = lubridate::year(date_instd)) %>%
  group_by(Year, boro) %>%
  summarise(geom = st_combine(geometry)) %>%
  ungroup() %>%
  filter(Year >= 1990) %>%
  mutate(dist = as.numeric(st_length(geom)),
         boro = ifelse(boro == 1, "Manhattan",
                       ifelse(boro == 4, "Queens",
                              ifelse(boro == 3, "Brooklyn",
                                     ifelse(boro == 2, "Bronx", "Staten Island")))))

bikerouteplot2 <- ggplot(bikeroutes_byyear2) +
  geom_point(aes(x = Year, y = dist, color = boro), size = 1, alpha = 0.3) +
  geom_smooth(aes(x = Year, y = dist, color = boro), se = FALSE, alpha = 0.6) +
  annotate("rect",
           xmin = 2012, xmax = 2014,
           ymin = 0, ymax = Inf,
           fill = "black", alpha = .2) +
  labs(y = "Distance of bicycle lanes built (in meters)",
       color = "Borough") +
  scale_color_manual(values = c("Brooklyn" = "#64c916",
                                "Manhattan" = "#e5734d",
                                "Queens" = "#a34ddf",
                                "Bronx" = "#4dc5df",
                                "Staten Island" = "#f4e01d")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15,
                                  hjust = 0.5),
        legend.position = "right",
        panel.background = element_blank())

bikerouteplot2

saveRDS(bikerouteplot2, "finaldata/bikelanebuilt_borough.rds")

# weather visualization

weatherplot1 <- weather %>%
  select(date, apptemp_low) %>%
  mutate(Year = as.character(lubridate::year(date)),
         Week = lubridate::week(date)) %>%
  group_by(Week, Year) %>%
  summarise(Value = mean(apptemp_low)) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = Week, y = Value, color = Year),
             size = .7, alpha = 0.4) +
  geom_smooth(aes(x = Week, y = Value, color = Year), 
              se = FALSE) +
  labs(x = "Week",
       y = "Apparent Low Temperature (degree Celsius)") +
  scale_color_brewer(palette = "YlOrRd") +
  theme_minimal()

weatherplot1
saveRDS(weatherplot1, "finaldata/weathertemp_year.rds")

# Citi Bike trips visualization

cbplot1 <- cbtrips %>%
  left_join(cbstns, by = "stn_id") %>%
  select(date, borough, start_counts) %>%
  mutate(Year = lubridate::year(date),
         Month = lubridate::month(date),
         NewDate = as.Date(paste("01", Month, Year, sep = "-"), format = "%d-%m-%Y")) %>%
  group_by(NewDate, borough) %>%
  summarise(ridership = sum(start_counts)) %>%
  ungroup() %>%
  filter(!is.na(borough)) %>%
  ggplot() +
  geom_point(aes(x = NewDate, y = ridership, color = borough)) +
  geom_line(aes(x = NewDate, y = ridership, color = borough)) +
  scale_color_manual(values = c("Brooklyn" = "#64c916",
                                "Manhattan" = "#e5734d",
                                "Queens" = "#a34ddf",
                                "Bronx" = "#4dc5df",
                                "Staten Island" = "#dee750")) +
  labs(x = "Date",
       y = "Monthly number of Citi Bike trips taken (in thousands)",
       color = "Borough") +
  theme_minimal() +
  scale_y_continuous(labels = function(x) x/1000) +
  theme(legend.position = "right")

cbplot1
saveRDS(cbplot1, "finaldata/cbtrips_borough.rds")

cbstns_sf <- cbtrips %>%
  distinct(stn_id) %>%
  left_join(cbstns) %>%
  st_as_sf(coords = c("stn_lon", "stn_lat"),
           crs = 4326)

nyc_map2 <- get_stamenmap(bbox = c(left = nyc_lng - 0.07, 
                                   bottom = nyc_lat - 0.055, 
                                   right = nyc_lng + 0.01,
                                   top = nyc_lat + 0.045),
                          zoom = 13, maptype = "toner-lite", crop = TRUE, messaging = FALSE,
                          urlonly = FALSE, color = "bw", force = FALSE)

cbplot2 <- ggmap(nyc_map2) +
  geom_sf(data = cbstns_sf, fill = "#3a47ff",
          shape = 21, color = "black", size = 1.5, stroke = 0.1,
          inherit.aes = FALSE) +
  labs(fill = "Opening Year") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank())

cbplot2
saveRDS(cbplot2, "finaldata/cbstns_map.rds")


######

cbstns_sf <- cbtrips %>%
  distinct(stn_id) %>%
  left_join(cbstns) %>%
  st_as_sf(coords = c("stn_lon", "stn_lat"),
           crs = 4326)

grandcentral <- subwaystns %>%
  filter(round(stop_lon, 5) == -73.97685) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = 4326)

grandcentral_circle <- subwaystns %>%
  filter(round(stop_lon, 5) == -73.97685) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = 4326) %>%
  st_transform(crs = 3857) %>%
  st_buffer(dist = 470) %>%
  st_transform(crs = 4326)

grandcentral_cbstns <- cbstns_sf %>%
  filter(stn_id %in% nearby_cbstns)

idx <- geosphere::distHaversine(c(-73.97685, 40.75178), cbstns[,c(4,3)]) <= 400
nearby_cbstns <- cbstns$stn_id[idx]

nyc_gc_map <- get_stamenmap(bbox = c(left = nyc_lng - 0.029, 
                                     bottom = nyc_lat + 0.017, 
                                     right = nyc_lng - 0.011,
                                     top = nyc_lat + 0.027),
                            zoom = 15, maptype = "toner-lite", crop = TRUE, messaging = FALSE,
                            urlonly = FALSE, color = "bw", force = FALSE)

cbplot3 <- ggmap(nyc_gc_map, darken = c(0.4, "white")) +
  geom_sf(data = grandcentral_circle, 
          alpha = 0.5, fill = "grey",
          inherit.aes = FALSE) +
  geom_sf(data = grandcentral, 
          size = 8, color = "orange",
          inherit.aes = FALSE) +
  geom_sf(data = cbstns_sf, fill = "#3a47ff",
          shape = 21, color = "black", size = 3, stroke = 0.1,
          inherit.aes = FALSE) +
  labs(fill = "Opening Year") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank())
cbplot3
saveRDS(cbplot3, "finaldata/cbstns_nearby.rds")

grandcentral_circle2 <- subwaystns %>%
  filter(round(stop_lon, 5) == -73.97685) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"),
           crs = 4326) %>%
  st_transform(crs = 3857) %>%
  st_buffer(dist = 1200) %>%
  st_transform(crs = 4326)

grandcentral_bikeroutes <- st_combine(st_intersection(st_geometry(grandcentral_circle2), st_geometry(st_transform(bikeroutes_byyear, 4326))))

nyc_gc_map2 <- get_stamenmap(bbox = c(left = nyc_lng - 0.04, 
                                     bottom = nyc_lat + 0.01, 
                                     right = nyc_lng - 0,
                                     top = nyc_lat + 0.035),
                            zoom = 15, maptype = "toner-lite", crop = TRUE, messaging = FALSE,
                            urlonly = FALSE, color = "bw", force = FALSE)

grandcentral_bikelanemap <- ggmap(nyc_gc_map2, darken = c(0.4, "white")) +
  geom_sf(data = grandcentral_circle2, 
          alpha = 0.5, fill = "grey",
          inherit.aes = FALSE) +
  geom_sf(data = grandcentral, 
          size = 8, color = "orange",
          inherit.aes = FALSE) +
  geom_sf(data = bikeroutes_byyear,
          size = 1.1, alpha = .5,
          color = "#13b426",
          inherit.aes = FALSE) +
  geom_sf(data = grandcentral_bikeroutes,
          size = 1.3,
          color = "#2786dd",
          inherit.aes = FALSE) +
  theme_minimal() +
  theme(legend.position = "right",
        axis.text = element_blank(),
        axis.title = element_blank())

grandcentral_bikelanemap
saveRDS(grandcentral_bikelanemap, "finaldata/grandcentral_bikelanemap.rds")


