##################################################
#####     Ice in ice out
##################################################

library(tidyverse)
library(magrittr)
library(tsibble)
library(timetk)
library(lubridate)
library(dataRetrieval)
library(withr)
library(RODBC)
library(rgdal) 
library(raster)
library(sf)
library(ggmap)
library(mapview)
library(forecast)
library(dygraphs)
library(janitor)
library(sweep)
library(tidyquant)

######################################################
#####    Connect to CORE_WU using ODCconnect    #####
######################################################

##### read needed data into R oobjects (ice data, core_wu, lake cnetroaids)

# Set up new User DSN on your computer
# Control Panel (on your computer, not within RStudio)/Administrative Tools/ODBC Data 
# Source Administrator (64-bit) User DSN tab/Add/OraClient11g_home2/Finish
# Data Source Name = DeltaW
# TNS Service Name = deltaw.pca.state.mn.us

# open ODBC connection
deltaw <- odbcConnect('deltaw', uid='tableau', pwd='data_origamiW')

# view tables inside a given schema
core_wu_tables <- sqlTables(deltaw, schema = 'CORE_WU')

# load table into R
v_wu_lake <- as.tibble(sqlQuery(deltaw, '
                                  SELECT *
                                  FROM
                                  CORE_WU.V_WU_LAKE'))

# when you are ready to close the ODBC connection
odbcClose(deltaw)

# read in ice data 
lake_ice_in_all_09_2019 <- read.table("H:/projects/Climate Change/ice/lake_ice_in_all_09_2019.txt",  header = FALSE, 
                                      sep = ";", quote = "\"", colClasses = c(V1 = "character"))
lake_ice_out_all_09_2019 <- read.table("H:/projects/Climate Change/ice/lake_ice_out_all_09_2019.txt",  header = FALSE, 
                                       sep = ";", quote = "\"", colClasses = c(V1 = "character"))

# read in lake centroids 
dowlakes_centroids_dnr <- read_sf("R:/surface_water/dowlakes_centroids_dnr.shp")
names(dowlakes_centroids_dnr)
crs(dowlakes_centroids_dnr)
st_crs(dowlakes_centroids_dnr)

# lower case
v_wu_lake <- clean_names(v_wu_lake)
lake_ice_in_all_09_2019 <- clean_names(lake_ice_in_all_09_2019)
lake_ice_out_all_09_2019 <- clean_names(lake_ice_out_all_09_2019)
dowlakes_centroids_dnr <- clean_names(dowlakes_centroids_dnr)
# names(v_wu_lake) %<>% tolower
# names(lake_ice_in_all_09_2019) %<>% tolower
# names(lake_ice_out_all_09_2019) %<>% tolower
# names(dowlakes_centroids_dnr) %<>% tolower

##### trim data 

# trim down v_wu_lake
v_wu_lake <- v_wu_lake %>% 
  dplyr::select(wid, wu_name, alt_name, depth_mean_ft, depth_max_ft, area_acres, ecoregion_code)

# trim dowlakes_centroids_dnr
dowlakes_centroids_dnr <- dowlakes_centroids_dnr %>% 
  dplyr::select(dowlknum, lake_class, acres, shore_mi, latddnad83, londdnad83)
# group and summarize   
dowlakes_centroids_dnr <- dowlakes_centroids_dnr %>% 
  group_by(dowlknum, latddnad83, londdnad83) %>% 
  summarise(acres = mean(acres), shore_mi = mean(shore_mi))

# name variables, identify "data"in" or "out"
lake_ice_in <- as_tibble(lake_ice_in_all_09_2019) %>% 
  rename(lake_id = v1, Date = v2, source = v3) %>% 
  mutate(measure = "ice_in") %>% 
  dplyr::select(lake_id, measure, Date, source)
lake_ice_out <- as_tibble(lake_ice_out_all_09_2019) %>% 
  rename(lake_id = v1, Date = v2, source = v3) %>% 
  mutate(measure = "ice_out") %>% 
  dplyr::select(lake_id, measure, Date, source)

# coerce measure as a factor, measure_date as a date, add WaterYear column 
lake_ice_in$source <- str_trim(lake_ice_in$source)
lake_ice_in$source <- as_factor(lake_ice_in$source)
lake_ice_in$measure <- as_factor(lake_ice_in$measure)
lake_ice_in$Date <- lake_ice_in$Date %>% 
  as_date('%Y-%m-%d') 
lake_ice_in <- addWaterYear(lake_ice_in)

lake_ice_out$source <- str_trim(lake_ice_out$source)
lake_ice_out$source <- as_factor(lake_ice_out$source)
lake_ice_out$measure <- as_factor(lake_ice_out$measure)
lake_ice_out$Date <- lake_ice_out$Date %>% 
  as_date('%Y-%m-%d') 
lake_ice_out <- addWaterYear(lake_ice_out)

# join by lake and waterYear, remove NAs for single table of lakes with matching ice_in ice_out years
lake_ice_duration <- full_join(lake_ice_out, lake_ice_in, by = c('lake_id', 'waterYear')) %>% 
  drop_na()
lake_ice_duration <- lake_ice_duration %>% 
  rename(ice_out_date = Date.x, ice_in_date = Date.y, source_out = source.x, source_in = source.y) %>% 
  dplyr::select(lake_id, source_in, ice_in_date, source_out, ice_out_date, waterYear)
# lake_ice_duration$lake_id <- as_factor(lake_ice_duration$lake_id)

# create an "ice on" time object in days
lake_ice <- lake_ice_duration %>% 
  mutate(ice_on_duration = ice_out_date - ice_in_date)

# create a ranked list of lakes and their data record periods
record_length <- lake_ice %>% 
  group_by(lake_id) %>% 
  summarise(
    record_length = max(waterYear) - min(waterYear) + 1
  ) %>% 
  arrange(desc(record_length)) 

record_length

# # remove duplicate records and convert to ts object
# lake_ice %>%
#   group_by(lake_id, waterYear) %>%
#   filter(row_number() == 1)
# 
# # add an index
# lake_ice %>% 
#   mutate(record = row_number()) %>% 
#   select(record, everything())
# 
# lake_ice <- lake_ice %>% 
#   as_tsibble(key = lake_id, index = waterYear)

# find multiple "ice on" "ice_off" dates for single year - if more than one, calculate mean date
lake_ice_in_mean <- lake_ice_in %>% 
  group_by(lake_id, waterYear) %>% 
  summarise(Date = mean(Date), ice_in_n = n())
lake_ice_out_mean <- lake_ice_out %>% 
  group_by(lake_id, waterYear) %>% 
  summarise(Date = mean(Date), ice_out_n = n())

# # find multiple "ice on" "ice_off" dates for single year - if more than one, calculate latest (max) ice_in and 
# # earliest (min) ice_off dates 
# lake_ice_in_max <- lake_ice_in %>% 
#   group_by(lake_id, waterYear) %>% 
#   summarise(Date = max(Date), ice_in_n = n())
# lake_ice_out_min <- lake_ice_out %>% 
#   group_by(lake_id, waterYear) %>% 
#   summarise(Date = min(Date), ice_out_n = n())

# join mean ice data by lake and waterYear, remove NAs for single table of lakes with matching ice_in ice_out years
lake_ice_mean <- full_join(lake_ice_out_mean, lake_ice_in_mean, by = c('lake_id', 'waterYear')) %>% 
  drop_na()
lake_ice_duration_mean <- lake_ice_mean %>% 
  rename(ice_out_date = Date.x, ice_in_date = Date.y) %>% 
  dplyr::select(lake_id, ice_in_date, ice_out_date, waterYear)
# lake_ice_duration_mean$lake_id <- as_factor(lake_ice_duration_mean$lake_id)

# create an "ice on" time object in days
lake_ice_mean <- lake_ice_duration_mean %>% 
  mutate(ice_on_duration = ice_out_date - ice_in_date)

# duplicate the "lake_id" column and insert hypens into the new column to change dnr nomenclature to mpca nomenclature, 
# then move new column to second position in table
lake_ice_mean <- lake_ice_mean %>% 
  mutate(lake_id_mpca = str_replace(lake_id,"(\\d{2})(\\d{4})(\\d{2})$","\\1-\\2-\\3")) %>% 
  dplyr::select(lake_id, lake_id_mpca, everything())

# join other data to "lake_ice_mean"
lake1 <- left_join(lake_ice_mean, v_wu_lake, by = c("lake_id_mpca" = "wid")) 
lake2 <- left_join(lake1, dowlakes_centroids_dnr, by = c("lake_id" = "dowlknum")) %>% 
  dplyr::select(-alt_name, -depth_mean_ft, -geometry) %>% 
  mutate(sq_mi = acres / 640)
lake3 <- left_join(lake2, record_length, by = "lake_id") %>% 
  dplyr::select(lake_id, lake_id_mpca, wu_name, record_length, everything()) %>% 
  na.omit() %>% 
  arrange(desc(record_length))

##### map all the lakes
# trim down columns and create data frame of lakes with lats and lons
lake_locations <- lake3 %>% 
  dplyr::select(lake_id, lake_id_mpca, record_length, latddnad83, londdnad83) %>% 
  group_by(lake_id, lake_id_mpca, record_length, latddnad83, londdnad83) %>% 
  summarise(n = mean(latddnad83)) %>% 
  dplyr::select(-n)
# create as sf object
lake_locations_sf <- st_as_sf(lake_locations, coords = c("londdnad83", "latddnad83"), crs = 4326)
# plot map
mapview(lake_locations_sf, color = "grey40", cex = 1)
mapview(lake_locations_sf, cex = "record_length")

ggplot(lake3, aes(x = waterYear, y = ice_on_duration)) + 
  geom_point() +
  xlim(1980, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  ggtitle('All MN lake ice duration')

all_mn_ice <- lake3 %>% 
  group_by(waterYear) %>% 
  summarise(mean(ice_on_duration))

ggplot(all_mn_ice, aes(x = waterYear, y = `mean(ice_on_duration)`)) + 
  geom_line() + 
  geom_point() + 
  xlim(1920, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  ggtitle('MN lake ice duration (all lakes annual means)')

##### limmit to lakes with 50 years of ice duration data
lake_ice_50 <- lake3 %>% 
  filter(record_length >= 50)

# plot line graphs of the > 50 lakes
ggplot(lake_ice_50, aes(x = waterYear, y = ice_on_duration)) + 
  geom_line(aes(group = wu_name)) +
  # geom_point(aes(color = WU_NAME)) + 
  # geom_smooth(aes(group = WU_NAME), method = "lm", se = FALSE) +
  xlim(1960, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  facet_wrap(~ wu_name) +
  ggtitle('Minnesota lake ice duration') 

# drop 50+ year lakes with excessive data gaps
remove_lakes <- c('Gull', 'Itasca', 'Kabekona', 'Lower Hay', 'Nokomis', 'North Long', 'Pepin', 'Shagawa')
lake_ice_50_trimmed <- lake_ice_50 %>% 
  filter(!wu_name %in% remove_lakes)

# plot line graphs of the trimmed > 50 lakes
# line
ggplot(lake_ice_50_trimmed, aes(x = waterYear, y = ice_on_duration)) + 
  geom_line(aes(group = wu_name)) +
  # geom_point(aes(color = WU_NAME)) + 
  # geom_smooth(aes(group = WU_NAME), method = "lm", se = FALSE) +
  xlim(1960, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  facet_wrap(~ wu_name) +
  ggtitle('Minnesota lake ice duration (16 lakes)')

# with geom_smooth
ggplot(lake_ice_50_trimmed, aes(x = waterYear, y = ice_on_duration)) + 
  geom_line(aes(group = wu_name)) + 
  geom_smooth(aes(group = wu_name), method = "lm", se = FALSE) +
  xlim(1960, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  facet_wrap(~ wu_name) +
  ggtitle('Minnesota lake ice duration (16 lakes)')

##### map the trimmed 50 year lakes with the mapview function
# trim down columns and create data frame of lakes with lats and lons
lake_locations_50_trimmed <- lake_ice_50_trimmed %>% 
  dplyr::select(lake_id, lake_id_mpca, wu_name, ecoregion_code, record_length, latddnad83, londdnad83) %>% 
  group_by(lake_id, lake_id_mpca, wu_name, ecoregion_code, record_length, latddnad83, londdnad83) %>% 
  summarise(n = mean(latddnad83)) %>% 
  dplyr::select(-n)

lake_locations_50_trimmed_sf <- st_as_sf(lake_locations_50_trimmed, coords = c("londdnad83", "latddnad83"), crs = 4326)

mapview(lake_locations_50_trimmed_sf, color = "grey40")
mapview(lake_locations_50_trimmed_sf, cex = "record_length")

mn_ice_50_trimmed <- lake_ice_50_trimmed %>% 
  group_by(waterYear) %>% 
  summarise(mean(ice_on_duration))

ggplot(mn_ice_50_trimmed, aes(x = waterYear, y = `mean(ice_on_duration)`)) + 
  geom_line() + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  xlim(1950, 2020) +
  ylab('lake ice (days)') +
  xlab('Year') +
  ggtitle('State wide lake ice duration')

###### convert mn_ice_50_trimmed to ts and reevaluate with forcast
mn_ice_50_trimmed$waterYear

# clean up and remove recoreds prior to 1925
mn_ice_50_trimmed2 <- mn_ice_50_trimmed %>% 
  rename(duration = `mean(ice_on_duration)`) %>% 
  mutate(date = make_date(waterYear)) %>% 
  dplyr::select(-waterYear) %>% 
  filter(date >= '1925-01-01')
mn_ice_50_trimmed2$duration <- as.numeric(mn_ice_50_trimmed2$duration)

mn_ice_50_trimmed2_ts <- tk_ts(mn_ice_50_trimmed2, start = 1925, frequency = 1, silent = TRUE)
has_timetk_idx(mn_ice_50_trimmed_ts)  

##### ets model
fit_ets <- mn_ice_50_trimmed2_ts %>% 
  ets()

sw_tidy(fit_ets) 
sw_glance(fit_ets)
sw_augment(fit_ets)

augment_fit_ets <- sw_augment(fit_ets)

# plot the residuals
augment_fit_ets %>%
  ggplot(aes(x = index, y = .resid)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(method = "loess") +
  scale_x_yearmon(n = 10) +
  labs(title = "MN 16 lake ice duration: ETS Residuals", x = "") + 
  theme_tq()

# decompose the time series
decomp_fit_ets <- sw_tidy_decomp(fit_ets)
decomp_fit_ets 

# visualize the decomposition - gather to reshape the data into long format data frame
decomp_fit_ets %>%
  gather(key = key, value = value, -index) %>%
  mutate(key = forcats::as_factor(key)) %>%
  ggplot(aes(x = index, y = value, group = key)) +
  geom_line(color = palette_light()[[2]]) +
  geom_ma(ma_fun = SMA, n = 12, size = 1) +
  facet_wrap(~ key, scales = "free_y") +
  scale_x_yearmon(n = 10) +
  labs(title = "MN 16 lake ice duration: ETS Decomposition", x = "") + 
  theme_tq() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# forcast the model
fcast_ets <- fit_ets %>%
  forecast(h = 10)
# tidy the forecast object
sw_sweep(fcast_ets, fitted = TRUE)

# visualize the forecast
sw_sweep(fcast_ets) %>%
  ggplot(aes(x = index, y = duration, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "MN Lake Ice Duration, ETS Model Forecast", x = "", y = "Days",
       subtitle = "Regular Time Index") +
  scale_y_continuous() +
  scale_x_yearmon(n = 1, format = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() 

sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  head()
sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  tail()

sw_sweep(fcast_ets, timetk_idx = TRUE) %>%
  ggplot(aes(x = index, y = duration, color = key)) +
  geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_line(size = 1) +
  labs(title = "MN Lake Ice Duration, ETS Model Forecast", x = "", y = "Days", 
       subtitle = "Irregular Time Index") +
  scale_y_continuous() +
  scale_x_date(date_breaks = "20 year", date_labels = "%Y") +
  scale_color_tq() +
  scale_fill_tq() +
  theme_tq() 

##### arima model
# quick plot of the data
autoplot(mn_ice_50_trimmed2_ts) + ylab("Ice cover (days)") + xlab("Year")

# first difference
mn_ice_50_trimmed2_ts %>% diff(lag = 1) %>% ggtsdisplay()
mn_ice_50_trimmed2_ts %>% diff(lag = 2) %>% ggtsdisplay()
mn_ice_50_trimmed2_ts %>% diff(lag = 3) %>% ggtsdisplay()
mn_ice_50_trimmed2_ts %>% diff(lag = 4) %>% ggtsdisplay()
mn_ice_50_trimmed2_ts %>% diff(lag = 5) %>% ggtsdisplay()

# take a seasonal difference
mn_ice_50_trimmed2_ts %>% diff(lag = 5) %>% ggtsdisplay()

# since raw data are clearly not stationary, take an additional first difference
mn_ice_50_trimmed2_ts %>% diff(lag = 4) %>% diff() %>% ggtsdisplay()

# try some different arima models
fit1 <- mn_ice_50_trimmed2_ts %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1))
fit2 <- mn_ice_50_trimmed2_ts %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1))
fit3 <- mn_ice_50_trimmed2_ts %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1))
fit_auto <- auto.arima(mn_ice_50_trimmed2_ts)

# try auto.arima
fit_auto %>%
  residuals() %>% 
  ggtsdisplay()
# check residuals
checkresiduals(fit_auto)
fit_auto %>% forecast(h = 10) %>% autoplot()


mn_ice_50_trimmed2_ts %>%
  Arima(order=c(0,1,1), seasonal=c(0,1,1)) %>%
  residuals() %>% ggtsdisplay()


# create univariate ts
univar_lakeice_ts <- ts(mn_ice_50_trimmed2$duration, start = 1925, end = 2019, frequency = 1) 
plot(univar_lakeice_ts)
# decompose the ts
univar_lakeice_ts_deseasonal <- seasadj(univar_lakeice_ts) 
plot(rainfall_deseasonal1)

##### nest the 16 lakes data by lake ####
lake_ice_16_nest <- lake_ice_50_trimmed %>% 
  group_by(lake_id, lake_id_mpca, wu_name, record_length, latddnad83, londdnad83) %>% 
  mutate(lat = latddnad83, lon = londdnad83) %>% 
  nest() %>% 
  rename(lake_name = wu_name) %>% 
  dplyr::select(lake_id, lake_id_mpca, lake_name, record_length, latddnad83, londdnad83, data) %>% 
  arrange(desc(record_length))
lake_ice_16_nest

lake_ice_16_nest <- lake_ice_16_nest %>% 
  mutate(
    lake_plots = map2(data, lake_name, ~ ggplot(data = .x, aes(x = waterYear, y = ice_on_duration)) + 
          geom_line() + 
          geom_point() + 
          geom_smooth(method = "lm", se = FALSE) +
          xlim(1950, 2020) +
          ylab('lake ice (days)') +
          xlab('Year') +
          ggtitle(.y)
        # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
        # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
        # ggtitle(label = loc_major_basin)
        # ggtitle('Minnesota stream Secchi tube measures')
    )
  )

# map2(paste0(lake_ice_16_nest$lake_name, ".pdf"), lake_ice_16_nest$lake_plots, ggsave)

# lake_ice_16_nest <- lake_ice_16_nest %>% 
#   mutate(
#     map(data, ~ ggplot(., aes(x = waterYear, y = ice_on_duration)) + 
#           geom_line() + 
#           geom_point() + 
#           geom_smooth(method = "lm", se = FALSE) +
#           xlim(1950, 2020) +
#           ylab('lake ice (days)') +
#           xlab('Year') +
#           ggtitle('Minnesota lake ice duration')
#         # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
#         # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
#         # ggtitle(label = loc_major_basin)
#         # ggtitle('Minnesota stream Secchi tube measures')
#     )
#   ) %>% 
#   rename(lake_plots =`map(...)`)
# lake_ice_16_nest

##### convert 16 50 year lakes into ts and plot with forcast #####
# lake_ice_16_nest_ts <- lake_ice_16_nest %>% 
#   mutate(data_ts = map(.x = data,
#                        .f = tk_ts,
#                        start = 1960,
#                        freq = 1))
# lake_ice_16_nest_ts












lake_ice_1975_means <- lake_ice_1975 %>% 
  group_by(waterYear) %>% 
  summarise(mean_ice_on_duration = mean(ice_on_duration) , n = n())

lake_ice_1975_train <- lake_ice_1975 %>% 
  filter(waterYear < 2010)

# plot training data
ggplot(lake_ice_1975_train, aes(x = waterYear, y = ice_on_duration)) +
  geom_line() + 
  ggtitle('Minnesota lake ice duration')

# nest data by lake 
lake_ice_nest <- lake_ice %>% 
  group_by(lake_id) %>% 
  nest()

lake_ice <- left_join(lake_ice, record_length, by = "lake_id")

lake_ice %>% 
  arrange(desc(record_length))

lake_ice_50 <- lake_ice %>% 
  filter(record_length >= 50)

lake_ice_50 <- lake_ice_50 %>% 
  mutate(
    map(data, ~ ggplot(., aes(x = waterYear, y = ice_on_duration)) + 
          geom_point(alpha = 0.5, size = 0.5) + 
          # labs(x = 'Year',
          #      y = 'Clarity (cm)',
          # title = 'Minnesota stream Secchi tube measures',
          # subtitle = loc_major_basin) +
          # ylim(0, 130) +
          ylab('lake ice (days)') +
          # xlim(1995, 2020) +
          xlab('Year') +
          geom_jitter() +
          # geom_smooth(method = 'loess') +
          # geom_abline(intercept = watershed_intercept, slope = watershed_slope, color = 'orange')
          # geom_abline(intercept = tobit_model$coefficients, slope = tobit_model$coefficients[2], color = 'green') 
          # ggtitle(label = loc_major_basin) 
          ggtitle('Minnesota lake ice duration')
    )
  ) %>% 
  rename(plots =`map(...)`)
  
##### ##### ##### #####


lake_ice_long <- rbind(lake_ice_in, lake_ice_out) 

lake_ice_long <- lake_ice_long %>% 
  mutate(id = rownames(lake_ice_long)) %>% 
  select(id, everything())

lake_ice_short <- lake_ice_long %>% 
  pivot_wider(names_from = measure, values_from = c(measure_date, year))

z <- lake_ice %>% 
  group_by(lake_id, measure, year) %>% 
  summarise(mean = mean.Date(measure_date)) %>% 
  rename(measure_date = mean) %>% 
  drop_na()

  

zz <- z %>% 
  arrange(lake_id, measure_date) %>% 
  group_by(lake_id) %>% 
  mutate(prior = lag(measure_date, 1)) %>%
  mutate(ice_cover = measure_date - lag(measure_date, 1)) %>% 
  mutate(record_length = max(year) - min(year)) %>% 
  select(lake_id, measure, year, prior, measure_date, ice_cover, record_length) %>% 
  drop_na()

zzz <- zz %>% 
  filter(measure == 'ice_out')
  
  

lake_ice <- lake_ice %>% 
  mutate(year = year(measure_date))

test <- read_csv("id, measure, measure_date
1, start, 1998-10-3 
2, start, 1998-11-12
1, stop, 1999-5-1
2, stop, 1999-5-25
1, stop, 2000-4-15
1, start, 1999-11-9
2, stop, 2000-6-1
2, start, 1999-12-1 ")

test$measure_date <- test$measure_date %>% as_date('%Y-%m-%d')

xxx <- test %>%
  arrange(id, measure_date) %>%
  group_by(id) %>%
  mutate(prior = lag(measure_date, 1)) %>%
  mutate(duration = measure_date - lag(measure_date, 1))


test <- lake_ice %>% 
  group_by(lake_id) %>% 
  nest()
