##################################################
#####     Ice in ice out
##################################################

library(tidyverse)
library(magrittr)
library(glue)
library(tsibble)
library(timetk)
library(tidyquant)
library(tibbletime)
library(lubridate)
library(cowplot)
library(recipes)
library(rsample)
library(yardstick) 
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
library(keras)
library(tfruns)

# ##########################################################################################
# 
# ######################################################
# #####    Connect to CORE_WU using ODCconnect    #####
# ######################################################
# 
# ##### read needed data into R oobjects (ice data, core_wu, lake cnetroaids)
# 
# # Set up new User DSN on your computer
# # Control Panel (on your computer, not within RStudio)/Administrative Tools/ODBC Data 
# # Source Administrator (64-bit) User DSN tab/Add/OraClient11g_home2/Finish
# # Data Source Name = DeltaW
# # TNS Service Name = deltaw.pca.state.mn.us
# 
# # open ODBC connection
# deltaw <- odbcConnect('deltaw', uid='tableau', pwd='data_origamiW')
# 
# # view tables inside a given schema
# core_wu_tables <- sqlTables(deltaw, schema = 'CORE_WU')
# 
# # load table into R
# v_wu_lake <- as.tibble(sqlQuery(deltaw, '
#                                   SELECT *
#                                   FROM
#                                   CORE_WU.V_WU_LAKE'))
# 
# # when you are ready to close the ODBC connection
# odbcClose(deltaw)
# 
# # read in ice data 
# lake_ice_in_all_09_2019 <- read.table("H:/projects/Climate Change/ice/lake_ice_in_all_09_2019.txt",  
#                                       header = FALSE, sep = ";", quote = "\"", 
#                                       colClasses = c(V1 = "character"))
# lake_ice_out_all_09_2019 <- read.table("H:/projects/Climate Change/ice/lake_ice_out_all_09_2019.txt",  
#                                        header = FALSE, sep = ";", quote = "\"", 
#                                        colClasses = c(V1 = "character"))
# 
# # read in lake centroids 
# dowlakes_centroids_dnr <- read_sf("R:/surface_water/dowlakes_centroids_dnr.shp")
# names(dowlakes_centroids_dnr)
# crs(dowlakes_centroids_dnr)
# st_crs(dowlakes_centroids_dnr)
# ##########################################################################################
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
  dplyr::select(wid, wu_name, alt_name, depth_mean_ft, depth_max_ft, area_acres, 
                ecoregion_code)

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

# with geom_smooth
ggplot(lake_ice_50_trimmed, aes(x = waterYear, y = ice_on_duration)) + 
  geom_line(aes(group = wu_name)) + 
  geom_smooth(aes(group = wu_name), method = "loess", se = FALSE) +
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
  ggtitle('All 16 lakes: ice duration')

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

lake_ice_16_nest$lake_plots[3]

##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
##### ETS and ARIMA forecasting        
##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####

##### convert mn_ice_50_trimmed to ts and reevaluate with forcast #####
mn_ice_50_trimmed$waterYear

# clean up and remove recoreds prior to 1925
mn_ice_50_trimmed2 <- mn_ice_50_trimmed %>% 
  rename(duration = `mean(ice_on_duration)`) %>% 
  mutate(date = make_date(waterYear)) %>% 
  dplyr::select(-waterYear) %>% 
  filter(date >= '1925-01-01')
mn_ice_50_trimmed2$duration <- as.numeric(mn_ice_50_trimmed2$duration)

mn_ice_50_trimmed2_ts <- tk_ts(mn_ice_50_trimmed2, start = 1925, frequency = 1, silent = TRUE)
has_timetk_idx(mn_ice_50_trimmed2_ts)  

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

#plot with years on x axis
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
# univar_lakeice_ts <- ts(mn_ice_50_trimmed$duration, start = 1919, end = 2019, frequency = 1) 
# plot(univar_lakeice_ts)

##### LSTM and Keras #####

sun_spots <- datasets::sunspot.month %>%
  tk_tbl() %>%
  mutate(index = as_date(index)) %>%
  as_tbl_time(index = index)

sun_pots

# clean up and remove recoreds prior to 1896
mn_ice_50trimmed_1896 <- mn_ice_50_trimmed %>% 
  rename(duration = `mean(ice_on_duration)`) %>% 
  mutate(date = make_date(waterYear)) %>% 
  dplyr::select(-waterYear) %>% 
  filter(date >= '1896-01-01')
mn_ice_50trimmed_1896$duration <- as.numeric(mn_ice_50trimmed_1896$duration)

# mn_ice_50trimmed_1899_ts <- tk_ts(mn_ice_50trimmed_1899, start = 1899, frequency = 1, silent = TRUE)
# has_timetk_idx(mn_ice_50trimmed_1899_ts)

# convert ts to time tibble
mn_ice_ttbl <- mn_ice_50trimmed_1896 %>%
  dplyr::select(date, duration) %>% 
  as_tbl_time(index = date)
  
# visualize with cowplot
p1 <- mn_ice_ttbl %>%
  ggplot(aes(date, duration)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "Ice cover (days)"
  )

p2 <- mn_ice_ttbl %>%
  filter_time("1999" ~ "2019") %>%
  ggplot(aes(date, duration)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1999 to 2019 (Zoomed In)",
    caption = "Lakes with > 50 years of data records"
  )

p_title <- ggdraw() + 
  draw_label("Minnesota Lake Ice", size = 18, fontface = "bold", 
             colour = palette_light()[[1]])

plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))

##### backtesting - time series cross validation #####
# the rsample package includes facitlities for backtesting on time series - the vignette, 
# https://tidymodels.github.io/rsample/articles/Applications/Time_Series.html, describes a 
# procedure that uses the rolling_origin() function to create samples designed for time 
# series cross validation

# 30 year training set, 15 years testing set and overlapps
periods_train <- 75
periods_test  <- 25
skip_span     <- 3

rolling_origin_resamples <- rolling_origin(
  mn_ice_ttbl,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)

rolling_origin_resamples

# plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, 
                       alpha = 1, size = 1, base_size = 14) {
  
  # manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = date) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # visualize
  g <- data_manipulated %>%
    ggplot(aes(x = date, y = duration, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to ", 
                      "{test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    mn_ice_time_summary <- mn_ice_ttbl %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(mn_ice_time_summary$start, 
                              mn_ice_time_summary$end))
  }
  
  g
}

rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) +
  theme(legend.position = "bottom")

# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 14, fontface = "bold", 
               colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, 
                 rel_heights = c(0.05, 1, 0.05))
  
  g
  
}

# visualize the entire backtesting strategy with plot_sampling_plan()
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")

rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Zoomed In")

# build the LSTM model - begin with a single sample from the backtesting strategy, 
# the most recent slice - then apply the model to all samples to investigate modeling 
# performance
# example_split1    <- rolling_origin_resamples$splits[[1]]
# example_split_id1 <- rolling_origin_resamples$id[[1]]
# 
# example_split2    <- rolling_origin_resamples$splits[[2]]
# example_split_id2 <- rolling_origin_resamples$id[[2]]
# 
# example_split3    <- rolling_origin_resamples$splits[[3]]
# example_split_id3 <- rolling_origin_resamples$id[[3]]
# 
# example_split4    <- rolling_origin_resamples$splits[[4]]
# example_split_id4 <- rolling_origin_resamples$id[[4]]

example_split <- rolling_origin_resamples$splits[[6]]
example_split_id <- rolling_origin_resamples$id[[6]]

plot_split(example_split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {example_split_id}"))


# data set up
# dedicate 2 thirds of the analysis set to training, and 1 third to validation
df_trn <- analysis(example_split)[1:75, , drop = FALSE]
df_val <- analysis(example_split)[76:100, , drop = FALSE]
df_tst <- assessment(example_split)

# combine the training and testing data sets into a single data set with a column 
# key that specifies where they came from (either “training” or “testing)” - note that 
# the tbl_time object will need to have the index respecified during the bind_rows() step, 
# but this issue should be corrected in dplyr soon
df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_val %>% add_column(key = "validation"),
  df_tst %>% add_column(key = "testing")
) %>%
  as_tbl_time(index = date)

df

# preprocessing with recipes
# the LSTM algorithm will usually work better if the input data has been centered and scaled - 
# we can do this using the recipes package - in addition to step_center and step_scale, we’re 
# using step_sqrt to reduce variance and remov outliers - the actual transformations are 
# executed when we bake the data according to the recipe
rec_obj <- recipe(duration ~ ., df) %>%
  step_sqrt(duration) %>%
  step_center(duration) %>%
  step_scale(duration) %>%
  prep()

df_processed_tbl <- bake(rec_obj, df)

df_processed_tbl

# next capture the original center and scale so we can invert the steps after modeling - 
# the square root step can then simply be undone by squaring the back-transformed data
center_history <- rec_obj$steps[[2]]$means["duration"]
scale_history  <- rec_obj$steps[[3]]$sds["duration"]

c("center" = center_history, "scale" = scale_history)

# reshaping the data
# keras LSTM expects the input as well as the target data to be in a specific shape - the 
# input has to be a 3-d array of size num_samples, num_timesteps, num_features

# num_samples is the number of observations in the set - this will get fed to the model in 
# portions of batch_size - the second dimension, num_timesteps, is the length of the hidden 
# state we were talking about above - the third dimension is the number of predictors we’re 
# using - for univariate time series, this is 1

# reshape the data - the main action here is creating the sliding windows of 12 steps of input, 
# followed by 12 steps of output each. This is easiest to understand with a shorter and simpler 
# example - say our input were the numbers from 1 to 10, and our chosen sequence length 
# (state size) were 4 - this is how the training input should look:

# 1,2,3,4
# 2,3,4,5
# 3,4,5,6

# and our target data, correspondingly:
  
# 5,6,7,8
# 6,7,8,9
# 7,8,9,10

# define a short function that does this reshaping on a given dataset - then finaly, add the 
# third axis that is formally needed (even though that axis is of size 1 in our case)

# these variables are being defined just because of the order in which
# we present things in this post (first the data, then the model)
# they will be superseded by FLAGS$n_timesteps, FLAGS$batch_size and n_predictions
# in the following snippet
n_timesteps <- 12
n_predictions <- n_timesteps
batch_size <- 10

# functions used
build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

# extract values from data frame
train_vals <- df_processed_tbl %>%
  filter(key == "training") %>%
  dplyr::select(duration) %>%
  pull()
valid_vals <- df_processed_tbl %>%
  filter(key == "validation") %>%
  dplyr::select(duration) %>%
  pull()
test_vals <- df_processed_tbl %>%
  filter(key == "testing") %>%
  dplyr::select(duration) %>%
  pull()

# build the windowed matrices
train_matrix <-
  build_matrix(train_vals, n_timesteps + n_predictions)
valid_matrix <-
  build_matrix(valid_vals, n_timesteps + n_predictions)
test_matrix <- build_matrix(test_vals, n_timesteps + n_predictions)

# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement)
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]
# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)




# separate matrices into training and testing parts
# also, discard last batch if there are fewer than batch_size samples
# (a purely technical requirement)
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_train <- X_train[1:(nrow(X_train) %/% batch_size * batch_size), ]
y_train <- y_train[1:(nrow(y_train) %/% batch_size * batch_size), ]

X_valid <- valid_matrix[, 1:n_timesteps]
y_valid <- valid_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_valid <- X_valid[1:(nrow(X_valid) %/% batch_size * batch_size), ]
y_valid <- y_valid[1:(nrow(y_valid) %/% batch_size * batch_size), ]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]
X_test <- X_test[1:(nrow(X_test) %/% batch_size * batch_size), ]
y_test <- y_test[1:(nrow(y_test) %/% batch_size * batch_size), ]
# add on the required third axis
X_train <- reshape_X_3d(X_train)
X_valid <- reshape_X_3d(X_valid)
X_test <- reshape_X_3d(X_test)

y_train <- reshape_X_3d(y_train)
y_valid <- reshape_X_3d(y_valid)
y_test <- reshape_X_3d(y_test)





# # decompose the ts
# univar_lakeice_ts_deseasonal <- seasadj(univar_lakeice_ts) 
# plot(rainfall_deseasonal1)

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




  


  
  




