#load required packages
library(lubridate)
library(forecast)
library(tidyverse)
library(zoo)
library(modeltime)
library(timetk)
library(tidymodels)
library(tidyverse)

# * Data Import and Preprocessing ----
##read the scrubbed ufo dataset in
ufodata <- read_csv('scrubbed_1.csv')


#data exploration
str(ufodata)
head(ufodata)
head(ufodata$datetime)

#re-read accounting for na strings
ufodata <- read.csv('scrubbed_1.csv', header = TRUE, na.strings = c("", "?"))
#glimpse
glimpse(ufodata)
#count n/a
apply(is.na(ufodata),2,sum)

#coerce to data object
ufodata$datetime <- mdy_hm(ufodata$datetime)

#separate datetime into two separate columns
newset <-  separate(ufodata, datetime, into = c('date', 'time'), sep=" ")

newset$year <- year(newset$date)


#reduce dataset, eliminate other date variables
newset3 <- newset %>%
  select(everything(),-date,-date.posted)

#group data by year and count
set4 <- newset3%>%
  group_by(year)%>%
  summarize(count_year =n())

##change year from numeric to date and then convert to a tibble
set4$year <- ymd(set4$year, truncated = 2L)
set4_tib <- tibble(set4)

#filter data to start in 1945 and end in 2013
set5 <- set4_tib%>%
  filter_by_time(year, '1945-01', '2013')

#plot the time series for visual inspection
set5 %>%
  plot_time_series(year, count_year, .plotly_slider = TRUE)

##barplot to inspect/visualize annual growth rate
ggplot(set5, aes(x=year,y=count_year, fill=count_year))+
  geom_col()+scale_fill_viridis_c(direction = -1)+ theme_minimal()+theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(
    title = 'Total Reported UFO Counts by Year',
    y = 'count') 


# * Forecasting ---- 
##split my data - 80% used for training models / 20% for testing
splits <- initial_time_split(set5, prop=.80)


##create models (exponential smoothing, prophet and xgboost)

model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(count_year ~ year, data = training(splits))


model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(count_year ~ year, data = training(splits))

# * XGBOOST 
model_fit_xgboost <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(count_year ~ as.numeric(year), 
      data = training(splits))


model_tbl <- modeltime_table(
  model_fit_ets,
  model_fit_prophet,
  model_fit_xgboost
)

calibration_tbl <- model_tbl %>%
  modeltime_calibrate(testing(splits))


calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = FALSE)

calibration_tbl %>%
  modeltime_forecast(
    new_data = testing(splits), 
    actual_data = set5,
    conf_interval = 0.80
  ) %>%
  plot_modeltime_forecast(.legend_show = TRUE, 
                          .legend_max_width = 25, .conf_interval_show = FALSE)

# * Refit  
#refit models to entire dataset
refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = set5) 

#forecast for next 9 years only using exponential smoothing model
forecast_tbl <- refit_tbl %>%
  filter(.model_id==1)%>%
  modeltime_forecast(
    h = "9 year",
    actual_data = set5,
    conf_interval = 0.95
  ) 

#plot 9 year forecast
forecast_tbl %>%
  plot_modeltime_forecast(.interactive = TRUE, .conf_interval_show = TRUE, .plotly_slider = TRUE)


