library(dplyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(knitr)

power = read.csv("PowerCurve.csv")
site_df = read.csv("SiteData.csv")
wind = read.csv("WindData.csv")
energy = read.csv("SiteEnergy.csv")

#wind data clean
year = as.numeric(substr(wind$Time,1,4))
year_filter = (year >= 2007 & year < 2016) & (!str_detect(wind$Time, "2015-1"))
wind_clean = wind[year_filter,] %>% 
                      mutate(Month = str_extract(Time, "\\d{4}-\\d{2}")) %>% 
                      group_by(Month) %>% 
                      summarise(avg.windspeed = mean(NW_spd_10m),
                                avg.winddir = mean(NW_dir_10m)) %>% 
                      merge(y = energy, by = "Month") %>% 
                      select(-Capacity.MW) %>% 
                      filter(!str_detect(.$Month,"2007-0"))

assign_power = function(x) {
  y = c()
  wind_speed = power$WindSpeed
  energy_pred = power$Power.kW
  
  for(i in seq_along(wind_speed)) {
    
    if (x > wind_speed[i] & x < wind_speed[i + 1]){
       
      y = mean(c(energy_pred[i], energy_pred[i+1]))}
    
    else if (x == wind_speed[i]) y = energy_pred[i]
    else if (x < 3.5 | x > 25)   y = 0
    }
  return(y)
}

energy.pred = sapply(wind_clean$avg.windspeed, assign_power) * 134 * 30 * 24/1000

df = data.frame(time = wind_clean$Month,
                true = wind_clean$Energy.MWh,
                pred = energy.pred)
df.long = melt(df, value.name = "value") %>% 
                              mutate(time = as.character(time))

ggplot(df.long, aes(x = time, y = value, group = 1, col = variable)) +
  geom_line(size = 1) +
  scale_x_discrete(breaks = unique(df.long$time)[seq(1,210,12)]) +
  theme(axis.text.x  = element_text(angle=60, vjust=0.5)) + 
  labs(title="Precited Vs Actual Power Produce",
       x ="Date", y = "Energy.MWh", col = "value") + 
  theme(plot.title = element_text(hjust = 0.5))

#regression
set.seed(123)
train_index = sample(1:nrow(wind_clean), 0.5*nrow(wind_clean))
train = wind_clean[train_index,]
test = wind_clean[-train_index,]
mod1 = lm(Energy.MWh ~ avg.windspeed + I(avg.windspeed^2) + I(avg.windspeed^3) + avg.winddir,
   data = train)

pred.lm = predict(mod1, newdata = test)
pred.pow = energy.pred[-train_index]

rmse = function(prediction) {
  rmse = sqrt(mean((test$Energy.MWh - prediction)^2))
  return(rmse)
}

df2 = data.frame(rmse(pred.lm),rmse(pred.pow))
colnames(df2) = c("linear_model", "power_curve")
kable(df2)

df3 = data.frame(index = 1:nrow(test),
                 linear_model = test$Energy.MWh - pred.lm,
                 power_curve = test$Energy.MWh - pred.pow)
df3.long = melt(df3, id = 'index', value.name = "value")

ggplot(df3.long, aes(x = index, y = value, col = variable)) +
  geom_line(size = 1) +
  labs(title="Residual for Linear Model Vs Power Curve Prediction",
       x ="Test set Sample", y = "Residual", col = "value") + 
  theme(plot.title = element_text(hjust = 0.5))
