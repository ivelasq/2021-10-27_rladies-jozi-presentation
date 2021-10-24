# Data
source("R/data_fake.R")

# Map for presentation ----------------------------------------------------

set.seed(2021)

# Create a random sequence of dates and times
x <- seq(as.POSIXct('2021/10/27'), as.POSIXct('2021/10/28'), by = "15 mins") # create random sequence

# Create dataset for plot

df_routes_jozi <-
  df_routes_def %>% 
  filter(src == 'JNB') %>% 
  mutate(depart_time = as.factor(as_hms(sample(x[hour(x) > "09:00" & hour(x) < "17:00"],
                                               n(),
                                               replace = TRUE))),
         gate = as.factor(sample(1:20, n(), replace = TRUE)))

ggplot(df_routes_jozi) +
  worldmap +
  coord_map("gilbert", xlim = c(-180,180)) +
  geom_segment(aes(x = long_src,
                   y = lat_src,
                   xend = long_dest,
                   yend = lat_dest,
                   color = depart_time),
               alpha = 0.3) +
  geom_point(aes(x = long_dest,
                 y = lat_dest,
                 fill = gate)) +
  theme_map() +
  ggtitle('O.R. Tambo International Airport connections') +
  theme(legend.position = 'bottom')
