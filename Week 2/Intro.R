library(statsr)
library(dplyr)
library(ggplot2)

data(nycflights)
names(nycflights)

# View structure of data
str(nycflights)

# Visualize departure delay
ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram()

ggplot(data = nycflights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)

rdu_flights <- nycflights %>%
  filter(dest == "RDU")
ggplot(data = rdu_flights, aes(x = dep_delay)) +
  geom_histogram(binwidth = 15)

# Summarize
rdu_flights %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

# Question 1
# Filter flights headed to San Francisco (SFO) in February:
sfo_feb_flights <- nycflights %>%
  filter(dest == "SFO", month == 2)
print(nrow(sfo_feb_flights))
# end

# Question 2
ggplot(data = sfo_feb_flights, aes(x = arr_delay)) +
  geom_histogram(binwidth = 5)
# Print number flights arriving on time or earlier than scheduled.
print(sum(sfo_feb_flights$arr_delay <= 0, na.rm = TRUE)) 
# end

rdu_flights %>%
  group_by(origin) %>%
  summarise(mean_dd = mean(dep_delay), sd_dd = sd(dep_delay), n = n())

# Question 3
sfo_feb_flights %>%
  group_by(carrier) %>%
  summarise(median = median(arr_delay), IQR = IQR(arr_delay))
# end

# Question 4
avg_dep_delay = nycflights %>%
  group_by(month) %>%
  summarise(mean_dd = mean(dep_delay)) %>%
  arrange(desc(mean_dd))

print(avg_dep_delay$month[1])
# end

# Question 5
median_dep_delay = nycflights %>%
  group_by(month) %>%
  summarise(median_dd = median(dep_delay)) %>%
  arrange(desc(median_dd))

print(median_dep_delay$month[1])
# end

ggplot(nycflights, aes(x = factor(month), y = dep_delay)) +
  geom_boxplot()

# Question 7

nycflights <- nycflights %>%
  mutate(dep_type = ifelse(dep_delay < 5, "on time", "delayed"))

on_time_dep_rate = nycflights %>%
  group_by(origin) %>%
  summarise(on_time_dep_rate = sum(dep_type == "on time") / n()) %>%
  arrange(desc(on_time_dep_rate))

print(on_time_dep_rate$origin[1])

# end

# Question 8

# avg_speed = nycflights %>%
#   group_by(tailnum) %>% 
#   summarise(sum_dis = sum(distance), sum_air_time = sum(air_time)) %>%
#   group_by(tailnum) %>% 
#   summarise(mean_speed = sum_dis / sum_air_time) %>%
#   arrange(desc(mean_speed))

avg_speed <- nycflights %>%
 mutate(avg_speed = distance / (air_time / 60)) %>%
  select(avg_speed, tailnum) %>%
    arrange(desc(avg_speed))

print(avg_speed$tailnum[1])

# end

# Question 9
nycflights = nycflights %>%
  mutate(avg_speed = distance / (air_time / 60))

ggplot(data = nycflights, aes(x = distance, y = avg_speed)) +
  geom_point()
# end

# Question 10
nycflights <- nycflights %>%
  mutate(arr_type = ifelse(arr_delay <= 0, "on time", "delayed"))

on_time_rate = nycflights %>% 
  group_by(dep_type) %>%
  summarise(on_time_rate = sum(arr_type == "on time") / n())

print(on_time_rate)

# end