data(arbuthnot)

ggplot(data = arbuthnot, aes(x = year, y = girls)) + geom_point()

ggplot(data = arbuthnot, aes(x = year, y = boys)) + geom_point()

ggplot(data = arbuthnot, aes(x = year, y = boys)) + geom_line()

# Initialize new data frame
total = data.frame(sum = 1 : nrow(arbuthnot), year = 1 : nrow(arbuthnot))
total$sum = arbuthnot$boys + arbuthnot$girls
total$year = arbuthnot$year
ggplot(data = total, aes(x = year, y = sum)) + geom_line()

# Another way to do this: 
# arbuthnot <- arbuthnot %>%
#   mutate(total = boys + girls)

ggplot(data = total, aes(x = year, y = sum)) + 
  geom_line() + 
  geom_point()

# Next dataset

data(present)
present <- present %>%
  mutate(total = boys + girls, 
         prop_boys = boys / (boys + girls),
         more_boys = boys > girls,
         prop_boy_girl = boys / girls
  )

ggplot(data = present, aes(x = year, y = total)) + 
  geom_line() + 
  geom_point()

ggplot(data = present, aes(x = year, y = prop_boys)) + 
  geom_line() + 
  geom_point()

ggplot(data = present, aes(x = year, y = more_boys)) + 
  geom_line() + 
  geom_point()

ggplot(data = present, aes(x = year, y = prop_boy_girl)) + 
  geom_line() + 
  geom_point()



