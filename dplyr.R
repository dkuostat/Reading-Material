library(nycflights13)
library(dplyr)
library(ggplot2)

head(flights)

### Filter rows ###
# Subsequent arguments to df are expressions that filter df
filter(flights, month == 1, day == 1)
filter(flights, month == 1 | month == 2)

### Arrange rows ###
# Reorder by variables
arrange(flights, year, month, day)
arrange(flights, desc(arr_delay))

### Select columns ###
# Name variables to keep
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

### Rename columns ###
select(flights, tail_num = tailnum) # Drop unmentioned columns
rename(flights, tail_num = tailnum) # Keep unmentioned columns

### Extract distinct rows ###
# By distinct combinations of named columns
distinct(flights, tailnum)
distinct(flights, origin, dest)

### Add new columns ###
# As function of existing columns
mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
mutate(flights,
       gain = arr_delay - dep_delay,           # "gain" is a new column
       gain_per_hour = gain / (air_time / 60)) # Reference new columns!
transmute(flights,                             # To only keep new vars
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60))

### Summarise values ###
# used with aggregate functions (vector => number)
# min(), max(), mean(), sum(), sd(), median(), IQR()
# dplyr-unique: n(): n_distinct(x), first(x), last(x), nth(x, n)
summarise(flights,
          delay = mean(dep_delay, na.rm = TRUE))

### Randomly sample rows ###
sample_n(flights, 10)      # fixed number
sample_frac(flights, 0.01) # fixed fraction
# To bootstrap, use replace = T

### Grouping ###
# grouped select() = ungrouped select(), except grouping vars are retained
# grouped arrange() orders first by grouping vars
# mutate() and filter() described in vignette("window-functions")
# sample_n() and sample_frac() sample specified rows in each group
# slice() extracts rows within each group
# summarise() will be shown:

# Split the complete dataset into individual planes and
# summarise each plane by counting:
#   number of flights (count = n())
#   computing the average distance (dist = mean(Distance, na.rm = TRUE)) 
#   arrival delay (delay = mean(ArrDelay, na.rm = TRUE))

by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,
                   count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)

ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

### CHAINING ###
# cont
