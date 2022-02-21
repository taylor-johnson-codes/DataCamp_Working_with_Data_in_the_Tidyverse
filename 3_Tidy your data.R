# dataset doesn't exist here so the code won't work

# tidy data tends to be long and more repetitive; untidy data is wider
# tidy data has one column for each variable and one row per observation

library(ggplot2)

# Plot of episode 1 viewers by series
ggplot(ratings, aes(series, e1)) + 
  geom_col()

# Adapt code to plot episode 2 viewers by series
ggplot(ratings, aes(x = series, y = e2)) +
  geom_col()


# gather() collapses multiple columns into two columns; reshaping data from wide to long (tidyr package)
# a tool for tidying messy columns
# dataset %>% gather(key = "column_you_want", value = "column_you_want", columns_you_have)
# row_number() and select() - dplyr package 

library(tidyr)
library(dplyr)

tidy_ratings <- ratings %>%
  # Gather and convert episode to factor
  gather(key = "episode", value = "viewers_7day", -series, 
         factor_key = TRUE, na.rm = TRUE) %>% 
        # Sort in ascending order by series and episode
        arrange(series, episode)  %>% 
        # Create new variable using row_number()
        mutate(episode_count = row_number())

# Plot viewers by episode and series
ggplot(tidy_ratings, aes(x = episode_count, y = viewers_7day, fill = series)) +
    geom_col()

# Select 7-day viewer ratings
week_ratings <- ratings2 %>% 
  select(series, ends_with("7day"))  %>% 
  # Gather 7-day viewers by episode
  gather(episode, viewers_7day, ends_with("7day"), na.rm = TRUE, factor_key = TRUE)

# Plot 7-day viewers by episode and series
ggplot(week_ratings, aes(x = episode, y = viewers_7day, group = series)) +
  geom_line() +
  facet_wrap(~series)


# separate() to make one column into two (tidyr package)
# dataset %>% separate(col = column_name_to_separate, into = c("new_col_name1", "new_col_name2"))
# add argument convert = TRUE to convert new columns to the correct data type
# parse_number() - readr package

library(readr)

week_ratings <- ratings2 %>% 
  select(series, ends_with("7day")) %>% 
  gather(episode, viewers_7day, ends_with("7day"), na.rm = TRUE) %>% 
  # Edit to separate key column and drop extra
  separate(episode, into = "episode", extra = "drop")  %>%
  # Edit to parse episode number
  mutate(episode = parse_number(episode))

# Print to view
week_ratings

# Edit your code to color by series and add a theme
ggplot(week_ratings, aes(x = episode, y = viewers_7day, group = series, color = series)) +
  geom_line() +
  facet_wrap(~series) +
  guides(color = FALSE) +
  theme_minimal()


# unite() is the opposite of separate() - tidyr package

ratings3 <- ratings2  %>% 
  # Unite viewers in millions and decimals together		
  unite(viewers_7day, viewers_millions, viewers_decimal)

# Print to view
ratings3

ratings3 <- ratings2  %>% 
  # Adapt to change the separator
  unite(viewers_7day, viewers_millions, viewers_decimal, sep = "") %>% 
  # Adapt to cast viewers as a number
  mutate(viewers_7day = parse_number(viewers_7day))

# Print to view
ratings3


# spread() is the opposite of gather(); reshapes data from long to wide; a tool for tidying messy rows (tidyr package)
# dataset %>% spread(key = key, value = value
# add argument convert = TRUE to convert new columns to the correct data type

# Create tidy data with 7- and 28-day viewers
tidy_ratings_all <- ratings2 %>%
  gather(key = episode, value = viewers, ends_with("day"), na.rm = TRUE) %>% 
  separate(episode, into = c("episode", "days")) %>%  
  mutate(episode = parse_number(episode), days = parse_number(days))


tidy_ratings_all %>% 
  # Count viewers by series and days
  count(series, days, wt = viewers)  %>%
  # Adapt to spread counted values
  spread(days, n, sep = "_")


# tidy multiple sets of columns
# step 1: gather all columns in to one column (data is temporarily less tidy)
# dataset %>% gather(key="key", value="value", col_x:col_z)
# step 2: separate the new key column into two new columns
# ... %>% separate(key, into = c("col1", "col2"), convert = TRUE)
# step 3: spread the value column
# ... %>% spread(col1, value)

# Gather viewer columns and remove NA rows
tidy_ratings <- ratings %>%
  gather(key = episode, value = viewers, -series, na.rm = TRUE)

# Adapt code to parse episode as a number
tidy_ratings <- ratings %>%
  gather(episode, viewers, -series, na.rm = TRUE) %>%  
  mutate(episode = parse_number(episode))

# Fill in blanks to get premiere/finale data
tidy_ratings <- ratings %>%
  gather(episode, viewers, -series, na.rm = TRUE) %>%
  mutate(episode = parse_number(episode)) %>% 
  group_by(series) %>% 
  filter(episode == 1 | episode == max(episode)) %>% 
  ungroup()

# Recode first/last episodes
first_last <- tidy_ratings %>% 
  mutate(episode = recode(episode, `1` = "first", .default = "last"))

# Fill in to make slope chart
ggplot(first_last, aes(x = episode, y = viewers, color = series)) +
  geom_point() +
  geom_line(aes(group = series))

# Switch the variables mapping x-axis and color
ggplot(first_last, aes(x = series, y = viewers, color = episode)) +
  geom_point() + 
  geom_line(aes(group = series)) + 
  coord_flip() 

# Spread into three columns
bump_by_series <- first_last %>% 
  spread(episode, viewers)

# Calculate relative increase in viewers
bump_by_series <- first_last %>% 
  spread(episode, viewers) %>% 
  mutate(bump = (last - first) / first)

# Fill in to make bar chart of bumps by series
ggplot(bump_by_series, aes(x = series, y = bump)) +
  geom_col() + 
  scale_y_continuous(labels = scales::percent) # converts to %