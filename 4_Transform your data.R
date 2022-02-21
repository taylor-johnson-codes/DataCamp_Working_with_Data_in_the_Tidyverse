# dataset doesn't exist here so the code won't work

# case_when() for more complex recoding, more than recode() - dplyr package
# think of case_when() like a sequence of if/else pairs
# if part on left side; must be a logical (phrased in a way that the answer is TRUE/FALSE)
# if the statement is TRUE, then the value on the right of ~ is used as the replacement value
# else statement: TRUE ~ then do this; default value for else is NA

library(dplyr)

# Create skills variable with 3 levels
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    TRUE ~ "well_rounded"
  ))

# Filter zeroes to examine skill variable
bakers_skill %>% 
  filter(star_baker == 0 & technical_winner == 0) %>% 
  count(skill)

# Edit skill variable to have 4 levels
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  ))

# drop_na() - tidyr package
library(tidyr)

# Add pipe to drop skill = NA
bakers_skill <- bakers %>% 
  mutate(skill = case_when(
    star_baker > technical_winner ~ "super_star",
    star_baker < technical_winner ~ "high_tech",
    star_baker == 0 & technical_winner == 0 ~ NA_character_,
    star_baker == technical_winner  ~ "well_rounded"
  )) %>% 
  drop_na(skill)

# Count bakers by skill
bakers_skill %>% 
  count(skill)


# forcats package is for working with categorical variables (factors); where variables have a fixed and known set of possible values
# pull() - dplyr package

library(forcats)

# Cast skill as a factor
bakers <- bakers %>% 
  mutate(skill = as.factor(skill))

# Examine levels
bakers %>% 
  pull(skill) %>% 
  levels()

# Plot counts of bakers by skill, fill by winner
ggplot(bakers, aes(x = skill, fill = series_winner)) +
  geom_bar()

# Edit to reverse x-axis order
ggplot(bakers, aes(x = fct_rev(skill), fill = series_winner)) +
  geom_bar()


# lubridate package makes it easier to work with dates (parsing and casting dates)
# parse dates: dmy(), mdy(), ymd()
# calculate difference between two dates: interval(start_date, end_date)

library(lubridate)

# Cast last_date_appeared_us as a date
baker_dates_cast <- baker_dates %>%
  mutate(last_date_appeared_us = dmy(last_date_appeared_us))

# Add a line to extract labeled month
baker_dates_cast <- baker_dates %>% 
  mutate(last_date_appeared_us = dmy(last_date_appeared_us),
         last_month_us = month(last_date_appeared_us, label = TRUE))

library(ggplot2)

# Make bar chart by last month
ggplot(baker_dates_cast, aes(x = last_month_us)) + 
  geom_bar()

# Create interval between first and last UK dates
baker_time <- baker_time %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk))

# Add a line to create weeks on air variable
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1))

# Add a line to create whole months on air variable
baker_time <- baker_time  %>% 
  mutate(time_on_air = interval(first_date_appeared_uk, last_date_appeared_uk),
         weeks_on_air = time_on_air / weeks(1),
         months_on_air = time_on_air %/% months(1))


# stringr package to make it easier to work with strings; usually used in mutate()
# str_to_upper(column_name), str_to_lower(column_name)
# str_detect(column_name, "str_you're_looking_for") returns TRUE/FALSE
# str_replace(column_name, "existing_str", "replacement_str")
# str_remove(column_name, "str_to_remove")
# str_trim(column_name) trims whitespace from beginning or end of a string

# separate() to turn one column into two columns - tidyr package
# parse_number() - readr package

library(stringr)
library(readr)

# Convert to upper case
bakers <- bakers %>% 
  mutate(position_reached = str_to_upper(position_reached))

# Add another mutate to replace "-" with " "
bakers <- bakers %>% 
  mutate(position_reached = str_to_upper(position_reached),
         position_reached = str_replace(position_reached, "-", " "))

# Add another mutate to replace "THIRD PLACE" with "RUNNER UP"and count
bakers <- bakers %>% 
  mutate(position_reached = str_to_upper(position_reached),
         position_reached = str_replace(position_reached, "-", " "),
         position_reached = str_replace(position_reached, "THIRD PLACE", "RUNNER UP"))

# Count rows
bakers %>%
  count(position_reached)

# Convert to lower case
bakers <- bakers %>%
  mutate(occupation = str_to_lower(occupation))

# Add a line to create new variable called student
bakers <- bakers %>% 
  mutate(occupation = str_to_lower(occupation), 
         student = str_detect(occupation, "student"))

# Find all students and examine occupations
bakers %>% 
  filter(student == TRUE) %>% 
  select(baker, occupation, student)