# dataset doesn't exist here so the code won't work

# parse columns to be the data type of the data
# there are parse functions for logical, number, character, factor, and date

# Load readr and dplyr
library(readr)
library(dplyr)

# Find format to parse "17 August 2010" 
parse_date("17 August 2010", format = "%d %B %Y")

# Edit to cast uk_airdate
desserts <- read_csv("desserts.csv", col_types = cols(uk_airdate = col_date(format = "%d %B %Y")))

# Arrange by descending uk_airdate
desserts %>% 
  arrange(desc(uk_airdate))

# Using problems() on a result of read_csv() will show you the rows and columns where parsing error occurred, what the parser expected to find (for example, a number), and the actual value that caused the parsing error.

# Try to cast technical as a number
desserts <- read_csv("desserts.csv", 
                    col_types = cols(uk_airdate = col_date(format = "%d %B %Y"),
                    technical = col_number()))
# console gives warning message with info about what went wrong

# View parsing problems
problems(desserts)

# Edit code to fix the parsing error 
desserts <- read_csv("desserts.csv",
                    col_types = cols(uk_airdate = col_date(format = "%d %B %Y"),
                    technical = col_number()),
                    na = c("", "NA", "N/A"))

# View parsing problems
problems(desserts)  # now no problems are listed


# Factors are categorical variables, where the possible values are a fixed and known set. 
# You can use parse_factor() to parse variables and col_factor() to cast columns as categorical. Both functions have a levels argument that is used to specify the possible values for the factors. 
# When levels is set to NULL, the possible values will be inferred from the unique values in the dataset. Alternatively, you can pass a list of possible values.

# Cast result a factor
desserts <- read_csv("desserts.csv", 
                     na = c("", "NA", "N/A"),
                     col_types = cols(
                       uk_airdate = col_date(format = "%d %B %Y"),
                       technical = col_number(),                       
                       result = col_factor(levels = NULL)))

# Glimpse to view
glimpse(desserts)


# Recoding values is like find-and-replace; recode() is in the dplyr package; recode() is best used inside mutate()
# recode(variable_name, old_value = new_value)  separate each recode pair with a comma inside recode()
# it's important to preserve the data type of the variable; R has built-in words to indicate missing data of various types, for example 
#    NA_character_ for missing value for a character variable, or NA_integer_ for missing integer data

# Count rows grouping by nut variable
desserts %>% 
  count(nut, sort = TRUE)

# Recode filberts as hazelnuts
desserts_2 <- desserts %>% 
  mutate(nut = recode(nut, "filbert" = "hazelnut"))

# Count rows again
desserts_2 %>% 
  count(nut, sort = TRUE)

# Edit code to recode "no nut" as missing
desserts_2 <- desserts %>% 
  mutate(nut = recode(nut, "filbert" = "hazelnut", 
                      "no nut" = NA_character_))

# Count rows again
desserts_2 %>% 
  count(nut, sort = TRUE)

# Create dummy variable: 1 if won, 0 if not
desserts <- desserts %>% 
  mutate(tech_win = recode(technical, `1` = 1,
                           .default = 0))

# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)

# Edit to recode tech_win as factor
desserts <- desserts %>% 
  mutate(tech_win = recode_factor(technical, `1` = 1,
                                  .default = 0))

# Count to compare values                      
desserts %>% 
  count(technical == 1, tech_win)


# filter() subsets your rows/observations
# select() subsets your columns/variables (dplyr package)
# select(dataset, var1, var2, var3) or select(dataset, var1:var3)
# drop column: dataset %>% select(-var1)
# starts_with() helper: select(dataset, starts_with("a")) - there's also ends_with() and contains()

# Recode channel as factor: "Channel 4" (0) or not (1)
ratings <- ratings %>% 
  mutate(bbc = recode_factor(channel, 
                             "Channel 4" = 0,
                             .default = 1))

# Select to look at variables to plot next
ratings %>%
  select(series, channel, bbc, viewer_growth)

library(ggplot2)

# Make a filled bar chart
ggplot(ratings, aes(x = series, y = viewer_growth, fill = bbc)) +
  geom_col()

# Move channel to first column
ratings %>% 
  select(channel, everything())

# Drop 7- and 28-day episode ratings
ratings %>% 
  select(-ends_with("day"))

# Move channel to front and drop 7-/28-day episode ratings
ratings %>% 
  select(channel, everything(), -ends_with("day"))


# using select() to rename variables
# select(dataset, new_name = old_name) - select helpers work here
# OR USE
# dataset %>% rename(new_name = old_name) - select helpers DON'T work here

# clean_names() to reformat all variable names in a dataset (janitor package)
# converts all variable names to snake_case by default; can insert other arguments like "upper_camel" or "lower_camel" or "all_caps"

# Glimpse to see variable names
glimpse(messy_ratings)

# Load janitor
library(janitor)

# Reformat to lower camelcase
ratings <- messy_ratings %>%
  clean_names("lower_camel")

# Glimpse new tibble
ratings

# Reformat to snake case
ratings <- messy_ratings %>%  
  clean_names()

# Glimpse cleaned names
glimpse(ratings)

# Select 7-day viewer data by series
viewers_7day <- ratings %>% 
  select(series, ends_with("7day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to also rename 7-day viewer data
viewers_7day <- ratings %>% 
  select(series, viewers_7day_ = ends_with("7day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to drop 28-day columns; keep 7-day in front
viewers_7day <- ratings %>% 
  select(viewers_7day_ = ends_with("7day"), 
         everything(), 
         -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)

# Adapt code to keep original order
viewers_7day <- ratings %>% 
  select(everything(), 
         viewers_7day_ = ends_with("7day"), 
         -ends_with("28day"))

# Glimpse
glimpse(viewers_7day)