# dataset doesn't exist here so the code won't work

# readr package is for reading rectangular data (rows & columns) into R
# csv (comma separated values) is a rectangular data file
# ?read_csv for more info
# read_csv(): file argument (path to file in "") is required
# if data file and script file are in the same directory, R will look for the named data file in that directory

# Load readr
library(readr)

# Create bakeoff from "bakeoff.csv"
bakeoff <- read_csv("bakeoff.csv")

# Print bakeoff
bakeoff

# Create bakeoff but skip first row
bakeoff <- read_csv("bakeoff.csv", skip=1)

# Print bakeoff
bakeoff

# Load dplyr
library(dplyr)

# Filter rows where showstopper is UNKNOWN 
bakeoff %>% 
  filter(showstopper == "UNKNOWN")

# Edit to add list of missing values
bakeoff <- read_csv("bakeoff.csv", skip = 1,
                    na = c("", "NA", "UNKNOWN"))

# Filter rows where showstopper is NA 
bakeoff %>%
  filter(is.na(showstopper))


# glimpse() to be able to see all variables in the dataset (dplyr package)

# On which date did the first episode of the show air in the US?
bakeoff %>% 
  arrange(us_airdate) %>% 
  glimpse()


# skim() to see some stats about the variables like min, max, missing, empty, etc. (skimr package)
    # note: min and max on char values refers to the length of the char

# Load skimr
library(skimr)

# Edit to filter, group by, and skim
bakeoff %>% 
  filter(!is.na(us_season)) %>% 
  group_by(us_season) %>% 
  skim()

# How many variables of each type do we have in the bakeoff data? 
bakeoff %>% 
  skim() %>%
  summary()


# Counting values within variables
# distinct() for how many distinct values in a variable there are (dplyr package)
# count() adds a new column to store the count (dplyr package); gives the same results as using group_by()...summarize()...ungroup()
# count(var1, var2, var3) separate multiple variables with a comma

# View distinct results
bakeoff %>%
  distinct(result)
  
# Count rows for each result
bakeoff %>% 
  count(result)

# Count whether or not star baker
bakeoff %>% 
  count(result == "SB")

# Count the number of rows by series and episode
bakeoff %>%
  count(series, episode)

# Add second count by series
bakeoff %>% 
  count(series, episode) %>%
  count(series)

# Count the number of rows by series and baker
bakers_by_series <- bakeoff %>%
  count(series, baker)

# Print to view
bakers_by_series

# Count again by series
bakers_by_series %>%
  count(series)

# Count again by baker
bakers_by_series %>%
  count(baker, sort = TRUE)

library(ggplot2)

# Make a bar chart using geom_bar to plot the number of bakers from bakeoff per episode along the x-axis. Use facet_wrap by series.
ggplot(bakeoff, aes(episode)) + 
  geom_bar() + 
  facet_wrap(~series)