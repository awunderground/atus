# Load packages and source files

library(tidyverse)
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_mac.R')


# Load Data (source: https://www.bls.gov/tus/datafiles_2016.htm)

respondent <- read_csv("data/atusresp_2016.dat", na = "-1")
activity <- read_csv("data/atussum_2016.dat", na = "-1")

# Convert variable names to lower case
names(respondent) <- tolower(names(respondent))
names(activity) <- tolower(names(activity))

# Merge 
atus <- left_join(respondent, activity, by = "tucaseid")

# Check to see if any observations don't have a match
dont_match <- anti_join(respondent, activity, by = "tucaseid")

# Show summary of every variable
glimpse(atus)

# Shows first six rows of data
head(atus)

# One way to select a vector
atus$teage

# Tidyverse method of selecting variables
small_atus <- select(atus, teage)

# Select first ten variables and filter rows by complete cases
small_atus <- atus %>%
	select(1:10) %>%
	filter(complete.cases(atus))

# Count the number of observations with no missing values
table(complete.cases(atus))

# Select 
television <- atus %>%
	select(tucaseid, teage, tesex, tv = t120303, religious_tv = t120304)

# Summarize mean amount of time watch television
television %>%
	summarize(mean(tv), mean(religious_tv))

# Summarize mean amount of time watching television by age range
television %>%
	mutate(total_tv = tv + religious_tv) %>%
	mutate(age_range = cut(teage, breaks = c(10, 20, 30, 40, 50, 60, 70, 100))) %>%
	group_by(age_range) %>%
	summarize(mean(total_tv))

# Summarize mean amount of time watching television by age
tv_age <- television %>%
	mutate(total_tv = tv + religious_tv) %>%
	group_by(teage) %>%
	summarize(mean(total_tv))

# Plot television watching by age with a layer for mean television time by age
television %>%
	mutate(total_tv = tv + religious_tv) %>%
	ggplot(aes(x = teage, total_tv)) +
		geom_point(alpha = 0.1) +
		geom_line(data = tv_age, aes(teage, `mean(total_tv)`), color = "red")