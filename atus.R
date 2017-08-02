# Exploratory Data Analysis of the American Time Use Survey

# Load packages and source files
library(tidyverse)
source('https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_mac.R')

# Load Data (source: https://www.bls.gov/tus/datafiles_2016.htm)
respondent <- read_csv("data/atusresp_2016.dat", na = "-1")
activity <- read_csv("data/atussum_2016.dat", na = "-1")

# Convert variable names to lower case
names(respondent) <- tolower(names(respondent))
names(activity) <- tolower(names(activity))

# Merge activity to respondent by tucaseid
atus <- left_join(respondent, activity, by = "tucaseid")

# Check to see if any observations don't have a match
dont_match <- anti_join(respondent, activity, by = "tucaseid")

# Show summary of every variable
glimpse(atus)

# Shows first six rows of data
head(atus)

# Three ways to select a vector by name
atus$teage               # returns a vector
atus[["teage"]]          # returns a vector
select(atus, teage)      # returns a tibble
atus[, "teage"]          # returns a tibble

# Two ways to select a variable by position
atus[, 1]                # returns a tibble
select(atus, 1)          # returns a tibble

# Tidyverse method of selecting variables (my preference)
small_atus <- select(atus, teage)

# Select first ten variables and filter rows by complete cases
small_atus <- atus %>%
	select(1:10) %>%
	filter(complete.cases(atus))

# complete.cases()
complete.cases(atus)

# Count the number of observations with no missing values
table(complete.cases(atus))    # zero!

# Subsetting a vector with a vector of booleans
boom <- c(1, 2, 3, NA, 4, NA, 6)
is.na(boom)
boom[!is.na(boom)]

df <- tibble(a = c(1, 2, 3),
			 b = c(4, NA, 6),
			 c = boom[1:3])

df[complete.cases(df), ]

# Select interesting variables
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
	ggplot(mapping = aes(x = teage, y = total_tv)) +
		geom_point(alpha = 0.1) +
		geom_line(data = tv_age, aes(teage, `mean(total_tv)`), color = "red")

# Create a data frame for Lea Bart's question
leas_data <- tibble(survey = c(1, 1, 1, 2, 2, 2),
										values = c(10, 12, 25, 8, 13, 15),
										class = c("a", "b", "c", "a", "b", "c"),
										gender = c("male", "male", "male", "female", "female", "female"))

# First way to build a bar plot with two subgroups (fill = )
leas_data %>%
	mutate(survey = factor(survey, levels = c(1, 2))) %>%
	ggplot(mapping = aes(x = factor(class), y = values, fill = survey)) +
		geom_bar(stat = "identity", position = "dodge") +
		scale_y_continuous(expand = c(0, 0), limits = c(0, 35)) +
		labs(title = "Lea's Data",
				 subtitle = "For R Lunch Lab 2.0",
				 x = "Class",
				 y = "Value",
				 caption = "Urban Institute")

# Second way to build a bar plot with two subgroups (facet_wrap())
leas_data %>%
	mutate(survey = factor(survey, levels = c(1, 2), labels = c("Men", "Women"))) %>%
	ggplot(mapping = aes(x = factor(class), y = values)) +
	geom_bar(stat = "identity", position = "dodge") +
	scale_y_continuous(expand = c(0, 0), limits = c(0, 35)) +
	labs(title = "Lea's Data",
			 subtitle = "For R Lunch Lab 2.0",
			 x = "Class",
			 y = "Value",
			 caption = "Urban Institute") +
	facet_wrap(~survey)

leas_data %>%
	mutate(
		gender = ifelse(gender == "male", "Male", gender),
		gender = ifelse(gender == "female", "Female", gender)
		)

## MORE PLOTS!
## IPUMS ATUS Variable List (https://www.atusdata.org/atus-action/variables/ACTIVITY#codes_section)

# sleep: 				t010101
# tv:						t120303 
# religious_tv: t120304

months <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

atus_small <- atus %>%
	select(teage, tv = t120303, sleep = t010101, month = tumonth) %>%
	mutate(agerange = cut(teage, breaks = c(14, 65, 85)),
				 agerange = factor(agerange, labels = c("15 to 64", "65+"))) %>%
	mutate(month = factor(month,  labels = months))

atus_small %>%
	group_by(agerange, month) %>%
	summarize(tv = mean(tv)) %>%
	ggplot(aes(agerange, tv, fill = agerange)) +
		geom_bar(stat = "identity") +
		scale_y_continuous(expand = c(0, 0), limits = c(0, 300)) +
		facet_wrap(~month) +
	labs(title = "Graham's Plot",
			 subtitle = "Mean Time Watching Television by Age Range and Month",
			 caption = "Urban Institute")