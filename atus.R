library(tidyverse)

# Data: https://www.bls.gov/tus/datafiles_2016.htm

  
respondent <- read_csv("data/atusresp_2016.dat", na = "-1")
activity <- read_csv("data/atussum_2016.dat", na = "-1")

names(respondent) <- tolower(names(respondent))
names(activity) <- tolower(names(activity))

atus <- left_join(respondent, activity, by = "tucaseid")
