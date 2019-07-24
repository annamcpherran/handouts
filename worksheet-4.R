## Tidy Concept

trial <- read.delim(sep = ',', header = TRUE, text = "
block, drug, control, placebo
    1, 0.22,    0.58,    0.31
    2, 0.12,    0.98,    0.47
    3, 0.42,    0.19,    0.40
")

## Gather 

library(tidyr)
tidy_trial <- gather(trial,
  key = "treatment",
  value = "response",
  -block)

## Spread 

survey <- read.delim(sep = ',', header = TRUE, text = "
participant,   attr, val
1          ,    age,  24
2          ,    age,  57
3          ,    age,  13
1          , income,  30
2          , income,  60
")

tidy_survey <- spread(survey,
  key = attr,
  value = val)

tidy_survey <- spread(survey,
  key = attr,
  value = val,
  fill = 0)

## Sample Data 

library(data.table)
cbp <- fread('data/cbp15co.csv')

str(cbp)

cbp <- fread(
  'data/cbp15co.csv',
  na.string = NULL,
  colClasses = c(
    FIPSTATE='character',
    FIPSCTY='character')
  )

str(cbp)

acs <- fread(
  'data/ACS/sector_ACS_15_5YR_S2413.csv',
  colClasses = c(FIPS = 'character'))

str(acs)

## dplyr Functions 

library(dplyr)
cbp2 <- filter(cbp,
  grepl('----', NAICS),
  !grepl('------', NAICS))

library(stringr)
cbp2 <- filter(cbp,
  str_detect(NAICS, '[0-9]{2}----'))

cbp3 <- mutate(cbp2,
  FIPS = str_c(FIPSTATE, FIPSCTY))

cbp3 <- mutate(cbp2,
               FIPS = str_c(FIPSTATE, FIPSCTY),
               NAICS = str_remove(NAICS, '-+'))

cbp <- cbp %>%
  filter(
    str_detect(NAICS, '[0-9]{2}----')
  ) %>%
  mutate(
    FIPS = str_c(FIPSTATE, FIPSCTY),
    NAICS = str_remove(NAICS, '-+')
  )

cbp <- cbp %>%
  select(
    FIPS,
    NAICS,
    starts_with('N')
  )

## Join

sector <- fread(
  'data/ACS/sector_naics.csv',
  colClasses = c(NAICS = 'character'))

cbp <- cbp %>%
  inner_join(sector)

## Group By 

cbp_grouped <- cbp %>%
  group_by(FIPS, Sector)

## Summarize 

cbp <- cbp %>%
  group_by(FIPS, Sector) %>%
  select(starts_with('N'), -NAICS) %>%
  summarize_all(sum)

acs_cbp <- cbp %>%
  inner_join(acs)


######################################
##Anna McPherran
##July 23, 2019
##Lesson 4 exercises

##Exercise 1
##Now that we have a tidy form of survey, convert it to a long_survey data frame using gather. 
##The only difference between survey and long_survey should be an additional row for zero income.
library(tidyr)
long_survey <- gather(tidy_survey,
                     key = "attr",
                     value = "val",
                     -participant)
long_survey

##Exercise 2
##Use filter and select to return just the annual payroll data for the top level 
##construction sector (“23----“).
original_cbp <- fread('data/cbp15co.csv')
cbp23_filter <- filter(original_cbp, NAICS == '23----')
cbp23_select <- cbp23_filter %>% select(starts_with('AP'))

##Exercise 3
##Write code to create a data frame giving, for each state, the number of counties in the 
##CBP survey with establishements in mining or oil and gas extraction (‘21—-‘) along with their 
##total employment (“EMP”). Group the data using both FIPSTATE and FIPSCTY and use the fact that 
##one call to summarize only combines across the lowest level of grouping. The dplyr function n 
##counts rows in a group.

cbp21 <- original_cbp %>%
  filter(NAICS == '21----') %>%
  group_by(FIPSTATE, FIPSCTY) %>%
  summarize(EMP = sum(EMP)) %>%
  summarize(EMP = sum(EMP), counties = n())
head(cbp21)

##Exercise 4
##A “pivot table” is a transformation of tidy data into a wide summary table. First, data are 
##summarized by two grouping factors, then one of these is “pivoted” into columns. Starting 
##from a filtered CBP data file, chain a split-apply-combine procedure into the tidyr function 
##spread to get the total number of employees (“EMP”) in each state (as rows) by 2-digit NAICS 
##code (as columns).

pivot_table <- original_cbp %>%
  filter(NAICS == '21----') %>%
  group_by(FIPSTATE, NAICS) %>%
  summarize(EMP = sum(EMP)) %>%
  spread(key = NAICS, value = EMP)
head(pivot_table)
