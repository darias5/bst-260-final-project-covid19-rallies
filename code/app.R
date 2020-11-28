# Final project, BST 260
# Project: In-person Presidential campaign rallies and trends in county-level COVID-19 cases and deaths
# Daniel Arias, Kritika Anand, and Tianxiao Zhao


##########################################
# Directory and file paths 
##########################################

path <- "~/3. PhD/Y2/Fall/BST 260/bst-260-final-project-covid19-rallies/"
outputpath <- paste(path,'results/', sep = "/")
datapath <- paste(path,'data/', sep = "/")

##########################################
# Libraries 
##########################################

# Libraries
library(shiny)
library(tidyverse)
library(lubridate)
library(zoo)
library(maps)


##########################################
# Data wrangling, same as from midterm.Rmd
##########################################

## Step 1a: Import Case Dataset 
cases <- read_csv(file = file.path(datapath,"time_series_covid19_confirmed_US.csv")) %>%
    mutate(state_fips = as.numeric(substr(FIPS,1,2))) %>%
    mutate(county_fips = as.numeric(substr(FIPS,3,5))) %>%
    mutate(fips = as.numeric(FIPS)) %>%
    drop_na(fips) %>%
    select(-UID, -iso2, -iso3, -code3, -County_Region, -FIPS) %>%
    filter(Province_State != "American Samoa", 
           Province_State != 'Puerto Rico',
           Province_State != "Guam", 
           Province_State != "Northern Mariana Islands", 
           Province_State != "Virgin Islands", 
           Province_State != 'Diamond Princess', 
           Province_State != 'Grand Princess', 
           Admin2 != "Unassigned") %>% 
    rename(county_name = Admin2, 
           state_name = Province_State, 
           lat = Lat, 
           long = Long_, 
           full_name = Combined_Key ) %>%
    pivot_longer(contains("/"), names_to = "date", values_to = "cases") %>%
    mutate(date = as.Date(date, "%m/%d/%y"))

## Step 1b: Import Death Dataset 
deaths <- read_csv(file = file.path(datapath,"time_series_covid19_deaths_US.csv")) %>%
    mutate(fips = as.numeric(FIPS)) %>%
    drop_na(fips) %>%
    filter(Province_State != "American Samoa", 
           Province_State != 'Puerto Rico',
           Province_State != "Guam", 
           Province_State != "Northern Mariana Islands", 
           Province_State != "Virgin Islands", 
           Province_State != 'Diamond Princess', 
           Province_State != 'Grand Princess',
           Admin2 != "Unassigned") %>% 
    select(-UID, -iso2, -iso3, -code3, -County_Region, -FIPS, -Lat, -Long_, -Province_State, -Combined_Key, -Admin2) %>%
    pivot_longer(contains("/"), names_to = "date", values_to = "deaths") %>%
    mutate(date = as.Date(date, "%m/%d/%y"))

## Step 1c: Merge the data together 
df <- cases %>%
    left_join(deaths, by = c('fips','date')) %>%
    arrange(fips, date)

rm(cases, deaths)

## Step 1d: Load US Census Data - By Race
load("~/3. PhD/Y2/Fall/BST 260/bst-260-final-project-covid19-rallies/data/cc-est2019-alldata.RData")

racial_demographics <- cc %>%
    rename_all(.funs = tolower) %>%
    filter(year == 12) %>% ## 2019
    filter(agegrp == 0) %>% ## Totals only 
    mutate(black_pop = ba_male + ba_female, 
           white_pop = wa_male + wa_female, 
           asian_pop = aa_male + aa_female, 
           native_pop = ia_male + ia_female, 
           pacific_pop = na_male + na_female, 
           hisp_pop = h_male + h_female) %>%
    select(state_fips = state, 
           county_fips = county, 
           county_name_long = ctyname,
           total_pop = tot_pop,
           black_pop, white_pop, asian_pop, native_pop,  pacific_pop, hisp_pop)

## Step 1e: Load US Census Data - By Age 
age_demographics <- cc %>%
    rename_all(.funs = tolower) %>%
    filter(year == 12) %>% ## 2019
    filter(agegrp > 13) %>% ## Keep if 65 or older
    select(state_fips = state, 
           county_fips = county, 
           total_pop = tot_pop) %>%
    group_by(state_fips, county_fips) %>%
    summarise(senior_pop = sum(total_pop))

## Step 1f: Merge Demographic Info with Case and Death Counts
df <- df %>%
    left_join(racial_demographics, by=c('state_fips','county_fips')) %>%
    left_join(age_demographics, by=c('state_fips','county_fips')) 

rm(racial_demographics, age_demographics)

## Step 1g: Load US Mobility data
mobility <- read_csv(file = file.path(datapath,"DL-us-mobility-daterow.csv")) %>%
    rename_all(.funs = tolower) %>%
    mutate(fips = as.numeric(fips)) %>%
    drop_na(admin1) %>%
    drop_na(admin2) %>%
    select(date, fips, m_samples = samples, m50, m50_index) 

df <- df %>% filter(date > ymd(200226))

## Step 1h: Merge Mobility Info with Full Dataset
df <- df %>%
    left_join(mobility, by = c('fips','date')) %>%
    arrange(fips, date)

rm(mobility)

## Step 1i: Load Median Income 
income <- read_csv(file = file.path(datapath,"nhgis0014_ds239_20185_2018_county_E.csv")) %>%
    select(median_inc = AJZAE001, state_name = STATE, county_name_long = COUNTY)

## Step 1j: Merge Median Income Info with Full Dataset
df <- df %>%
    left_join(income, by = c('state_name','county_name_long'))

rm(income)
rm(cc)

## Step 2: Summary Statistics and visualizations

df <- df[-c(3:4)] #Cleaning data to drop columns that are not needed
df <- rename(df, "population" = "Population")

# Creating a variable that contains the number of new confirmed cases
df <- df %>% 
    arrange(date) %>% 
    group_by(fips)  %>% 
    mutate(new_cases = cases - lag(cases))

# Creating a rolling 7-day average for the number of new confirmed cases
df <- df %>% 
    arrange(date) %>% 
    group_by(fips) %>% 
    mutate(new_cases_7dayavg = rollmean(new_cases, k = 7, fill = NA))

# Creating a new variable that calculates the seven-day rolling average of new cases per million
df <- df %>% 
    mutate(new_cases_7dayavg_per_mil_cap = new_cases_7dayavg / population * 1000000)

# Creating a variable that contains the number of new confirmed cases
df <- df %>% 
    arrange(date) %>% 
    group_by(fips)  %>% 
    mutate(new_deaths = deaths - lag(deaths))

# Creating a rolling 7-day average for the number of new confirmed deaths
df <- df %>% 
    arrange(date) %>% 
    group_by(fips) %>% 
    mutate(new_deaths_7dayavg = rollmean(new_deaths, k = 7, fill = NA))


# Creating a new variable that calculates the seven-day rolling average of new deaths per million
df <- df %>% 
    mutate(new_deaths_7dayavg_per_mil_cap = new_deaths_7dayavg / population * 1000000)

# Adding rally data

rally_data <- read_xlsx("../data/rallies_cc_da.xlsx") 
rally_data$date <- as.Date(rally_data$date)

# Filtering out rallies that predate February 26, 2020 (first community spread in February 26)
rally_data <- rally_data %>%
    filter(date > ymd(200226)) %>%
    filter(date <= max(df$date)) %>%
    select(date, fips) %>%
    mutate(rally = 1)

# Full join rally and df by fips
df <- df %>%
    full_join(rally_data, by = c("fips", "date")) %>%
    group_by(fips) %>%
    mutate(rally_ind = ifelse(sum(rally, na.rm = T) != 0, 1, 0))


temp <- df %>%
    group_by(fips) %>%
    filter(sum(rally, na.rm = T) != 0) %>%
    mutate(day_to_rally_1 = date - date[which(rally == 1)][1]) %>%
    mutate(day_to_rally_2 = date - date[which(rally == 1)][2]) %>%
    select(date, fips, day_to_rally_1, day_to_rally_2)

df <- df %>%
    full_join(temp, by = c("fips", "date"))

df$rally <- NULL
rm(temp)
rm(rally_data)

# Downloading map data
us_map = map_data("county")
fips <- data.frame(county.fips)
us_map$polyname <- paste(us_map$region, us_map$subregion, sep = ",")
us_map <- full_join(fips, us_map, by = "polyname")

# Merging map data into dataframe

df <- full_join(df, us_map, by = c("fips"))
rm(us_map)
rm(fips)


##########################################
# Define UI 
##########################################

ui <- fluidPage(
    
    
    
) # Closing the ui

##########################################
# Define server 
##########################################

server <- function(input, output) {
    

    
}   # Closing  server


##########################################
# Run the application 
##########################################

shinyApp(ui = ui, server = server)
