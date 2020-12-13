# Final project, BST 260
# Project: In-person Presidential campaign rallies and trends in county-level COVID-19 cases and deaths
# Daniel Arias, Kritika Anand, and Tianxiao Zhao


##########################################
# Directory and file paths 
##########################################

path <- "C:/Users/kriti/OneDrive - Harvard University/HSPH Semester/Semester 3/BST 260/Final Project/Final Project/bst-260-final-project-covid19-rallies/"
outputpath <- paste(path,'results/', sep = "/")
datapath <- paste(path,'data/', sep = "/")

##########################################
# Libraries 
##########################################

# Libraries
library(shiny)
library(tidyverse)
library(dplyr)
library(lubridate)
library(foreach)
library(ggplot2)
library(maps)
library(viridis)
library(zoo)
library(readxl)


##########################################
# Data wrangling, same as from midterm.Rmd
##########################################

## Step 1a: Import Case Dataset 
cases <- read_csv(file = file.path(datapath,"time_series_covid19_confirmed_US.csv")) %>%
  mutate(state_fips = as.numeric(substr(FIPS,1,2))) %>%
  mutate(county_fips = as.numeric(substr(FIPS,3,5))) %>%
  mutate(fips = as.numeric(FIPS)) %>%
  drop_na(fips) %>%
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -FIPS) %>%
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
  select(-UID, -iso2, -iso3, -code3, -Country_Region, -FIPS, -Lat, -Long_, -Province_State, -Combined_Key, -Admin2) %>%
  pivot_longer(contains("/"), names_to = "date", values_to = "deaths") %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

## Step 1c: Merge the data together 
df <- cases %>%
  left_join(deaths, by = c('fips','date')) %>%
  arrange(fips, date)

rm(cases, deaths)

## Step 2: Summary Statistics and visualizations

df <- df[-c(3:4)] #Cleaning data to drop columns that are not needed
df <- rename(df, "population" = "Population")

########CASES

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

######### DEATHS

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


# Add code to load in rally csv

rally_data <- read_xlsx("../data/rallies_cc_da.xlsx") 
rally_data$date <- as.Date(rally_data$date)

# Filter out rallies that predate February 26, 2020 (first community spread in February 26)
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

# Merging land area in Km and creating density variable
land_area <- read_excel(path = file.path(datapath,"Land area by counties.xlsx"))
land_area$fips <- as.numeric(land_area$STCOU)
df <- df %>% 
  left_join(land_area[,c(3,4)], by="fips") %>% 
  mutate(density = population/LND110200D)
rm(land_area)

# Downloading map data
us_map <- map_data("county")
fips <- data.frame(county.fips)
us_map$subregion[(us_map$subregion == "shannon") & (us_map$region == "south dakota")] <- "ogala lakota"
fips$fips[which(fips$fips == 46113)] <- 46102
fips$polyname[which(fips$fips == 46102)] <- "south dakota,ogala lakota"
us_map$polyname <- paste(us_map$region, us_map$subregion, sep = ",")
us_map <- full_join(fips, us_map, by = "polyname")
rm(fips)

# Merging map data into dataframe
df <- full_join(df, us_map, by = c("fips"))
rm(us_map)


##########################################
# Define UI 
##########################################

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  
  tabsetPanel(
    

    ##  Tab: Bar plot, specific on rally
    {
      tabPanel(
        "Rally-related", 
        
        p("Novel coronavirus disease 2019 (COVID-19)is caused due to coronavirus 2 (SARS-CoV-2)
      and is characterized by severe acute respiratory symptoms.This was first identified 
      in Wuhan, Hubei, China, in late 2019. This highly communicable disease rapidly spread
      throughout the world, and is now recognized as",
          a("pandemic by WHO.",href="https://www.who.int/news-room/detail/29-06-2020-covidtimeline")),
          
          p("Coronavirus is spread from person to person contact thorugh respiratory droplets.
          In-person presidential rallies are large events and may contribute to the spread of COVID-19. 
      President Trump had conducted numerous in-person rallies, both before and after the 
      White House COVID-19 outbreak of September-October 2020 in the United States of America.
      Public health researchers, professionals, and observers had concerns that these in-person 
      rallies may be contributing to the spread of COVID-19."),
        
        ##  Title Panel
        titlePanel("3 Weeks Before and After rallies"),
        ##  Sidebar 
        sidebarLayout(
          ##  "Buttons"
          sidebarPanel(
            p("Use this tab to visualize the increase in deaths and cases per capita for counties
              which had Trump rallies. Please select upto 5 counties to view the trends.
              You can also choose between the graph of New Cases and Death using the radiobutton."),
            br(), 
           
          
            selectizeInput(
              inputId = "chosen_county_rally",
              label = "Please specify counties with rally",
              choices = unique(df[which(df$rally_ind == 1),]$full_name),
              multiple = T,
              options = list(maxItems = 5)
            ),
            radioButtons(
              inputId = "y_type_rally",
              label = "The data shown would be:",
              choiceNames = c("New cases", 
                              "New death"),
              choiceValues = c(1, 2),
              selected = 1
            )
          ),
          mainPanel(
            plotOutput("box_plot_rally")
          )
        )
      )
    },
    
    ##  Tab: Summary line plot, capable of many thing
    {
      tabPanel(
        "Summary",
        
        p("Novel coronavirus disease 2019 (COVID-19)is caused due to coronavirus 2 (SARS-CoV-2)
      and is characterized by severe acute respiratory symptoms.This was first identified 
      in Wuhan, Hubei, China, in late 2019. This highly communicable disease rapidly spread
      throughout the world, and is now recognized as",
          a("pandemic by WHO",href="https://www.who.int/news-room/detail/29-06-2020-covidtimeline"),
          
          "Coronavirus is spread from person to person contact thorugh respiratory droplets.
          In-person presidential rallies are large events and may contribute to the spread of COVID-19. 
      President Trump had conducted numerous in-person rallies, both before and after the 
      White House COVID-19 outbreak of September-October 2020 in the United States of America.
      Public health researchers, professionals, and observers had concerns that these in-person 
      rallies may be contributing to the spread of COVID-19."),
        
        ##  Title Panel
        titlePanel("New Cases on 7-day Average per million capita"),
        ##  Sidebar 
        sidebarLayout(
          ##  "Buttons"
          sidebarPanel(
            
            p("Use this tab to visualize compare the differences in counties with rallies and
            other counties of your choice. Please select one county will rally and upto 5 
            other counties to view the trends.You can also choose between the graph of New Cases
            and Death using the radiobutton."),
            br(), 
            
            selectInput(
              inputId = "chosen_rally_county_summary",
              label = "Please specify a county with rally",
              choices = unique(df[which(df$rally_ind == 1),]$full_name)
            ),
            selectizeInput(
              inputId = "chosen_county_summary",
              label = "Or, choose counties you wanted",
              choices = unique(df$full_name)[order(unique(df$full_name))],
              multiple = T,
              options = list(maxItems = 3)
            ),
            radioButtons(
              inputId = "y_type_summary",
              label = "The data shown would be:",
              choiceNames = c("New cases", 
                              "New death"),
              choiceValues = c(1, 2),
              selected = 1
            )
          ),
          ##  Main panel
          mainPanel(
            plotOutput("line_plot"),
            textOutput("rally_info")
          )
        )
      )
    },
    
    ##  Tab for spaital plot
    tabPanel(
      "Spatial Plot",
      
      p("Novel coronavirus disease 2019 (COVID-19)is caused due to coronavirus 2 (SARS-CoV-2)
      and is characterized by severe acute respiratory symptoms.This was first identified 
      in Wuhan, Hubei, China, in late 2019. This highly communicable disease rapidly spread
      throughout the world, and is now recognized as",
        a("pandemic by WHO",href="https://www.who.int/news-room/detail/29-06-2020-covidtimeline"),
        
        "Coronavirus is spread from person to person contact thorugh respiratory droplets.
          In-person presidential rallies are large events and may contribute to the spread of COVID-19. 
      President Trump had conducted numerous in-person rallies, both before and after the 
      White House COVID-19 outbreak of September-October 2020 in the United States of America.
      Public health researchers, professionals, and observers had concerns that these in-person 
      rallies may be contributing to the spread of COVID-19."),
      ##  Title Panel
      titlePanel("Map of COVID-19 data per million people"),
      
      sidebarLayout(
        sidebarPanel(
          
          p("Use this tab to visualize the temporal trends of COVID-19 across different regions in the 
          United States. Please select a date to veiw the heatmap on a particular day.
          You can also choose between the graph of New Cases and Death using the radiobutton."),
          br(), 
          dateInput(
            inputId = "chosen_date_geo",
            label = "Please select a date",
            value = "2020-10-01",
            min = min(df$date, na.rm = T)+3,
            max = max(df$date, na.rm = T)-3
          ),
          radioButtons(
            inputId = "y_type_geo",
            label = "The data shown would be:",
            choiceNames = c("New cases", 
                            "New death"),
            choiceValues = c(1, 2),
            selected = 1
          )
        ),
        mainPanel(
          plotOutput("geo_plot")
        )
      )
      
    )
    
  )
  
) # Closing the ui

##########################################
# Define server 
##########################################

server <- function(input, output) {
  
  ##  Server for rally-related tab
  {
    ##  Fetch chosen data
    rally_county_data <- reactive({
      df %>% 
        filter(rally_ind == 1) %>%
        filter(full_name %in% input$chosen_county_rally) %>%
        select(-polyname, -long, -lat, -group, -order, -region, -subregion) %>%
        distinct() %>%
        filter(day_to_rally_1 %in% -21:21) %>%
        group_by(day_to_rally_1)
    })
    
    ##  Plot part
    output$box_plot_rally <- renderPlot({
      if(input$y_type_rally == 1){
        rally_county_data() %>%
          ggplot(aes(x = day_to_rally_1,
                     y = new_cases_7dayavg_per_mil_cap,
                     col = full_name)) +
          scale_color_brewer(type = "div", palette = "Set1") + 
          geom_line() +
          geom_vline(aes(xintercept = 0),
                     colour = "red",
                     linetype = "dashed") +
          labs(x = "Day to Rally", 
               y = "New Cases per million cap (7-day average)",
               col = "County") +
          theme_bw()
      }
      else{
        rally_county_data() %>%
          ggplot(aes(x = day_to_rally_1, 
                     y = new_deaths_7dayavg_per_mil_cap,
                     col = full_name)) +
          scale_color_brewer(type = "div", palette = "Set1") + 
          geom_line() +  
          geom_vline(aes(xintercept = 0),
                     colour = "red",
                     linetype = "dashed") + 
          labs(x = "Day to Rally",
               y = "New Deaths per million cap (7-day average)",
               col = "County") + 
          theme_bw()
      }
    })
  }
  
  
  ##  Server for summary tab
  {  
    ##  Fetch chosen data; support 2 types
    selected_lineplot_data <- reactive({
      ##  only 1 rally-county chosen
      if(is.null(input$chosen_county_summary)){
        df %>% 
          filter(full_name == input$chosen_rally_county_summary) %>%
          select(-polyname, -long, -lat, -group, -order, -region, -subregion) %>%
          distinct()
      }
      ##  > 1 county, used for comparison
      else{
        df %>% 
          filter(full_name %in% c(input$chosen_rally_county_summary, 
                                  input$chosen_county_summary)) %>%
          select(-polyname, -long, -lat, -group, -order, -region, -subregion) %>%
          distinct()
      }
    })
    
    ##  Numbers of rally 
    rally_num <- reactive(
      sum(selected_lineplot_data()$day_to_rally_1 == 0, na.rm = T) + 
        sum(selected_lineplot_data()$day_to_rally_2 == 0, na.rm = T)
    )
    
    ##  Rally dates
    rally_1_date <- reactive(selected_lineplot_data() %>% 
                               ungroup() %>% 
                               filter(day_to_rally_1 == 0) %>% select(date))
    rally_2_date <- reactive(selected_lineplot_data() %>% 
                               ungroup() %>% 
                               filter(day_to_rally_2 == 0) %>% select(date))
    ##  Plot part; 
    p1 <- renderPlot({
      if(is.null(input$chosen_county_summary)){
        if(input$y_type_summary == 1){
          selected_lineplot_data() %>% 
            ggplot(aes(x = date, y = new_cases_7dayavg_per_mil_cap)) + 
            geom_line() + 
            geom_vline(aes(xintercept = max(rally_1_date()[[1]], 0)),
                       colour = "red",
                       linetype = "dashed"
            ) + 
            geom_vline(aes(xintercept = max(rally_2_date()[[1]], 0)),
                       colour = "blue",
                       linetype = "dashed"
            ) + 
            labs(x = "Date", y = "New Cases per million cap (7-day average)") + 
            theme_bw()
        }
        else{
          selected_lineplot_data() %>% 
            ggplot(aes(x = date, y = new_deaths_7dayavg_per_mil_cap)) + 
            geom_line() + 
            geom_vline(aes(xintercept = max(rally_1_date()[[1]], 0)),
                       colour = "red",
                       linetype = "dashed"
            ) + 
            geom_vline(aes(xintercept = max(rally_2_date()[[1]], 0)),
                       colour = "blue",
                       linetype = "dashed"
            ) + 
            labs(x = "Date", y = "New Deaths per million cap (7-day average)") + 
            theme_bw()
        }
      }
      else{
        if(input$y_type_summary == 1){
          selected_lineplot_data() %>% 
            ggplot(aes(x = date, y = new_cases_7dayavg_per_mil_cap, col = full_name)) + 
            geom_line() + 
            scale_color_brewer(type = "div", palette = "Set1") + 
            geom_vline(aes(xintercept = max(rally_1_date()[[1]], 0)),
                       colour = "red",
                       linetype = "dashed"
            ) + 
            geom_vline(aes(xintercept = max(rally_2_date()[[1]], 0)),
                       colour = "blue",
                       linetype = "dashed"
            ) + 
            labs(x = "Date", y = "New Cases per million cap (7-day average)", col = "County") + 
            theme_bw()
        }
        else{
          selected_lineplot_data() %>% 
            ggplot(aes(x = date, y = new_deaths_7dayavg_per_mil_cap, col = full_name)) + 
            geom_line() + 
            scale_color_brewer(type = "div", palette = "Set1") + 
            geom_vline(aes(xintercept = max(rally_1_date()[[1]], 0)),
                       colour = "red",
                       linetype = "dashed"
            ) + 
            geom_vline(aes(xintercept = max(rally_2_date()[[1]], 0)),
                       colour = "blue",
                       linetype = "dashed"
            ) + 
            labs(x = "Date", y = "New Deaths per million cap (7-day average)", col = "County") + 
            theme_bw()
        }
      }
    })
    output$line_plot <- p1
    
    ##  Text output
    output$rally_info <- renderText({
      if(rally_num() == 1){
        paste0("There was 1 rally in ", 
               selected_lineplot_data()$county_name[which(selected_lineplot_data()$rally_ind == 1)][1],
               ". The red dash line indicates the date of this rally.")
      }else{
        paste0("There were ", 
               rally_num(),
               " rallies in ", 
               selected_lineplot_data()$county_name[which(selected_lineplot_data()$rally_ind == 1)][1],
               ". The red/blue dash line indicate the date of the first/second rally.")
      }
    })
  }
  
  ##  Server for the geo tab
  {
    output$geo_plot <- renderPlot({
      if(input$y_type_geo == 1){
        df %>% filter(date == input$chosen_date_geo) %>%
          ggplot(aes(x = long, y = lat, group = group, fill = new_cases_7dayavg_per_mil_cap)) +
          geom_polygon() + 
          theme(panel.grid.major = element_blank(), 
                panel.background = element_blank(),
                axis.title = element_blank(), 
                axis.text = element_blank(),
                axis.ticks = element_blank()) +
          labs(title="Number of new confirmed COVID-19 cases per million people",
               subtitle=paste0("on ", format(input$chosen_date_geo, "%B %d, %Y")),
               caption="Source: Center for Systems Science and Engineering, Johns Hopkins University",
               fill="Rolling 7-day average") +
          scale_fill_viridis_c(option = "plasma",
                               limits = c(0,500),    # Forcing upper limit of 500 cases per million capita per day
                               oob = scales::squish)
      }else{
        df %>% filter(date == input$chosen_date_geo) %>%
          ggplot(aes(x = long, y = lat, group = group, fill = new_deaths_7dayavg_per_mil_cap)) +
          geom_polygon() + 
          theme(panel.grid.major = element_blank(), 
                panel.background = element_blank(),
                axis.title = element_blank(), 
                axis.text = element_blank(),
                axis.ticks = element_blank()) +
          labs(title="Number of new confirmed COVID-19 deaths per million people",
               subtitle=paste0("on ", format(input$chosen_date_geo, "%B %d, %Y")),
               caption="Source: Center for Systems Science and Engineering, Johns Hopkins University",
               fill="Rolling 7-day average") +
          scale_fill_viridis_c(option = "plasma",
                               limits = c(0,100),    # Forcing upper limit of 100 cases per million capita per day
                               oob = scales::squish)
      }
    })
  }
  
}   # Closing  server


##########################################
# Run the application 
##########################################

shinyApp(ui = ui, server = server)
