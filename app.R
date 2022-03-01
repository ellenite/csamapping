#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Loading the necessary packages

library(shiny)
library(shinydashboard)
library(DT)
library(shinyWidgets)
library(dplyr)
library(echarts4r)
library(leaflet)
library(googlesheets4)
library(shinyjs)

# Define UI for the application
ui <- dashboardPage(skin = "green",

    dashboardHeader(
        title = "Mapping of Climate Change Initiatives in West and Central  Africa", titleWidth = 600
    ),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                tabName = "overview", text = "Overview", icon = icon("map")
            )
            , menuItem(
                tabName = "initiative", text = "Initiative", icon = icon("th-list")
            )
        )
    ),
    dashboardBody(
      useShinyjs(), # Set up shinyjs
       tabItems(
            tabItem(
                tabName = "overview"
                , fillPage(
                    fluidRow(
                        box(width = 8, status = "info", title = "Filters"
                            , column(width = 12, uiOutput("country_check_box"))
                            , height = "20vh"
                            )
                        , box(width = 4, status = "info", title = "In summary"
                              , uiOutput("vb_number_initiative")
                              , height = "20vh")
                    )
                    , fluidRow(
                        box(width = 12, status = "info"
                           , leafletOutput("map", height = "65vh"))
                    )
                )
            )
            , tabItem(
                tabName = "initiative"
                , fillPage(
                    fluidRow(
                        box(width = 12, status = "info", title = "Filters"
                            , column(width = 4, uiOutput("country_picker"))
                            , column(width = 4, uiOutput("year_slider"))
                            , column(width = 4, uiOutput("status_picker"))
                            , height = "20vh"
                        )
                    )
                    , fluidRow(
                        box(width = 12, status = "info", title = "List of initiative(s) matching the criterias"
                            , column(width = 2
                                     , actionBttn("view_factsheet", label = "View Factsheet"
                                                  , icon = icon("file-invoice"), block=TRUE)
                            ) 
                            , br()
                            , br()
                            , DTOutput("csa_data_table"))
                    )
                )
            )
            
        )
    ),
    title = "Climate Change Initiatives Mapping"
    
)

# Define server logic
server <- function(input, output) {
  
  # Load the data
  # Using the google auth from cache directory
  
  cache_directory <- ".cache/"
  
  # Connect to google drive and reading the google sheet
  gs4_auth(email = "kpavode.ellenite@gmail.com", cache = cache_directory)
  csa_data_url <- "https://docs.google.com/spreadsheets/d/1ZdE0Lu7Y3LFvYz2uPzYof2M1xjWIUEsZPvLdY8nvXRM/edit#gid=62873840"
  csa_data <- range_read(ss = csa_data_url)
    
  # Overview Tab -----
  # Filters box -----
  
  # Country Picker
  output$country_check_box <- renderUI({
    countries <- csa_data %>%
      select(data_collected_in) %>% 
      pull() %>% 
      unique() %>%
      sort()
    countries <- countries[countries != ""]
    checkboxGroupButtons(
      "country_check_box"
      , label = "Countries"
      , choices = countries
      , selected = countries
      , status = "primary"
      , size = "normal"
      , direction = "horizontal"
      , width = "100%"
      , individual = TRUE
    )
  })
  
  
  # Reactive Map Data 
  
  reactive_map_data <- reactive({
    req(input$country_check_box)
    csa_data %>%
      filter(data_collected_in %in% input$country_check_box)
  })
  
  # Value Boxes 
  output$vb_number_initiative <- renderValueBox({
    initiatives <- reactive_map_data() %>%
      nrow()
    
    valueBox(value = format(initiatives, big.mark=","), subtitle = "initiatives matching the criteria")
  })
  
  # Map -----
  output$map <- renderLeaflet({
    reactive_map_data() %>%
      mutate(popup = paste(
        "<center> <b> Name of the initiative </b>"
        , "<br>"
        , title_initiative
        , "</center>"
      )
      ) %>%
      leaflet() %>%
      addTiles() %>%  
      addMarkers(lng= ~as.numeric(unlist(longitude))
                 , lat= ~as.numeric(unlist(latitude))
                 , label = ~id
                 , popup = ~popup)
  })
  
  # Initiative Tab -----
  # Filters box -----
  
  # Country Picker
  output$country_picker <- renderUI({
    countries <- csa_data %>%
      select(data_collected_in) %>% 
      pull() %>% 
      unique() %>%
      sort()
    countries <- countries[countries != ""]
    pickerInput("country_picker", "Select country"
                , choices = countries)
  })
  
  # Year Slider
  output$year_slider <- renderUI({
    req(input$country_picker)
    all_start_year <- csa_data %>%
      filter(data_collected_in == input$country_picker) %>%
      select(start_year) %>%
      pull() %>% 
      unique()
    all_start_year <- all_start_year[!is.na(all_start_year)]
    min_year <- min(all_start_year)
    
    req(input$country_picker)
    all_end_year <- csa_data %>%
      filter(data_collected_in == input$country_picker) %>%
      select(end_year) %>%
      pull() %>% 
      unique()
    all_end_year <- all_end_year[!is.na(all_end_year)]
    max_year <- max(all_end_year)
    
    sliderInput("year_slider", "Year Slider (Start and end year)"
                , min_year, max_year, value = c(min_year, max_year))
  })
  
  # Status Picker
  output$status_picker <- renderUI({
    req(input$country_picker)
    req(input$year_slider)
    status <- csa_data %>%
      filter(data_collected_in == input$country_picker
             , start_year >= input$year_slider[1]
             , end_year <= input$year_slider[2]) %>%
      select(status) %>% 
      pull() %>% 
      unique() %>%
      sort()
    status <- status[status != ""]
    pickerInput("status_picker", "Select initiative status"
                , choices = status)
  })
  
  # Data table -----
  # Reactive Data
  
  reactive_csa_data <- reactive({
    req(input$country_picker)
    req(input$year_slider)
    req(input$status_picker)
    csa_data %>%
      filter(data_collected_in == input$country_picker
             , start_year >= input$year_slider[1]
             , end_year <= input$year_slider[2]
             , status == input$status_picker)
  })
  
  # Table
  output$csa_data_table <- renderDT({
    reactive_csa_data() %>%
      select("Title of the Initiative"=title_initiative
             , "Type of Initiative"=type_initiative
             , "Geographique Coverage"=country
             , "Start Year"=start_year
             , "End Year"=end_year
             , "Status"=status
             , "Name of Lead organization"=name_lead_organisation
             , "Partners"=partners
             , "Specific Objective"=specific_objective
             , "Budget allocation (EUR)"=budget) %>%
      datatable(rownames=F, selection = c("single")
                , options = list(paging=F
                                 , scrollY = "40vh"
                                 , scrollX = "100%"))
  })
  
  # Show factSheet -----
  observeEvent(input$view_factsheet, {
    
    data_fs <- reactive_csa_data()
    row_selected <- input$csa_data_table_rows_selected
    
    if (is.null(row_selected)) {
      showNotification("Please select a row first", type = "error", duration = 5)
    }
    else 
    {
      showModal(
        modalDialog(title = "Initiative Facksheet", size = "l"
                    , footer = fluidPage(
                      column(width = 12
                               , actionBttn("dismiss", icon = icon("times"), color = "danger")
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , disabled(textAreaInput("title", "Title of the Initiative", width = "100%"
                                         , value = data_fs[row_selected, 4])))
                    )
                    , fluidRow(
                      column(width = 6
                             , disabled(textInput("type", "Type of initiative", width = "100%"
                                                  , value = data_fs[row_selected, 5]))
                             , disabled(textAreaInput("keyword", "Keywords", width = "100%"
                                                  , value = data_fs[row_selected, 6]))
                             , disabled(textAreaInput("country", "Geographic Coverage", width = "100%"
                                                      , value = data_fs[row_selected, 7]))
                             , disabled(textAreaInput("location", "Specific Location", width = "100%"
                                                      , value = data_fs[row_selected, 8]))
                             , disabled(textInput("period", "Period of implementation", width = "100%"
                                                      , value = paste(data_fs[row_selected, 11], 
                                                                      data_fs[row_selected, 11],
                                                                      sep="-")))
                             , disabled(textInput("status", "Status of the initiative", width = "100%"
                                                  , value = data_fs[row_selected, 13]))
                             , disabled(textInput("lead_name", "Lead Organisation Name", width = "100%"
                                                      , value = data_fs[row_selected, 14]))
                             , disabled(textInput("lead_type", "Lead Organisation Type", width = "100%"
                                                  , value = data_fs[row_selected, 15]))
                             , disabled(textAreaInput("partners", "Partners", width = "100%"
                                                      , value = data_fs[row_selected, 16]))
                             , disabled(textAreaInput("so", "Specific Objective", width = "100%"
                                                      , value = data_fs[row_selected, 17]))
                             , disabled(textInput("relevance_productivity" 
                                                  , "Relevance on opportunities to increase agricultural productivity"
                                                  , width = "100%"
                                                  , value = data_fs[row_selected, 18]))
                             , disabled(textInput("relevance_resilience" 
                                                  , "Relevance on improving resilience to climate change"
                                                  , width = "100%"
                                                  , value = data_fs[row_selected, 19]))
                      )
                      , column(width = 6
                               , disabled(textInput("relevance_greenhouse" 
                                                    , "Relevance on long-term reductions in  greenhouse gas emissions"
                                                    , width = "100%"
                                                    , value = data_fs[row_selected, 20]))
                               , disabled(textAreaInput("activities", "Main activities", width = "100%"
                                                        , value = data_fs[row_selected, 21]))
                               , disabled(textInput("indicator1m", "Number of smallholders practicing CSA approaches - Men", width = "100%"
                                                    , value = as.numeric(data_fs[row_selected, 23])))
                               , disabled(textInput("indicator1w", "Number of smallholders practicing CSA approaches - Women", width = "100%"
                                                    , value = as.numeric(data_fs[row_selected, 25])))
                               , disabled(textInput("indicator2", "Agricultural systems in which CSA practices have been adopted - # Hectares "
                                                    , width = "100%"
                                                    , value = as.numeric(data_fs[row_selected, 27])))
                               , disabled(textAreaInput("hypotheses", "Hypotheses for scaling out", width = "100%"
                                                        , value = data_fs[row_selected, 28]))
                               , disabled(textAreaInput("source", "Sources of information", width = "100%"
                                                        , value = data_fs[row_selected, 29]))
                               , disabled(textAreaInput("knowledge", "Knowledge sharing and communication products", width = "100%"
                                                        , value = data_fs[row_selected, 30]))
                               , disabled(textInput("budget", "Budget allocation (EUR)", width = "100%"
                                                    , value = as.numeric(data_fs[row_selected, 31])))
                               , disabled(textInput("contact_name", "Contact Name", width = "100%"
                                                    , value = unlist(data_fs[row_selected, 32])))
                               , disabled(textInput("contact_email", "Contact Email", width = "100%"
                                                    , value = unlist(data_fs[row_selected, 33])))
                               , disabled(textAreaInput("contact_url", "Contact URL", width = "100%"
                                                    , value = data_fs[row_selected, 34]))
                      )
                    )
                    , fluidRow(
                      column(width = 12
                             , disabled(textAreaInput("comments", "Comments", width = "100%"
                                                      , value = data_fs[row_selected, 35])))
                    )
                    
        )
      )
    }
    
    
  })
  
  observeEvent(input$dismiss, {
    removeModal()
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
