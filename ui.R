## CLIENT

ui <- navbarPage(
  "Nuclear Recon Explorer",
  # custom styling
  tags$style(
    type = "text/css",
    "
        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar { }
        .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-from, .js-irs-1 .irs-to {background: red;
                                                    border-top: 1px solid red ;
                                                    border-bottom: 1px solid red ;}
        .subtitle { padding-left: 15px;
                    color: #808080; }
        .selectize-input { max-height: 200px;
                           overflow-y: auto;}
        .leaflet-control-zoom { z-index: -1;
                                color = 'red';}
      "
  ),
  
  tabPanel("Timeline",
           sidebarLayout(
             sidebarPanel(
               HTML("<strong>Filter Facilities</strong>"),
               checkboxInput("showUnknown", "Show Facilities with Unknown Start Dates", value = FALSE),
               checkboxInput("showCaptures", "Show Spotted Facilities", value = FALSE),
               sliderInput(
                 "dateSlider",
                 "Adjust Date:",
                 min = min_date,
                 max = max_date,
                 value = min_date,
                 step = 49
               )
               # animate = animationOptions(interval = 250))
             ),
             mainPanel(leafletOutput(
               "map1", height = "600px", width = "800px"
             ))
           )),
  
  tabPanel("Explore",
           sidebarLayout(
             # FACILITY SELECTION
             sidebarPanel(
               selectInput(
                 inputId = "searchmode",
                 label = "Select facilities by:",
                 choices = c("Facility name", "Country")
               ),
               conditionalPanel(
                 condition = "input.searchmode == 'Facility name'",
                 selectInput(
                   inputId = "facility",
                   label = "Select facilities:",
                   choices = paste(facs$facility_name, " [", facs$country, "]", sep = ""),
                   multiple = TRUE
                 )
               ),
               conditionalPanel(
                 condition = "input.searchmode == 'Country'",
                 selectInput(
                   inputId = "country",
                   label = "Select countries:",
                   choices = unique(facs$country),
                   multiple = TRUE
                 )
               ),
               br(),
               # actionButton(inputId = "search",
               #              label = "Search"),
               # uiOutput(outputId = "map2label"),
               leafletOutput("map2", height = "300px", width = "auto")
             ),
             
             # VISUALIZATION OPTIONS
             mainPanel(
               selectInput(
                 inputId = "select_plot",
                 label = "Choose a Visualization:",
                 choices = c(
                   "Photo list",
                   "Capture Frequency by Camera Resolution",
                   "Capture Frequency by Facility",
                   "Capture Frequency by Data Source"
                   # "Total Visits per Facility"
                 )
               ),
               conditionalPanel(
                 condition = "input.select_plot == 'Photo list'",
                 uiOutput("capture_table_label"),
                 DTOutput(outputId = "capture_table", width = "100%")
               ),
               # conditionalPanel(condition = "input.select_plot == 'Total Visits per Facility'",
               #                  h3("BRUH."),
               #                  textOutput("capture_totals_label"),
               #                  DTOutput("capture_totals_table")),
               # make a label that is common across all the capture plot options
               conditionalPanel(
                 condition = "['Capture Frequency by Camera Resolution', 'Capture Frequency by Facility', 'Capture Frequency by Data Source'].includes(input.select_plot)", # if we're displaying any of the capture plots
                 uiOutput("cap_plot_label")
               ),
               conditionalPanel(condition = "input.select_plot == 'Capture Frequency by Camera Resolution'",
                                h5("Differentiated by camera resolution"),
                                plotOutput("capture_frequency_by_res")),
               conditionalPanel(condition = "input.select_plot == 'Capture Frequency by Facility'",
                                h5("Differentiated by facility"),
                                plotOutput("capture_frequency_by_fac")),
               conditionalPanel(condition = "input.select_plot == 'Capture Frequency by Data Source'",
                                h5("Differentiated by data source"),
                                plotOutput("capture_frequency_by_data"))
             )
           )
  )
)
