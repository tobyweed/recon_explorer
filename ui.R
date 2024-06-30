## CLIENT

ui <- navbarPage(
  "NUCLEAR RECON EXPLORER",
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style1.css"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "style2.css"),
    # tags$link(rel = "stylesheet", type = "text/css", href = "style3.css"),
    
    tags$script(src = "https://kit.fontawesome.com/3ba4309900.js"),
    
    tags$script(HTML("
      $(document).ready(function() {
        $('#toggle-icon').click(function() {
          $('.map1-controls').toggleClass('collapsed');
          if ($('.map1-controls').hasClass('collapsed')) {
            $('#toggle-icon').removeClass('fa-minus').addClass('fa-plus');
          } else {
            $('#toggle-icon').removeClass('fa-plus').addClass('fa-minus');
          }
        });
      });
    ")),
  ),

  tabPanel("MAP",
           leafletOutput(
               "map1",
               height = "100vh",
               width = "100%"
           ),
           div(
             class = "map1-controls",
             div(
               class = "control-header",
               h4("Controls"),
               tags$i(id = "toggle-icon", class = "toggle-icon fas fa-minus")
             ),
             div(
               class = "control-content",
               div(
                 class = "slider-container",
                 sliderInput(
                   "dateSlider",
                   "Timeline",
                   min = min_date,
                   max = max_date,
                   value = min_date,
                   step = 365.25/4,
                   animate = animationOptions(interval = 500/4,
                                              loop = TRUE))
                 ),
               checkboxInput("showUnknown", "Show Facilities with Unknown Start Dates", value = FALSE),
               # checkboxInput("showCaptures", "Show Spotted Facilities", value = FALSE)
             )
           ),
           div(
             class = "map1-controls",
             id = "map1-key",
             div(
               class = "control-header",
               id = "key-header",
               h4("Key"),
               tags$i(id = "toggle-icon", class = "toggle-icon fas fa-minus")
             ),
             div(
               class = "control-content",
               id = "key-content",
               div(
                 class = "slider-container",
                 sliderInput(
                   "dateSlider",
                   "Timeline",
                   min = min_date,
                   max = max_date,
                   value = min_date,
                   step = 365.25/4,
                   animate = animationOptions(interval = 500/4,
                                              loop = TRUE))
               ),
               checkboxInput("showUnknown", "Show Facilities with Unknown Start Dates", value = FALSE),
               # checkboxInput("showCaptures", "Show Spotted Facilities", value = FALSE)
             )
           )
  ),
  
  tabPanel("SEARCH",
           class = "search",
           sidebarLayout(
             # FACILITY SELECTION
             sidebarPanel(
               class = "search-sidebar",
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
               class="search-main",
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
