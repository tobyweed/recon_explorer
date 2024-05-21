## CLIENT

ui <- navbarPage(
  "Nuclear Recon Explorer",
  
  tags$head(
    # custom styling
    tags$style(
      type = "text/css",
      "
        body {
              font-family: 'Roboto', sans-serif;
        }
        
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
                                
        /* .container-fluid >.tab-content[data-value='Search'] {
          padding-right: 0px;
          padding-left: 0px;
        }
        
        .navbar>.container-fluid .navbar-brand {
            margin-left: 0px;
        } 
                                        
        .navbar {
          margin-bottom: 0;
        } 
        
        .tab-pane[data-value='Search'] {
          margin-top: 20px;
          padding-right: 15px;
          padding-left: 15px;
        } */
                                
        #map1 { 
          position: fixed; 
          top: 0; 
          left: 0;
          padding: 0;
          margin: 0;
        } 
        
        .map1-controls {
          position: relative;
          width: 28vw;
          display: inline-block;
          z-index: 1000; 
          background: white;
          padding: 10px 25px 10px 25px;
          border-radius: 5px;
          box-shadow: 0 2px 6px rgba(0,0,0,0.3);
        }
        
        .control-header {
          display: flex;
          justify-content: space-between;
          align-items: center;
          font-size: 18px;
        }
        
        .toggle-icon {
          cursor: pointer;
        }
        
        .map1-controls.collapsed {
          height: auto;
          overflow: hidden; 
        }
        
        .map1-controls.collapsed .control-content {
          display: none;
        }
             
        .map1-controls .control-content {
          padding-top: 20px;
          border-top: 1px solid #ddd;
        }
      
        .map1-controls .control-content .slider-container label {
          font-weight: normal;
          margin-left: -20px;
        }
        
        .map1-controls .control-content .slider-container {
          margin-left: 20px;
        }
      "
    ),
    
    tags$script(src = "https://kit.fontawesome.com/3ba4309900.js"),
    
    tags$script(HTML("
      $(document).ready(function() {
        $('#toggle-icon').click(function() {
          $('.map1-controls').toggleClass('collapsed');
          if ($('.map1-controls').hasClass('collapsed')) {
            $('#toggle-icon').removeClass('fa-plus').addClass('fa-minus');
          } else {
            $('#toggle-icon').removeClass('fa-minus').addClass('fa-plus');
          }
        });
      });
    ")),
  ),

  tabPanel("Map",
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
                   "Adjust Date:",
                   min = min_date,
                   max = max_date,
                   value = min_date,
                   step = 365.25/4,
                   animate = animationOptions(interval = 500/4,
                                              loop = TRUE))
                 ),
               checkboxInput("showUnknown", "Show Facilities with Unknown Start Dates", value = FALSE),
               # checkboxInput("showCaptures", "Show Spotted Facilities", value = FALSE),
               # actionButton("toggle-button", "Toggle Control")
             )
           )
  ),
  
  tabPanel("Search",
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
