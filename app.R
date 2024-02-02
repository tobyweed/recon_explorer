library(tidyverse)
library(shiny)
library(shinyjs)
library(leaflet)
library(DT)

## SETUP
facs <- read_csv("facilities.csv")[-1]
missiles <- read_csv("missiles.csv")[-1]
fac_caps <- read_csv("fac_caps_with_unknown.csv")[-1]
miss_caps <- read_csv("miss_captures.csv")[-1]

# get date range
min_date = min(append(facs$start_date, missiles$start_date), na.rm = TRUE)
max_date = max(append(facs$start_date, missiles$start_date), na.rm = TRUE)



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





##  ------------------------------- SERVER ---------------------------------- ##

server <- function(input, output, session) {
  state <- reactiveValues(fac = facs,
                          miss = missiles)
  
  # -------------------------------- UTILS -------------------------------------
  # handle unknown dates
  adjust_nas <- function() {
    state$fac <- facs
    
    if (input$showUnknown) {
      state$fac$start_date <-
        replace(state$fac$start_date,
                is.na(state$fac$start_date),
                min_date) # replace NA values with the min slider date
    } else {
      state$fac <-
        state$fac %>% filter(!is.na(start_date)) #filter them out
    }
  }
  
  # filter facilities by date range shown on slider
  fac_filtered_by_dates <- function() {
    state$fac[state$fac$start_date <= input$dateSlider,]
  }
  
  # filter missiles by date range shown on slider
  miss_filtered_by_dates <- function() {
    miss_not_expired <-
      ifelse(!is.na(state$miss$end_date),
             state$miss$end_date >= input$dateSlider,
             TRUE)
    state$miss[(state$miss$start_date <= input$dateSlider) &
                 miss_not_expired,]
  }
  
  # filter for facilities which have been captured bf date
  captured_facs <- function() {
    fac_caps %>% filter(`Acquisition Date` <= input$dateSlider) %>% # filter for fac_caps bf current date
      filter(facility_name %in% state$fac$facility_name) %>% # filter facs which aren't in current state (e.g. NA)
      group_by(facility_name) %>% # group by facility
      filter(`Acquisition Date` == max(`Acquisition Date`)) %>% # keep the most recent fac_caps
      distinct(facility_name, .keep_all = TRUE) # make sure only one capture of each facility is present.
  }
  
  # filter for missiles which have been captured
  captured_miss <- function() {
    miss_caps %>% filter(`Acquisition Date` <= input$dateSlider) %>% # filter for miss_caps bf current date
      group_by(address_found) %>% # group by facility
      filter(`Acquisition Date` == max(`Acquisition Date`)) %>% # keep the most recent fac_caps
      distinct(address_found, .keep_all = TRUE) # make sure only one capture of each facility is present.
  }
  
  # return a vector of popup windows for captured facilities or missiles
  get_popups <- function(caps, is_facs) {
    popups <- c()
    
    # fill in fields of popup windows for markers
    if (nrow(caps) >= 1) {
      for (i in 1:nrow(caps)) {
        popup <-
          paste(
            ifelse(
              is_facs,
              paste(caps$facility_name[i], "<br>"),
              "Missile Site <br>"
            ),
            "Start Date: ",
            caps$start_date[i],
            "<br>",
            "Most Recently Photographed: ",
            caps$`Acquisition Date`[i],
            "<br>",
            a(
              "Click to See Image and Metadata",
              href = caps$pic_URL[i],
              target = "_blank"
            )
          )
        
        popups <- c(popups, popup)
      }
    }
    
    popups
  }
  
  # add markers where there should be markers
  populate_map1 <- function() {
    leafletProxy(mapId = 'map1') %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = fac_filtered_by_dates(),
        lng = ~ lng,
        lat = ~ lat,
        radius = 2.8,
        weight = 1,
        color = "#2a297b",
        opacity = 1,
        fillOpacity = 1,
        popup = ~ paste(
          facility_name,
          "<br>",
          "Facility Start Date: ",
          start_date,
          ifelse(input$showCaptures, "<br>Not Yet Photographed", "")
        )
      )
    # addMarkers(data = miss_filtered_by_dates(),
    #            lng = ~lng, lat = ~lat,
    #            icon = makeIcon(
    #              iconUrl = "img/tri_#2a297b.png",
    #              iconWidth = 7.5, iconHeight = 7.5
    #            ),
    #            popup = ~paste("Missile Site",
    #                           "<br>Start Date: ", start_date,
    #                           ifelse(input$showCaptures, "<br>Not Yet Photographed", "")))
    
    if (input$showCaptures) {
      fac_caps <- captured_facs()
      fac_lngs <- fac_caps$lng
      fac_lats <- fac_caps$lat
      fac_popups <- get_popups(caps = fac_caps, is_facs = TRUE)
      
      # miss_caps <- captured_miss()
      # miss_lngs <- miss_caps$lng
      # miss_lats <- miss_caps$lat
      # miss_popups <- get_popups(caps = miss_caps, is_facs = FALSE)
      
      
      leafletProxy(mapId = 'map1') %>%
        addCircleMarkers(
          data = fac_caps,
          lng = fac_lngs,
          lat = fac_lats,
          radius = 2.8,
          weight = 1,
          color = "#dd2119",
          opacity = 1,
          fillOpacity = 1,
          popup = fac_popups
        )
      # addMarkers(data = miss_caps,
      #                  lng = miss_lngs, lat = miss_lats,
      #                  icon = makeIcon(
      #                    iconUrl = "img/tri_#dd2119.png",
      #                    iconWidth = 7.5, iconHeight = 7.5
      #                  ),
      #                  popup = miss_popups
      # )
    }
  }
  
  # map for Map page. run on startup. Adjust NAs and filter by date
  output$map1 <- renderLeaflet({
    map <- leaflet(leafletOptions(minZoom = 0)) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 0,
              zoom = 1)
    map
  })
  
  # remove the countries listed after the facility names in the input, in the format "Facility Name [Country]"
  remove_country_name <- function(fac_name_string) {
    sub(" \\[.*\\]", "", fac_name_string)
  }
  
  # create capture table
  create_capture_table <- function(fac_names) {
    # create capture table
    output$capture_table_label <- renderUI({
      if (input$searchmode == "Facility name") {
        HTML("<h4>List of photographs of selected facilities: </h4>")
      } else if (input$searchmode == "Country") {
        HTML(paste(
          "<h4>List of photographs of facilities in ",
          paste(input$country, collapse = ", "),
          ":</h4>",
          sep = ""
        ))
      }
    })
    
    cap_table <- fac_caps %>%
      filter(facility_name %in% fac_names) %>%
      select(
        `Acquisition Date`,
        facility_name,
        Mission,
        `Camera Resolution`,
        `Data Source`,
        pic_URL
      )
    
    colnames(cap_table) <- c("Acquisition Date",
        "Facility",
        "Mission",
        "Camera Resolution",
        "Data Source",
        "Photo URL"
      )
    
    cap_table$`Photo URL` <- sprintf('<a href="%s" target="_blank" class="btn btn-primary">See Image</a>', cap_table$`Photo URL`)
    
    output$capture_table <- renderDT({
      datatable(
        cap_table,
        options = list(
          lengthMenu = c(5, 10, 15), # set the options for the number of entries per page
          pageLength = 5 # set the default number of entries per page)
        ), 
        selection = "none", # disable selecting rows to get rid of annoying blue selection
        escape = FALSE)
      })
  }
  
  
  # render the reactive capture frequency plot labels
  output$cap_plot_label <- renderUI({
    if (input$searchmode == "Facility name") {
      HTML("<h4>Number of days selected facilities were photographed per year</h4>")
    } else if (input$searchmode == "Country") {
      HTML(paste(
        "<h4>Number of days facilities in ",
        paste(input$country, collapse = ", "),
        " were photographed per year</h4>",
        sep = ""
      ))
    }
  })
  
  
  # render the capture frequency plots
  render_plots <- function(fac_names) {
    captures <- fac_caps %>%
      filter(facility_name %in% fac_names) %>%
      mutate(`Abbreviated Mission` = substr(Mission, 1, 4),
             Resolution = ifelse(`Camera Resolution` %in% c("Stereo High", "Vertical High", "2 to 4 feet"), "high", "low"),
             `Facility` = facility_name)
    
    output$capture_frequency_by_res <- renderPlot({ create_frequency_plot(captures, "Resolution") })
    output$capture_frequency_by_fac <- renderPlot({ create_frequency_plot(captures, "Facility") })
    output$capture_frequency_by_data <- renderPlot({ create_frequency_plot(captures, "Data Source") })
  }
  
  
  # create capture frequency plot for selected facilities
  create_frequency_plot <- function(captures, fill_property) {
    # count up the number of days each facility received a visit, differentiating by the variable indicated by fill property
    cap_days <- captures %>%
      group_by(facility_name, `Acquisition Date`, `Abbreviated Mission`, start_date, !!sym(fill_property)) %>%
      summarise(`Days Visited` = 1) %>%
      mutate(`Year` = as.numeric(substr(`Acquisition Date`, 1, 4))) %>%
      group_by(Year, !!sym(fill_property)) %>%
      summarise(`Days Visited` = sum(`Days Visited`)) %>%
      ungroup()
    
    # return the bar plot
    cap_days %>%
      ggplot(aes(x = Year)) +
      geom_col(aes(y = `Days Visited`, fill = !!sym(fill_property))) 
  }
  
  
  # render map for Search page on startup
  output$map2 <- renderLeaflet({
    facilities <- NULL
    
    if (input$searchmode == "Facility name") {
      fac_names <-  remove_country_name(input$facility)
      facilities <- facs %>% filter(facility_name %in% fac_names)
    } else if (input$searchmode == "Country") {
      facilities <- filter(facs, country %in% input$country)
    }
    
    map <- leaflet(options = leafletOptions(minZoom = 0,
                                            zoomControl = FALSE)) %>%
      addTiles() %>%
      addCircleMarkers(
        data = facilities,
        lng = facilities$lng,
        lat = facilities$lat,
        radius = 2.8,
        weight = 1,
        color = "#2a297b",
        opacity = 1,
        fillOpacity = 1,
        popup = ~ paste(facility_name, "<br>",
                        "Facility Start Date: ", start_date)
      ) %>%
      htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'bottomright' }).addTo(this)
      }")
    
    map
  })
  
  
  
  # --------------------------- EVENT HANDLERS ---------------------------------
  # re-create map whenever date slider is changed
  observeEvent(eventExpr = {
    input$dateSlider
  }, handlerExpr = {
    adjust_nas()
    populate_map1()
  })
  
  # re-create map when showUnkown checkbox is toggled
  observeEvent(eventExpr = {
    input$showUnknown
  }, handlerExpr = {
    adjust_nas()
    populate_map1()
  })
  
  # re-create map when showUnkown checkbox is toggled
  observeEvent(eventExpr = {
    input$showCaptures
  }, handlerExpr = {
    adjust_nas()
    populate_map1()
  })
  
  # search for a facility by name
  observeEvent(eventExpr = {
    input$facility
  }, handlerExpr = {
    fac_names <-  remove_country_name(input$facility)
    
    create_capture_table(fac_names)
    render_plots(fac_names)
  })
  
  # search for facilities by country
  observeEvent(eventExpr = {
    input$country
  }, handlerExpr = {
    facs_of_country <- filter(facs, country %in% input$country)
    fac_names <- unique(facs_of_country$facility_name)
    
    create_capture_table(fac_names)
    render_plots(fac_names)
  })
  
  
}

shinyApp(ui, server)



## SCRATCH


# # create capture plot
# capture_counts <- fac_caps %>%
#   mutate(`Abbreviated Mission` = substr(Mission,1,4)) %>%
#   filter(facility_name == fac_name) %>%
#   group_by(facility_name, `Acquisition Date`, `Abbreviated Mission`, start_date) %>%
#   summarise(n_caps = n())
#
# yearly_counts <- capture_counts %>%
#   mutate(`Year` = as.numeric(substr(`Acquisition Date`,1,4))) %>%
#   group_by(facility_name, Year) %>%
#   summarise(n_caps = sum(n_caps)) %>%
#   ungroup()
#
# yearly_counts %>%
#   ggplot(aes(x = Year, y = n_caps, group = 1)) +
#   geom_line()
#
# output$capture_plot <- renderPlot({
#   yearly_counts %>%
#     ggplot(aes(x = Year, y = n_caps, group = 1)) +
#     geom_col()
# })