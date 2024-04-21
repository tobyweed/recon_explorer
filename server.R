##  ------------------------------------------------------------------------- ##
##  ------------------------------ SERVER     ------------------------------- ##
##  ------------------------------------------------------------------------- ##

library(data.table)

server <- function(input, output, session) {
  
  state <- reactiveValues(prev_date = min_date,
                          fac_total = facs)
  
  
  ##  ----------------------------------------------------------------------- ##
  ##  ----------------------------    MAP     ------------------------------- ##
  ##  ----------------------------------------------------------------------- ##
  
  # -------------------------------- UTILS -------------------------------------
  # if the Show Unknown Facilities button isn't activated, remove facilities with unknown start dates from consideration
  adjust_nas <- function() {
    state$facs_total <- facs
    
    if (input$showUnknown) {
      state$facs_total$start_date <-
        replace(state$facs_total$start_date,
                is.na(state$facs_total$start_date),
                min_date) # replace NA values with the min slider date
    } else {
      state$facs_total <-
        state$facs_total %>% filter(!is.na(start_date)) #filter them out
    }
  }
  
  # wrapper functions for adding markers so I don't have to re-enter parameters that don't change
  add_fac_markers <- function(map, facs, color = "#2a297b", type = "uncapped") {
    map %>% addCircleMarkers(
      data = facs,
      layerId=paste(as.character(facs$facility_name),type),
      lng = ~ lng, lat = ~ lat,
      radius = 2.8,
      weight = 1,
      color = color,
      opacity = 1,
      fillOpacity = 1
      # popup = ~ paste(facility_name, "<br>", "Facility Start Date: ", start_date,
      #                ifelse(input$showCaptures, "<br>Not Yet Photographed", ""))
    )
  }
  
  # completely reset the markers
  reset_map1 <- function() {
    leafletProxy(mapId = 'map1') %>%
      clearMarkers()
    
    current_date <- input$dateSlider
    update_map1( prev_date = min_date, current_date = current_date) # set the previous date to the minimum date so all markers are re-drawn
    state$prev_date <- current_date
  }
  
  # add markers ONLY where there should be markers. Don't re-add existing markers or take away old markers.
  update_map1 <- function(prev_date, current_date) {
    
    if(prev_date < current_date) {
      fac_exist <- state$facs_total[state$facs_total$start_date >= prev_date & state$facs_total$start_date <= current_date]
      fac_low <- state$facs_total[state$facs_total$cap_date_low_res >= prev_date & state$facs_total$cap_date_low_res <= current_date]
      fac_high <- state$facs_total[state$facs_total$cap_date_high_res >= prev_date & state$facs_total$cap_date_high_res <= current_date]

      # draw markers
      leafletProxy(mapId = 'map1') %>%
        add_fac_markers(facs = fac_exist) %>%
        add_fac_markers(facs = fac_low,
                        color = "green",
                        type = "capped_low") %>%# add low res markers
        add_fac_markers(facs = fac_high,
                        color = "red",
                        type = "capped_high") # add high res markers
      
    } else if(prev_date > current_date) {
      fac_exist <- state$facs_total[state$facs_total$start_date <= prev_date & state$facs_total$start_date >= current_date]
      fac_low <- state$facs_total[state$facs_total$cap_date_low_res <= prev_date & state$facs_total$cap_date_low_res >= current_date]
      fac_high <- state$facs_total[state$facs_total$cap_date_high_res <= prev_date & state$facs_total$cap_date_high_res >= current_date]
      
      leafletProxy(mapId = 'map1') %>%
        removeMarker(paste(as.character(fac_exist$facility_name),"uncapped")) %>%
        removeMarker(paste(as.character(fac_low$facility_name),"capped_low")) %>%
        removeMarker(paste(as.character(fac_high$facility_name),"capped_high"))
    }
  }
  
  # map for Map page. run on startup. Adjust NAs and filter by date
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 0,
                                     zoomControl = FALSE,
                                     scrollWheelZoom = FALSE)) %>%
      addTiles() %>%
      setView(lng = 0,
              lat = 30,
              zoom = 2) %>%
      # addControl(position = "topright") %>%
      htmlwidgets::onRender("function(el, x) {
          L.control.zoom({ position: 'bottomright' }).addTo(this)
      }")
  })
  
  
  # --------------------------- EVENT HANDLERS ---------------------------------
  # re-create map whenever date slider is changed
  observeEvent(eventExpr = {
    input$dateSlider
  }, handlerExpr = {
    current_date <- input$dateSlider
    update_map1(state$prev_date, current_date)
    state$prev_date <- current_date
  })
  
  # re-create map when showUnknown checkbox is toggled
  observeEvent(eventExpr = {
    input$showUnknown
  }, handlerExpr = {
    adjust_nas()
    reset_map1()
  })
  
  
  
  
  
  ##  ----------------------------------------------------------------------- ##
  ##  ----------------------------  EXPLORE   ------------------------------- ##
  ##  ----------------------------------------------------------------------- ##
  
  
  # -------------------------------- UTILS -------------------------------------
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
          lengthMenu = c(5, 15, 50, 100, 1000), # set the options for the number of entries per page
          pageLength = 15 # set the default number of entries per page)
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
             # Resolution = ifelse(`Camera Resolution` %in% c("Stereo High", "Vertical High", "2 to 4 feet"), "high", "low"),
             Resolution = `Camera Resolution (General)`,
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




#SCRATCH

# # return a vector of popup windows for captured facilities or missiles
# # NEW: name, start date, capture status (not yet photographed, capped in low def, capped in HD), click to see facility page
# get_popups <- function(caps, is_facs) {
#   popups <- c()
#   
#   # fill in fields of popup windows for markers
#   if (nrow(caps) >= 1) {
#     for (i in 1:nrow(caps)) {
#       popup <-
#         paste(
#           ifelse(
#             is_facs,
#             paste(caps$facility_name[i], "<br>"),
#             "Missile Site <br>"
#           ),
#           "Start Date: ",
#           caps$start_date[i],
#           "<br>",
#           "Most Recently Photographed: ",
#           caps$`Acquisition Date`[i],
#           "<br>",
#           a(
#             "Click to See Image and Metadata",
#             href = caps$pic_URL[i],
#             target = "_blank"
#           )
#         )
#       
#       popups <- c(popups, popup)
#     }
#   }
#   
#   popups
# }
# 


# add_miss_markers <- function(map, miss) {
#   map %>% addMarkers(data = miss,
#                      lng = ~lng, lat = ~lat,
#                      icon = makeIcon(
#                        iconUrl = "img/tri_#2a297b.png",
#                        iconWidth = 7.5, iconHeight = 7.5
#                      ),
#                      popup = ~paste("Missile Site", "<br>Start Date: ", start_date,
#                                     ifelse(input$showCaptures, "<br>Not Yet Photographed", "")))
# }



# # add markers where there should be markers
# populate_map1 <- function() {
#   # update_facs_exist () # update the displayed facilities
#   # update_facs_low()
#   # update_facs_high()
#   # print("populate map")
#   # # state$toggle_markers <- !state$toggle_markers
#   # 
#   # leafletProxy(mapId = 'map1') %>%
#   #   clearMarkers() %>%
#   #   add_fac_markers(facs = state$facs_exist) %>% # markers for extant facilities
#   #   add_fac_markers(facs = state$facs_low,
#   #                   color = "green",
#   #                   type = "capped_low") %>% # add low res markers
#   #   add_fac_markers(facs = state$facs_high,
#   #                   color = "red",
#   #                   type = "capped_high") %>% # add high res markers
#   #   add_miss_markers(miss = miss_filtered_by_dates()) # markers for extant missile sites
#   
#   # print(state$toggle_markers)
#   # show_caps()
# }


# # filter for missiles which have been captured by date selected on slider
# captured_miss <- function() {
#   miss_caps %>% filter(address %in% miss_filtered_by_dates()$address, # make sure the missile exists on the map right now
#                        `Acquisition Date` <= input$dateSlider) %>%  # make sure the capture occurred before the present date
#     group_by(address) %>% # group by site
#     filter(`Acquisition Date` == max(`Acquisition Date`)) %>% # keep the most recent miss_caps
#     distinct(address, .keep_all = TRUE) # make sure only one capture of each missile is present.
# }