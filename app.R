#load libraries
library(shiny)
library(bslib)
library(leaflet)
library(plotly)

#build the UI that the user will see and interact with
ui <- page_sidebar(

  sidebar = sidebar(

    #pick the year(s)
    selectizeInput(
      "SelectedYears",
      "Select your year(s) below:",
      list(2023, 2024) #expand this later
    ),

    #pick the logger(s)
    selectizeInput(
      "SelectedLoggers",
      "Select your logger(s) below:",
      list("FTZ1", "FTZ2", "FTZ6", "WHI6", "WHI4", "WHI5", "BUR13", "WHI1", "BUR2", "BUR4", 
           "BUR1", "TUL10", "TUL3", "RM7", "RM10", "RM1", "RM8")
    ),

    #choose which data flags to keep
    checkboxGroupInput(
      "DataFlagsKept",
      "Choose which data flags to keep (select to keep):",
      c(
        "0: No_QC_performed" = 0,
        "1: Good_data" = 1,
        "2: Probably_good_data" = 2,
        "3: Bad_data_that_are_potentially_correctable" = 3,
        "4: Bad_data" = 4,
        "5: Value_changed" = 5
      ),
      selected = c(
        "1: Good_data" = 1,
        "2: Probably_good_data" = 2
      )
    ),

    #let the user decide if they want to aggregate to daily mean values
    input_switch(
      "DailyValues", 
      "Do you want to aggregate to daily values? (Yes by default)",
      value = TRUE
    
    ) #end of sidebar
  
    
    #aggregate to daily values (or keep at 10min intervals) -- switch

  ),

  #create two columns
  layout_columns(
    card(

      #create the line plot (interactive)
      plotlyOutput("plot")

    ),
    card(#within this card stack the map above the download button
      card(
        #create the interactive map
        leafletOutput("map")
      ),
      card(
        #create the download button
        downloadButton("DownloadData", "Download your data here."),
        max_height = "100px"
      ),

    ),
    col_widths = c(8,4)
  )

  #line plot(s)

  #interactive map of logger location

  #download the data -- download button

)

#build the server which contains and execute all of the R code
server <- function(input, output){

  #craft the correct url for the desired logger * year combo
  craft_url <- reactive({

    #access the year(s) that the user requested
    target_years <- input$SelectedYears

    #access the logger name(s) that the user requested
    target_loggers <- input$SelectedLoggers

    #create a pairwise combination of the years and loggers to form the inputs for each query
    target_matrix <- expand.grid(Years = target_years, Loggers = target_loggers)

    #map over year and logger lists
    logger_date_pair <- pmap(target_matrix, function(Years, Loggers){
      
      #create a url to the catalogue in xml format, based on year(s) selected
      catalogue_url <- glue("https://thredds.aodn.org.au/thredds/catalog/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/catalog.xml")

      #open the url as an object in R
      catalogue <- read_html(catalogue_url)

      #pull out the dataset variable from the raw xml
      nc_files <- xml_find_all(catalogue, ".//dataset")

      #pull out the id from this object (which is the name of each of the logger datasets)
      file_names <- xml_attr(nc_files, "id")

      #create a vector of logger names
      logger_names <- str_extract_all(file_names, "_.{3,5}_(?=FV01)")
      logger_names <- str_remove_all(unlist(logger_names), "_")

      #create a vector of logger deployment dates
      logger_dates <- str_extract_all(file_names, "\\d{8}(?=Z)")
      logger_dates <- str_remove_all(unlist(logger_dates), "_")

      #record the index for each logger name that matches what the user requested
      logger_indicies <- which(logger_names %in% Loggers)

      #extract the logger deployment dates associated with those loggers using the index determined based on name
      logger_dates <- logger_dates[logger_indicies]
      
      #build the completed url
      completed_url <- glue("https://thredds.aodn.org.au/thredds/dodsC/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/AIMS_MMP-WQ_KUZ_{logger_dates}Z_{Loggers}_FV01_timeSeries_FLNTU.nc")

    })

    #return the list as the final output of the function
    return(logger_date_pair)  
  })

  #extract data from urls that were crafted above
  extracted_data <- reactive({

    #pull urls from the reactive function
    all_data_requested <- map(logger_date_pair, function(x){

      #open the url
      nc <- nc_open(x)

      #extract all variable and dimension names
      variable_names <- names(nc$var)
      dimension_names <- names(nc$dim)

      #replace the timeseries variable name, with the time dimension name
      vec_of_data_names <- str_replace(variable_names, "TIMESERIES", dimension_names)

      #map over the vector and extract the data associated with each name. Store the result in a list
      target_data <- purrr::map(vec_of_data_names, function(x) ncvar_get(nc, x))
      
      #name each item in the list
      names(target_data) <- vec_of_data_names

      #return that list as a sublist under our main list that has one item per url
      return(target_data)

      })
    
    #return the list that contains sublists. Each list relates to one url, each sublist contains all variable information from the url
    return(all_data_requested)
  })

  #combine data if multi year

  #tbd:
      #extract the current time vals
      #old_time_vals <- target_data$TIME

      #assign an origin value to our "zero", make sure it has the UTC timezone, and contains hms
      #time_origin <- ymd_hms("1950-01-01 00:00:00", tz = "UTC")

      #calculate new values by converting old values to absolute time intervals (purely total seconds), then adding that to our formatted origin
      #new_time_vals <- time_origin + ddays(old_time_vals)
      #note that ddays() is not a typo, it stands for duration (rather than using days as a unit)

  #finalise data into a csv

  #extract logger location

  #create map
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      setView(lng = 154, lat = -19, zoom = 5)#need to define view based on lat and long of logger
  })

  #flag filter
  
  #aggregation (T/F)

  #create plot
  output$plot <- renderPlotly({
    #create my plot in here
  })

  #download button  

}


#combine the ui and server elements
shinyApp(ui = ui, server = server)

