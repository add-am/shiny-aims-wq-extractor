#load libraries
library(shiny)
library(bslib)
library(openxlsx2)
library(leaflet)
library(plotly)
library(purrr)
library(glue)
library(ncdf4)
library(xml2)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)


#build the UI that the user will see and interact with
ui <- page_navbar(

  #define the theme to be used by the UI
  theme = bs_theme(preset = "bootstrap",
                   primary = "#007bc2",
                   success = "#802918",
                   info = "#03c7e8", 
                   base_font = font_face(family = "Metropolis Regular",
                                         src = "url('../Metropolis-Regular.otf') format('opentype')")),

  #set the full title of the app (very top right)
  title = tags$span("AIMS MMP WQ Data Access Tool", class = "text-success"), 

  #create a navpanel (this is a multipage app)
  nav_panel(

    #set the title of the panel, this is the front page
    title = tags$span("Welcome", class = "text-success"),

    #define the layout of this nav panel
    layout_columns(
      card(#this contains the HWP logo
        style = "
          display: flex;
          justify-content: center;
          align-items: center;
        ",
        tags$img(
          src = "logo.jpg",
          style = "
            max-height: 100%;
            max-width: 100%;
            object-fit: contain;
            display: block;
          "
        )
      ),
      card(#this card contains the majority of the introductory text
        card_header(h1("Welcome")),
        p(
          'This tool has been developed to allow the easy download of AIMS MMP water quality logger data. The 
          main tool is found under the "Data Access" tab. Data are stored on a THREDDS server in the netCDF format 
          and require specific processing steps to access properly. These steps are explained in detail below.'
        ),
        p(
          "There are several additional resources reccomended should you wish to learn more about the methods or data.
          Please note that these resources are", tags$b("not"), "required to access or use the data:"
        ),
        tags$ul(
          tags$li(
            "A document detailing R methods can be found here: ", 
            tags$a(href = "n3_water-quality_new-mmp-data.html", target = "_blank", "Data Access Methods."),            
          ),          
          tags$li("An introduction to the netCDF file type can be found here:", 
            tags$a(href = "https://www.unidata.ucar.edu/software/netcdf/", "Unidata | NetCDF")
          ),
          tags$li("An explanation of the THREDDS server can be found here:",
            tags$a(href = "https://www.unidata.ucar.edu/software/tds/", "Unidata | TDS")
          ),
          tags$li("And a view of the raw data we are accessing can be found here:",
            tags$a(href = "https://thredds.aodn.org.au/thredds/catalog/AIMS/catalog.html", "AODN AIMS THREDDS Catalogue"))
        )
      ),
      card(#this card contains specific instructions on using the tool
        layout_columns(
          card(
            card_header(h5("1: Select Year(s)")),
            p("Pick the year or years of data you wish to visualise and download. Increasing the number of years 
            will increase the processing time.")
          ),
          card(
            card_header(h5("2: Select Logger(s)")),
            p("Pick the logger or loggers you wish to visualise and download. Increasing the number of loggers 
            will increase the processing time.")
          ),
          card(
            card_header(h5("3: Request Data")),
            p(strong("Manually"), 'click the "Send Request:" button. A pop-up will appear informing you of the
            progress.')
          ),
          card(
            card_header(h5("4: Visualise Data")),
            p("Data will be automatically visualised. You can interact with these visuals and adjust specific 
            parameters (see next).")
          ),
          card(
            card_header(h5("5: Flags and Aggregation")),
            p("Adjust the data quality flags to filter by data qualit (flags 1 and 2 reccomended). Adjust the 
            aggregation to present data as daily mean values, or as 10-minute intervals.")
          ),
          card(
            card_header(h5("6: Download Data")),
            p("Downloaded data using the button at the bottom right of the page. NOTE: There is
            currently a download size limit; 1 logger 1 year with 10-minute data, or several loggers/years with daily mean data.")
          ),
          col_widths = c(2,2,2,2,2,2)
        )
      ),
      col_widths = c(3,9, 12)
    )
  ),

  #create a second nav panel (i.e. page 2)
  nav_panel(

    #set the title
    title = tags$span("Data Access", calss = "text-success"),

    #define the layout of this nav panel, we want a sidebar for this one
    layout_sidebar(

      #this defines the stuff in the sidebar
      sidebar = sidebar(

        #pick the year(s)
        selectizeInput(
          "SelectedYears",
          "Select your year(s) below:",
          list(2023, 2024), #expand this later
          multiple = TRUE
        ),

        #pick the logger(s)
        selectizeInput(
          "SelectedLoggers",
          "Select your logger(s) below:",
          list("FTZ1", "FTZ2", "FTZ6", "WHI6", "WHI4", "WHI5", "BUR13", "WHI1", "BUR2", "BUR4", 
              "BUR1", "TUL10", "TUL3", "RM7", "RM10", "RM1", "RM8"),
          multiple = TRUE
        ),

        actionButton("UserSubmitted", "Send Request:"),

        #choose which data flags to keep
        tooltip(
          checkboxGroupInput(
            "DataFlagsKept",
            "Choose which data flags to keep (select to keep):",
            c(0,1,2,3,4,5),
            selected = c(1,2)
          ),
          "0 =  No_QC_performed \n1 = Good_data \n2 = Probably_good_data \n3 = Bad_data_that_are_potentially_correctable \n4 = Bad_data \n5 = Value_changed",
          placement = "right"
        ),

        #let the user decide if they want to aggregate to daily mean values
        input_switch(
          "DailyValues", 
          "Aggregate to daily values?",
          value = TRUE
        )
      ),

      #(we are now on the main page)

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
          )
        ),
        col_widths = c(8,4)
      )
    ),

  )
  
)

#build the server which contains and execute all of the R code
server <- function(input, output){

  #craft the correct url for the desired logger * year combo
  return_user_data <- bindEvent(
    reactive({

      withProgress( 
        message = 'Accessing Data', 
        detail = 'This may take a while...', 
        value = 0, {

          #access the year(s) that the user requested
          target_years <- input$SelectedYears

          #access the logger name(s) that the user requested
          target_loggers <- input$SelectedLoggers

          #create a pairwise combination of the years and loggers to form the inputs for each query
          target_matrix <- expand.grid(Years = target_years, Loggers = target_loggers)

          #map over year and logger lists to create unique urls, pull var and dim names, use this to extract data and build a df
          retrieve_data <- pmap(target_matrix, function(Years, Loggers){
            
            #create a url to the catalogue in xml format, based on year(s) selected
            catalogue_url <- glue("https://thredds.aodn.org.au/thredds/catalog/AIMS/Marine_Monitoring_Program/FLNTU_timeseries/{Years}/catalog.xml")

            incProgress(0.3, detail = "Building URL")
            Sys.sleep(0.2)

            #open the url as an object in R
            catalogue <- read_html(catalogue_url)

            #pull out the dataset variable from the raw xml
            nc_files <- xml_find_all(catalogue, ".//dataset")

            #pull out the id from this object (which is the name of each of the logger datasets)
            file_names <- xml2::xml_attr(nc_files, "id")

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

            incProgress(0.5, detail = "Accessing Metadata")
            Sys.sleep(0.2)

            #open the url
            nc <- nc_open(completed_url)

            #extract all variable and dimension names
            variable_names <- names(nc$var)
            dimension_names <- names(nc$dim)

            #replace the "timeseries" variable name, with the "time dimension name
            vec_of_data_names <- str_replace(variable_names, "TIMESERIES", dimension_names)

            #map over the vector and extract the data associated with each name. Store the result in a list
            target_data <- purrr::map(vec_of_data_names, function(x) ncvar_get(nc, x))
            
            #name each item in the list using the vector of variable and dimension names
            names(target_data) <- vec_of_data_names

            #extract the time vals
            time_vals <- target_data$TIME
            
            #assign an origin value to our "zero", make sure it has the UTC timezone, and contains hms
            time_origin <- ymd_hms("1950-01-01 00:00:00", tz = "UTC")

            #calculate new values by converting old values to absolute time intervals (purely total seconds), then adding that to our formatted origin
            time_vals <- time_origin + ddays(time_vals)

            #add 10 hours to bring time to EST
            time_vals <- time_vals + hours(10)

            #create a dataframe from the time, chla and turbidity values, plus their data flags
            simple_df <- data.frame(
              Time = time_vals, 
              Concentration_Chlorophyll = target_data$CPHL,
              Flags_Chlorophyll = target_data$CPHL_quality_control,
              Concentration_Turbidity = target_data$TURB,
              Flags_Turbidity = target_data$TURB_quality_control,
              Latitude = target_data$LATITUDE,
              Longitude = target_data$LONGITUDE
            )

            incProgress(0.7, detail = "Constructing Table")
            Sys.sleep(0.2)

            #add columns that track the year logger to the csv, plus the units for each variable
            simple_df <- simple_df |> 
              mutate(Logger = Loggers,
                    Year = Years,
                    Units_Chlorophyll = nc$var$CPHL$units,
                    Units_Turbidity = "NTU")
            
            #pivot the data longer, stacking turb and chla, and their flags
            pivot_df <- simple_df |> 
              pivot_longer(cols = c(Concentration_Chlorophyll, Flags_Chlorophyll, Units_Chlorophyll, Concentration_Turbidity, Flags_Turbidity, Units_Turbidity),
                          names_to = c(".value", "Indicator"),
                          names_pattern = "(.*)_(.*)")
            

            #return the df as an element in the over arching list
            return(pivot_df)

          })

          #combine the list of dataframes into one large dataframe
          final_df <- bind_rows(retrieve_data)

          #return this single df as the final output of the reactive function
          return(final_df)
        }
      )
    }), input$UserSubmitted

  )

  #if the user wants to filter by flag, do that
  filtered_user_data <- reactive({

    #pull data from previous func
    data <- return_user_data()

    #filter the data by the requested flags
    data_filtered <- data |> 
      filter(Flags %in% input$DataFlagsKept)
    
    #return the filtered data
    return(data_filtered)
  })

   #if the user wants to aggregate data, do that
  aggregate_user_data <- reactive({

    #pull data from previous func
    data <- filtered_user_data()

    #pull out date values (i.e. separate time and day)
    data <- data |> 
      rename(DateTime = Time) |> 
      separate_wider_delim(cols = DateTime, delim = " ", names = c("Date", "Time"), cols_remove = FALSE) |> 
      mutate(Date = ymd(Date),
             Time = hms(Time))

    #if switch is true, aggregate, otherwise do nothing
    if(input$DailyValues){

      #group by date and summarise
      data <- data |> 
        group_by(Date, Latitude, Longitude, Logger, Indicator, Units) |> 
        summarise(Concentration = mean(Concentration, na.rm = T),
                  Flags = as.numeric(paste(unique(Flags), collapse = ", ")))
    }
    
    #return the aggregated data
    return(data)

  })

  #create map
  output$map <- renderLeaflet({

    req(input$SelectedLoggers)

    #create the base map (not logger points)
    base_map <- leaflet() |> 
      addTiles() |> 
      setView(lng = 154, lat = -19, zoom = 5) 

    #if the user has created a logger table
    if (!is.null(return_user_data())) {
      
      #get a df of just unique lat and longs
      unique_lat_lon <- return_user_data() |> 
        select(Logger, Latitude, Longitude) |> 
        unique()

      base_map <- base_map |> 
        addMarkers(
          data = unique_lat_lon,
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~Logger)
    }

    #return the map
    base_map
  })

  #create plot
  output$plot <- renderPlotly({

    req(input$SelectedLoggers)

    #if switch is true, set x axis to date
    if (input$DailyValues){

      #create a simple plot
      p <- ggplot(aggregate_user_data(), aes(x = Date, y = Concentration, colour = Logger)) +
        geom_line() +
        facet_wrap(
          ~ Indicator, 
          ncol = 1, 
          scales = "free_y",
          labeller = as_labeller(c(Chlorophyll = "Chlorophyll Concentration (mg/m-3)", Turbidity = "Turbidity (NTU)")), 
          strip.position = "top"
        ) +
        labs(y = NULL) +
        theme_bw() +
        theme(strip.background = element_blank(), strip.placement = "outside")

    } else { #set x axis to DateTime

      #create a simple plot
      p <- ggplot(aggregate_user_data(), aes(x = DateTime, y = Concentration, colour = Logger)) +
        geom_line() +
        facet_wrap(
          ~ Indicator, 
          ncol = 1, 
          scales = "free_y",
          labeller = as_labeller(c(Chlorophyll = "Chlorophyll Concentration (mg/m-3)", Turbidity = "Turbidity (NTU)")), 
          strip.position = "top"
        ) +
        labs(y = NULL) +
        theme_bw() +
        theme(strip.background = element_blank(), strip.placement = "outside")

    }

    ggplotly(p)
  })

  #link the data to the download button
  output$DownloadData <- downloadHandler(
    filename = function() {
      
      #get all logger names and collapse into single string separated by a dash
      log_name <- paste(input$SelectedLoggers, collapse = "-")

      #get all logger dates and collapse into single string separated by a dash
      log_date <- paste(input$SelectedYears, collapse = "-")

      #combine name and logger into a single file name
      file_name <- paste0(paste(log_name, log_date, collapse = "_"), ".xlsx")

      #return the file name as the final output for the function
      return(file_name)
    },
    
    #takes a file path, and writes an object to that path
    content = function(file){

      #translate T/F to human readable
      if (input$DailyValues) {day_ag <- "Daily"} else {day_ag <- "10-minute intervals"}
      
      #build a dataframe the contains a record of metadata changes
      metadata <- data.frame(
        Key = c(
          "Date Accessed:", 
          "Data Aggregation:", 
          "Flags Kept:", 
          "Loggers Accessed:", 
          "Years Requested:"),
        Value = c(
          as.character(Sys.Date()), 
          day_ag, 
          paste(input$DataFlagsKept, collapse = ", "),
          paste(input$SelectedLoggers, collapse = ", "),
          paste(input$SelectedYears, collapse = ", ")
        )
      )

      wb <- wb_workbook() |> 
        wb_add_worksheet("Metadata") |> 
        wb_add_worksheet("Data") |> 
        wb_add_data("Metadata", metadata) |> 
        wb_add_data("Data", aggregate_user_data())

      wb_save(wb, file)

  })  

}


#combine the ui and server elements
shinyApp(ui = ui, server = server)

