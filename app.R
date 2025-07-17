#load libraries
library(shiny)
library(bslib)
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
      card(
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
        card_header(h1("Instructions")),
        h3("Introduction:"),
        p(
          "This tool has been developed to allow the easy download of AIMS (Australian Institute of Marine Science) MMP (Marine Monitoring 
          Program) water quality data, specifically logger data. The data are stored on a THREDDS server in the netCDF 
          format, and require specific processing steps to access properly."
        ),
        p(
          "On the second page of this app you can find a user interface that will allow you to conduct a preliminary exploration of the
          AIMS MPP water quality data. In the user interface you can:"
        ),
        tags$ul(
          tags$li("Select data by year,"),
          tags$li("Select data by logger,"),
          tags$li("Filter data by quality code,"),
          tags$li("Return data in 10 minute intervals or as daily averages,"),
          tags$li("Visualise data on an interactive lineplot and interactive map, and"),
          tags$li("Download selected data as a CSV")
        ),
        h3("Additional Resources:"),
        p(
          "There are several additional resources reccomended should you wish to learn more about the methods or data.
          Please note that these resources are", tags$b("not"), "required to access or use the data:"
        ),
        tags$ul(
          tags$li("A document detailing R methods can be found here: [link tbd], this includes:",
            tags$ul(
              tags$li("How to find data,"),
              tags$li("Methods to learn about data type and quality,"),
              tags$li("How to automatically extract data, and"),
              tags$li("How to conduct preliminary analyis"),
            )
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
      col_widths = c(6,6)
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
  return_user_data <- reactive({

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

      #add columns that track the year and logger to the csv
      simple_df <- simple_df |> 
        mutate(Logger = Loggers,
               Year = Years)
      
      #pivot the data longer, stacking turb and chla, and their flags
      pivot_df <- simple_df |> 
        pivot_longer(cols = c(Concentration_Chlorophyll, Flags_Chlorophyll, Concentration_Turbidity, Flags_Turbidity),
                     names_to = c(".value", "Indicator"),
                     names_pattern = "(.*)_(.*)")
      

      #return the df as an element in the over arching list
      return(pivot_df)

    })

    #combine the list of dataframes into one large dataframe
    final_df <- bind_rows(retrieve_data, .id = "column_label")

    #return this single df as the final output of the reactive function
    return(final_df)

  })

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
        group_by(Date, Logger, Indicator) |> 
        summarise(Concentration = mean(Concentration, na.rm = T))
    }
    
    #return the aggregated data
    return(data)

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

            #create a simple plot
      p <- ggplot(final_df, aes(x = DateTime, y = Concentration, colour = Logger)) +
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

  #download button  

}


#combine the ui and server elements
shinyApp(ui = ui, server = server)

