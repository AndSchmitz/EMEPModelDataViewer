# EMEP MSC-W modelled air concentrations and depositions
# Visalization is based on EMEP web map services. The data show are 2000-2016 re-calculations (Type 2) on daily level.
# See EMEP website for details https://www.emep.int/mscw/mscw_ydata.html

library(shiny)
library(leaflet)
library(leaflet.extras)
library(stringr)

#List all available days
FirstYear <- 2000
LastYear <- 2016
DateDOYs <- seq(
    from = as.Date(paste0(FirstYear,"-01-01")),
    to = as.Date(paste0(LastYear,"-12-31")),
    by = 1
)

#List all available layers
#Extracting layers from 2000 data, assuming available layers stay the same between
#2000 and 2016
WMSCapabilitiesURL <- "https://thredds.met.no/thredds/wms/data/EMEP/2019_Reporting/EMEP01_L20EC_rv4_33_day.2000met_2000emis_rep2019.nc?service=WMS&version=1.3.0&request=GetCapabilities"
WMSCapabilities <- readLines(
    con = WMSCapabilitiesURL
)
idx_LayerNames <- str_which(
    string = WMSCapabilities,
    pattern = "<Layer queryable"
)
LayerNames <- WMSCapabilities[idx_LayerNames+1] %>%
    str_extract(
        pattern = ">.+<"
    ) %>%
    str_replace(
        pattern = "<",
        replacement = ""
    ) %>%
    str_replace(
        pattern = ">",
        replacement = ""
    ) 



#Define user interface component
ui <- bootstrapPage(

    titlePanel("EMEP MSC-W modelled air concentrations and depositions"),
    p("Visalization is based on EMEP web map services. The data shown are 2000-2016 re-calculations (Type 2) on daily level."),
    a("See EMEP website for details",href = "https://www.emep.int/mscw/mscw_ydata.html"),
    #Adding some vertical blank space:
    fluidRow(
        column(
            width = 6, 
            style='padding-top:20px'
        )
    ),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    
    leafletOutput(
        "mymap",
        width = "100%",
        height = "100%"
    ),
            
    absolutePanel(
        top = 170,
        right = 30,
        
        sliderInput(
            inputId = "time",
            label = "Date",
            min = min(DateDOYs),
            max = max(DateDOYs),
            value = min(DateDOYs),
            step=1,
            animate = animationOptions(
                #Update interval in ms. If faster than
                #WMS image download, no data is visible
                interval = 1000
            )
        ),
            
        selectInput(
            inputId = "LayerSelection",
            label = "Data layer",
            choices = LayerNames,
            selected = "SURF_ug_PM25" #defaulting to colorful PM2.5 maps
        )

    ) #end of absolutePanel
) #end of UI



#Define server component
server <- function(input, output) {

    #Handle updates of user inputs
    UserSelection <- reactive({
        TimeString <- paste0(input$time,"T18:00:00.000Z")
        CurrentYear <- format(input$time, format = "%Y")
        DataBaseURL <- paste0(
            "https://thredds.met.no/thredds/wms/data/EMEP/2019_Reporting/EMEP01_L20EC_rv4_33_day.",
            CurrentYear,
            "met_",
            CurrentYear,
            "emis_rep2019.nc?service=WMS"
        )
        Layer <- input$LayerSelection
        return(
            list(
                TimeString = TimeString,
                DataBaseURL = DataBaseURL,
                Layer = Layer
            )
        )
    })

    #Legends can be taken from URLs like this:
    #https://thredds.met.no/thredds/wms/data/EMEP/2019_Reporting/EMEP01_L20EC_rv4_33_day.2008met_2008emis_rep2019.nc?REQUEST=GetLegendGraphic&LAYER=SURF_ug_PM10_rh50&PALETTE=uemep_pm10_palette
    #See the following URL for a full list of legends:
    #https://thredds.met.no/thredds/wms/data/EMEP/2019_Reporting/EMEP01_L20EC_rv4_33_day.2008met_2008emis_rep2019.nc?service=WMS&version=1.3.0&request=GetCapabilities
    #Unfortunately, all legends seem to go from -50 to 50 (not useable)
    LegendURIString <-  reactive({
        CurrentYear <- format(input$time, format = "%Y")
        CurrentLegendURIString <- paste0(
            "https://thredds.met.no/thredds/wms/data/EMEP/2019_Reporting/EMEP01_L20EC_rv4_33_day.",
            CurrentYear,
            "met_",
            CurrentYear,
            "emis_rep2019.nc?REQUEST=GetLegendGraphic&LAYER=SURF_ugN_NOX"
        )
        return(CurrentLegendURIString)
    })

    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(33, 65, zoom = 3) 
            # addWMSLegend(
            #     uri = LegendURIString(),
            #     position = "bottomleft"
            # )
    })
    
    #Update map on updates of user input
    observeEvent(
        eventExpr = UserSelection(),
        handlerExpr = {
            #Use leafletProxy to not redraw underlying basemap on each update
            leafletProxy("mymap") %>%
                removeTiles(
                    layerId = "EMEPTile"
                ) %>%
                addWMSTiles(
                    layerId = "EMEPTile",
                    baseUrl = UserSelection()$DataBaseURL,
                    layers = UserSelection()$Layer,
                    options = WMSTileOptions(
                        time = UserSelection()$TimeString,
                        format = "image/png",
                        transparent = TRUE,
                        opacity = 0.5
                    )
                )
        }
    )

} #end of server function

# Run the application 
shinyApp(ui = ui, server = server)
