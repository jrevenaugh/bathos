require(shiny)
require(leaflet)
require(leaflet.extras)
require(tidyverse)
require(magrittr)
require(plotly)
require(RColorBrewer)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # REACTIVES -------------------------------
  # Reactive expression for recentering earthquake data
  eqData <- reactive({
    tcat <- eqcat
    if ( !input$gmt == "gmt" ) { tcat <- tcat %>%
      mutate( Longitude = ifelse( Longitude < 0,
                                  360 + Longitude,
                                  Longitude ) )
    }
    tcat
  })

  # Reactive expression for recentering volcano data
  volData <- reactive({
    vols <- volcanoes
    if ( input$gmt == "pac" ) { vols <- vols %>%
      mutate( Longitude = ifelse( Longitude < 0,
                                  360 + Longitude,
                                  Longitude ) )
    }
    vols
  })

  # ReactiveValues to hold elevation selection
  elevation <- reactiveValues( line = NA, rect = NA, shape = NA )

  # TabPanel 1 -----------------------------------------------------------------
  # Leaflet map with optional seismicity, volcanoes and plate boundaries
  # Also implements data selection for analysis
  output$map <- renderLeaflet({
    m <- leaflet( options = leafletOptions( minZoom = 2,
                                            maxZoom = 9,
                                            worldCopyJump = FALSE,
                                            preferCanvas = TRUE ) ) %>%
      addProviderTiles( providers$Esri.OceanBasemap ) %>%
      addScaleBar( position = "bottomright" ) %>%
      addDrawToolbar(
        targetGroup = 'Selected',
        singleFeature = TRUE,
        markerOptions = FALSE,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        circleMarkerOptions = FALSE,
        polylineOptions = drawPolylineOptions(allowIntersection = FALSE,
                                              metric = TRUE,
                                              shapeOptions = drawShapeOptions(
                                                fillOpacity = 0,
                                                color = "white",
                                                weight = 3)),
        rectangleOptions = drawRectangleOptions(metric = TRUE,
                                                shapeOptions = drawShapeOptions(
                                                  fillOpacity = 0,
                                                  color = "white",
                                                  weight = 3)),
        editOptions = editToolbarOptions(edit = FALSE,
                                         selectedPathOptions = selectedPathOptions())
      ) %>%
      addCircles( lng = ~Longitude, lat = ~Latitude,
                  data = eqData(),
                  group = "quakes",
                  color = "white",
                  weight = 1,
                  fillColor = ~eqColorPal( dCol ),
                  fillOpacity = 0.5,
                  radius = ~1.5 * exp( ( Magnitude - 5 ) / 1.5 ) * 10000 ) %>%
      addCircles( lng = ~Longitude, lat = ~Latitude,
                  data = volData(),
                  group = "volcanoes",
                  color = "black",
                  weight = 1,
                  fillColor = "darkorange",
                  fillOpacity = 0.5,
                  radius = 5 * 10000 )

    for (i in unique(limit_pac$group)) {
      m %<>%
        addPolylines( data = limit_pac[limit_pac$group == i, ],
                      lng = ~Longitude, lat = ~Latitude,
                      group = "limit_pac",
                      weight = 2.5, opacity = 1, color = "red" )
    }
    for (i in unique(limit_gmt$group)) {
      m %<>%
        addPolylines( data = limit_gmt[limit_gmt$group == i, ],
                      lng = ~Longitude, lat = ~Latitude,
                      group = "limit_gmt",
                      weight = 2.5, opacity = 1, color = "red" )
    }
    pb <- plates$gmt
    for (i in unique(pb$group)) {
      m %<>%
        addPolylines( data = pb[pb$group == i, ],
                      lng = ~Longitude, lat = ~Latitude,
                      group = "plates_gmt",
                      weight = 2, opacity = 0.5, color = "black" )
    }
    pb <- plates$pac
    for (i in unique(pb$group)) {
      m %<>%
        addPolylines( data = pb[pb$group == i, ],
                      lng = ~Longitude, lat = ~Latitude,
                      group = "plates_pac",
                      weight = 2, opacity = 0.5, color = "black" )
    }
    m
  })

  # Reset view for change of map center
  observeEvent( input$gmt, {
    proxy <- leafletProxy( "map" )
    if ( input$gmt == "gmt" ) {
      proxy %<>%
        setView( 0, 0, 3 ) %>%
        setMaxBounds( -180, -70, 180, 70 ) %>%
        hideGroup("limit_pac") %>%
        showGroup("limit_gmt")
    } else {
      proxy %<>%
        setView( 180, 0, 3 ) %>%
        setMaxBounds( 0, -70, 360, 70 ) %>%
        hideGroup("limit_gmt") %>%
        showGroup("limit_pac")
    }
    proxy
  })

  # Reset markers for changes in visibility
  observeEvent( c( input$features,
                   input$gmt ), {
                     proxy <- leafletProxy("map")
                     if (!("quakes" %in% input$features)) {
                       proxy %<>%
                         hideGroup("quakes")
                     } else {
                       proxy %<>%
                         showGroup("quakes")
                     }
                     if (!("volcanoes" %in% input$features)) {
                       proxy %<>%
                         hideGroup("volcanoes")
                     } else {
                       proxy %<>%
                         showGroup("volcanoes")
                     }
                     if (!("plates" %in% input$features)) {
                       proxy %<>%
                         hideGroup("plates_gmt") %>%
                         hideGroup("plates_pac")
                     } else if ( input$gmt == "gmt" ) {
                       proxy %<>%
                         showGroup("plates_gmt")
                     } else {
                       proxy %<>%
                         showGroup("plates_pac")
                     }
                     proxy
                   })


  # Pop up help for map centering/selection
  observeEvent( input$gethelp, {
    showModal( modalDialog(
      title = "Selection",
      HTML( paste( "Use the line or rectangle shape tool to select regions for visualization",
                   "and hypsometry.",
                   "Select the trash can to remove shape or simply create a new one",
                   "and the old one will be removed automatically.",
                   tags$br(), tags$br(),
                   "Do not extend a selection across either vertical red line.",
                   "If the region you wish to select crosses a red line, change",
                   "the map's centering.") ),
      easyClose = TRUE )
    )
  })

  # Observe input map shape, process based on new selection shape
  observeEvent( input$map_draw_new_feature, {
    feature_type <- input$map_draw_new_feature$properties$feature_type
    if (feature_type == "polyline") {
      elevation$line <- findCross(input$map_draw_new_feature, etopo)
      elevation$shape <- "polyline"
    } else if (feature_type == "rectangle") {
      elevation$rect <- findElev(input$map_draw_new_feature, etopo)
      elevation$shape <- "rectangle"
    } else {
      elevation$shape <- NA
    }
  })

  # TabPanel 2 -----------------------------------------------------------------
  # Visualization (profile or surface)

  output$profile <- renderPlotly({
    # Determine plot type
    if (is.na(elevation$shape)) return()

    # Produce plotly topographic profile
    if (elevation$shape == "polyline") {
      profile <- elevation$line$profile
      coords <- elevation$line$coords
      p <- plotlyProfile(profile, coords, input$vertExag)
    }

    # Produce plotly surface object
    if (elevation$shape == "rectangle") {
      elev <- elevation$rect
      p <- plotlySurface(elev, input$vertExag, bathColorPal, input$waterOpacity)
    }

    p
  })

  # TabPanel 3 -----------------------------------------------------------------
  # Hypsometry plot and statistics table

  output$ghypso <- renderPlotly({
    if (is.na(elevation$shape)) return()

    # Assemble elevation dataframe for plot output; process depends on selection shape
    if (elevation$shape == "polyline") {
      profile <- elevation$line$profile
      coords <- elevation$line$profile
      elev <- data.frame(z = sort(profile$Elevation))
      elev$d <- 1:dim(elev)[1] / dim(elev)[1]
    }
    if (elevation$shape == "rectangle") {
      obj <- elevation$rect
      elev <- data.frame( z = as.numeric(t( obj$z ) ) / 1000, y = obj$y)
      elev %<>% arrange(z) %>%
        mutate(wgt = cos(y * pi /180)) %>%
        mutate(d = cumsum(wgt)) %>%
        mutate(d = (d - min(d))/max(d))
    }
    p <- plotlyHypso(elev)
    p
  })

  output$thypso <- renderTable({
    if (is.na(elevation$shape)) return()

    # Assemble statistics for table output; process depends on selection shape
    if (elevation$shape == "polyline") {
      elev <- data.frame(z = elevation$line$profile$Elevation)
      elev$wgt <- cos(as.numeric(elevation$line$coords[,2]) * pi /180)
      obj <- data.frame(x = as.numeric(elevation$line$coords[,1]),
                        y = as.numeric(elevation$line$coords[,2]))
    }
    if (elevation$shape == "rectangle") {
      obj <- elevation$rect
      elev <- data.frame( z = as.numeric( t( obj$z ) ) / 1000, y = obj$y )
      elev %<>% arrange(z) %>%
        mutate(wgt = cos(y * pi /180))
    }

    emin <- min( elev$z, na.rm = TRUE )
    emax <- max( elev$z, na.rm = TRUE )
    emean <- sum( elev$z * elev$wgt, na.rm = TRUE ) / sum( elev$wgt, na.rm = TRUE )
    if (elevation$shape == "polyline") {
      # Using endpoints (fudges multiple segment profiles--oh well...)
      n <- nrow(elevation$line$coords)
      first <- elevation$line$coords[1,]
      last <- elevation$line$coords[n,]
      elonmin <- min(first[1], last[1])
      elonmax <- max(first[1], last[1])
      elatmin <- min(first[2], last[2])
      elatmax <- max(first[2], last[2])
    } else {
      elonmin <- min( obj$x )
      elonmax <- max( obj$x )
      elatmin <- min( obj$y )
      elatmax <- max( obj$y )
    }
    n <- length( which( elev$z >= 0 ) )
    efrac <- n / length( elev$z ) * 100

    snames <- c( "Minimum Longitude",
                 "Maximum Longitude",
                 "Minimum Latitude",
                 "Maximum Latitude",
                 "Minimum Elevation (km)",
                 "Maximum Elevation (km)",
                 "Mean Elevation (km)",
                 "Land Fraction (%)",
                 "# of Points" )

    snums <- c( round( elonmin, 2 ),
                round( elonmax, 2 ),
                round( elatmin, 2 ),
                round( elatmax, 2 ),
                round( emin, 2 ),
                round( emax, 2 ),
                round( emean, 2 ),
                round( efrac, 2 ),
                length( elev$z ) )

    df <- data.frame( Statistic = snames, Value = as.character( snums ) )
  }, striped = TRUE, bordered = TRUE, hover = TRUE, align = "lr" )
})
