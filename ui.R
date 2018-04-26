require(shiny)
require(shinythemes)
require(plotly)
require(leaflet)

ui <- navbarPage("Bathos V1.0",
                 theme = shinytheme("united"),
                 tabPanel("Location",
                          fluidRow(leafletOutput("map", height = "550px" )),
                          # Would prefer an absolutePanel for this, but that's not working right now.
                          fluidRow(column(3,
                                          checkboxGroupInput(inputId = "features",
                                                             label = "Map Features",
                                                             choices = c("Seismicity" = "quakes",
                                                                         "Volcanoes" = "volcanoes",
                                                                         "Plates" = "plates"),
                                                             selected = c("quakes","plates"),
                                                             inline = TRUE)
                          ),
                          column(3,
                                 radioButtons(inputId = "gmt",
                                              label = "Map Centering",
                                              choices = c("Prime Meridian" = "gmt",
                                                          "Anti-Meridian" = "pac"),
                                              selected = "gmt",
                                              inline = TRUE)
                          ),
                          column(3,
                                 h5(""),
                                 actionButton( "gethelp", "Help" )
                          )
                          )
                 ),

                 tabPanel("Visualization",
                          plotlyOutput( "profile", width = "100%" ),
                          tags$head(tags$style("#profile{height:90vh !important;}")),
                          absolutePanel(bottom = 15, right = 35,
                                        draggable = TRUE, width = "200px",
                                        wellPanel(numericInput(inputId = "vertExag",
                                                               label = "Vertical Exaggeration",
                                                               min = 0,
                                                               max = 1000,
                                                               value = 50,
                                                               step = 5),
                                                  sliderInput(inputId = "waterOpacity",
                                                              label = "Sea Level Opacity",
                                                              min = 0,
                                                              max = 1,
                                                              step = 0.1,
                                                              ticks = FALSE,
                                                              value = 0.5,
                                                              width = "250px"),
                                                  style = "background-color: rgba(255, 255, 255, 0.5)"
                                        )
                          )
                 ),
                 tabPanel("Hypsometry",
                          column(4,
                                 h4("Hypsometry Data"),
                                 tableOutput("thypso")
                          ),
                          column(8,
                                 plotlyOutput("ghypso", width = "90%")
                          )
                 ),

                 tabPanel("More Information",   # Contact Information.
                          "Any questions or comments can be sent to:", br(),
                          "Justin Revenaugh:", br(),
                          "University of Minnesota", br(),
                          "Earth Sciences", br(),
                          "Minneapolis, MN 55455 USA", br(),
                          a("justinr@umn.edu", href="mailto:justinr@umn.edu"), br(),
                          a("See the code", href="https://github.com/jrevenaugh/Bathos"), br()
                 )
)
