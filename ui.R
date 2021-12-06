source("R-scripts/load-data.R")

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        # Application title
        titlePanel("Base de datos de huracanes: HURDAT2"),
        
        # Sidebar with a slider input for number of bins
        sidebarLayout(
            sidebarPanel(
                materialSwitch(
                    inputId = "switchUI",
                    value = T),
                conditionalPanel(#Investigar conditional panel
                    condition = "input.switchUI",
                    uiOutput("periodoUI")
                ),
                conditionalPanel(
                    condition = "!input.switchUI",
                    uiOutput("fechaUI")
                ),
                uiOutput("statusUI")
                ,width = 4),
            
            # Show a plot of the generated distribution
            mainPanel(
                plotOutput("distTotal")
            )
        ),
        sidebarLayout(
            sidebarPanel(
                sliderInput(
                    inputId = "alphaSer",
                    label = "Opacidad",
                    min = 0,
                    max= 1,
                    value = 1
                ),
                conditionalPanel(
                    condition = "input.alphaSer != 1",
                    sliderTextInput(
                        inputId = "statusvientoSer", 
                        label = "Status",
                        choices = estatusc, 
                        selected = "HU" 
                    )
                )
            ),
            mainPanel(
                plotOutput("compvipe")
            )
        ),
        sidebarLayout(
            sidebarPanel(
                #        verbatimTextOutput("prueba"),
                #        tableOutput("prueba2")
                width = 4,
                sliderTextInput(
                    inputId = "fecha_mapa",
                    label = "Fecha",
                    choices = unique(hurdat$fecha),
#                    min = min(hurdat$fecha),
#                    max = max(hurdat$fecha),
                    selected = min(hurdat$fecha),
#                    timeFormat = "%d-%m-%Y",
                    animate = T
                )
            ),
            mainPanel(
                leafletOutput(outputId = "mapSer", height = "65rem"),
                width = 8
            )
        )
    )
)