source("R-scripts/librerias.R")

hurdat <- read_rds("data/hurdat2.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Base de datos de huracanes: HURDAT2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("periodoUI"),
            pickerInput(
                "estatus_total", label = "Estatus", 
                choices = unique(hurdat$estatus), 
                selected = unique(hurdat$estatus),
                width = "100%",
                options = list(`actions-box` = TRUE,
                               `deselect-all-text` = "Quitar selección",
                               `select-all-text` = "Seleccionar todo",
                               `none-selected-text` = "Ninguno"), 
                multiple = T
            ),
            textOutput("borrar")
        ,width = 4),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distTotal")
            )
        )
    )
    )
