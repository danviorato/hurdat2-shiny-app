source("R-scripts/librerias.R")

estatus <- data.frame(
    clave = c("HU","TS","TD","EX","SS","SD","LO","WV","DB"),
    nombre = c("Tropical cyclone of hurricane intensity", 
               "Tropical cyclone of tropical storm intensity",
               "Tropical cyclone of tropical depression intensity",
               "Extratropical cyclone",
               "Subtropical cyclone of subtropical storm intensity",
               "Subtropical cyclone of subtropical depression intensity",
               "A low", "Tropical Wave", "Disturbance")
)

hurdat <- read_rds("data/hurdat2.rds") %>%
    mutate(estatus = factor(estatus, levels = unlist(estatus$clave)))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$periodoUI <- renderUI({
        dateRangeInput(
            "periodoSer", "Periodo", 
            start = min(hurdat$fecha), 
            end = max(hurdat$fecha),
            min = min(hurdat$fecha), 
            max = max(hurdat$fecha),
            separator = " - ",
            format = "dd/mm/yyyy",
            language = "es",
            weekstart = 1
        )
    })
    
    base <- reactive({

        hurdat %>%
            filter(
                year >= min(format(input$periodoSer)),
                year <= max(format(input$periodoSer)),
                estatus %in% input$estatus_total
            )
    })
    
    output$borrar <- renderText({
        format(input$periodoSer)
    })

    output$distTotal <- renderPlot({
        base() %>%
            ggplot(aes(year, fill = estatus)) +
            geom_histogram(binwidth = 1) +
            scale_fill_viridis(discrete = T)+
            theme_minimal()
    })

})

