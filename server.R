source("R-scripts/librerias.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    estatusc <- c("HU","TS","TD","EX","SS","SD","LO","WV","DB")
    estatusn <- paste(estatusc, "-", c("Tropical cyclone of hurricane intensity", 
                                       "Tropical cyclone of tropical storm intensity",
                                       "Tropical cyclone of tropical depression intensity",
                                       "Extratropical cyclone",
                                       "Subtropical cyclone of subtropical storm intensity",
                                       "Subtropical cyclone of subtropical depression intensity",
                                       "A low", "Tropical Wave", "Disturbance"))
    
    
    hurdat <- read_rds("data/hurdat2.rds") %>%
        mutate(estatus = factor(estatus, 
                                levels = estatusc))
    
    output$periodoUI <-renderUI({
        sliderInput(
            "periodoSer", "Periodo",
            min = min(hurdat$fecha), 
            max = max(hurdat$fecha),
            value = c(min(hurdat$fecha), 
                      max(hurdat$fecha))
        )
    })
    
    output$statusUI <- renderUI({
        i <- pickerInput(
            "statusSer", label = "Status", 
            choices = unique(estatusn), 
            selected = unique(estatusn),
            width = "100%",
            options = list(`actions-box` = TRUE,
                           `deselect-all-text` = "Quitar selección",
                           `select-all-text` = "Seleccionar todo",
                           `none-selected-text` = "Ninguno"), 
            multiple = T
        )
        return(i)
    })
    
    base <- reactive({

        a <- hurdat %>%
            filter(
                year >= min(format(input$periodoSer)),
                year <= max(format(input$periodoSer)),
                estatus %in% estatusc[match(input$statusSer, estatusn)]
            )
        return(a)
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
    
    output$statusvientoUI <- renderUI({
        sliderTextInput(
            "statusvientoSer", label = "Status",
            choices = c("All", estatusc), selected = "All" 
        )
    })

    output$prueba <- renderPrint({ "a" })

    
    base_point <- reactive({
        norder <- if(input$statusvientoSer == "All") { 
            estatusc} else { 
                c(input$statusvientoSer,
                  estatusc[-grep(input$statusvientoSer,
                                 estatusc)])}
        
        a <- hurdat %>% 
            filter(!is.na(min_presion))%>%
            group_by(clave) %>%
            summarise(max_nudos_viento = mean(max_nudos_viento),
                      min_presion = mean(min_presion),
                      estatus = factor(names(which.max(table(estatus))),
                                       levels = norder),
                      balpha = ifelse(input$statusvientoSer == "All", T,
                                      ifelse(estatus == input$statusvientoSer,
                                             T,F)), .groups = "drop")
        a <- a[unlist(sapply(1:9, function(x) which(prueba$estatus==rev(estatusc)[x]))),]
        return(a)
    })
    
    output$compvipe <- renderPlot(
        base_point() %>%
            ggplot(aes(x = max_nudos_viento, y = min_presion, 
                       color = estatus, alpha = balpha))+
            geom_point()+
            theme_minimal()+
            scale_color_viridis(discrete = T) +
            scale_alpha_ordinal(range = c(0.5,1))
        #https://stackoverflow.com/questions/7556320/reorder-legend-without-changing-order-of-points-on-plot
    )
    
})

