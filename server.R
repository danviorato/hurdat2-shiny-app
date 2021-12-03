source("R-scripts/load-data.R")

#Pat: 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

# Define server logic required to draw a histogram
shinyServer(
    function(input, output, session) {
        #First Graph filters
        output$periodoUI <-renderUI({
            sliderInput(
                inputId = "periodoSer", 
                label = "Periodo",
                min = min(hurdat$fecha), 
                max = max(hurdat$fecha),
                value = c(min(hurdat$fecha), 
                          max(hurdat$fecha)),
                timeFormat = "%d-%m-%Y"
            )
        })
        
        output$fechaUI <-renderUI({
            dateRangeInput(
                inputId = "fechaSer", 
                label = "Periodo",
                start = min(hurdat$fecha),
                end = max(hurdat$fecha),
                min = min(hurdat$fecha), 
                max = max(hurdat$fecha),
                format = "dd-mm-yyyy",
                separator = " - "
            )
        })
        
        observeEvent(
            input$switchUI,{
                updateDateRangeInput(
                    inputId = "fechaSer",
                    start = input$periodoSer[[1]],
                    end = input$periodoSer[[2]])
        })
        
        observeEvent(
            input$fechaSer, {
                updateSliderInput(
                    inputId = "periodoSer",
                    value = c(input$fechaSer[[1]],
                              input$fechaSer[[2]]),
                    timeFormat = "%d-%m-%Y")
        })
        
        output$statusUI <- renderUI({
            i <- pickerInput(
                inputId = "statusSer", 
                label = "Status", 
                choices = unique(estatusn), 
                selected = unique(estatusn),
                width = "100%",
                options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Quitar selección",
                    `select-all-text` = "Seleccionar todo",
                    `none-selected-text` = "Ninguno"),
                multiple = T
            )
            return(i)
        })
        
        output$distTotal <- renderPlot({ #Frequency histogram
            hurdat %>%
                group_by(clave) %>%
                summarise(year = min(year), 
                          estatus = factor(names(which.max(table(estatus))),
                                           levels = estatusc)) %>%
                filter(
                    year >= min(format(input$periodoSer)),
                    year <= max(format(input$periodoSer)),
                    estatus %in% estatusc[match(input$statusSer, estatusn)]
                ) %>%
                ggplot(aes(year, fill = estatus)) +
                geom_histogram(binwidth = 1) +
                scale_fill_viridis(discrete = T)+
                theme_minimal()
        })

        #Second graph filtered base
        base_point <- reactive({
            if(input$alphaSer != 1){
                norder <- c(estatusc[-grep(input$statusvientoSer, estatusc)],
                            input$statusvientoSer)} else{
                    norder <- estatusc}
            
            a <- hurdat %>% 
                filter(!is.na(min_presion))%>%
                group_by(clave) %>%
                summarise(
                    max_nudos_viento = mean(max_nudos_viento),
                    min_presion = mean(min_presion),
                    estatus = factor(names(which.max(table(estatus))),
                                     levels = norder),
                    balpha = ifelse(input$alphaSer == 1, T,
                                    ifelse(estatus == input$statusvientoSer,
                                           T,F)), .groups = "drop")
            a <- a[unlist(sapply(1:9, 
                                 function(x) which(a$estatus == norder[x]))),]
            return(a)
        })
        
        output$compvipe <- renderPlot( #Point graph
            base_point() %>%
                ggplot(aes(x = max_nudos_viento, y = min_presion, 
                           color = estatus, alpha = balpha, 
                           size = balpha))+
                geom_point(shape = 16) +
                theme_minimal()+
                scale_alpha_ordinal(
                    range = if(input$statusvientoSer == "All"){
                        c(0.55, 0.55)} else{
                        c(0.55 * input$alphaSer, 1)})+
                scale_size_ordinal(range = c(2,2.5))+
                scale_color_manual(values=viridis(9),breaks = estatusc)
        )
        
        base_mapa <- reactive({
            hurdat %>%
                filter(fecha == input$fecha_mapa)
        })
        
        output$mapSer <- renderLeaflet({

            map_filtered <- base_mapa() %>%
                mutate(label=paste(sep = "<br/>",
                                   nombre,
                                   paste0("Estatus: ", estatus)
                                   ))

            pal <- colorNumeric("viridis", hurdat$max_nudos_viento, reverse = T)
            
            #pos <- ifelse(!is.nan(mean(map_filtered$long)),
            #              c(mean(map_filtered$long),mean(map_filtered$lat)),
            #              c(pos[1],pos[2]))
            
            map <- leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(lng = map_filtered$long, lat = map_filtered$lat,
                                 radius = 20,
                                 popup = map_filtered$label,
                                 #weight = 20,
                                 color = pal(map_filtered$max_nudos_viento), 
                                 opacity = 1,
                                 fillOpacity = 0.5
                ) %>%
                addLegend("topright", pal = pal, values = hurdat$max_nudos_viento,
                          title = "Velocidad en nudos"#, opacity = .5
                ) %>%
                setView(lng = -10, lat = 30, zoom = 3) %>%
                fitBounds(lng1 = min(hurdat$long), lat1 = min(hurdat$lat),
                          lng2 = max(hurdat$long), lat2 = max(hurdat$lat))
        })
    }
)