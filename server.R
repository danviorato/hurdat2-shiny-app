source("R-scripts/librerias.R")

hurdat <- read_rds("data/hurdat2.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distTotal <- renderPlot({

        # generate bins based on input$bins from ui.R
        years    <- as.numeric(substr(as.character(hurdat$fecha),1,4))%>%
            unique() %>% sort()
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(years, breaks = bins, col = 'darkgray', border = 'white')

    })

})

