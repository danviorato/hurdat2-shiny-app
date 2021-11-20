source("R-scripts/librerias.R")

base_original <- read.csv("data/HURDAT2 May 29, 2021.txt", header = F)

base_descripcion <- base_original %>%
        filter(is.na(V7)) %>% select(V1,V2,V3) %>% mutate(V3 = as.numeric(V3)) %>%
        setNames(., c("clave","nombre", "lista"))

base_datos <- base_original %>%
        filter(!is.na(V7)) %>% 
        setNames(.,c("fecha","hora",""))