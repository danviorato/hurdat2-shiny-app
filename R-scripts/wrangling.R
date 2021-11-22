source("R-scripts/librerias.R")

base_original <- read.csv("data/HURDAT2 May 29, 2021.txt", header = F)

base_descripcion <- base_original %>%
        filter(is.na(V7)) %>% select(V1,V2,V3) %>% mutate(V3 = as.numeric(V3)) %>%
        setNames(., c("clave","nombre", "lista"))

base_datos <- base_original %>%
        filter(!is.na(V7)) %>% 
        setNames(.,c("fecha","hora","identificador","estatus","lator","longor",
                     "max_nudos_viento","min_presion"))

nombre <- gsub("\\s+","",sapply(1:nrow(base_descripcion), function(x) rep(base_descripcion$nombre[x],base_descripcion$lista[x]))%>%
        unlist())

clave <- sapply(1:nrow(base_descripcion), function(x) rep(base_descripcion$clave[x],base_descripcion$lista[x]))%>%
                                   unlist()

base_datos <- base_datos[,-(9:21)]

base_datos <- base_datos %>% 
        mutate(fecha = as.Date(fecha, "%Y%m%d"),
               hora = gsub(" ","",hora),
               identificador = gsub(" ","",identificador),
               estatus = gsub(" ","",estatus),
               min_presion = ifelse(min_presion == -999, NA, min_presion),
               clave = clave, nombre = nombre,
               hem = substr(lator,nchar(lator),nchar(lator)),
               mer = substr(longor,nchar(longor),nchar(longor)),
               lat = ifelse(hem == "N", 1,-1) * as.numeric(
                       substr(gsub(" ","",lator), 1,nchar(gsub(" ","",lator))-1)),
               long = ifelse(mer == "E", 1,-1) * as.numeric(
                       substr(gsub(" ","",longor), 1,nchar(gsub(" ","",longor))-1)),
               year = as.numeric(substr(as.character(fecha),1,4)))

write_rds(base_datos, "data/hurdat2.rds")
