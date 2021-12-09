source("R-scripts/librerias.R")
estatusc <- c("HU","TS","TD","EX","SS","SD","LO","WV","DB")
estatusn <- paste(estatusc, "-", c("Tropical cyclone of hurricane intensity", 
                                   "Tropical cyclone of tropical storm intensity",
                                   "Tropical cyclone of tropical depression intensity",
                                   "Extratropical cyclone",
                                   "Subtropical cyclone of subtropical storm intensity",
                                   "Subtropical cyclone of subtropical depression intensity",
                                   "A low", 
                                   "Tropical Wave", 
                                   "Disturbance"))


hurdat <- read_rds("data/hurdat2.rds") %>%
        mutate(estatus = factor(estatus, 
                                levels = estatusc))

stormid <- paste0(hurdat$nombre[!duplicated(hurdat$clave)], 
                  " (",hurdat$clave[!duplicated(hurdat$clave)],")")
