# PRIO-GRID CONSTANTS

prio_crs <- function(){"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"}
prio_resolution <- function(){0.5}
prio_extent <- function(){raster::extent(-180, 180, -90, 90)}
prio_nrow <- function(){360}
prio_ncol <- function(){720}

prio_midatlantic <- -29

makeheader <- "
                         _    _             
                        | |  (_)            
         _ __ ___   __ _| | ___ _ __   __ _ 
        | '_ ` _ \\ / _` | |/ / | '_ \\ / _` |
        | | | | | | (_| |   <| | | | | (_| |
        |_| |_| |_|\\__,_|_|\\_\\_|_| |_|\\__, |
                                       __/ |
                                      |___/ 
 _______ _______    _____   ___      ______  _______    _____ ______    
|_   __ \\_   __ \\  |_   _|.'   `.  .' ___  ||_   __ \\  |_   _|_   _ `.  
  | |__) || |__) |   | | /  .-.  \\/ .'   \\_|  | |__) |   | |   | | `. \\ 
  |  ___/ |  __ /    | | | |   | || |   ____  |  __ /    | |   | |  | | 
 _| |_   _| |  \\ \\_ _| |_\\  `-'  /\\ `.___]  |_| |  \\ \\_ _| |_ _| |_.' / 
|_____| |____| |___|_____|`.___.'  `._____.'|____| |___|_____|______.'  
"
                                                                        


resolution_factor <- function(x){
  priogrid::prio_resolution()/raster::res(x) # Calculates the factor to aggregate by to get 0.5 x 0.5 resolution
}
