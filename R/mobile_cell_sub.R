#skipping the first two empty rows
data <- read.csv("./data_temp/API_IT.CEL.SETS.P2_DS2_en_csv_v2_3740.csv", skip = 3, header = T)


#replacing NAs with 0 and creating a subset with country names and years
data[is.na(data)] <- 0

data <- subset(data, select = -c(Indicator.Name, Indicator.Code))


#removing the X-es from each year
col_indexes <- (ncol(data) - 64):ncol(data)
col_names <- names(data)[col_indexes]
new_col_names <- sub("X", "", col_names)
names(data)[col_indexes] <- new_col_names
#removing the new objects, they are no longer needed
rm(col_indexes, col_names, new_col_names)





#using the rnaturalearth package to create an empty admin 0 map
install.packages("rnaturalearthdata")
install.packages("rnaturalearth")
library(rnaturalearth)
library(rnaturalearthdata)

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map_terra <- vect(world_map)
