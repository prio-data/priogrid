library(sf)
library(terra)
library(tools)
library(devtools)
# Install PRIOgrid package from GitHub
devtools::install_github("prio-data/priogrid")
library(priogrid)

install.packages("sf")
install.packages("terra")
install.packages("tools")
install.packages("devtools")
#First download all variables in the 1:10 Cultural Vectors Data set from Natural Earth
#Store the file in the data structure from 00_project_setup_R.
#Here: data - raw
#Un-zip the files in the data raw folder.

base_path <- "~/Documents/PRIO_grid_RA"

# Define the path to raw data for natural earth, saved with the date of the download
data_path <- "data/raw/NaturalEarth_121124/10m_cultural"

# List all .shp files in the folder
shapefiles <- list.files(data_path, pattern = "\\.shp$", full.names = TRUE)

#The data was downloaded:November 12th, 2024.

# Load the shape files for disputed areas - Admin 0 – Breakaway, Disputed Areas
disputed_areas <- st_read(file.path(data_path, "ne_10m_admin_0_disputed_areas.shp"))
#head(disputed_areas) - for inspection

#PRIOGRID is WGS84 - always ensure the dataset is in correct CRS (natural earth has several)
# Check the CRS and transform if necessary
st_crs(disputed_areas)

if (st_crs(disputed_areas) != st_crs(4326)) {
  disputed_areas <- st_transform(disputed_areas, crs = 4326)
}

#check geometry type
st_geometry_type(disputed_areas) #multipolygon

#plot basic map
plot(st_geometry(disputed_areas), main = "Disputed Areas")

#Inspect disputed areas data
str(disputed_areas) #structure --> 99 obs. of  169 variables
names(disputed_areas)  #column names
summary(disputed_areas)
View(disputed_areas)
colSums(is.na(disputed_areas))

table(disputed_areas$TYPE)

#make a dummy variable for disputed land
disputed_areas$disputed_areas_dum <- 1

#############Function to load data#########################
# Function to read the disputed areas dataset
read_disputed_areas <- function(base_path = "~/Documents/PRIO_grid_RA") {
  # Define the relative path to the dataset
  data_path <- file.path(base_path, "data/raw/NaturalEarth_121124/10m_cultural")

  # Define the specific shapefile for disputed areas
  shapefile <- file.path(data_path, "ne_10m_admin_0_disputed_areas.shp")

  # Check if the shapefile exists
  if (!file.exists(shapefile)) {
    stop(paste("Shapefile not found at:", shapefile))
  }

  # Load the shapefile
  disputed_areas <- sf::st_read(shapefile)

  # Check and ensure CRS is WGS84 (EPSG:4326)
  if (sf::st_crs(disputed_areas)$epsg != 4326) {
    disputed_areas <- sf::st_transform(disputed_areas, crs = 4326)
  }

  # Return the dataset
  return(disputed_areas)
}

# Test the function to load disputed areas
disputed_areas_function_read <- read_disputed_areas()
# Inspect the data
plot(sf::st_geometry(disputed_areas), main = "Disputed Areas")
summary(disputed_areas)
table(disputed_areas$TYPE)

# Create a dummy variable for disputed land
disputed_areas$disputed_areas_dum <- 1

############## Make PRIO Grid Function for Disputed Areas #################
# Function to generate raster for disputed areas coverage
gen_disputed_areas_cover <- function(data = read_disputed_areas()) {
  # Ensure the dataset CRS is WGS84 (EPSG:4326)
  if (sf::st_crs(data) != sf::st_crs(4326)) {
    data <- sf::st_transform(data, crs = 4326)
  }

  # Load the blank PRIO grid
  pg <- prio_blank_grid()

  # Calculate the proportion of cell area covered
  disputed_coversh <- terra::rasterize(
    terra::vect(data),
    pg,
    fun = function(vals, ...) mean(vals),  # Calculate the proportion of cell area covered
    cover = TRUE
  )

  # Set the raster layer name
  names(disputed_coversh) <- "disputed_land_share_of_cell"

  # Return the raster
  return(disputed_coversh)
}

## Test function
disputed_cover_function_read <- gen_disputed_areas_cover ()
#table(df$disputed_land_share_of_cell)

## Test Data - works
natural_earth_disputes_areas_cover <- as.data.frame(c(pg, disputed_cover))

############ Coverage variables for Natural Earth ############
# Function to generate a raster for specific disputed areas type
gen_naturalearth_disputed_areas_type <- function(type, disputed_areas = read_disputed_areas()) {
  # Group by TYPE and summarize geometries
  disputed_types <- disputed_areas |>
    dplyr::group_by(TYPE) |>
    dplyr::summarize(geometry = sf::st_union(geometry))

  # Filter the data by the specified type
  disputed_type <- disputed_areas |> dplyr::filter(TYPE == type) |> terra::vect()

  # Load the blank PRIO grid
  pg <- prio_blank_grid()

  # Rasterize the filtered data
  r <- terra::rasterize(disputed_type, pg, fun = "max", cover = TRUE)

  # Set the raster layer name
  names(r) <- paste0("naturalearth_disputed_areas_", type) |> tolower()

  # Return the raster
  return(r)
}

# Function to generate raster specifically for "Disputed" type
gen_naturalearth_disputed_areas_disputed <- function() {
  gen_naturalearth_disputed_areas_type(type = "Disputed")
}

# The types of disputed are listed below, and can be substituted in the function above:
#"Breakaway"
#"Disputed"
#"Geo subunit"
#"Geo unit"
#"Indeterminate"
#"Lease"
#"Overlay"

# Test the function for "Disputed" type
disputed_type_raster <- gen_naturalearth_disputed_areas_disputed()

# Plot the raster to verify output
plot(disputed_type_raster, main = "Natural Earth Disputed Areas - Disputed Type")


###########For text variables in Natural Earth################
#For string variables - turn priogrid into sf
pg_sf <- sf::st_as_sf(terra::as.polygons(pg)) #OBS - takes some time, 2 min!
pg_sf_raw <- pg_sf

# Ensure CRS compatibility
if (sf::st_crs(disputed_areas) != sf::st_crs(pg_sf_raw)) {
  disputed_areas <- sf::st_transform(disputed_areas, crs = sf::st_crs(pg_sf_raw))
}

# Find overlaps between PRIO grid cells (pg_sf_raw) and disputed areas
overlaps <- sf::st_intersects(pg_sf_raw, disputed_areas) #takes some time, 10 sec

# additional variables
# Add text attributes for natural_earth_disputed_areas_names, IDs, and types
pg_sf_raw$natural_earth_disputed_areas_names <- sapply(overlaps, function(indices) {
  if (length(indices) > 0) {
    paste(unique(disputed_areas$BRK_NAME[indices]), collapse = ";")
  } else {
    NA
  }
})

pg_sf_raw$natural_earth_disputed_areas_id <- sapply(overlaps, function(indices) {
  if (length(indices) > 0) {
    paste(unique(disputed_areas$BRK_A3[indices]), collapse = ";")
  } else {
    NA
  }
})

pg_sf_raw$natural_earth_disputed_areas_type <- sapply(overlaps, function(indices) {
  if (length(indices) > 0) {
    paste(unique(disputed_areas$TYPE[indices]), collapse = ";")
  } else {
    NA
  }
})

# Convert the PRIO grid + Natural Earth data to a data frame
natural_earth_disputed_areas_pg <- as.data.frame(pg_sf_raw)

#Attach it to the raster data from above
#check for duplicates
anyDuplicated(natural_earth_disputes_areas_cover$pgid)
anyDuplicated(natural_earth_disputed_areas_pg$pgid)

# Merge the datasets by pgid
natural_earth_disputed_areas_pg_df  <- merge(
  natural_earth_disputes_areas_cover,
  natural_earth_disputed_areas_pg,
  by = "pgid",
  all = TRUE  # Use all=TRUE to keep non-matching rows
)

#drop the geometry
natural_earth_disputed_areas_pg_df <- sf::st_drop_geometry(natural_earth_disputed_areas_pg_df)

########################### functions for text to priogrid for natural earth ###################

#function to convert priogrid to sf object
convert_pg_to_sf <- function(pg, output_path = "pg_sf.rds") {
  if (file.exists(output_path)) {
    message("Loading pre-saved pg_sf object...")
    return(readRDS(output_path))
  }
  message("Converting PRIO grid to sf object...")
  pg_sf <- sf::st_as_sf(terra::as.polygons(pg))
  saveRDS(pg_sf, output_path)
  return(pg_sf)
}

#Function to Ensure CRS Compatibility
ensure_crs <- function(data, reference) {
  if (sf::st_crs(data) != sf::st_crs(reference)) {
    message("Transforming CRS for compatibility...")
    data <- sf::st_transform(data, crs = sf::st_crs(reference))
  }
  return(data)
}


#############Inspection################
disputed_areas <- read_disputed_areas() #reads disputed areas from natural earth
#If project directory changes, pass a different base_path: disputed_areas <- read_disputed_areas(base_path = "/path/to/your/project")

#the main data with all variables:
natural_earth_disputed_areas_pg_df

##############Other variables to consider################
#The BRK_NAME and BRK_A3 (in line with ISO 3166-1 Alpha 3) highlights the name of the conflict
#The SOVEREIGNT highlight the sovereign state overseeing the disputed land with "SOV_A3" as ID
# The ADM0_DIF is a dummy variable highlighting if there is a dispute of sovereignty
# "ADMIN" and "ADM0_A3" thus highlight the alternative administrative unit and their ID

