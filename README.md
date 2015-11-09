Using the replication scripts
---------------------

### System Requirements

To successfully replicate PRIO-GRID using the provided scripts and files, the user needs: 

1. a Linux system (tested on Ubuntu version 14.04 LTS, 64-bit)
2. a user account and folder with read/write privileges
3. a PostgreSQL installation (tested on version 9.4)
4. a GDAL installation registered to environment variables (only needed for converting SPEI NetCDF files)
5. a Python 2.7 installation, the Python GDAL bindings, BeautifulSoup, and gdal_merge importable (only needed for downloading and combining monthly SPI files)

Note: PRIO-GRID can also be set up and created on Windows, but in those cases, the user will have to modify the run.sh and run_log.sh Linux bash scripts to Windows bat scripts  (replacing all "#" comments with "::" comments, and renaming the files to run.bat and run_log.bat). But most importantly, the Windows version of PostGIS does not have built-in support for loading NetCDF files with the raster2psql utility, so that the user will have to manually convert any NetCDF files to another format such as GeoTIFF and update the run.bat script to account for this change. Depending on how the data are converted, meta-data information from the original data file such as raster geo-referencing coefficients or no-data values may be lost in the process, which could lead to further errors and faulty results in the SQL scripts.  The user may or may not encounter additional system differences or errors that we are unable to account for. 

### Setting up the file workspace and scripts

Once the system requirements are satisfied the user needs to set up the file workspace and scripts by cloning this repository.

### Collecting the data

The data and variables contained in PRIO-GRID and created by the replication files are simply processed versions of existing third-party data sources. For legal and practical reasons the data files needed to recreate PRIO-GRID is not provided in the replication package (the files have a combined file size of 30-40 GB).  Instead, the user must first obtain and collect all the original data files used as input.  To facilitate this process, we provide a table of the data sources that the user must obtain,  along with the download url, version number, and other information. Each data source must be saved in their original form and stored in the indicated data folder in the "Input" folder.

| Data | Version | SubData | DownloadLink | TargetFolder |
| ----- | ----- | ----- | ----- | ----- |
| cshapes | 0.4-2 (last modified 22 Mar 2015) | <None> | http://nils.weidmann.ws/projects/cshapes | Input/cshapes |
| UNEP mountain watch, "Mountains and Tree Cover in Mountain Regions" | 2001 | <None> | http://www.unep-wcmc.org/resources-and-data/mountains-and-forests-in-mountains | Input/mountains |
| Diamonds | 1a | <None> | https://www.prio.org/Data/Geographical-and-Resource-Datasets/Diamond-Resources/ | Input/diamonds |
| Goldata | 1.2 | <None> | http://www.researchgate.net/profile/Sara_Balestri | Input/gold |
| Gemdata | Downloaded on 17 aug 2015 | <None> | http://paivilujala.weebly.com/gemdata.html | Input/gems |
| Drugdata | Downloaded on 17 aug 2015 | <None> | http://paivilujala.weebly.com/drugdata.html | Input/drugs |
| Petroleum Dataset | 1.2 | <None> | https://www.prio.org/Data/Geographical-and-Resource-Datasets/Petroleum-Dataset/Petroleum-Dataset-v-12/ | Input/petroleum |
| EPR | 2014 | EPR-2014.csv | http://www.icr.ethz.ch/data/epr | Input/epr |
| GeoEPR | 2014, Update 2, released on March 16, 2015 | <None> | http://www.icr.ethz.ch/data/geoepr | Input/geoepr |
| Nordhaus | 0.4 | <None> | http://gecon.yale.edu/ | Input/nordhaus |
| accesstimes | 2008-2009 | <None> | http://forobs.jrc.ec.europa.eu/products/gam/ | Input/accesstimes |
| CIESIN GPW | v3 | Folders starting with "glcount..." or "glfecount..." | http://sedac.ciesin.columbia.edu/data/collection/gpw-v3 | Input/population_gpw |
| CIESIN, Global Subnational Infant Mortality Rates | v1 | imr.asc | http://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-infant-mortality-rates | Input/imr |
| CIESIN, Global Subnational Prevalence of Child Malnutrition | v1 | uw.asc | http://sedac.ciesin.columbia.edu/data/set/povmap-global-subnational-prevalence-child-malnutrition | Input/childmalnut |
| Globcover 2009 | v2.3 | <None> | http://due.esrin.esa.int/page_globcover.php | Input/globcover |
| GPCP | v2.2 Combined Precipitation Data Set (downloaded from NOAA) | "Monthly Mean" | http://www.esrl.noaa.gov/psd/data/gridded/data.gpcp.html | Input/precip_gpcp |
| GPCC | v7 | "GPCC Full Data Reanalysis Version 7" | ftp://ftp.dwd.de/pub/data/gpcc/html/fulldata_v7_doi_download.html | Input/precip_gpcc |
| GHCN/CAMS  | V2 | "air.mon.mean.nc" | http://www.esrl.noaa.gov/psd/data/gridded/data.ghcncams.html | Input/temp |
| SPI CAMS/OPI  | ? | Downloaded from http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/ | http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_1-Month/ | Input/spi1 |
| SPI CAMS/OPI  | ? | Downloaded from http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/ | http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_1-Month/ | Input/spi3 |
| SPEI Global Drought Monitor | Continually updated? (downloaded 15 July 2015) | SPEI1 | http://sac.csic.es/spei/map/maps.html | Input/spei1_gdm |
| SPEI Global Drought Monitor | Continually updated? (downloaded 15 July 2015) | SPEI3 | http://sac.csic.es/spei/map/maps.html | Input/spei3_gdm |
| SPEIbase | v2.3 | SPEI1 | https://digital.csic.es/handle/10261/104742 | Input/spei1_base |
| SPEIbase | v2.3 | SPEI3 | https://digital.csic.es/handle/10261/104742 | Input/spei3_base |
| Historical irrigation data (HID) | 1.0 | AEI_EARTHSTAT_IR | https://mygeohub.org/publications/8 | Input/irrigation |
| DMSP OLS Nighttime Lights | 4.0 | Files named "stablelights_avg_vis", downloaded from links marked "Average Visible, Stable Lights, & Cloud Free Coverages" (not "Average Lights x Pct") | http://ngdc.noaa.gov/eog/dmsp/downloadV4composites.html | Input/nightlights |
| MIRCA2000 | 1.1 | Cropping Periods List, 30 min ("CELL_SPECIFIC_CROPPING_CALENDARS_30MN.TXT") | http://www.uni-frankfurt.de/45218031 | Input/croptypes |
| ISAM-HYDE landuse | ? | <None> | https://www.atmos.illinois.edu/~meiyapp2/datasets.htm | Input/isamhyde |
| HYDE population data | 3.1 | Files starting with "popc_" | http://themasites.pbl.nl/tridion/en/themasites/hyde/download/index-2.html | Input/population_hyde |

All data files are processed in their original form and require no preprocessing. However, there are some special cases where we were required to add some preprocessing steps into the bash script, so as long as your system satisfies the required dependencies this should be taken care of automatically. Specifically, this means that both Python and GDAL's gdal_translate tool must be in your system's environment variables so that they can be called from the terminal, BeautifulSoup for Python installed, and the "gdal_merge" tool must be importable by Python. 


### Running the scripts

The scripts are now ready to run, but we recommend that the user first makes sure that:

1. The run_log.sh and run.sh files are executable. 
2. Your system user account has read/write privileges for the PRIOGRID replication folder, which it uses to create the output log file. 
3. Your Linux user account has automated log on privileges to the PostgreSQL server, for instance by saving your PostgreSQL username and password in a password configuration file. This is the only way to allow automated access to the PostgreSQL server without having to type your username and password for each line of code. For more information on the password file, see: 

http://www.postgresql.org/docs/9.4/static/libpq-pgpass.html

Once the user has taken care of the previous steps, the entire PRIO-GRID database can be recreated by simply running the run_log.sh script using the following lines in the Linux terminal:

    >>> cd /path/to/priogrid_top_folder
    >>> ./run_log.sh

The terminal should now start outputting a bunch of lines from all the scripts being run. There are many computationally heavy operations so completion time is likely to be 48 hours or more, depending on your system specifications. 

If anything were to go wrong in the scripts or the computer shuts down unexpectedly, the user can find how far the script got and identify the problem  by looking at the progress made in the "run_log_output.log" file. To avoid having to start all over again, the user should comment out ("#") all the lines from the run.sh file that were successfully completed in the previous run, and run the run_log.sh script again. 

### The finished database

Once all the scripts are finished, a new "priogrid2" database should exist on the PostgreSQL server. 

The priogrid2 database is organized as follows:

- priogrid2 database
  - "orig" schema (containing tables for all the input data in their original form)
  - "dev" schema (containing only development related tables, including backups of intermediary tables that were time consuming to produce and necessary for the calculation of the final data)
  - "public" schema (containing the final tables) 
    - "priogrid_land" spatial reference table containing all terrestrial cells.
    - "finalstatic" final table with all static variables
    - "finalyearly" final table with all yearly variables
    - tables used as input for the "final*" tables
  - "test" schema (empty, used for storing test tables, e.g. for visualization as shown below)

In addition to the final tables shown above, the priogrid2 database comes with a preregistered function that the user can use to assign the correct cell geometries to any table containing a "gid", for instance so that the user can visualize the table spatially in a GIS.  The function is invoked with the following SQL query:

    >>> SELECT JOIN2PRIOGRID('schemaname.tablename')
	
After completion, the user should now have a new table with the same name as the original table inside the "test" schema, correctly situated in the PRIO-GRID spatial grid infrastructure for easier visualization and analysis. 
