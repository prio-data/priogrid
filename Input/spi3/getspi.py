
import urllib2
import sys
import os
import BeautifulSoup as bs

# download files
baseurl = "http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_3-Month"
filelistingurl = baseurl + "/downloadsarcinfo.html?missing_value=-9999."
page = bs.BeautifulSoup(urllib2.urlopen(filelistingurl))
filelist = []
for tablerow in page.findAll("td"):
    linkobj = tablerow.find("a")
    if linkobj:
        url = baseurl + "/" + linkobj.get("href")
        filename = url.split("=")[-1] + ".asc" # url ends with 'filename=...'
        print filename
        # write data
        with open(filename, "w") as writer:
            writer.write(urllib2.urlopen(url).read())
        # collect file list
        filelist.append(filename)

# merge using gdal (requires python gdal bindings)
# using via python instead of commandline because latter has char limit

# first prep args ala commandline style
curdir = os.path.abspath("")
outfile = os.path.abspath("spi3.tif")
args = [curdir, "-separate", "-o", outfile]
args.extend((os.path.abspath(filepath) for filepath in filelist))

# run it
import gdal_merge
gdal_merge.main(args)
