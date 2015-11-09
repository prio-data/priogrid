
import urllib2
import sys
import os


# download files
for yr in range(1979, 2014+1):
    for month in "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split():
        url = "http://iridl.ldeo.columbia.edu/SOURCES/.IRI/.Analyses/.SPI/.SPI-CAMSOPI_"
        url += "1-Month"
        url += "/T+%28"+month
        url += "%20"+str(yr)
        url += "%29+VALUE/-9999.0/setmissing_value/%5BX+Y%5D/arcinfo.asc"
        url += "?filename=data"+month+"_"+str(yr)
        print yr, month
        with open("data%s_%i.asc" % (month, yr), "w") as writer:
            writer.write(urllib2.urlopen(url).read())

# collect file list
filelist = []
for yr in range(1979,2014+1):
    for month in "Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split():
        filelist.append("data%s_%i.asc" % (month, yr) )

# merge using gdal (requires python gdal bindings)
# using via python instead of commandline because latter has char limit

# first prep args ala commandline style
curdir = os.path.abspath("")
outfile = os.path.abspath("spi1.tif")
args = [curdir, "-separate", "-o", outfile]
args.extend((os.path.abspath(filepath) for filepath in filelist))

# run it
import gdal_merge
gdal_merge.main(args)

