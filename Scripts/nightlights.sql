
-- Whole script ETA: 7 hours, due to huge rasters and many years, but mostly due to the raster recalibration operations ETA 30 mins per year. 

-- Combines all nightlight tables together into one
DROP TABLE IF EXISTS dev.nightlights;
CREATE TABLE dev.nightlights
AS
(
SELECT 	-- Also, nightlights is special since it includes an additional pixel and is centered on corners instead of in the middle...
	--   So offset each tile down and right by half a 30-arcsecond, and set both scales to 30 arcsecond, then clip away the excess pixel that goes beyond the -180, 90, 180, -90 bounds
	--   In addition to having an extra pixel, nightlight data uses slightly smaller scale than proper 30-arcsecond in order to fit the extra pixel
	--   This is a hack, should find the true reason for this odd data format, and use more theoretically sound solution in future...
	--   Also note that rounding to 12 decimals for final upperlefty is necessary to avoid tiny e-number (eg 2.6e-13) from being interpreted as large integer (eg 2.6)

	ST_SetGeoreference(rast, round(-180.0+(((43200*(ST_UpperLeftX(rast)+180.0+0.00416666665))::integer/360.0)*0.0083333333333333), 12), round(-65.0+(((16800*(ST_UpperLeftY(rast)+65-0.00416666665))::integer/140.0)*0.0083333333333333), 12), 0.0083333333333333, -0.0083333333333333, 0, 0) AS rast
	,
	"year"
FROM 
	(
	SELECT *, 1992 AS "year" FROM orig.nightlights92
	UNION ALL
	SELECT *, 1993 AS "year" FROM orig.nightlights93
	UNION ALL
	SELECT *, 1994 AS "year" FROM orig.nightlights94
	UNION ALL
	SELECT *, 1995 AS "year" FROM orig.nightlights95
	UNION ALL
	SELECT *, 1996 AS "year" FROM orig.nightlights96
	UNION ALL
	SELECT *, 1997 AS "year" FROM orig.nightlights97
	UNION ALL
	SELECT *, 1998 AS "year" FROM orig.nightlights98
	UNION ALL
	SELECT *, 1999 AS "year" FROM orig.nightlights99
	UNION ALL
	SELECT *, 2000 AS "year" FROM orig.nightlights00
	UNION ALL
	SELECT *, 2001 AS "year" FROM orig.nightlights01
	UNION ALL
	SELECT *, 2002 AS "year" FROM orig.nightlights02
	UNION ALL
	SELECT *, 2003 AS "year" FROM orig.nightlights03
	UNION ALL
	SELECT *, 2004 AS "year" FROM orig.nightlights04
	UNION ALL
	SELECT *, 2005 AS "year" FROM orig.nightlights05
	UNION ALL
	SELECT *, 2006 AS "year" FROM orig.nightlights06
	UNION ALL
	SELECT *, 2007 AS "year" FROM orig.nightlights07
	UNION ALL
	SELECT *, 2008 AS "year" FROM orig.nightlights08
	UNION ALL
	SELECT *, 2009 AS "year" FROM orig.nightlights09
	UNION ALL
	SELECT *, 2010 AS "year" FROM orig.nightlights10
	UNION ALL
	SELECT *, 2011 AS "year" FROM orig.nightlights11
	UNION ALL
	SELECT *, 2012 AS "year" FROM orig.nightlights12
	UNION ALL
	SELECT *, 2013 AS "year" FROM orig.nightlights13
	) AS foo
);


-- Make spatial index
CREATE INDEX nightlights_idx_all ON dev.nightlights USING BTREE("year");
CREATE INDEX nightlights_idx_convexhull ON dev.nightlights USING GIST(ST_ConvexHull(rast));
ANALYZE dev.nightlights;



-- This query makes one raster for each PRIO-GRID cell. Clip and union is the procedure.
DROP TABLE IF EXISTS nightlights;
CREATE TABLE nightlights
AS
(
SELECT gid, "year", ST_Union(rast) as rast  -- union the clipped raster tiles inside the cell
FROM 
	(
	SELECT p.gid, n.year, ST_Clip(n.rast, p.cell) AS rast 
	FROM dev.nightlights AS n, priogrid_land AS p
	WHERE ST_Intersects(n.rast, p.cell)
	) AS foo -- all tiles that intersect with each priogrid cell clipped to cell extents
GROUP BY gid, "year"
);



---------------------------------------


/* Default BandNoDataValue is 0. Raster value 0 means no light, not no data. Setting to NULL. This produces correct results. */
UPDATE nightlights SET rast = ST_SetBandNoDataValue(rast, 1, NULL);

-- Add year index to make selection for mapalgebra go faster.
CREATE INDEX nightlights_idx_all ON nightlights USING BTREE(year);
ANALYZE nightlights;


-------------------------------------


-- Calculate stats
ALTER TABLE nightlights 	ADD COLUMN nl_sum double precision,
				ADD COLUMN nl_mean double precision,
				ADD COLUMN nl_sd double precision,
				ADD COLUMN nl_min double precision, 
				ADD COLUMN nl_max double precision, 
				ADD COLUMN nl_count integer;

UPDATE nightlights SET nl_sum = (ST_SummaryStats(rast)).sum;
UPDATE nightlights SET nl_mean = (ST_SummaryStats(rast)).mean;
UPDATE nightlights SET nl_sd = (ST_SummaryStats(rast)).stddev;
UPDATE nightlights SET nl_min = (ST_SummaryStats(rast)).min;
UPDATE nightlights SET nl_max = (ST_SummaryStats(rast)).max;
UPDATE nightlights SET nl_count = (ST_SummaryStats(rast)).count;

-- Years 1997-1999 are missing some northern observations
UPDATE nightlights SET nl_sum=NULL, nl_mean=NULL, nl_sd=NULL, nl_min=NULL, nl_max=NULL, nl_count=NULL WHERE gid >= 223201 and year = 1997;
UPDATE nightlights SET nl_sum=NULL, nl_mean=NULL, nl_sd=NULL, nl_min=NULL, nl_max=NULL, nl_count=NULL WHERE gid >= 226801 and year = 1998;
UPDATE nightlights SET nl_sum=NULL, nl_mean=NULL, nl_sd=NULL, nl_min=NULL, nl_max=NULL, nl_count=NULL WHERE gid >= 227521 and year = 1999;

-------------------------------------

-- Create misc functions for creating sensor sensitivity calibrated version of nightlights data

CREATE OR REPLACE FUNCTION nlcalibCap(double precision) RETURNS double precision
    AS 
    '
    select 
     case
	when $1 > 63 THEN 63
	else $1
     end
     as cap;'
    LANGUAGE SQL;

CREATE OR REPLACE FUNCTION nlcalibStandardize(V double precision, minV double precision, maxV double precision) RETURNS double precision
    AS 
    '
    select (V - minV)/(maxV - minV) as standardized;'
    LANGUAGE SQL;

-- Perform the new calibrated value calculation based on the original mean, and capping any values above 63
ALTER TABLE nightlights ADD COLUMN nl_cal_mean_raw double precision;

UPDATE nightlights SET nl_cal_mean_raw = (-2.0570 + 1.5903*nl_mean - 0.0090*(nl_mean^2)) WHERE year = 1992;
UPDATE nightlights SET nl_cal_mean_raw = (-1.0582 + 1.5983*nl_mean - 0.0093*(nl_mean^2)) WHERE year = 1993;
UPDATE nightlights SET nl_cal_mean_raw = (-0.6890 + 1.1770*nl_mean - 0.0025*(nl_mean^2)) WHERE year = 1994;
UPDATE nightlights SET nl_cal_mean_raw = (-0.0515 + 1.2293*nl_mean - 0.0038*(nl_mean^2)) WHERE year = 1995;
UPDATE nightlights SET nl_cal_mean_raw = (-0.0959 + 1.2727*nl_mean - 0.0040*(nl_mean^2)) WHERE year = 1996;
UPDATE nightlights SET nl_cal_mean_raw = (-1.1323 + 1.7696*nl_mean - 0.0122*(nl_mean^2)) WHERE year = 1997;
UPDATE nightlights SET nl_cal_mean_raw = (-0.1917 + 1.6321*nl_mean - 0.0101*(nl_mean^2)) WHERE year = 1998;
UPDATE nightlights SET nl_cal_mean_raw = (-0.1557 + 1.5055*nl_mean - 0.0078*(nl_mean^2)) WHERE year = 1999;
UPDATE nightlights SET nl_cal_mean_raw = (0.1254 + 1.0452*nl_mean - 0.0010*(nl_mean^2)) WHERE year = 2000;
UPDATE nightlights SET nl_cal_mean_raw = (-0.7024 + 1.1081*nl_mean - 0.0012*(nl_mean^2)) WHERE year = 2001;
UPDATE nightlights SET nl_cal_mean_raw = (0.0491 + 0.9568*nl_mean - 0.0010*(nl_mean^2)) WHERE year = 2002;
UPDATE nightlights SET nl_cal_mean_raw = (0.2217 + 1.5122*nl_mean - 0.0080*(nl_mean^2)) WHERE year = 2003;
UPDATE nightlights SET nl_cal_mean_raw = (0.2853 + 1.1955*nl_mean - 0.0034*(nl_mean^2)) WHERE year = 2004;
UPDATE nightlights SET nl_cal_mean_raw = (-0.0001 + 1.4159*nl_mean - 0.0063*(nl_mean^2)) WHERE year = 2005;
UPDATE nightlights SET nl_cal_mean_raw = (0.1065 + 1.1371*nl_mean - 0.0016*(nl_mean^2)) WHERE year = 2006;
UPDATE nightlights SET nl_cal_mean_raw = (0.6394 + 0.9114*nl_mean + 0.0014*(nl_mean^2)) WHERE year = 2007;
UPDATE nightlights SET nl_cal_mean_raw = (0.5564 + 0.9931*nl_mean - 0.0000*(nl_mean^2)) WHERE year = 2008;
UPDATE nightlights SET nl_cal_mean_raw = (0.9492 + 1.0683*nl_mean - 0.0016*(nl_mean^2)) WHERE year = 2009;
UPDATE nightlights SET nl_cal_mean_raw = (2.3430 + 0.5102*nl_mean + 0.0065*(nl_mean^2)) WHERE year = 2010;
UPDATE nightlights SET nl_cal_mean_raw = (1.8956 + 0.7345*nl_mean + 0.0030*(nl_mean^2)) WHERE year = 2011;
UPDATE nightlights SET nl_cal_mean_raw = (1.8750 + 0.6203*nl_mean + 0.0052*(nl_mean^2)) WHERE year = 2012;

-- Standardize the calibrated values to range between 0 and 63 based on global min and max calibrated value 
ALTER TABLE nightlights ADD COLUMN nl_cal_mean double precision;

DROP TABLE IF EXISTS temp_nlcalstats;  -- a table containing the global max and min
CREATE TEMP TABLE temp_nlcalstats AS (SELECT MIN(nl_cal_mean_raw) AS nlcalmin, MAX(nl_cal_mean_raw) AS nlcalmax FROM nightlights);

UPDATE nightlights SET nl_cal_mean = nlcalibStandardize(nl_cal_mean_raw, stats.nlcalmin, stats.nlcalmax) FROM temp_nlcalstats AS stats;  -- final calculation

