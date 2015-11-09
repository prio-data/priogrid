
-- Loading the data did not preserve the original geotransform
-- Therefore geotransform to realign correctly
DROP TABLE IF EXISTS spei1_gdm_georef;
CREATE TEMP TABLE spei1_gdm_georef AS
(
-- separate georef of each tile, and requires total raster widht and height hardcoded below
-- but maybe its not full width and height? because website says spatial coverage is 88.75N - 88.75S, 1.25E - 358.75E
SELECT rid, ST_SetGeoReference(rast, 
		360 * (ST_UpperLeftX(rast) / 720) - 180, -- upperleftx relative position to total raster with and offset
		180 - (180 * (1+(ST_UpperLeftY(rast) / 360))) - 90, -- upperlefty relative position to total raster height and offset (NOTE: raster goes in upwards direction, so switch upperlefty from top to bottom and bottom to top)
		0.5, 0.5, -- scalex, scaley (NOTE: raster goes in upwards direction, so do not use the usual negative scaley)
		0, 0) -- skewx, skewy
		AS rast
FROM orig.spei1_gdm
);










-- ADD INDEX
CREATE INDEX spei1_gdm_georef_idx ON spei1_gdm_georef USING gist(ST_ConvexHull(rast));
ANALYZE spei1_gdm_georef;






-- Summarize stats, yearly average of monthly precip
-- Because the raster cells are larger than the priogrid cell polygons, the usual tile clip and summary stats won't work.
-- Instead using a simple nearest value approach so clusters of smaller priogrid cells "inherit" value from their nearest overarching raster cell


-- DEFINE MAIN FUNCTION
CREATE OR REPLACE FUNCTION calcYearlySPEI1(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS dev.spei1_gdm;
	CREATE TABLE dev.spei1_gdm(	gid integer, "year" integer,
					jan decimal,
					feb decimal,
					mar decimal,
					apr decimal,
					may decimal,
					jun decimal,
					jul decimal,
					aug decimal,
					sep decimal,
					oct decimal,
					nov decimal,
					des decimal,
					average decimal
					);

	-- Calculate stats for each band
	FOR i IN _from.._to LOOP
		RAISE NOTICE '%', i;

		-- add row
		INSERT INTO dev.spei1_gdm
		SELECT 	p.gid, 
			i AS "year", 
			-- value of a particular month/band, and because the value is an average/index we assume it to be true for all smaller priogrid cells
			-- band 1 is jan 1950 and increments linearly up, so some math to convert year to correct month band nr
			ST_NearestValue(pr.rast, (i-1950)*12+1, p.centroid, FALSE) AS jan,
			ST_NearestValue(pr.rast, (i-1950)*12+2, p.centroid, FALSE) AS feb,
			ST_NearestValue(pr.rast, (i-1950)*12+3, p.centroid, FALSE) AS mar,
			ST_NearestValue(pr.rast, (i-1950)*12+4, p.centroid, FALSE) AS apr,
			ST_NearestValue(pr.rast, (i-1950)*12+5, p.centroid, FALSE) AS may,
			ST_NearestValue(pr.rast, (i-1950)*12+6, p.centroid, FALSE) AS jun,
			ST_NearestValue(pr.rast, (i-1950)*12+7, p.centroid, FALSE) AS jul,
			ST_NearestValue(pr.rast, (i-1950)*12+8, p.centroid, FALSE) AS aug,
			ST_NearestValue(pr.rast, (i-1950)*12+9, p.centroid, FALSE) AS sep,
			ST_NearestValue(pr.rast, (i-1950)*12+10, p.centroid, FALSE) AS oct,
			ST_NearestValue(pr.rast, (i-1950)*12+11, p.centroid, FALSE) AS nov,
			ST_NearestValue(pr.rast, (i-1950)*12+12, p.centroid, FALSE) AS des
		FROM priogrid_land AS p, spei1_gdm_georef AS pr
		WHERE p.centroid && pr.rast -- bboxes intersect (faster)
		;
	END LOOP;

	-- set null values
	UPDATE dev.spei1_gdm SET jan = NULL WHERE jan < -1111.0 OR jan > 1111.0;
	UPDATE dev.spei1_gdm SET feb = NULL WHERE feb < -1111.0 OR feb > 1111.0;
	UPDATE dev.spei1_gdm SET mar = NULL WHERE mar < -1111.0 OR mar > 1111.0;
	UPDATE dev.spei1_gdm SET apr = NULL WHERE apr < -1111.0 OR apr > 1111.0;
	UPDATE dev.spei1_gdm SET may = NULL WHERE may < -1111.0 OR may > 1111.0;
	UPDATE dev.spei1_gdm SET jun = NULL WHERE jun < -1111.0 OR jun > 1111.0;
	UPDATE dev.spei1_gdm SET jul = NULL WHERE jul < -1111.0 OR jul > 1111.0;
	UPDATE dev.spei1_gdm SET aug = NULL WHERE aug < -1111.0 OR aug > 1111.0;
	UPDATE dev.spei1_gdm SET sep = NULL WHERE sep < -1111.0 OR sep > 1111.0;
	UPDATE dev.spei1_gdm SET oct = NULL WHERE oct < -1111.0 OR oct > 1111.0;
	UPDATE dev.spei1_gdm SET nov = NULL WHERE nov < -1111.0 OR nov > 1111.0;
	UPDATE dev.spei1_gdm SET des = NULL WHERE des < -1111.0 OR des > 1111.0;
	
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calcYearlySPEI1(1950, 2014);




