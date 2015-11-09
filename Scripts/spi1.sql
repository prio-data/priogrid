



-- RECENTER RASTER ON EUROPE INSTEAD OF PACIFIC
DROP TABLE IF EXISTS spi1_recentered;
CREATE TEMP TABLE spi1_recentered 
AS
(
SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
FROM roll_raster_table('orig.spi1', 'rast', 180)  -- roll the pacific and americas off the right edge and back around the left edge
);





-- ADD INDEX
CREATE INDEX spi1_recentered_idx ON spi1_recentered USING gist(ST_ConvexHull(rast));
ANALYZE spi1_recentered;






-- Summarize stats, yearly average of monthly precip
-- Because the raster cells are larger than the priogrid cell polygons, the usual tile clip and summary stats won't work.
-- Instead using a simple nearest value approach so clusters of smaller priogrid cells "inherit" value from their nearest overarching raster cell

/*below is just for ad hoc visualization purposes:
DROP TABLE IF EXISTS silly_sumstat;
CREATE TABLE silly_sumstat AS
SELECT p.cell, ST_NearestValue(prec.rast, 1, p.centroid)/25 AS prec  
FROM priogrid_land AS p, precip_gpcp_recentered AS prec
WHERE ST_Intersects(p.centroid, prec.rast)
;*/


-- DEFINE MAIN FUNCTION
CREATE OR REPLACE FUNCTION calcYearlySPI1(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS dev.spi1;
	CREATE TABLE dev.spi1(	gid integer, "year" integer,
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
		INSERT INTO dev.spi1
		SELECT 	p.gid, 
			i AS "year", 
			-- value of a particular month/band, and because the value is an average/index we assume it to be true for all smaller priogrid cells
			-- band 1 is jan 1979 and increments linearly up, so some math to convert year to correct month band nr
			ST_NearestValue(pr.rast, (i-1979)*12+1, p.centroid, FALSE) AS jan,
			ST_NearestValue(pr.rast, (i-1979)*12+2, p.centroid, FALSE) AS feb,
			ST_NearestValue(pr.rast, (i-1979)*12+3, p.centroid, FALSE) AS mar,
			ST_NearestValue(pr.rast, (i-1979)*12+4, p.centroid, FALSE) AS apr,
			ST_NearestValue(pr.rast, (i-1979)*12+5, p.centroid, FALSE) AS may,
			ST_NearestValue(pr.rast, (i-1979)*12+6, p.centroid, FALSE) AS jun,
			ST_NearestValue(pr.rast, (i-1979)*12+7, p.centroid, FALSE) AS jul,
			ST_NearestValue(pr.rast, (i-1979)*12+8, p.centroid, FALSE) AS aug,
			ST_NearestValue(pr.rast, (i-1979)*12+9, p.centroid, FALSE) AS sep,
			ST_NearestValue(pr.rast, (i-1979)*12+10, p.centroid, FALSE) AS oct,
			ST_NearestValue(pr.rast, (i-1979)*12+11, p.centroid, FALSE) AS nov,
			ST_NearestValue(pr.rast, (i-1979)*12+12, p.centroid, FALSE) AS des
		FROM priogrid_land AS p, spi1_recentered AS pr
		WHERE p.centroid && pr.rast -- bboxes intersect (faster)
		;
	END LOOP;

	-- set null values
	UPDATE dev.spi1 SET jan = NULL WHERE jan < -999;
	UPDATE dev.spi1 SET feb = NULL WHERE feb < -999;
	UPDATE dev.spi1 SET mar = NULL WHERE mar < -999;
	UPDATE dev.spi1 SET apr = NULL WHERE apr < -999;
	UPDATE dev.spi1 SET may = NULL WHERE may < -999;
	UPDATE dev.spi1 SET jun = NULL WHERE jun < -999;
	UPDATE dev.spi1 SET jul = NULL WHERE jul < -999;
	UPDATE dev.spi1 SET aug = NULL WHERE aug < -999;
	UPDATE dev.spi1 SET sep = NULL WHERE sep < -999;
	UPDATE dev.spi1 SET oct = NULL WHERE oct < -999;
	UPDATE dev.spi1 SET nov = NULL WHERE nov < -999;
	UPDATE dev.spi1 SET des = NULL WHERE des < -999;
	
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calcYearlySPI1(1979, 2014);


