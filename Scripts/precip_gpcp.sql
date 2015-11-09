

-- RECENTER RASTER ON EUROPE INSTEAD OF PACIFIC
DROP TABLE IF EXISTS dev.precip_gpcp_recentered;
CREATE TABLE dev.precip_gpcp_recentered
AS
(
SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
FROM roll_raster_table('orig.precip_gpcp', 'rast', 180)  -- roll the pacific and americas off the right edge and back around the left edge
);

-- ADD INDEX
CREATE INDEX precip_gpcp_recentered_idx ON dev.precip_gpcp_recentered USING gist(ST_ConvexHull(rast));
ANALYZE dev.precip_gpcp_recentered;






-- Summarize stats, yearly average of monthly precip
-- Because the raster cells are larger than the priogrid cell polygons, the usual tile clip and summary stats won't work.
-- Instead using a simple nearest value approach so clusters of smaller priogrid cells "inherit" value from their nearest overarching raster cell

-- DEFINE MAIN FUNCTION
CREATE OR REPLACE FUNCTION calcYearlyGPCP(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS precip_gpcp;
	CREATE TABLE precip_gpcp(	gid integer, "year" integer,
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
					total decimal
					);

	-- Calculate stats for each band
	FOR i IN _from.._to LOOP
		RAISE NOTICE '%', i;

		-- add row
		INSERT INTO precip_gpcp
		SELECT 	p.gid, 
			i AS "year", 
			-- Value of a particular month/band, and because the value is an average/index we assume it to be true for all smaller priogrid cells
			-- ALSO: since, since value is monthly mean (of daily rain totals), multiplying by number of days in month gives approximate monthly total
			--       which allows us to compare with gpcc's monthly totals values.
			-- Band 1 is jan 1979 and increments linearly up, so some math to convert year to correct month band nr
			ST_NearestValue(pr.rast, (i-1979)*12+1, p.centroid, FALSE) * days_in_month(i, 1) AS jan,
			ST_NearestValue(pr.rast, (i-1979)*12+2, p.centroid, FALSE) * days_in_month(i, 2) AS feb,
			ST_NearestValue(pr.rast, (i-1979)*12+3, p.centroid, FALSE) * days_in_month(i, 3) AS mar,
			ST_NearestValue(pr.rast, (i-1979)*12+4, p.centroid, FALSE) * days_in_month(i, 4) AS apr,
			ST_NearestValue(pr.rast, (i-1979)*12+5, p.centroid, FALSE) * days_in_month(i, 5) AS may,
			ST_NearestValue(pr.rast, (i-1979)*12+6, p.centroid, FALSE) * days_in_month(i, 6) AS jun,
			ST_NearestValue(pr.rast, (i-1979)*12+7, p.centroid, FALSE) * days_in_month(i, 7) AS jul,
			ST_NearestValue(pr.rast, (i-1979)*12+8, p.centroid, FALSE) * days_in_month(i, 8) AS aug,
			ST_NearestValue(pr.rast, (i-1979)*12+9, p.centroid, FALSE) * days_in_month(i, 9) AS sep,
			ST_NearestValue(pr.rast, (i-1979)*12+10, p.centroid, FALSE) * days_in_month(i, 10) AS oct,
			ST_NearestValue(pr.rast, (i-1979)*12+11, p.centroid, FALSE) * days_in_month(i, 11) AS nov,
			ST_NearestValue(pr.rast, (i-1979)*12+12, p.centroid, FALSE) * days_in_month(i, 12) AS des
		FROM priogrid_land AS p, dev.precip_gpcp_recentered AS pr
		WHERE p.centroid && pr.rast -- bboxes intersect (faster)
		;
	END LOOP;

	-- set null values
	UPDATE precip_gpcp SET jan = NULL WHERE jan < -999999999;
	UPDATE precip_gpcp SET feb = NULL WHERE feb < -999999999;
	UPDATE precip_gpcp SET mar = NULL WHERE mar < -999999999;
	UPDATE precip_gpcp SET apr = NULL WHERE apr < -999999999;
	UPDATE precip_gpcp SET may = NULL WHERE may < -999999999;
	UPDATE precip_gpcp SET jun = NULL WHERE jun < -999999999;
	UPDATE precip_gpcp SET jul = NULL WHERE jul < -999999999;
	UPDATE precip_gpcp SET aug = NULL WHERE aug < -999999999;
	UPDATE precip_gpcp SET sep = NULL WHERE sep < -999999999;
	UPDATE precip_gpcp SET oct = NULL WHERE oct < -999999999;
	UPDATE precip_gpcp SET nov = NULL WHERE nov < -999999999;
	UPDATE precip_gpcp SET des = NULL WHERE des < -999999999;

	-- calc yearly column
	UPDATE precip_gpcp
	SET total = jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + des
	;
	
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calcYearlyGPCP(1979, 2014);


