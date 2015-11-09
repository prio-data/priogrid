


-- Summarize stats, yearly average of monthly precip
-- Because the raster cells are larger than the priogrid cell polygons, the usual tile clip and summary stats won't work.
-- Instead using a simple nearest value approach so clusters of smaller priogrid cells "inherit" value from their nearest overarching raster cell
-- ETA: 5-6 hrs

-- DEFINE MAIN FUNCTION
CREATE OR REPLACE FUNCTION calcYearlyGPCC(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS precip_gpcc;
	CREATE TABLE precip_gpcc(	gid integer, "year" integer,
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
		INSERT INTO precip_gpcc
		SELECT 	p.gid, 
			i AS "year", 
			-- band 1 is jan 1901 and increments linearly up, so some math to convert year to correct month band nr
			-- FALSE at end necessary to accept null values instead of searching for nearest non-null value which would be wrong.
			ST_NearestValue(pr.rast, (i-1901)*12+1, p.centroid, FALSE) AS jan,
			ST_NearestValue(pr.rast, (i-1901)*12+2, p.centroid, FALSE) AS feb,
			ST_NearestValue(pr.rast, (i-1901)*12+3, p.centroid, FALSE) AS mar,
			ST_NearestValue(pr.rast, (i-1901)*12+4, p.centroid, FALSE) AS apr,
			ST_NearestValue(pr.rast, (i-1901)*12+5, p.centroid, FALSE) AS may,
			ST_NearestValue(pr.rast, (i-1901)*12+6, p.centroid, FALSE) AS jun,
			ST_NearestValue(pr.rast, (i-1901)*12+7, p.centroid, FALSE) AS jul,
			ST_NearestValue(pr.rast, (i-1901)*12+8, p.centroid, FALSE) AS aug,
			ST_NearestValue(pr.rast, (i-1901)*12+9, p.centroid, FALSE) AS sep,
			ST_NearestValue(pr.rast, (i-1901)*12+10, p.centroid, FALSE) AS oct,
			ST_NearestValue(pr.rast, (i-1901)*12+11, p.centroid, FALSE) AS nov,
			ST_NearestValue(pr.rast, (i-1901)*12+12, p.centroid, FALSE) AS des
		FROM priogrid_land AS p, orig.precip_gpcc AS pr
		WHERE p.centroid && pr.rast -- bboxes intersect (faster)
		;
	END LOOP;

	-- set null values
	UPDATE precip_gpcc SET jan = NULL WHERE jan < 0;
	UPDATE precip_gpcc SET feb = NULL WHERE feb < 0;
	UPDATE precip_gpcc SET mar = NULL WHERE mar < 0;
	UPDATE precip_gpcc SET apr = NULL WHERE apr < 0;
	UPDATE precip_gpcc SET may = NULL WHERE may < 0;
	UPDATE precip_gpcc SET jun = NULL WHERE jun < 0;
	UPDATE precip_gpcc SET jul = NULL WHERE jul < 0;
	UPDATE precip_gpcc SET aug = NULL WHERE aug < 0;
	UPDATE precip_gpcc SET sep = NULL WHERE sep < 0;
	UPDATE precip_gpcc SET oct = NULL WHERE oct < 0;
	UPDATE precip_gpcc SET nov = NULL WHERE nov < 0;
	UPDATE precip_gpcc SET des = NULL WHERE des < 0;

	-- calc yearly total column
	UPDATE precip_gpcc
	SET total = jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + des
	;
	
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calcYearlyGPCC(1946, 2013);



