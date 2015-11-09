
-- RECENTER RASTER ON EUROPE INSTEAD OF PACIFIC
DROP TABLE IF EXISTS dev.temp_recentered;
CREATE TABLE dev.temp_recentered
AS
(
SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
FROM    -- NOTE: not using the "recenter_raster_table" function since there is something odd with temp that results in an error, so switching sides manually using a different clip offset
	(
	WITH 
	_input AS
		(SELECT * FROM orig.temp)
		,
	_clipped AS
	-- clip all tiles to the left and right of new rightedge
		(		
		SELECT ST_Clip(rast, ST_SetSRID(ST_MakeEnvelope(0, -90, 180, 90), 4326)) AS rast  
		FROM _input
		WHERE rast && ST_MakeEnvelope(0, -90, 180, 90) --intersects left side 
		
		UNION ALL
		
		SELECT ST_Clip(rast, ST_SetSRID(ST_MakeEnvelope(180.0001, -90, 360, 90), 4326)) AS rast
		FROM _input
		WHERE rast && ST_MakeEnvelope(180.0001, -90, 360, 90) --intersects right side (decimals to avoid weird edge intersection empty band clip error)
		)
	-- switch sides the tiles around the middle (left offset any tiles that are beyond the middle, and right offset all the other ones)
	SELECT (CASE WHEN ST_UpperLeftX(rast) >= 180
		THEN ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast))
		ELSE ST_SetUpperLeft(rast, ST_UpperLeftX(rast)+180, ST_UpperLeftY(rast))
		END) AS rast
	FROM _clipped
	) AS foo
);



-- ADD INDEX
CREATE INDEX temp_recentered_idx ON dev.temp_recentered USING gist(ST_ConvexHull(rast));
ANALYZE dev.temp_recentered;





-- Summarize stats, yearly average of monthly temp
-- Because the raster cells are larger than the priogrid cell polygons, the usual tile clip and summary stats won't work.
-- Instead using a simple nearest value approach so clusters of smaller priogrid cells "inherit" value from their nearest overarching raster cell

-- DEFINE MAIN FUNCTION
CREATE OR REPLACE FUNCTION calcYearlyTemp(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS "temp";
	CREATE TABLE "temp"(	gid integer, "year" integer,
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
		INSERT INTO "temp"
		SELECT 	p.gid, 
			i AS "year", 
			-- value of a particular month/band taken as is, because same resolution as priogrid, so one to one correspondance
			-- band 1 is jan 1948 and increments linearly up, so some math to convert year to correct month band nr
			-- FALSE at end necessary to accept null values instead of searching for nearest non-null value which would be wrong.
			ST_NearestValue(pr.rast, (i-1948)*12+1, p.centroid, FALSE) AS jan,
			ST_NearestValue(pr.rast, (i-1948)*12+2, p.centroid, FALSE) AS feb,
			ST_NearestValue(pr.rast, (i-1948)*12+3, p.centroid, FALSE) AS mar,
			ST_NearestValue(pr.rast, (i-1948)*12+4, p.centroid, FALSE) AS apr,
			ST_NearestValue(pr.rast, (i-1948)*12+5, p.centroid, FALSE) AS may,
			ST_NearestValue(pr.rast, (i-1948)*12+6, p.centroid, FALSE) AS jun,
			ST_NearestValue(pr.rast, (i-1948)*12+7, p.centroid, FALSE) AS jul,
			ST_NearestValue(pr.rast, (i-1948)*12+8, p.centroid, FALSE) AS aug,
			ST_NearestValue(pr.rast, (i-1948)*12+9, p.centroid, FALSE) AS sep,
			ST_NearestValue(pr.rast, (i-1948)*12+10, p.centroid, FALSE) AS oct,
			ST_NearestValue(pr.rast, (i-1948)*12+11, p.centroid, FALSE) AS nov,
			ST_NearestValue(pr.rast, (i-1948)*12+12, p.centroid, FALSE) AS des
		FROM priogrid_land AS p, dev.temp_recentered AS pr
		WHERE p.centroid && pr.rast -- bboxes intersect (faster)
		;
	END LOOP;

	-- set null values
	UPDATE "temp" SET jan = NULL WHERE jan < -999999999;
	UPDATE "temp" SET feb = NULL WHERE feb < -999999999;
	UPDATE "temp" SET mar = NULL WHERE mar < -999999999;
	UPDATE "temp" SET apr = NULL WHERE apr < -999999999;
	UPDATE "temp" SET may = NULL WHERE may < -999999999;
	UPDATE "temp" SET jun = NULL WHERE jun < -999999999;
	UPDATE "temp" SET jul = NULL WHERE jul < -999999999;
	UPDATE "temp" SET aug = NULL WHERE aug < -999999999;
	UPDATE "temp" SET sep = NULL WHERE sep < -999999999;
	UPDATE "temp" SET oct = NULL WHERE oct < -999999999;
	UPDATE "temp" SET nov = NULL WHERE nov < -999999999;
	UPDATE "temp" SET des = NULL WHERE des < -999999999;

	-- convert from kelvin degrees to celsius degrees
	UPDATE "temp" SET jan = jan - 273.15 WHERE jan IS NOT NULL;
	UPDATE "temp" SET feb = feb - 273.15 WHERE feb IS NOT NULL;
	UPDATE "temp" SET mar = mar - 273.15 WHERE mar IS NOT NULL;
	UPDATE "temp" SET apr = apr - 273.15 WHERE apr IS NOT NULL;
	UPDATE "temp" SET may = may - 273.15 WHERE may IS NOT NULL;
	UPDATE "temp" SET jun = jun - 273.15 WHERE jun IS NOT NULL;
	UPDATE "temp" SET jul = jul - 273.15 WHERE jul IS NOT NULL;
	UPDATE "temp" SET aug = aug - 273.15 WHERE aug IS NOT NULL;
	UPDATE "temp" SET sep = sep - 273.15 WHERE sep IS NOT NULL;
	UPDATE "temp" SET oct = oct - 273.15 WHERE oct IS NOT NULL;
	UPDATE "temp" SET nov = nov - 273.15 WHERE nov IS NOT NULL;
	UPDATE "temp" SET des = des - 273.15 WHERE des IS NOT NULL;

	-- calc yearly avg column
	UPDATE "temp"
	SET average = (jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + des) / 12
	;
	
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calcYearlyTemp(1948, 2014);

