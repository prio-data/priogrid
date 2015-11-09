
-- Join all years together into one raster table

DROP TABLE IF EXISTS dev.irrigation;
CREATE TABLE dev.irrigation(rast, "year")
AS
(
SELECT rast, "year"
FROM
	(
	SELECT i.rast, 1950 AS "year" FROM orig.irrigation50 AS i
	UNION ALL
	SELECT i.rast, 1960 AS "year" FROM orig.irrigation60 AS i
	UNION ALL
	SELECT i.rast, 1970 AS "year" FROM orig.irrigation70 AS i
	UNION ALL
	SELECT i.rast, 1980 AS "year" FROM orig.irrigation80 AS i
	UNION ALL
	SELECT i.rast, 1985 AS "year" FROM orig.irrigation85 AS i
	UNION ALL
	SELECT i.rast, 1990 AS "year" FROM orig.irrigation90 AS i
	UNION ALL
	SELECT i.rast, 1995 AS "year" FROM orig.irrigation95 AS i
	UNION ALL
	SELECT i.rast, 2000 AS "year" FROM orig.irrigation00 AS i
	UNION ALL
	SELECT i.rast, 2005 AS "year" FROM orig.irrigation05 AS i
	) AS foo
);

CREATE INDEX irrigation_st_convexhull_idx ON dev.irrigation USING GIST(ST_Convexhull(rast));
ANALYZE dev.irrigation;

-- Clip raster tiles to fit within each priogrid cell

DROP TABLE IF EXISTS irrigation;
CREATE TABLE irrigation(gid, "year", rast)
AS
(
SELECT gid, "year", ST_Union(rast) as rast
FROM 
	(
	SELECT p.gid, i."year", ST_Clip(i.rast, p.cell) as rast
	FROM dev.irrigation AS i, priogrid_land AS p
	WHERE i.rast && p.cell
	) AS foo
GROUP BY gid, "year"
);

-- Summary stats on each raster tile/priogrid cell

ALTER TABLE irrigation ADD COLUMN sum double precision, 
			ADD COLUMN mean double precision,
			ADD COLUMN sd double precision,  
			ADD COLUMN min double precision, 
			ADD COLUMN max double precision, 
			ADD COLUMN count integer;

UPDATE irrigation SET sum = (ST_SummaryStats(rast)).sum;
UPDATE irrigation SET mean = (ST_SummaryStats(rast)).mean;
UPDATE irrigation SET sd = (ST_SummaryStats(rast)).stddev;
UPDATE irrigation SET min = (ST_SummaryStats(rast)).min;
UPDATE irrigation SET max = (ST_SummaryStats(rast)).max;
UPDATE irrigation SET count = (ST_SummaryStats(rast)).count;


