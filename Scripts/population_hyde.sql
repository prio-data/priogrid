
-- Join all years together into one raster table

DROP TABLE IF EXISTS dev.pop_hyde;
CREATE TABLE dev.pop_hyde(rast, "year")
AS
(
SELECT rast, "year"
FROM
	(
	SELECT i.rast, 1950 AS "year" FROM orig.pophyde_1950 AS i
	UNION ALL
	SELECT i.rast, 1960 AS "year" FROM orig.pophyde_1960 AS i
	UNION ALL
	SELECT i.rast, 1970 AS "year" FROM orig.pophyde_1970 AS i
	UNION ALL
	SELECT i.rast, 1980 AS "year" FROM orig.pophyde_1980 AS i
	UNION ALL
	SELECT i.rast, 1990 AS "year" FROM orig.pophyde_1990 AS i
	UNION ALL
	SELECT i.rast, 2000 AS "year" FROM orig.pophyde_2000 AS i
	UNION ALL
	SELECT i.rast, 2005 AS "year" FROM orig.pophyde_2005 AS i
	) AS foo
);

CREATE INDEX pophyde_st_convexhull_idx ON dev.pop_hyde USING GIST(ST_Convexhull(rast));
ANALYZE dev.pop_hyde;

-- Clip raster tiles to fit within each priogrid cell

DROP TABLE IF EXISTS pop_hyde;
CREATE TABLE pop_hyde(gid, "year", rast)
AS
(
SELECT gid, "year", ST_Union(rast) as rast
FROM 
	(
	SELECT p.gid, i."year", ST_Clip(i.rast, p.cell) as rast
	FROM dev.pop_hyde AS i, priogrid_land AS p
	WHERE i.rast && p.cell
	) AS foo
GROUP BY gid, "year"
);

-- Summary stats on each raster tile/priogrid cell

ALTER TABLE pop_hyde ADD COLUMN sum double precision, 
			ADD COLUMN mean double precision,
			ADD COLUMN sd double precision,  
			ADD COLUMN min double precision, 
			ADD COLUMN max double precision, 
			ADD COLUMN count integer;

UPDATE pop_hyde SET sum = (ST_SummaryStats(rast)).sum;
UPDATE pop_hyde SET mean = (ST_SummaryStats(rast)).mean;
UPDATE pop_hyde SET sd = (ST_SummaryStats(rast)).stddev;
UPDATE pop_hyde SET min = (ST_SummaryStats(rast)).min;
UPDATE pop_hyde SET max = (ST_SummaryStats(rast)).max;
UPDATE pop_hyde SET count = (ST_SummaryStats(rast)).count;


