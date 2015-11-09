-- Add year column

ALTER TABLE orig.pop_gpw90 DROP COLUMN IF EXISTS "year";
ALTER TABLE orig.pop_gpw95 DROP COLUMN IF EXISTS "year";
ALTER TABLE orig.pop_gpw00 DROP COLUMN IF EXISTS "year";
ALTER TABLE orig.pop_gpw05 DROP COLUMN IF EXISTS "year";
ALTER TABLE orig.pop_gpw10 DROP COLUMN IF EXISTS "year";

ALTER TABLE orig.pop_gpw90 ADD COLUMN "year" integer;
ALTER TABLE orig.pop_gpw95 ADD COLUMN "year" integer;
ALTER TABLE orig.pop_gpw00 ADD COLUMN "year" integer;
ALTER TABLE orig.pop_gpw05 ADD COLUMN "year" integer;
ALTER TABLE orig.pop_gpw10 ADD COLUMN "year" integer;

UPDATE orig.pop_gpw90 SET "year" = 1990;
UPDATE orig.pop_gpw95 SET "year" = 1995;
UPDATE orig.pop_gpw00 SET "year" = 2000;
UPDATE orig.pop_gpw05 SET "year" = 2005;
UPDATE orig.pop_gpw10 SET "year" = 2010;

-- Join all years together into one raster table

DROP TABLE IF EXISTS dev.pop_gpw;
CREATE TABLE dev.pop_gpw
AS
(
SELECT rast, "year"
FROM
	(
	SELECT * FROM orig.pop_gpw90
	UNION ALL
	SELECT * FROM orig.pop_gpw95
	UNION ALL
	SELECT * FROM orig.pop_gpw00
	UNION ALL
	SELECT * FROM orig.pop_gpw05
	UNION ALL
	SELECT * FROM orig.pop_gpw10
	) as foo
);

-- Clip raster tiles to fit within each priogrid cell

DROP TABLE IF EXISTS pop_gpw;
CREATE TABLE pop_gpw
AS
(
SELECT gid, "year", ST_Union(rast) as rast
FROM 
	(
	SELECT p.gid, gpw."year", ST_Clip(gpw.rast, p.cell) as rast 
	FROM dev.pop_gpw AS gpw, priogrid_land AS p
	WHERE ST_Intersects(gpw.rast, p.cell)
	) AS foo
GROUP BY gid, "year"
);

-- Summary stats on each raster tile/priogrid cell

ALTER TABLE pop_gpw ADD COLUMN population_sum double precision, 
						ADD COLUMN population_mean double precision,
						ADD COLUMN population_sd double precision,  
						ADD COLUMN population_min double precision, 
						ADD COLUMN population_max double precision, 
						ADD COLUMN population_count integer;

UPDATE pop_gpw SET population_sum = (ST_SummaryStats(rast)).sum;
UPDATE pop_gpw SET population_mean = (ST_SummaryStats(rast)).mean;
UPDATE pop_gpw SET population_sd = (ST_SummaryStats(rast)).stddev;
UPDATE pop_gpw SET population_min = (ST_SummaryStats(rast)).min;
UPDATE pop_gpw SET population_max = (ST_SummaryStats(rast)).max;
UPDATE pop_gpw SET population_count = (ST_SummaryStats(rast)).count;
