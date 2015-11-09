
-- WARNING, THIS SCRIPT TAKES ?? HOURS ---

-- Clip globcover raster tiles to priogrid cells

DROP TABLE IF EXISTS globcover_clipped;
CREATE TEMP TABLE globcover_clipped AS
(
SELECT gid, ST_Union(raster) as rast
FROM 
	(
	SELECT p.gid, ST_Clip(g.rast, p.cell) as raster 
	FROM orig.globcover AS g, priogrid_land AS p
	WHERE g.rast && p.cell
	) AS foo
GROUP BY gid
);

-- For each gid/raster, create multiple rows containing the count for each unique value
DROP TABLE IF EXISTS dev.globcover;
CREATE TABLE dev.globcover
AS
(
SELECT gid, (ST_ValueCount(rast)).*
FROM globcover_clipped
);

-- Add index because will be using many where queries on the "value" column
CREATE INDEX globcover_idx_1 ON dev.globcover USING BTREE("value");
ANALYZE dev.globcover;


-- Calculate cell counts of different landcovers in new table

DROP TABLE IF EXISTS globcover;
CREATE TABLE globcover
AS
(
SELECT p.gid, u.num_urban, c.num_crop, f.num_forest, sh.num_shrub, hr.num_herb, av.num_aquaveg, b.num_barren, w.num_water, a.num_cells 
FROM priogrid_land AS p
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_urban 
	FROM dev.globcover as m 
	WHERE value = 190
	GROUP BY gid
	) as u
ON p.gid = u.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_crop
	FROM dev.globcover as m 
	WHERE value <= 30
	GROUP BY gid
	) as c
ON p.gid = c.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_forest 
	FROM dev.globcover as m 
	WHERE 	(value >= 40 AND value <=120) 
	GROUP BY gid
	) AS f
ON p.gid = f.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_shrub 
	FROM dev.globcover as m 
	WHERE value = 130
	GROUP BY gid
	) AS sh
ON p.gid = sh.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_herb
	FROM dev.globcover as m 
	WHERE value = 140
	GROUP BY gid
	) AS hr
ON p.gid = hr.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_aquaveg
	FROM dev.globcover as m 
	WHERE value >= 150 AND value <= 180
	GROUP BY gid
	) AS av
ON p.gid = av.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_barren 
	FROM dev.globcover as m 
	WHERE value = 200 OR value = 220
	GROUP BY gid
	) as b
ON p.gid = b.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_water 
	FROM dev.globcover as m 
	WHERE value = 210
	GROUP BY gid
	) as w
ON p.gid = w.gid
FULL OUTER JOIN
	(
	SELECT gid, sum(count) as num_cells 
	FROM dev.globcover as m 
	GROUP BY gid
	) as a
ON p.gid = a.gid
);


-- Calculate new stats vars as proportion of total cells

ALTER TABLE globcover 	ADD COLUMN urban double precision,
			ADD COLUMN crop double precision,
			ADD COLUMN forest double precision,
			ADD COLUMN shrub double precision,
			ADD COLUMN herb double precision,
			ADD COLUMN aquaveg double precision,
			ADD COLUMN barren double precision,
			ADD COLUMN water double precision;

UPDATE globcover SET urban = num_urban::float / num_cells::float * 100;
UPDATE globcover SET crop = num_crop::float / num_cells::float * 100;
UPDATE globcover SET forest = num_forest::float / num_cells::float * 100;
UPDATE globcover SET shrub = num_shrub::float / num_cells::float * 100;
UPDATE globcover SET herb = num_herb::float / num_cells::float * 100;
UPDATE globcover SET aquaveg = num_aquaveg::float / num_cells::float * 100;
UPDATE globcover SET barren = num_barren::float / num_cells::float * 100;
UPDATE globcover SET water = num_water::float / num_cells::float * 100;


-- Make missing into 0 values
UPDATE globcover SET urban = 0 WHERE urban IS NULL;
UPDATE globcover SET crop = 0 WHERE crop IS NULL;
UPDATE globcover SET forest = 0 WHERE forest IS NULL;
UPDATE globcover SET shrub = 0 WHERE shrub IS NULL;
UPDATE globcover SET herb = 0 WHERE herb IS NULL;
UPDATE globcover SET aquaveg = 0 WHERE aquaveg IS NULL;
UPDATE globcover SET barren = 0 WHERE barren IS NULL;
UPDATE globcover SET water = 0 WHERE water IS NULL;



