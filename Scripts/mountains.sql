
-- CREATE EMPTY RASTER DATA STRUCTURE
DROP TABLE IF EXISTS mountains;
CREATE TABLE mountains
(
  gid integer,
  rast raster
)
WITH (
  OIDS=FALSE
);


/* Since mountains rarely move or change significantly, we treat mountains as a stable variable across time. */
-- FOR EACH PRIO LAND GEOM:
-- 		CREATE NEW TILE THAT UNIONS ALL TILES THAT INTERSECT GEOM (WITH WATER MASKED AWAY BY CLIP)
--		AND INSERT INTO ABOVE RASTER
INSERT INTO mountains (gid, rast)
(SELECT gid, ST_Union(raster) as rast
FROM 
(SELECT p.gid, ST_Clip(m.rast, p.cell) as raster
FROM orig.mountains m, priogrid_land p
WHERE ST_Intersects(m.rast, p.cell)
)
as priorast
GROUP BY gid
);

-- ADD OUTPUT COLUMNS
ALTER TABLE mountains ADD COLUMN mountains_sum double precision,
			ADD COLUMN mountains_mean double precision, 
			ADD COLUMN mountains_sd double precision, 
			ADD COLUMN mountains_min double precision, 
			ADD COLUMN mountains_max double precision, 
			ADD COLUMN mountains_count integer;

-- CALCULATE STATS FOR EACH PRIOGRID EQUIVALENT SUBRASTER TILE
UPDATE mountains SET mountains_sum = (ST_SummaryStats(rast)).sum;
UPDATE mountains SET mountains_mean = (ST_SummaryStats(rast)).mean;
UPDATE mountains SET mountains_sd = (ST_SummaryStats(rast)).stddev;
UPDATE mountains SET mountains_min = (ST_SummaryStats(rast)).min;
UPDATE mountains SET mountains_max = (ST_SummaryStats(rast)).max;
UPDATE mountains SET mountains_count = (ST_SummaryStats(rast)).count;
