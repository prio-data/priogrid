

-- Clip raster tiles to priogrid cells
--   Needs special handling since the raster is so huge with many tiles that georef raises alignment error due to float rounding error
--   Solution: Instead of the usual clip -> union, reversed to union -> clip 
DROP TABLE IF EXISTS accesstimes;
CREATE TABLE accesstimes
AS
(
SELECT unioned.gid, ST_Clip(unioned.rast, p.cell) AS rast -- clip away the excesses of the unioned rasters that do not overlap the priogrid cell
FROM 
	(
	SELECT p.gid, ST_Union(a.rast) AS rast 
	FROM orig.accesstimes AS a, priogrid_land AS p
	WHERE ST_Intersects(a.rast, p.cell) 
	GROUP BY gid
	) AS unioned -- unioned raster of all tiles that intersect with each priogrid cell
	
	LEFT JOIN priogrid_land AS p
	ON unioned.gid = p.gid -- match with priogrid cell geometry after the union, otherwise have to use slow group-by operation on cell geometry
);



-- ADD OUTPUT COLUMNS
ALTER TABLE accesstimes ADD COLUMN access_sum double precision, 
						ADD COLUMN access_mean double precision,
						ADD COLUMN access_sd double precision,  
						ADD COLUMN access_min double precision, 
						ADD COLUMN access_max double precision, 
						ADD COLUMN access_count integer;


-- CALCULATE STATS FOR EACH PRIOGRID EQUIVALENT SUBRASTER TILE
UPDATE accesstimes SET access_sum = (ST_SummaryStats(rast)).sum;
UPDATE accesstimes SET access_mean = (ST_SummaryStats(rast)).mean;
UPDATE accesstimes SET access_sd = (ST_SummaryStats(rast)).stddev;
UPDATE accesstimes SET access_min = (ST_SummaryStats(rast)).min;
UPDATE accesstimes SET access_max = (ST_SummaryStats(rast)).max;
UPDATE accesstimes SET access_count = (ST_SummaryStats(rast)).count;

