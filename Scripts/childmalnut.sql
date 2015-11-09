
-- Clip raster tiles to priogrid cells
DROP TABLE IF EXISTS childmalnut;
CREATE TABLE childmalnut
AS
(
SELECT gid, ST_Union(rast) as rast
FROM 
	(
	SELECT p.gid, ST_Clip(cmr.rast, p.cell) as rast 
	FROM orig.childmalnut AS cmr, priogrid_land AS p
	WHERE ST_Intersects(cmr.rast, p.cell)
	) AS foo
GROUP BY gid
);




-- Correct value error in original data, where value is supposed to be in percent but is only an integer and should have comma one digit to the left
-- Error cause and solution confirmed via email correspondence with Linda Pistolesi at CIESIN. 
UPDATE childmalnut SET rast = ST_MapAlgebra(rast, '32BF', '[rast.val]::numeric / 10.0');




-- Calculate stats

ALTER TABLE childmalnut 	ADD COLUMN cmr_sum double precision, 
				ADD COLUMN cmr_mean double precision,
				ADD COLUMN cmr_sd double precision,  
				ADD COLUMN cmr_min double precision, 
				ADD COLUMN cmr_max double precision, 
				ADD COLUMN cmr_count integer;

UPDATE childmalnut SET cmr_sum = (ST_SummaryStats(rast)).sum;
UPDATE childmalnut SET cmr_mean = (ST_SummaryStats(rast)).mean;
UPDATE childmalnut SET cmr_sd = (ST_SummaryStats(rast)).stddev;
UPDATE childmalnut SET cmr_min = (ST_SummaryStats(rast)).min;
UPDATE childmalnut SET cmr_max = (ST_SummaryStats(rast)).max;
UPDATE childmalnut SET cmr_count = (ST_SummaryStats(rast)).count;



