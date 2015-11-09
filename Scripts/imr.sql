
/* TESTING
WITH imr_georef AS
	(SELECT ST_SetUpperLeft(rast, round(ST_UpperLeftX(rast)::numeric,16), round(ST_UpperLeftY(rast)::numeric,16)) AS rast FROM orig.imr)
SELECT gid, (ST_Metadata(rast)).*
FROM 
	(
	SELECT p.gid, ST_Clip(imr.rast, p.cell) as rast 
	FROM imr_georef AS imr, priogrid_land AS p
	WHERE imr.rast && p.cell
	--AND NOT ST_Disjoint(ST_Envelope(imr.rast), p.cell)
	) AS foo
	limit 100
*/





/* NEED TO TRY THIS ONE, MIGHT WORK
-- Clip raster tiles to priogrid cells
DROP TABLE IF EXISTS imr;
CREATE TABLE imr
AS
(
SELECT foo.gid, ST_Clip(foo.rast, p.cell) as rast
FROM 
	(
	SELECT p.gid, ST_Union( rast ) as rast 
	FROM orig.imr AS imr, priogrid_land AS p
	WHERE imr.rast && p.cell
	AND NOT ST_Disjoint(ST_Envelope(imr.rast), p.cell) 
	GROUP BY gid
	) AS foo
	,
	priogrid_land AS p
WHERE foo.gid = p.gid
);

select (st_metadata(rast)).* from imr
*/





DROP TABLE IF EXISTS imr;
CREATE TABLE imr
AS
(
SELECT gid, ST_Union(rast) as rast
FROM 
	(
	SELECT p.gid, ST_Clip(imr.rast, p.cell) as rast 
	FROM orig.imr AS imr, priogrid_land AS p
	WHERE imr.rast && p.cell
	AND NOT ST_Disjoint(ST_Envelope(imr.rast), p.cell)
	) AS foo
GROUP BY gid
);





-- Calculate stats

ALTER TABLE imr ADD COLUMN imr_sum double precision, 
						ADD COLUMN imr_mean double precision,
						ADD COLUMN imr_sd double precision,  
						ADD COLUMN imr_min double precision, 
						ADD COLUMN imr_max double precision, 
						ADD COLUMN imr_count integer;

UPDATE imr SET imr_sum = (ST_SummaryStats(rast)).sum;
UPDATE imr SET imr_mean = (ST_SummaryStats(rast)).mean;
UPDATE imr SET imr_sd = (ST_SummaryStats(rast)).stddev;
UPDATE imr SET imr_min = (ST_SummaryStats(rast)).min;
UPDATE imr SET imr_max = (ST_SummaryStats(rast)).max;
UPDATE imr SET imr_count = (ST_SummaryStats(rast)).count;



