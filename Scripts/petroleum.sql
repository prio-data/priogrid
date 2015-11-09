

-- ADD GWNO COLUMN BY JOINING TO LOOKUP TABLE, AND SIMPLIFY GEOM (VERY IMPORTANT FOR SPEED)
DROP TABLE IF EXISTS petrongw;
CREATE TEMP TABLE petrongw AS 
	(SELECT d.geom AS geom, d.prod, d.disc, c.gwno FROM orig.petron AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);

-- Set some null values
UPDATE petrongw AS p SET disc = NULL WHERE disc = -9999;
UPDATE petrongw AS p SET prod = NULL WHERE prod = -9999;

-- Change earliest year values to a minimum of 1946
UPDATE petrongw SET disc = 1946 WHERE disc < 1946;
UPDATE petrongw SET prod = 1946 WHERE prod < 1946;

-- Add spatial index
CREATE INDEX petrongw_idx_geom ON petrongw USING gist(geom);
ANALYZE petrongw;



-- Create yearly petroleum presence dummy for records with discovery dates

DROP TABLE IF EXISTS petroleum_y;
CREATE TABLE petroleum_y
AS
(
SELECT gid, generate_series(startyear, 2003) as year, petroleum
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.prod), MIN(d.disc)) as startyear, -- earliest occurance (disc or prod) of all the polys in a cell
		1 AS petroleum
	FROM petrongw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);





-- Create static petroleum presence dummy for records without dates

DROP TABLE IF EXISTS petroleum_s;
CREATE TABLE petroleum_s
AS
(
SELECT 	p.gid, 
	1 AS petroleum
FROM petrongw AS pt, priogrid_land AS p
WHERE 	(pt.prod IS NULL AND pt.disc IS NULL)
	AND ST_Intersects(pt.geom, p.cell)
GROUP BY p.gid
);




-- Offshore
-- Decided not to use, since most of priogrid is land only









