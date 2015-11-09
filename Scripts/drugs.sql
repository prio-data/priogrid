
-- First combine all drug type tables into one
CREATE TEMP TABLE drugs
AS
(
SELECT product_ AS product, country_ AS country, begin_ AS startyear, end_ AS endyear, geom FROM orig.drugs_cannabis
UNION ALL
SELECT product, country, begin_ AS startyear, end_ AS endyear, geom FROM orig.drugs_coca
UNION ALL
SELECT product, country, "begin" AS startyear, "end" AS endyear, geom FROM orig.drugs_opium
);

-- Begin and end dates are somehow loaded as text, so convert to integer
ALTER TABLE drugs ALTER COLUMN startyear TYPE integer USING (trim(startyear)::integer);
ALTER TABLE drugs ALTER COLUMN endyear TYPE integer USING (trim(endyear)::integer);

-- Set null values for ending to latest possible year
UPDATE drugs SET endyear = 2002 WHERE endyear IS NULL;

-- Change earliest year values to a minimum of 1946
UPDATE drugs SET startyear = 1946 WHERE startyear < 1946;


-- Add spatial index
CREATE INDEX drugs_geom_idx ON drugs USING gist(geom);
ANALYZE drugs;


-- Create yearly dummy series
DROP TABLE IF EXISTS drugs_y;
CREATE TABLE drugs_y
AS
(
SELECT gid, generate_series(startyear, endyear) as year, drugs
FROM (
	SELECT 	p.gid, 
		MIN(d.startyear) as startyear, 
		MAX(endyear) as endyear,
		1 AS drugs
	FROM drugs AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);




