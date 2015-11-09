/* 
Here, we made the decision not to check whether points are in the same country as coded for the PRIO-GRID cell.
Users should be aware of this, and make these checks themselves if they find this relevant.
*/

-- ADD GWNO COLUMN BY JOINING TO LOOKUP TABLE
DROP TABLE IF EXISTS diamlootgw;
CREATE TEMP TABLE diamlootgw AS
	(SELECT d.*, c.gwno FROM orig.diamloot AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);
DROP TABLE IF EXISTS diamnonlootgw;
CREATE TEMP TABLE diamnonlootgw AS
	(SELECT d.*, c.gwno FROM orig.diamnonloot AS d LEFT JOIN orig.cow2gw AS c ON d.min_cowcod = c.cow);

-- SET SOME NULL DATA
UPDATE diamlootgw SET disc_year = NULL WHERE disc_year = 9999;
UPDATE diamlootgw SET prod_year = NULL WHERE prod_year = 9999;
UPDATE diamnonlootgw SET min_disc_y = NULL WHERE min_disc_y = 9999;
UPDATE diamnonlootgw SET min_prod_y = NULL WHERE min_prod_y = 9999;

-- CREATE SPATIAL INDEXES
CREATE INDEX diamlootgw_idx_geom ON diamlootgw USING GIST(geom);
ANALYZE diamlootgw;
CREATE INDEX diamnonlootgw_idx_geom ON diamnonlootgw USING GIST(geom);
ANALYZE diamnonlootgw;





-- Create yearly diamond presence dummy for records with discovery dates

-- lootable
CREATE TEMP TABLE diamloot_yearly
AS
(
SELECT gid, generate_series(startyear, 2005) as year, diamloot
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.prod_year), MIN(d.disc_year)) as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS diamloot
	FROM diamlootgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);

-- nonlootable
CREATE TEMP TABLE diamnonloot_yearly
AS
(
SELECT gid, generate_series(startyear, 2005) as year, diamnonloot
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.min_prod_y), MIN(d.min_disc_y)) as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS diamnonloot
	FROM diamnonlootgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);

-- combine
DROP TABLE IF EXISTS diamonds_y;
CREATE TABLE diamonds_y
AS
(
SELECT gid, year, dl.diamloot, dnl.diamnonloot
FROM diamloot_yearly AS dl
FULL OUTER JOIN diamnonloot_yearly AS dnl USING(gid, year)
);




-- Create static diamond presence dummy for records without dates

-- lootable
CREATE TEMP TABLE diamloot_static
AS
(
SELECT 	p.gid,
	1 AS diamloot
FROM diamlootgw AS d, priogrid_land AS p
WHERE 	(d.disc_year IS NULL AND d.prod_year IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);

-- nonlootable
CREATE TEMP TABLE diamnonloot_static
AS
(
SELECT 	p.gid, 
	1 AS diamnonloot
FROM diamnonlootgw AS d, priogrid_land AS p
WHERE 	(d.min_disc_y IS NULL AND d.min_prod_y IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);

-- combine
DROP TABLE IF EXISTS diamonds_s;
CREATE TABLE diamonds_s
AS
(
SELECT gid, dl.diamloot, dnl.diamnonloot
FROM diamloot_static AS dl
FULL OUTER JOIN diamnonloot_static AS dnl USING(gid)
);




