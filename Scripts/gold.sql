/* 
Here, we made the decision not to check whether points are in the same country as coded for the PRIO-GRID cell.
Users should be aware of this, and make these checks themselves if they find this relevant.
*/


-- ADD GWNO COLUMN BY JOINING TO LOOKUP TABLE
DROP TABLE IF EXISTS goldlootgw;
CREATE TEMP TABLE goldlootgw AS
	(SELECT d.*, c.gwno FROM orig.goldloot AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);
DROP TABLE IF EXISTS goldsemilootgw;
CREATE TEMP TABLE goldsemilootgw AS
	(SELECT d.*, c.gwno FROM orig.goldsemiloot AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);
DROP TABLE IF EXISTS goldnonlootgw;
CREATE TEMP TABLE goldnonlootgw AS
	(SELECT d.*, c.gwno FROM orig.goldnonloot AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);

-- SET SOME NULL DATA
UPDATE goldlootgw SET discyear = NULL WHERE discyear = 9999;
UPDATE goldlootgw SET prodyear = NULL WHERE prodyear = 9999;
UPDATE goldsemilootgw SET discyear = NULL WHERE discyear = 9999;
UPDATE goldsemilootgw SET prodyear = NULL WHERE prodyear = 9999;
UPDATE goldnonlootgw SET discyear = NULL WHERE discyear = 9999;
UPDATE goldnonlootgw SET prodyear = NULL WHERE prodyear = 9999;

-- CREATE SPATIAL INDEXES
CREATE INDEX goldlootgw_idx_geom ON goldlootgw USING GIST(geom);
ANALYZE goldlootgw;
CREATE INDEX goldsemilootgw_idx_geom ON goldsemilootgw USING GIST(geom);
ANALYZE goldsemilootgw;
CREATE INDEX goldnonlootgw_idx_geom ON goldnonlootgw USING GIST(geom);
ANALYZE goldnonlootgw;





-- Create yearly gold presence dummy for records with discovery dates

-- lootable
CREATE TEMP TABLE goldloot_yearly
AS
(
SELECT gid, generate_series(startyear, 2012) as year, goldloot
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.prodyear), MIN(d.discyear))::int as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS goldloot
	FROM goldlootgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);

-- semi lootable
CREATE TEMP TABLE goldsemiloot_yearly
AS
(
SELECT gid, generate_series(startyear, 2012) as year, goldsemiloot
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.prodyear), MIN(d.discyear))::int as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS goldsemiloot
	FROM goldsemilootgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);

-- non lootable
CREATE TEMP TABLE goldnonloot_yearly
AS
(
SELECT gid, generate_series(startyear, 2012) as year, goldnonloot
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.prodyear), MIN(d.discyear))::int as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS goldnonloot
	FROM goldnonlootgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);

-- combine into final yearly table
DROP TABLE IF EXISTS gold_y;
CREATE TABLE gold_y
AS
(
select gid, year, gl.goldloot, gsl.goldsemiloot, gnl.goldnonloot
FROM goldloot_yearly AS gl
FULL OUTER JOIN goldsemiloot_yearly AS gsl USING(gid, year)
FULL OUTER JOIN goldnonloot_yearly AS gnl USING(gid, year)
);






-- Create static gold presence dummy for records without dates

-- lootable
CREATE TEMP TABLE goldloot_static
AS
(
SELECT 	p.gid,
	1 AS goldloot
FROM goldlootgw AS d, priogrid_land AS p
WHERE 	(d.discyear IS NULL AND d.prodyear IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);

-- semilootable
CREATE TEMP TABLE goldsemiloot_static
AS
(
SELECT 	p.gid,
	1 AS goldsemiloot
FROM goldsemilootgw AS d, priogrid_land AS p
WHERE 	(d.discyear IS NULL AND d.prodyear IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);

-- nonlootable
CREATE TEMP TABLE goldnonloot_static
AS
(
SELECT 	p.gid, 
	1 AS goldnonloot
FROM goldnonlootgw AS d, priogrid_land AS p
WHERE 	(d.discyear IS NULL AND d.prodyear IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);

-- combine
DROP TABLE IF EXISTS gold_s;
CREATE TABLE gold_s
AS
(
SELECT gid, gl.goldloot, gsl.goldsemiloot, gnl.goldnonloot
FROM goldloot_static AS gl
FULL OUTER JOIN goldsemiloot_static AS gsl USING(gid)
FULL OUTER JOIN goldnonloot_static AS gnl USING(gid)
);




