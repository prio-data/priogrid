/* 
Here, we made the decision not to check whether points are in the same country as coded for the PRIO-GRID cell.
Users should be aware of this, and make these checks themselves if they find this relevant.
*/






-- ADD GWNO COLUMN BY JOINING TO LOOKUP TABLE
DROP TABLE IF EXISTS gemsgw;
CREATE TEMP TABLE gemsgw AS
	(SELECT d.*, c.gwno FROM orig.gems AS d LEFT JOIN orig.cow2gw AS c ON d.cowcode = c.cow);

-- SET SOME NULL DATA
UPDATE gemsgw SET disc_y = NULL WHERE disc_y = 0;
UPDATE gemsgw SET pro_y = NULL WHERE pro_y = 0;

-- SET EARLIEST START YEAR TO 1946
UPDATE gemsgw SET disc_y = 1946 WHERE disc_y < 1946;
UPDATE gemsgw SET pro_y = 1946 WHERE pro_y < 1946;

-- CREATE SPATIAL INDEXES
CREATE INDEX gemsgw_idx_geom ON gemsgw USING GIST(geom);
ANALYZE gemsgw;







-- Create yearly gem presence dummy for records with discovery dates

DROP TABLE IF EXISTS gems_y;
CREATE TABLE gems_y
AS
(
SELECT gid, generate_series(startyear, 2004) as year, gems
FROM (
	SELECT 	p.gid, 
		LEAST(MIN(d.pro_y), MIN(d.disc_y)) as startyear, -- earliest occurance (disc or prod) of all the points in a cell
		1 AS gems
	FROM gemsgw AS d, priogrid_land AS p
	WHERE ST_Intersects(d.geom, p.cell)
	GROUP BY p.gid
) as foo
);


-- Create static gem presence dummy for records without dates

DROP TABLE IF EXISTS gems_s;
CREATE TABLE gems_s
AS
(
SELECT 	p.gid,
	1 AS gems
FROM gemsgw AS d, priogrid_land AS p
WHERE 	(d.disc_y IS NULL AND d.pro_y IS NULL)
	AND ST_Intersects(d.geom, p.cell)
GROUP BY p.gid
);






