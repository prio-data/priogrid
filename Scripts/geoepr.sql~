
-- Create one gid year entry for each group that intersects a priogrid cell, allowing multiple overlapping cells when multiple groups in a cell. Group must belong to same country as cell.
DROP TABLE IF EXISTS dev.geoepr;
CREATE TABLE dev.geoepr
AS
(
SELECT p.gid, p.year, g.gwgroupid, g."type" AS grptype
FROM orig.geoepr AS g, cshapes AS p 
WHERE p.gwno = g.gwid
AND p."year" >= g."from"
AND p."year" <= g."to"
AND ST_Intersects(g.geom, p.cell)
);

-- Add index
CREATE INDEX geoepr_idx_1 ON dev.geoepr USING BTREE(gid, year, gwgroupid);
CREATE INDEX geoepr_idx_2 ON dev.geoepr USING BTREE(gwgroupid);
CREATE INDEX geoepr_idx_3 ON dev.geoepr USING BTREE(gid, year);
CREATE INDEX geoepr_idx_4 ON dev.geoepr USING BTREE(gid);
CREATE INDEX geoepr_idx_5 ON dev.geoepr USING BTREE(year);
ANALYZE dev.geoepr; 

-- Join with EPR table in order to obtain group status
DROP TABLE IF EXISTS geoepr_epr;
CREATE TEMP TABLE geoepr_epr
AS
(
SELECT g.*, e.status FROM
dev.geoepr AS g, orig.epr AS e
WHERE g.gwgroupid = e.gwgroupid
AND g.year >= e."from"
AND g.year <= e."to"
);

-- Create final count of excluded groups per cell
DROP TABLE IF EXISTS excluded_epr;
CREATE TABLE excluded_epr
AS
(
SELECT gid, year, SUM(excluded) as nexcluded 
FROM
	(SELECT g.gid, g.year,
		(CASE WHEN (g.status = 'POWERLESS' OR g.status = 'DISCRIMINATED') THEN 1
		ELSE 0 END) as excluded
	FROM geoepr_epr AS g
	) as foo
GROUP BY gid, year
);

-- Add indexes
CREATE INDEX excluded_epr_idx_1 ON excluded_epr USING BTREE(gid, year);
CREATE INDEX excluded_epr_idx_2 ON excluded_epr USING BTREE(gid);
CREATE INDEX excluded_epr_idx_3 ON excluded_epr USING BTREE(year);
ANALYZE excluded_epr;
