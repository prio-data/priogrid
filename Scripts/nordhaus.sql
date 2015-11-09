
-- Reshape
CREATE TEMP TABLE nordhaus_reshaped
AS
(
SELECT *
FROM
	(
	SELECT 1990 AS year, lat, longitude, newcountryid, mer1990_40 AS mer, popgpw_1990_40 AS pop, ppp1990_40 AS ppp, quality
	FROM orig.nordhaus
	UNION ALL
	SELECT 1995 AS year, lat, longitude, newcountryid, mer1995_40 AS mer, popgpw_1995_40 AS pop, ppp1995_40 AS ppp, quality
	FROM orig.nordhaus
	UNION ALL
	SELECT 2000 AS year, lat, longitude, newcountryid, mer2000_40 AS mer, popgpw_2000_40 AS pop, ppp2000_40 AS ppp, quality
	FROM orig.nordhaus
	UNION ALL
	SELECT 2005 AS year, lat, longitude, newcountryid, mer2005_40 AS mer, popgpw_2005_40 AS pop, ppp2005_40 AS ppp, quality
	FROM orig.nordhaus
	) AS foo
);

-- Cell coords are upper left, so offset to the center
UPDATE nordhaus_reshaped SET lat=lat+0.5, longitude=longitude+0.5;

-- Spatialize table to centroids and cells
ALTER TABLE nordhaus_reshaped DROP COLUMN IF EXISTS geom;
SELECT AddGeometryColumn('nordhaus_reshaped', 'geom', 4326, 'POINT', 2);
UPDATE nordhaus_reshaped SET geom = ST_SetSRID(ST_Point(longitude, lat),4326);
ALTER TABLE nordhaus_reshaped DROP COLUMN IF EXISTS cell;
SELECT AddGeometryColumn('nordhaus_reshaped', 'cell', 4326, 'POLYGON', 2);
UPDATE nordhaus_reshaped SET cell=geometry(ST_MakeEnvelope(longitude-0.5, lat-0.5, longitude+0.5, lat+0.5, 4326));

-- Link nordhaus to gwcode lookup table
CREATE TEMP TABLE nordhaus_gw AS
(
SELECT n.*, g.gwno 
FROM nordhaus_reshaped AS n 
LEFT JOIN orig.gw2iso AS g 
ON n.newcountryid = g.iso1num 
ORDER BY gwno, "year"
);

-- Create index for faster matching
CREATE INDEX nordhaus_gw_idx_1 ON nordhaus_gw USING BTREE(gwno, lat, longitude, "year");
CREATE INDEX nordhaus_gw_idx_2 ON nordhaus_gw USING BTREE(gwno);
CREATE INDEX nordhaus_gw_idx_3 ON nordhaus_gw USING BTREE("year");
CREATE INDEX nordhaus_gw_idx_4 ON nordhaus_gw USING BTREE(gwno, "year");
ANALYZE nordhaus_gw;

-- Count number of priogrid cells per nordhaus year cell in separate table, and keep lat/lon/year identifier so can be linked to later
CREATE TEMP TABLE nordhaus_count
AS
(
SELECT n.lat, n.longitude, n."year", COUNT(p.gid) AS "count"
FROM cshapes AS p, nordhaus_gw AS n
-- choose only the nordhaus cells belonging to the same priogrid country as is also done in the main aggegation further below
WHERE p.centroid && n.cell
AND p.gwno = n.gwno 
-- match the years too
AND p."year" = n."year"
-- group by each unique nordhaus cell
GROUP BY n.lat, n.longitude, n."year"
);

-- Aggregate to Priogrid cells
-- Notes:
-- 1: When dividing gdp values to fit the higher priogrid resolution, must divide by the actual number of priogrid cells contained in the nordhaus cell, because this can vary in coastal regions
-- 2: Nordhaus sometimes has 2 overlapping cells because border between 2 countries, this is solved by only choosing the ones matching the priogrid gwno
-- 3: Post soviet countries missing in 1990 due to gwcodes not considering soviet independence until 1991
-- 4: Some countries still missing in later years, however they actually arent missing, the cells are there, they just have NULL values when visualized...
-- 5: Special case of missing cells between two countries in northafrica, due to nordhaus using different border data that is located further north for that particular border, so the nordhaus country cells cannot be linked to the corresponding priogrid country cells in that location. Just have to accept it...

DROP TABLE IF EXISTS nordhaus;
CREATE TABLE nordhaus AS
(
SELECT 	p.gid, 
	n."year", 
	n.lat,
	n.longitude,
	n.mer/foo.count AS gecon_gcpmer, 
	n.ppp/foo.count AS gecon_gcpppp, 
	n.pop/foo.count AS gecon_pop,
	--(n.mer/foo.count)/((n.pop+0.00000000001)/foo.count) AS gecon_gcpmer_cap, 
	--(n.ppp/foo.count)/((n.pop+0.00000000001)/foo.count) AS gecon_gcpppp_cap, 
	n.quality AS gecon_qual
FROM cshapes AS p, nordhaus_gw AS n, nordhaus_count AS foo
-- join nordhaus and priogrid only where year and gwno match
WHERE p.centroid && n.cell
AND p.gwno = n.gwno
AND p."year" = n."year"
-- link to count table based on lat/lon/year identifier
AND n."year" = foo."year"
AND n.lat = foo.lat
AND n.longitude = foo.longitude
);

-- Add per capita calcs
ALTER TABLE nordhaus ADD COLUMN gecon_gcpmer_cap decimal;
UPDATE nordhaus SET gecon_gcpmer_cap = gecon_gcpmer/gecon_pop WHERE gecon_pop != 0;
ALTER TABLE nordhaus ADD COLUMN gecon_gcpppp_cap decimal;
UPDATE nordhaus SET gecon_gcpppp_cap = gecon_gcpppp/gecon_pop WHERE gecon_pop != 0;

-- Add index
CREATE INDEX nordhaus_idx_1 ON nordhaus USING BTREE(gid, "year");
CREATE INDEX nordhaus_idx_2 ON nordhaus USING BTREE(gid);
CREATE INDEX nordhaus_idx_3 ON nordhaus USING BTREE("year");
ANALYZE nordhaus;
