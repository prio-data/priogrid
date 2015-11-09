
-- Create index
DROP INDEX IF EXISTS croptypes_idx_1;
CREATE INDEX croptypes_idx_1 ON orig.croptypes USING BTREE(cell_ID, row, "column", area);
ANALYZE orig.croptypes;

-- croptypes is same resolution as priogrid and is in table format
-- therefore join to priogrid where rows and columns equal, choosing only the values for the subcrop with the largest "area" column
DROP TABLE IF EXISTS croptypes;
CREATE TABLE croptypes AS
(
-- limit crop table to only one row/crop per cellid, based on the max area
WITH c_winner_id AS 
	(
	SELECT c.cell_ID, MIN(c.crop) AS crop, MIN(subcrop) AS subcrop  -- for ties, decide biggest croptype as the one with the lowest croptype code (arbitrary)
	FROM 	orig.croptypes AS c,
		-- a filtered table that only contains the max area for each cellid
		(SELECT cell_ID, MAX(area) AS area FROM orig.croptypes GROUP BY cell_ID) AS c_max
	-- Only pick those rows that have the same cellid and area as the maximum area crop for that cellid
	WHERE c.cell_ID = c_max.cell_ID
	AND c.area = c_max.area
	GROUP BY c.cell_ID  -- in case of ties, limit so there is only one row with maximum area per cell id
	)
	,
largest_crop AS
	(
	SELECT c.* 
	FROM orig.croptypes AS c, c_winner_id AS cwin
	WHERE c.cell_ID = cwin.cell_ID
	AND c.crop = cwin.crop
	AND c.subcrop = cwin.subcrop  -- important to join on subcrop as well, because can be several subcrops per crop, each with different harvarea and growseason, so only one unique crop/subcrop can be the winner
	)
-- assign priogrid gid to the croptable by matching row and column ids 
SELECT p.gid, c.crop, c.subcrop, c.area AS harvarea, c.start AS growstart, c.end AS growend
FROM priogrid_land AS p, largest_crop AS c
WHERE p.row = 360 - c.row -- invert croptypes row number because goes in opposite direction of priogrid (downwards instead of upwards)
AND p.col = c."column"
);


