
------ Define growthperiod for each gid as the 3 months within a year that most frequently have the most precip across all years since 1946
------ (Needs precip_gpcc_prio to have been created first)

-- transform to gid, year, month structure
DROP TABLE IF EXISTS precipmonths;
CREATE TEMP TABLE precipmonths AS
	(
	SELECT 	gid, "year", 1 AS "month", jan AS precip
	FROM precip_gpcc

	UNION ALL

	SELECT 	gid, "year", 2 AS "month", feb AS precip
	FROM precip_gpcc

	UNION ALL

	SELECT 	gid, "year", 3 AS "month", mar AS precip
	FROM precip_gpcc

	UNION ALL

	SELECT 	gid, "year", 4 AS "month", apr AS precip
	FROM precip_gpcc

	UNION ALL

	SELECT 	gid, "year", 5 AS "month", may AS precip
	FROM precip_gpcc

	UNION ALL

	SELECT 	gid, "year", 6 AS "month", jun AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 7 AS "month", jul AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 8 AS "month", aug AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 9 AS "sep", apr AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 10 AS "month", oct AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 11 AS "month", nov AS precip
	FROM precip_gpcc
	
	UNION ALL

	SELECT 	gid, "year", 12 AS "month", des AS precip
	FROM precip_gpcc
	);


-- add index
CREATE INDEX precipmonths_idx_all ON precipmonths USING BTREE(gid, year, month);
ANALYZE precipmonths;


-- zip monthscores so each row contains scores for 3 contiguous months
-- NOTE: we append zeros to the offset columns to make them equally long to avoid postgres looping the rows several times until it creates all possible combinations
DROP TABLE IF EXISTS precip3;
CREATE TEMP TABLE precip3
AS
(
SELECT 	UNNEST(ARRAY((select gid from precipmonths order by gid, year, month))) AS gid,
	UNNEST(ARRAY((select year from precipmonths order by gid, year, month))) AS year,
	UNNEST(ARRAY((select month from precipmonths order by gid, year, month))) AS month,
	UNNEST(ARRAY((select precip from precipmonths order by gid, year, month))) AS p1, 
	UNNEST(ARRAY_APPEND(ARRAY((select precip from precipmonths order by gid, year, month offset 1)), NULL)) AS p2,
	UNNEST(ARRAY_APPEND(ARRAY_APPEND(ARRAY((select precip from precipmonths order by gid, year, month offset 2)), NULL), NULL)) AS p3
);


-- calculate sum of 3 contiguous months
ALTER TABLE precip3 ADD COLUMN psum numeric;
UPDATE precip3 SET psum = p1 + p2 + p3;


-- find max 3-month precip value for each gid year
DROP TABLE IF EXISTS pmax;
CREATE TEMP TABLE pmax 
AS
(
SELECT gid, year, max(psum) AS maxpsum
FROM precip3
GROUP BY gid, year
);


-- select all year months, and make dummy for winning months with the same precip as the max value for each year (so we can later account for possibility of ties)
DROP TABLE IF EXISTS maxvals;
CREATE TEMP TABLE maxvals 
AS
(
SELECT p.gid, p.year, p.month, (CASE WHEN p.psum = pmax.maxpsum THEN 1 ELSE 0 END) AS win
FROM precip3 AS p, pmax AS pmax
WHERE p.gid = pmax.gid
AND p.year = pmax.year
);


-- add index for speedup because very large
CREATE INDEX maxvals_idx_all ON maxvals USING BTREE(gid, year, month);
ANALYZE maxvals;


-- gather frequency score for how many times each month had the maximum yearly precipitation (ie the number of times they "won")
DROP TABLE IF EXISTS monthscores;
CREATE TEMP TABLE monthscores 
AS
(
SELECT gid, month, sum(win) AS "score"
FROM maxvals
GROUP BY gid, month
);


-- add index for speedup because very large
CREATE INDEX monthscores_idx_all ON monthscores USING BTREE(gid, month);
ANALYZE monthscores;


-- for each gid find the highest win frequency across all years
DROP TABLE IF EXISTS maxwin;
CREATE TEMP TABLE maxwin
AS
(
SELECT gid, max("score") AS maxscore
FROM monthscores
GROUP BY gid
);


-- for each gid find the row (ie the month) that matches the highest score, using earliest month if any ties
DROP TABLE IF EXISTS rainseason;
CREATE TABLE rainseason
AS
(
SELECT sc.gid, MIN(sc.month) AS startmonth -- min() makes sure only the lowest month is chosen if any ties
FROM monthscores AS sc, maxwin AS mx
WHERE sc.gid = mx.gid
AND sc.score = mx.maxscore -- choose all months that match the highest score
GROUP BY sc.gid -- only have one row per gid
);

-- add index
CREATE INDEX rainseason_idx_1 ON rainseason USING BTREE(gid, startmonth);
ANALYZE rainseason;

