
------ Calculates a version of drought based on SPIbase -------


-- 1: For 3-month rainseason, calculate droughtstart(spi1 in first growing month) and droughtend(spi3 in last growing month)

-- transform spi1 to gid, year, month structure
DROP TABLE IF EXISTS spi1months;
CREATE TEMP TABLE spi1months AS
	(
	WITH source AS (SELECT * FROM dev.spi1)
	
	SELECT 	gid, "year", 1 AS "month", jan AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 2 AS "month", feb AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 3 AS "month", mar AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 4 AS "month", apr AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 5 AS "month", may AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 6 AS "month", jun AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 7 AS "month", jul AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 8 AS "month", aug AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 9 AS "sep", sep AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 10 AS "month", oct AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 11 AS "month", nov AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 12 AS "month", des AS val
	FROM source
	);

-- add monthnum
ALTER TABLE spi1months ADD COLUMN monthnum int;
UPDATE spi1months SET monthnum = (year - 1979) * 12 + month;

-- add index
CREATE INDEX spi1months_idx_join ON spi1months USING BTREE(gid, year, monthnum);
CREATE INDEX spi1months_idx_all ON spi1months USING BTREE(gid, year, month);
ANALYZE spi1months;

-- transform spi3 to gid, year, month structure
DROP TABLE IF EXISTS spi3months;
CREATE TEMP TABLE spi3months AS
	(
	WITH source AS (SELECT * FROM dev.spi3)
	
	SELECT 	gid, "year", 1 AS "month", jan AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 2 AS "month", feb AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 3 AS "month", mar AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 4 AS "month", apr AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 5 AS "month", may AS val
	FROM source

	UNION ALL

	SELECT 	gid, "year", 6 AS "month", jun AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 7 AS "month", jul AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 8 AS "month", aug AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 9 AS "sep", sep AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 10 AS "month", oct AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 11 AS "month", nov AS val
	FROM source
	
	UNION ALL

	SELECT 	gid, "year", 12 AS "month", des AS val
	FROM source
	);

-- add monthnum
ALTER TABLE spi3months ADD COLUMN monthnum int;
UPDATE spi3months SET monthnum = (year - 1979) * 12 + month;

-- add index
CREATE INDEX spi3months_idx_join ON spi3months USING BTREE(gid, monthnum);
CREATE INDEX spi3months_idx_all ON spi3months USING BTREE(gid, month);
ANALYZE spi3months;







-- choose 1 month indicator at the rainseason start and connect to 3 month indicator with a 3 month lag
DROP TABLE IF EXISTS drought_rainseas;
CREATE TEMP TABLE drought_rainseas
AS
(
SELECT s1.gid, s1.year, r.startmonth, s3.year AS endyear, s3.month AS endmonth, s1.val AS droughtstart, s3.val AS droughtend
FROM rainseason AS r

LEFT JOIN spi1months AS s1
ON s1.gid = r.gid AND s1.monthnum = ((s1.year - 1979) * 12 + r.startmonth)   -- the 1 month indicator is chosen as months since start of the data for that year + the rainseason startmonth for that gid

LEFT JOIN spi3months AS s3
ON s3.gid = r.gid AND s3.monthnum = s1.monthnum + 3    -- the 3 month indicator is 3 months after the 1 month indicator
);








-- 2: For each year, calculate fraction of contiguous months below -1.5 that ended in that year. 

DROP TABLE IF EXISTS drought_year;
CREATE TEMP TABLE drought_year
AS
(
WITH drought_with_contigid AS
	-- for each contiguous streak of months below -1.5 within a gid, assign unique id for that streak
	-- adapted from: http://stackoverflow.com/questions/17046204/how-to-find-the-boundaries-of-groups-of-contiguous-sequential-numbers
	(SELECT *, ROW_NUMBER() OVER (PARTITION BY gid ORDER BY gid, monthnum) - monthnum AS contigid
	FROM spi1months 
	WHERE val <= -1.5)
	,
droughtstreaks AS
	-- for all months that are part of a contigious drought streak within a gid, group and count length of each streak, while noting the endyear in which the streak ended
	-- adapted from: http://stackoverflow.com/questions/17046204/how-to-find-the-boundaries-of-groups-of-contiguous-sequential-numbers
	(
	SELECT gid, MAX(year) AS endyear, contigid, COUNT(month) AS streak
	FROM drought_with_contigid
	GROUP BY gid, contigid
	)
	,
streakfraction AS
	-- choose the longest streak that ended in each year
	(SELECT gid, endyear AS year, MAX(streak)/12.0 AS contigdrought
	FROM droughtstreaks
	GROUP BY gid, endyear)
	,
validareas AS
	(SELECT gid,year FROM (SELECT gid, year, MAX(val) AS maxval
				FROM spi1months 
				GROUP BY gid, year) AS foo
	WHERE maxval IS NOT NULL) -- drought data for that gid year must have at least one valid month

SELECT gid, year, s.contigdrought

FROM streakfraction AS s
RIGHT OUTER JOIN validareas
USING (gid, year)
);

-- Above is limited to areas with valid data, so any NULL values are actually just 0 (absence of drought)
UPDATE drought_year SET contigdrought = 0 WHERE contigdrought IS NULL;









-- 3: For growing season of main crop, calculate fraction of contiguous months below -1.5 that ended in that year. 
--    Includes both the end of the previous growing season and the next when it spans across two years

DROP TABLE IF EXISTS drought_crop;
CREATE TEMP TABLE drought_crop
AS
(
WITH drought_with_contigid AS
	-- for each contiguous streak of months below -1.5 within a gid's growing season, assign unique id for that streak
	-- adapted from: http://stackoverflow.com/questions/17046204/how-to-find-the-boundaries-of-groups-of-contiguous-sequential-numbers
	(SELECT s.*, c.growstart, c.growend, ROW_NUMBER() OVER (PARTITION BY s.gid ORDER BY s.gid, s.monthnum) - monthnum AS contigid
	FROM spi1months AS s, croptypes AS c
	WHERE s.val <= -1.5
	AND s.gid = c.gid
	AND 	(
		(c.growstart <= c.growend AND s.month >= c.growstart AND s.month <= c.growend)  -- for growing season that starts and ends in same year, the month must be in between
		OR
		(c.growstart > c.growend AND (s.month >= c.growstart OR s.month <= c.growend))  -- for growing season that starts in one year and ends in the next (startmonth > endmonth), the month can be EITHER after the start OR before the end
		)
	)
	,
droughtstreaks AS
	-- for all months that are part of a contigious drought streak within a gid, group and count length of each streak, while noting the endyear in which the streak ended
	-- adapted from: http://stackoverflow.com/questions/17046204/how-to-find-the-boundaries-of-groups-of-contiguous-sequential-numbers
	(SELECT gid, MAX(year) AS endyear, contigid, COUNT(month) AS streak
	FROM drought_with_contigid
	GROUP BY gid, contigid)
	,
streakfraction AS 
	(SELECT d.gid, 
		d.endyear AS year, 
		-- choose the longest streak that ended in each year, and divide by total grow period length even if spans across two years (the MIN() of CASE WHEN... is just to avoid having to put in GROUP BY, should only be one row)
		MAX(d.streak)/MIN(CASE WHEN c.growstart <= c.growend 
					THEN c.growend-c.growstart+1 
					ELSE (12-c.growstart)+c.growend+1 
					END)::numeric AS contigdrought 
	FROM droughtstreaks AS d
	LEFT JOIN croptypes AS c ON d.gid = c.gid
	GROUP BY d.gid, d.endyear)
	,
validareas AS
	(SELECT gid,year
	FROM 
		(SELECT gid,year
		FROM 	(SELECT gid, year, MAX(val) AS maxval
			FROM spi1months 
			GROUP BY gid, year) AS foo
		WHERE maxval IS NOT NULL) AS bar  -- drought data for that gid year must have at least one valid month
	INNER JOIN croptypes USING(gid) )  -- and the area must have data for crop type
	
SELECT gid, year, st.contigdrought

FROM streakfraction AS st
RIGHT OUTER JOIN validareas 
USING (gid,year)
);

-- Above is limited to areas with valid data, so any NULL values are actually just 0 (absence of drought)
UPDATE drought_crop SET contigdrought = 0 WHERE contigdrought IS NULL;









-- Join all drought variables together

DROP TABLE IF EXISTS drought_spi;
CREATE TABLE drought_spi
AS
(
SELECT gid, year, d1.droughtstart, d1.droughtend, d2.contigdrought AS droughtyr, d3.contigdrought AS droughtcrop

FROM drought_rainseas AS d1

FULL OUTER JOIN drought_year AS d2 USING(gid, year)

FULL OUTER JOIN drought_crop AS d3 USING(gid, year)
);


