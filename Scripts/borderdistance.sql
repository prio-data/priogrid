
-- CREATE CSHAPES CROPPED TO WORLD EXTENTS
DROP TABLE IF EXISTS dev.cshapesvalid;
SELECT a.gid, a.gwcode, a.startdate, a.enddate, 
	ST_Intersection(a.geom, ST_MakeEnvelope(-180, -90, 180, 90, 4326))::geography AS geog,
	ST_Intersection(a.geom, ST_MakeEnvelope(-180, -90, 180, 90, 4326)) AS geom
INTO dev.cshapesvalid 
FROM orig.cshapes AS a;

-- ADD INDEX
CREATE INDEX cshapesvalid_idx_1 ON dev.cshapesvalid (gid,gwcode,startdate,enddate);
CREATE INDEX cshapesvalid_idx_2 ON dev.cshapesvalid (gwcode,startdate,enddate);
CREATE INDEX cshapesvalid_idx_3 ON dev.cshapesvalid (gid,gwcode);
CREATE INDEX cshapesvalid_idx_4 ON dev.cshapesvalid (gid);
CREATE INDEX cshapesvalid_idx_geog ON dev.cshapesvalid USING gist(geog);
CREATE INDEX cshapesvalid_idx_geom ON dev.cshapesvalid USING gist(geom);
ANALYZE dev.cshapesvalid;


-------------------------------
-- BORDER DISTANCES (ETA 12 hrs total incl capital distance functions)
-------------------------------

-- CALCULATE ALL POSSIBLE BORDERDIST BETWEEN EACH LAND CELL AND THE BORDER OUTLINE OF ALL COUNTRIES
DROP TABLE IF EXISTS dev.borderdist_precalc;
CREATE TABLE dev.borderdist_precalc AS
(
SELECT  p.gid AS gid,
	c.gid AS cid,
	--the below ST_Boundary makes polygon into a multilinestring outline of each (multi) polygon, otherwise the distance between points inside the polygon would be 0. 
	ST_Distance(p.centgeog, ST_Boundary(c.geom)::geography)/1000 AS dist 
FROM priogrid_land AS p, dev.cshapesvalid AS c
);

CREATE INDEX borderdist_precalc_idx_1 ON dev.borderdist_precalc (gid, cid);
CREATE INDEX borderdist_precalc_idx_2 ON dev.borderdist_precalc (gid, cid, dist);
CREATE INDEX borderdist_precalc_idx_3 ON dev.borderdist_precalc (gid);
CREATE INDEX borderdist_precalc_idx_4 ON dev.borderdist_precalc (cid);
ANALYZE dev.borderdist_precalc;

------------------

-- AFTER, JUST DETERMINE WHICH GID AND CID TO COMPARE
-- AND FROM THOSE JUST PICK THE ONE WITH THE LOWEST PRECALCULATED DISTANCE

-- DEFINE MAIN BORDER DIST FUNCTION
CREATE OR REPLACE FUNCTION createBorderDistance(integer, integer) 
RETURNS integer AS
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS borderdist1;
	DROP TABLE IF EXISTS borderdist2;
	DROP TABLE IF EXISTS borderdist3;
	CREATE TEMP TABLE borderdist1(gid integer, "year" integer, bdist1 decimal);
	CREATE TEMP TABLE borderdist2(gid integer, "year" integer, bdist2 decimal);
	CREATE TEMP TABLE borderdist3(gid integer, "year" integer, bdist3 decimal);

	-- insert new rows for each year
	FOR i IN _from.._to LOOP
		RAISE NOTICE '%', i;

		-- cell distance to closest contiguous country
		INSERT INTO borderdist1
			SELECT 	p.gid AS gid, 
				p."year" AS "year",
				MIN(alldist.dist) AS bdist1
			FROM cshapes AS p, dev.cshapesvalid AS cself, dev.cshapesvalid AS cneigh, dev.borderdist_precalc AS alldist
			WHERE 	-- dont select same as self
				p.gwno != cneigh.gwcode
				AND p.gwno = cself.gwcode -- important, so limits cself to the gwcode of the cell
				-- select current yr
				AND p."year" = i 
				AND cself.startdate <= (i::text ||'/12/31')::date AND cself.enddate >= (i::text ||'/12/31')::date
				AND cneigh.startdate <= (i::text ||'/12/31')::date AND cneigh.enddate >= (i::text ||'/12/31')::date
				-- select contiguous neighbour (ISNT WORKING)
				AND cneigh.geom && cself.geom -- bbox overlaps
				AND ST_Intersects(cneigh.geom, cself.geom) -- actual shapes intersect, ie touch, ie are neighbours
				-- link to precalculated distances between the selected dyads
				AND p.gid = alldist.gid
				AND cneigh.gid = alldist.cid
			GROUP BY p.gid, p."year", alldist.gid;

		-- cell distance to any closest country, even across water
		INSERT INTO borderdist2	
			SELECT 	p.gid AS gid, 
				p."year" AS "year", 
				MIN(alldist.dist) AS bdist2
			 FROM cshapes AS p, dev.cshapesvalid AS c, dev.borderdist_precalc AS alldist
			 WHERE 	-- dont select same as self
				p.gwno != c.gwcode 
				-- select current yr
				AND c.startdate <= (i::text ||'/12/31')::date AND c.enddate >= (i::text ||'/12/31')::date
				AND p."year" = i
				-- link to precalculated distances between the selected dyads
				AND p.gid = alldist.gid
				AND c.gid = alldist.cid
			GROUP BY p.gid, p."year", alldist.gid;

		-- cell distance to own country boundary...
		INSERT INTO borderdist3
			SELECT 	p.gid as gid, 
				p."year" as "year",
				MIN(alldist.dist) AS bdist3
			FROM cshapes AS p, dev.cshapesvalid AS c, dev.borderdist_precalc AS alldist
			WHERE 	-- select only the same country
				p.gwno = c.gwcode
				-- select current yr
				AND c.startdate <= (i::text ||'/12/31')::date AND c.enddate >= (i::text ||'/12/31')::date
				AND p."year" = i
				-- link to precalculated distances between the selected dyads
				AND p.gid = alldist.gid
				AND c.gid = alldist.cid
			GROUP BY p.gid, p."year", c.gid;
	END LOOP;
	RETURN 0;
END;
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT createBorderDistance(1946, 2014);

-------------------------------
-- CAPITAL DISTANCES
-------------------------------

-- DEFINE CAPITAL DIST FUNCTION
CREATE OR REPLACE FUNCTION calculatecapdist(integer, integer) returns integer as 
$$
DECLARE
	_from ALIAS FOR $1;
	_to ALIAS FOR $2;
	result integer;
BEGIN
	DROP TABLE IF EXISTS borderdistcap;
	CREATE TEMP TABLE borderdistcap(gid int, "year" int, capdist double precision);

	FOR i IN _from.._to LOOP
		RAISE NOTICE '%', i;
		INSERT INTO borderdistcap(gid, "year", capdist) 
			SELECT gid, "year", ST_Distance(centgeog, ST_SetSRID(ST_MakePoint(caplong, caplat), 4326)::geography)/1000 as capdist
			FROM cshapes 
			WHERE "year" = i;
	END LOOP;
	RETURN 0;
END
$$ language 'plpgsql';	

-- RUN FUNCTION
SELECT calculatecapdist(1946, 2014);

-- JOIN ALL DISTANCE TABLES INTO ONE FINAL TABLE
CREATE TABLE borderdist
AS
(
SELECT p.gid, p.year, bd1.bdist1, bd2.bdist2, bd3.bdist3, bdcap.capdist
FROM cshapes AS p
LEFT OUTER JOIN borderdist1 AS bd1 ON (bd1.gid = p.gid AND bd1.year = p.year)
LEFT OUTER JOIN borderdist2 AS bd2 ON (bd2.gid = p.gid AND bd2.year = p.year)
LEFT OUTER JOIN borderdist3 AS bd3 ON (bd3.gid = p.gid AND bd3.year = p.year)
LEFT OUTER JOIN borderdistcap AS bdcap ON (bdcap.gid = p.gid AND bdcap.year = p.year)
);



