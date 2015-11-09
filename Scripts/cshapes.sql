-----------------------
/* FIX CSHAPES DATE */
-- STARTDATE
ALTER TABLE orig.cshapes ADD COLUMN startdate date;
UPDATE orig.cshapes SET startdate = to_date(CAST(gwsyear AS int)||'-'||CAST(gwsmonth AS int)||'-'||CAST(gwsday AS int), 'YYYY-MM-DD');
-- ENDDATE (LAST UPDATE IN 2015, SO SET TO END OF 2014)
ALTER TABLE orig.cshapes ADD COLUMN enddate date;
UPDATE orig.cshapes SET gweyear = 2014, gwemonth = 12, gweday = 31 WHERE gweyear = 2012 AND gwemonth = 06 AND gweday = 30;
UPDATE orig.cshapes SET enddate = to_date(CAST(gweyear AS int)||'-'||CAST(gwemonth AS int)||'-'||CAST(gweday AS int), 'YYYY-MM-DD')
	WHERE gwcode != -1;

-------------------------------------
/* DICE CSHAPES INTO THE PREVIOUSLY CREATED LARGE 5x5 GRID */
DROP TABLE IF EXISTS dev.cshapes_diced;
CREATE TABLE dev.cshapes_diced AS
SELECT
    ST_Intersection(a.geom, b.cell) AS geometry,
    a.gid, a.cntry_name, a.area, a.capname, a.caplong, a.caplat,
    a.featureid, a.cowcode, a.cowsyear, a.cowsmonth, a.cowsday, a.coweyear,
    a.cowemonth, a.coweday, a.gwcode, a.gwsyear, a.gwsmonth, a.gwsday,
    a.gweyear, a.gwemonth, a.gweday, a.isoname, a.iso1num, a.iso1al2,
    a.iso1al3, a.startdate, a.enddate
FROM
    orig.cshapes AS a,
    dev.largegrid AS b
WHERE
    ST_Intersects(a.geom, b.cell);

CREATE INDEX cshapes_diced_idx_geom ON dev.cshapes_diced USING gist(geometry);
CREATE INDEX cshapes_diced_idx_1 ON dev.cshapes_diced (gwcode);
CREATE INDEX cshapes_diced_idx_2 ON dev.cshapes_diced (startdate, enddate);
CREATE INDEX cshapes_diced_idx_3 ON dev.cshapes_diced (gwcode, startdate, enddate);
CREATE INDEX cshapes_diced_idx_4 ON dev.cshapes_diced (gwsyear, gweyear, gwcode);
ANALYZE dev.cshapes_diced;

---------------------------------------------------------------
/* FIND PRIOGRID CELLS WITH LAND SURFACE (2008 is arbitrary) */
DROP TABLE IF EXISTS priogrid_land;
CREATE TABLE priogrid_land
AS
(
SELECT 	p.*, ST_Area(foo.landgeom::geography, FALSE)/(1000^2) AS landarea -- /(1000^2) -> convert sqm to sqkm. Using default spheroid measurement with ST_Area leads to a bug/error when geographies cross or even just touch the equator at latitude 0, so solution is to set default spheroid to FALSE which instead uses less accurate spheroid but raises no errors, see http://osgeo-org.1560.x6.nabble.com/How-to-validate-OpenLayers-area-calculation-using-PostGIS-td5163540.html.
FROM 	priogrid AS p,
	( -- get union of all cshapes inside the cell so can get the combined landarea regardless of country
	SELECT 	p.gid, 
		ST_Union(ST_Intersection(c.geometry, p.cell)) AS landgeom
	FROM priogrid AS p, dev.cshapes_diced AS c
	WHERE c.gwsyear <= 2008 AND c.gweyear >= 2008 
	AND p.cell && c.geometry  -- share bbox (because disjoint doesnt use spatial indexes)
	AND NOT ST_Disjoint(p.cell, c.geometry) -- shares any area (excludes pure touching)
	GROUP BY p.gid
	) AS foo
WHERE p.gid = foo.gid
);

-- ADD GEODESIC CENTER TO PRIOGRID_LAND
ALTER TABLE priogrid_land ADD COLUMN centgeog geography;
UPDATE priogrid_land SET centgeog = centroid::geography;

-- Add indexes
CREATE INDEX priogrid_land_idx_geom ON priogrid_land USING gist(cell);
CREATE INDEX priogrid_land_idx_centroid ON priogrid_land USING gist(centroid);
CREATE INDEX priogrid_land_idx_centroidgeog ON priogrid_land USING gist(centgeog);
CREATE INDEX priogrid_land_idx_1 ON priogrid_land (gid);
ANALYZE priogrid_land;

------------------------------------------------------
/* APPEND COUNTRY AREA PER CELL INFORMATION ON A YEARLY PRIOGRID */
DROP TABLE IF EXISTS cshapes_areal;
CREATE TEMPORARY TABLE cshapes_areal (
	gid int NOT NULL, 
	"year" int, 
	col int, 
	"row" int, 
	gwno int, 
	area decimal, 
	caplong decimal, 
	caplat decimal, 
	cell geometry(Polygon, 4326), 
	centroid geometry(Point, 4326));

CREATE OR REPLACE FUNCTION 
createYearlyGrid(
	-- PARAMETERS
		integer, integer) 
	-- RETURNS
		RETURNS integer AS 
	-- PROCESS
		$$
		DECLARE
			_from ALIAS FOR $1;
			_to ALIAS FOR $2;
			result integer;
		BEGIN
			FOR i IN _from.._to LOOP
				-- FOR EACH YEAR
				RAISE notice 'Year: %', i;
				-- CREATE LAND GRID
				INSERT INTO cshapes_areal(gid, "year", col, "row", gwno, area, caplong, caplat, cell, centroid)
				SELECT
					-- 
					a.gid, i AS "year", a.col, a."row", b.gwcode, 
					ST_Area(ST_Intersection(a.cell, b.geometry)::geography, FALSE)/(1000^2) AS area, -- /(1000^2) -> convert sqm to sqkm. Using default spheroid measurement with ST_Area leads to a bug/error when geographies cross or even just touch the equator at latitude 0, so solution is to set default spheroid to FALSE which instead uses less accurate spheroid but raises no errors, see http://osgeo-org.1560.x6.nabble.com/How-to-validate-OpenLayers-area-calculation-using-PostGIS-td5163540.html.
					b.caplong, b.caplat, a.cell, a.centroid
				FROM
					priogrid_land AS a, 
					dev.cshapes_diced AS b 
				WHERE 	
					a.cell && b.geometry  -- share bbox (because disjoint doesnt use spatial indexes)
					AND NOT ST_Disjoint(a.cell, b.geometry) -- shares any area (excludes pure touching)
					AND b.gwsyear <= i
					AND b.gweyear >= i
					AND b.startdate <= (i::text ||'/12/31')::date
					AND b.enddate >= (i::text ||'/12/31')::date
					AND b.gwcode != -1;
			END LOOP;

			RETURN 0;
		END
		$$ language 'plpgsql';

-- RUN FUNCTION
SELECT createYearlyGrid(1946, 2014);
UPDATE cshapes_areal SET area = NULL WHERE area::text = 'NaN'; -- Weird error bug causes NaN value instead of NULL for some area calculations, maybe same as https://trac.osgeo.org/postgis/ticket/2932

CREATE INDEX cshapes_areal_idx_all ON cshapes_areal USING btree(gid, "year", gwno, area);
ANALYZE cshapes_areal;


-- CREATE FINAL CSHAPES TABLE
DROP TABLE IF EXISTS cshapes;
CREATE TABLE cshapes(
	gid int NOT NULL, 
	"year" int, 
	col int, 
	"row" int, 
	gwno int, 
	area decimal, 
	caplong decimal, 
	caplat decimal, 
	cell geometry(Polygon, 4326), 
	centroid geometry(Point, 4326));

-- ONLY CHOOSE LARGEST COUNTRY WHERE CELLS OVERLAP
INSERT INTO cshapes(gid, "year", col, "row", gwno, area, caplong, caplat, cell, centroid)
SELECT 
	p.gid, p."year", p.col, p."row", p.gwno, foo.area, 
	p.caplong, p.caplat, p.cell, p.centroid 
FROM 
	cshapes_areal AS p,
	(SELECT gid, "year", MAX(area) AS area FROM cshapes_areal GROUP BY gid, "year") AS foo
WHERE 
	foo.gid = p.gid 
	AND foo."year"= p."year" 
	AND foo.area = p.area;

-- ADD GEODESIC CENTER TO CSHAPES
ALTER TABLE cshapes DROP COLUMN IF EXISTS centgeog;
ALTER TABLE cshapes ADD COLUMN centgeog geography;
UPDATE cshapes SET centgeog = centroid::geography;	


	
-- CREATE INDEXES
CREATE INDEX cshapes_idx_cell ON cshapes USING gist(cell);
CREATE INDEX cshapes_idx_centroid ON cshapes USING gist(centroid);
CREATE INDEX cshapes_idx_centgeog ON cshapes USING gist(centgeog);
CREATE INDEX cshapes_idx_1 ON cshapes USING btree(gid, "year", gwno);
CREATE INDEX cshapes_idx_2 ON cshapes USING btree(gid);
CREATE INDEX cshapes_idx_3 ON cshapes USING btree("year");

ANALYZE cshapes;
