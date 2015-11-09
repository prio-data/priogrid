---------------------------------------------------------------
-- CREATES VARIOUS PRIOGRID TABLES
---------------------------------------------------------------

-------------------------
/* DEFINE GRID CREATION
	http://gis.stackexchange.com/questions/16374/how-to-create-a-regular-polygon-grid-in-postgis
	ST_CreateFishnet by Mike Toews. Slightly changed (columns comes "first", rows second. 
	This result in a grid starting lower left, moving right and then up)
*/
CREATE OR REPLACE FUNCTION 
ST_CreateFishnet(
	-- PARAMETERS
        nrow integer, ncol integer,
        ysize float8, xsize float8,
        y0 float8 DEFAULT 0, x0 float8 DEFAULT 0,
        srid integer DEFAULT 4326,
        OUT "row" integer, OUT col integer,
        OUT geom geometry)
	-- RETURNS
		RETURNS SETOF record AS
	-- PROCESS
		$$
		SELECT i + 1 AS row, j + 1 AS col, ST_SetSRID(ST_Translate(cell, j * $3 + $5, i * $4 + $6), $7) AS geom
		FROM generate_series(0, $1 - 1) AS j,
			 generate_series(0, $2 - 1) AS i,
			 (SELECT ('POLYGON((0 0, 0 '||$4||', '||$3||' '||$4||', '||$3||' 0,0 0))')::geometry AS cell) AS foo;
		$$ LANGUAGE sql IMMUTABLE STRICT;


----------------------------------
/* CREATE 0.5x0.5 VECTOR GRID */
DROP TABLE IF EXISTS priogrid;
CREATE TABLE priogrid (
	gid serial NOT NULL, 
	"row" integer, 
	col integer, 
	cell geometry(Polygon, 4326), 
	CONSTRAINT priogrid_pkey PRIMARY KEY (gid));
	
INSERT INTO priogrid ("row", col, cell) SELECT * FROM ST_CreateFishnet(720, 360, 0.5, 0.5, -180, -90, 4326) AS cells;

-- Add centroid geometry
ALTER TABLE priogrid ADD COLUMN centroid geometry(Point,4326);
UPDATE priogrid SET centroid = ST_Centroid(cell);

-- Add x/y coords
ALTER TABLE priogrid ADD COLUMN xcoord double precision;
ALTER TABLE priogrid ADD COLUMN ycoord double precision;
UPDATE priogrid SET xcoord = ST_X(centroid);
UPDATE priogrid SET ycoord = ST_Y(centroid);

-- Add indexes
CREATE INDEX priogrid_idx_centroid ON priogrid USING gist (centroid);
CREATE INDEX priogrid_idx_cell ON priogrid USING gist (cell);
CREATE INDEX priogrid_idx_1 ON priogrid (gid);
ANALYZE priogrid;


-----------------------------
/* CREATE 5x5 VECTOR GRID */
DROP TABLE IF EXISTS dev.largegrid;
CREATE TABLE dev.largegrid (
	gid serial NOT NULL, 
	"row" integer, 
	col integer, 
	cell geometry(Polygon, 4326), 
	CONSTRAINT largegrid_pkey PRIMARY KEY (gid));
	
INSERT INTO dev.largegrid ("row", col, cell) SELECT * FROM ST_CreateFishnet(72, 36, 5, 5, -180, -90, 4326) AS cells;
CREATE INDEX largegrid_idx_cell ON dev.largegrid USING gist (cell);
CREATE INDEX largegrid_idx_1 ON dev.largegrid (gid);
ANALYZE dev.largegrid;





