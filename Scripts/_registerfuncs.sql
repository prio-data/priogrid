
-- Function for joining finished tables to priogrid with geometries for visualizing, works on both static and yearly tables
CREATE FUNCTION Join2PrioGrid(text) RETURNS integer AS
$$
DECLARE 
	tablename ALIAS FOR $1;
BEGIN
	EXECUTE 'CREATE TABLE test.silly_' || tablename || ' AS SELECT p.cell, d.* FROM priogrid_land AS p, ' || tablename || ' AS d WHERE d.gid = p.gid;';
	RETURN 0;
END
$$ language 'plpgsql';







-- DEFINE FUNCTION TO GET DAYS OF MONTH
-- source: http://www.postgresql.org/message-id/Pine.LNX.4.20.0112070925260.3394-100000@s18.pradeshta.net
CREATE FUNCTION Days_In_Month(int, int) RETURNS float8 AS -- year, month
	'SELECT date_part(''day'',
		(($1::text || ''-'' || $2::text || ''-01'')::date
			+ ''1 month''::interval
			- ''1 day''::interval)) AS days'
LANGUAGE 'sql';






-- Function for horizontally recentering a tiled raster table, so left and right side are switched
CREATE FUNCTION Recenter_Raster_Table(text, text) 
RETURNS TABLE(rast raster) AS  
$$
DECLARE
	_rastertablename ALIAS FOR $1;
	_rastercolname ALIAS FOR $2;
BEGIN
	RETURN QUERY EXECUTE '
	WITH 
	_input AS
		(SELECT '||_rastercolname||' AS rast FROM '||_rastertablename||')
		,
	_clipped AS
	-- clip all tiles to the left and right of new rightedge
		(		
		SELECT ST_Clip(rast, ST_SetSRID(ST_MakeEnvelope(0, -90, 179.99999999, 90), 4326)) AS rast  
		FROM _input
		WHERE rast && ST_MakeEnvelope(0, -90, 179.99999999, 90) --intersects left side (decimals to avoid weird edge intersection empty band clip error)
		
		UNION ALL
		
		SELECT ST_Clip(rast, ST_SetSRID(ST_MakeEnvelope(180, -90, 360, 90), 4326)) AS rast
		FROM _input
		WHERE rast && ST_MakeEnvelope(180, -90, 360, 90) --intersects right side 
		)
	-- switch sides the tiles around the middle (left offset any tiles that are beyond the middle, and right offset all the other ones)
	SELECT (CASE WHEN ST_UpperLeftX(rast) >= 180
		THEN ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast))
		ELSE ST_SetUpperLeft(rast, ST_UpperLeftX(rast)+180, ST_UpperLeftY(rast))
		END) AS rast
	FROM _clipped
	';
END
$$ language 'plpgsql';	
	





-- Function for horizontally rolling a tiled raster table by x coordinates rightwards and wrapping the parts that fall off back around to the left side
-- MAYBE TODO: Make faster by instead of doing clip on all tiles, do a "UNION ALL" type combine on:
-- 1) those tiles not intersecting the new xcenter vertical line, 
-- 2) those tiles intersecting clipped on the left side, and 
-- 3) those tiles intersecting clipped on the right side.
CREATE FUNCTION Roll_Raster_Table(text, text, numeric) 
RETURNS TABLE(rast raster) AS  
$$
DECLARE
	_rastertablename ALIAS FOR $1;
	_rastercolname ALIAS FOR $2;
	_xoffset ALIAS FOR $3;
BEGIN
	RETURN QUERY EXECUTE '
	WITH
	_wholerastmeta AS
		-- whole raster table meta (assumes right and up increasing orientation of coords)
		(
		WITH 
			leftedges AS (SELECT ST_UpperLeftX('||_rastercolname||') AS val FROM '||_rastertablename||'),
			rightedges AS (SELECT ST_UpperLeftX('||_rastercolname||')+ST_Width('||_rastercolname||')*ST_ScaleX('||_rastercolname||') AS val FROM '||_rastertablename||'),
			topedges AS (SELECT ST_UpperLeftY('||_rastercolname||') AS val FROM '||_rastertablename||'),
			bottomedges AS (SELECT ST_UpperLeftY('||_rastercolname||')+ST_Height('||_rastercolname||')*ST_ScaleY('||_rastercolname||') AS val FROM '||_rastertablename||')
		SELECT MIN(leftedges.val) AS leftedge, MAX(rightedges.val) AS rightedge, MIN(bottomedges.val) AS bottomedge, MAX(topedges.val) AS topedge
		FROM leftedges, rightedges, bottomedges, topedges
		)
		,
	_clipped AS
		-- clip all tiles to the left and right of new rightedge
		(
		SELECT ST_Clip('||_rastercolname||', ST_SetSRID(ST_MakeEnvelope(leftedge, bottomedge, rightedge-'||_xoffset||', topedge), 4326)) AS rast
		FROM '||_rastertablename||', _wholerastmeta
		WHERE '||_rastercolname||' && ST_MakeEnvelope(leftedge, bottomedge, rightedge-'||_xoffset||', topedge)

		UNION ALL

		SELECT ST_Clip('||_rastercolname||', ST_SetSRID(ST_MakeEnvelope(rightedge-'||_xoffset||', bottomedge, rightedge, topedge), 4326)) AS rast
		FROM '||_rastertablename||', _wholerastmeta
		WHERE '||_rastercolname||' && ST_MakeEnvelope(rightedge-'||_xoffset||', bottomedge, rightedge, topedge)
		)
	-- left offset any tiles that are beyond the new rightedge, and rightoffset the ones on the left
	SELECT (CASE WHEN ST_UpperLeftX(rast) >= rightedge-'||_xoffset||'
		THEN ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-(rightedge-leftedge)+'||_xoffset||', ST_UpperLeftY(rast))
		ELSE ST_SetUpperLeft(rast, ST_UpperLeftX(rast)+'||_xoffset||', ST_UpperLeftY(rast))
		END) AS rast
	FROM _clipped, _wholerastmeta
	';
END
$$ language 'plpgsql';	



