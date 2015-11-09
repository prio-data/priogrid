
-- Join all years together into one raster table, while recentered each from the pacific to europe

DROP TABLE IF EXISTS dev.isamhyde_all;
CREATE TABLE dev.isamhyde_all(rast, "year", landgroup)
AS
(
SELECT rast, "year", landgroup
FROM
	(

	--------- 1950 ----------

	-- Urban

	SELECT rast, 1950 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 1950 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 1950 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1950 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 1950 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 1950 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 1950 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Barren

	UNION ALL

	SELECT rast, 1950 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1950 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 1950 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_1950', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered




	--------- 1960 ----------

	UNION ALL

	-- Urban

	SELECT rast, 1960 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 1960 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 1960 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge

		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1960 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 1960 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 1960 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 1960 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	-- Barren

	UNION ALL

	SELECT rast, 1960 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1960 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 1960 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_1960', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered





	--------- 1970 ----------

	UNION ALL

	-- Urban

	SELECT rast, 1970 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 1970 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 1970 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1970 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 1970 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 1970 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 1970 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	-- Barren

	UNION ALL

	SELECT rast, 1970 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1970 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 1970 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_1970', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered






	--------- 1980 ----------

	UNION ALL

	-- Urban

	SELECT rast, 1980 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 1980 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 1980 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1980 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 1980 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 1980 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 1980 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	-- Barren

	UNION ALL

	SELECT rast, 1980 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1980 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 1980 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_1980', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered






	--------- 1990 ----------

	UNION ALL

	-- Urban

	SELECT rast, 1990 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 1990 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 1990 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 1990 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 1990 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 1990 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 1990 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	-- Barren

	UNION ALL

	SELECT rast, 1990 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 1990 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 1990 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_1990', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered





	--------- 2000 ----------

	UNION ALL

	-- Urban

	SELECT rast, 2000 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 2000 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 2000 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2000 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 2000 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 2000 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 2000 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Barren

	UNION ALL

	SELECT rast, 2000 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2000 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 2000 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_2000', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered






	--------- 2010 ----------

	UNION ALL

	-- Urban

	SELECT rast, 2010 AS "year", 'urban' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_urban_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Cropland

	UNION ALL

	SELECT rast, 2010 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3crop_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'crop' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4crop_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Pastureland

	UNION ALL

	SELECT rast, 2010 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3past_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'pasture' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4past_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Primary forest

	UNION ALL

	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpebf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_trpdbf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpebf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpenf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tmpdbf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_borenf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_bordnf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Secondary forest

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpebf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectrpdbf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpebf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpenf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_sectmpdbf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secborenf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL
	
	SELECT rast, 2010 AS "year", 'forest' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_secbordnf_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Grass

	UNION ALL

	SELECT rast, 2010 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c3grass_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'grass' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_c4grass_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Shrub

	UNION ALL

	SELECT rast, 2010 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_denseshrub_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'shrub' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_openshrub_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Savanna

	UNION ALL

	SELECT rast, 2010 AS "year", 'savanna' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_savanna_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered
		
	-- Barren

	UNION ALL

	SELECT rast, 2010 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_tundra_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_desert_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	UNION ALL

	SELECT rast, 2010 AS "year", 'barren' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_pdri_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	-- Water

	UNION ALL

	SELECT rast, 2010 AS "year", 'water' AS landgroup
	FROM 	(SELECT ST_SetUpperLeft(rast, ST_UpperLeftX(rast)-180, ST_UpperLeftY(rast)) AS rast  -- offset the (0,360) coordinates leftwards to the (-180,180) realm
		FROM recenter_raster_table('orig.isamhyde_water_2010', 'rast')  -- roll the pacific and americas off the right edge and back around the left edge
		) AS recentered

	) AS foo
);

CREATE INDEX isamhyde_all_st_convexhull_idx ON dev.isamhyde_all USING GIST(ST_Convexhull(rast));
ANALYZE dev.isamhyde_all;


-- Get nearest values and gid for all, includes duplicate gid year rows, one for each subcategory (eg c3crop and c4crop within the crop category), so these must be summed later. 

DROP TABLE IF EXISTS dev.isamhyde_gid;
CREATE TABLE dev.isamhyde_gid(gid, landgroup, "year", val)
AS
(
SELECT 	p.gid, 
	ih.landgroup,
	ih.year, 
	ST_NearestValue(ih.rast, p.centroid, FALSE)*0.01 AS val -- False to settle for missing value instead of keep searching for nearest valid value. To get percent values must multiply by 0.01, a scaling factor used by isamhyde for efficient data storage.
FROM 
	priogrid_land AS p
	,
	dev.isamhyde_all AS ih
WHERE ih.rast && p.centroid
);

CREATE INDEX isamhyde_gid_idx_1 ON dev.isamhyde_gid USING BTREE(gid, landgroup,year);
ANALYZE dev.isamhyde_gid;


-- Set null value (OR MAYBE NOT??)
UPDATE dev.isamhyde_gid SET val = NULL WHERE val < 0;


-- Create separate columns that sums the percent area grouped by land groupings and eyar

DROP TABLE IF EXISTS isamhyde;
CREATE TABLE isamhyde
AS
(
WITH isamhyde_aggreg AS 
	(SELECT gid, landgroup, year, SUM(val) AS sum 
	FROM dev.isamhyde_gid 
	GROUP BY gid, landgroup, year)
	
SELECT 	gid,
	year,
	ur.sum AS urban,
	cr.sum AS crop,
	ps.sum AS pasture,
	fr.sum AS forest,
	gr.sum AS grass,
	sh.sum AS shrub,
	sv.sum AS savanna,
	br.sum AS barren,
	wt.sum AS water
	
FROM (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'urban') AS ur

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'crop') AS cr
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'pasture') AS ps
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'forest') AS fr
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'grass') AS gr
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'shrub') AS sh
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'savanna') AS sv
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'barren') AS br
USING(gid, year)

FULL OUTER JOIN (SELECT * FROM isamhyde_aggreg WHERE landgroup = 'water') AS wt
USING(gid, year)

);






