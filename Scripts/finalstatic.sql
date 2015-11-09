 -- Merge all static tables into one final one

DROP TABLE IF EXISTS finalstatic;


CREATE TABLE finalstatic AS
  (SELECT gid,
          p.row::smallint, 
          p.col::smallint, 
          p.xcoord::real,
          p.ycoord::real,
          p.landarea::real ,
          access_mean::real AS ttime_mean,
          access_sd::real AS ttime_sd,
          access_min::real AS ttime_min,
          access_max::real AS ttime_max ,
          mountains_mean::real,
          gc.urban::real AS urban_gc,
          gc.crop::real AS agri_gc,
          gc.forest::real AS forest_gc,
          gc.shrub::real AS shrub_gc,
          gc.herb::real AS herb_gc,
          gc.aquaveg::real AS aquaveg_gc,
          gc.barren::real AS barren_gc,
          gc.water::real AS water_gc ,
          imr_mean::real,
          imr_sd::real,
          imr_min::real,
          imr_max::real,
          cmr_mean::real,
          cmr_sd::real,
          cmr_min::real,
          cmr_max::real,
          petroleum::boolean AS petroleum_s ,
          diamloot::boolean AS diamsec_s,
          diamnonloot::boolean AS diamprim_s ,
          goldloot::boolean AS goldplacer_s,
          goldsemiloot::boolean AS goldsurface_s,
          goldnonloot::boolean AS goldvein_s ,
          gems::boolean AS gem_s ,
          cr.crop::smallint AS maincrop,
          cr.harvarea::real,
          cr.growstart::smallint,
          cr.growend::smallint,
          startmonth::smallint AS rainseas
   FROM priogrid_land AS p
   FULL OUTER JOIN accesstimes USING(gid)
   FULL OUTER JOIN mountains USING(gid)
   FULL OUTER JOIN globcover AS gc USING(gid)
   FULL OUTER JOIN imr USING(gid)
   FULL OUTER JOIN childmalnut USING(gid)
   FULL OUTER JOIN petroleum_s USING(gid)
   FULL OUTER JOIN diamonds_s USING(gid)
   FULL OUTER JOIN gold_s USING(gid)
   FULL OUTER JOIN gems_s USING(gid)
   FULL OUTER JOIN croptypes AS cr USING(gid)
   FULL OUTER JOIN rainseason USING(gid));

 -- Add indexes

CREATE INDEX finalstatic_idx_1 ON finalstatic USING BTREE(gid);
--CREATE INDEX finalstatic_idx_2 ON finalstatic USING BTREE(row, col);
ANALYZE finalstatic;

