 -- Merge all yearly tables into one final one

DROP TABLE IF EXISTS finalyearly;


CREATE TABLE finalyearly AS
  (SELECT gid,
          "year",
          gwno,
          area::real AS gwarea,
          bdist1::real,
          bdist2::real,
          bdist3::real,
          capdist::real,
          pop_gpw.population_sum::real AS pop_gpw_sum,
          pop_gpw.population_sd::real AS pop_gpw_sd,
          pop_gpw.population_min::real AS pop_gpw_min,
          pop_gpw.population_max::real AS pop_gpw_max ,
          nexcluded::smallint AS excluded ,
          gecon_gcpmer::real AS gcp_mer,
          gecon_gcpppp::real AS gcp_ppp,
          gecon_qual::real AS gcp_qual ,
          petroleum::boolean AS petroleum_y ,
          diamloot::boolean AS diamsec_y,
          diamnonloot::boolean AS diamprim_y ,
          goldloot::boolean AS goldplacer_y,
          goldsemiloot::boolean AS goldsurface_y,
          goldnonloot::boolean AS goldvein_y ,
          gems::boolean AS gem_y ,
          drugs::boolean AS drug_y ,
          gpcp.total::real AS prec_gpcp ,
          gpcc.total::real AS prec_gpcc ,
          temp.average::real AS temp,
          dspi.droughtcrop::real AS droughtcrop_spi,
          dspeibase.droughtcrop::real AS droughtcrop_speibase,
          dspeigdm.droughtcrop::real AS droughtcrop_speigdm,
          dspi.droughtstart::real AS droughtstart_spi,
          dspeibase.droughtstart::real AS droughtstart_speibase,
          dspeigdm.droughtstart::real AS droughtstart_speigdm,
          dspi.droughtend::real AS droughtend_spi,
          dspeibase.droughtend::real AS droughtend_speibase,
          dspeigdm.droughtend::real AS droughtend_speigdm,
          dspi.droughtyr::real AS droughtyr_spi,
          dspeibase.droughtyr::real AS droughtyr_speibase,
          dspeigdm.droughtyr::real AS droughtyr_speigdm,
          irrig.sum::real AS irrig_sum,
          irrig.sd::real AS irrig_sd,
          irrig.min::real AS irrig_min,
          irrig.max::real AS irrig_max ,
          pop_hyde.sum::real AS pop_hyd_sum,
          pop_hyde.sd::real AS pop_hyd_sd,
          pop_hyde.min::real AS pop_hyd_min,
          pop_hyde.max::real AS pop_hyd_max ,
          ih.urban::real AS urban_ih,
          ih.crop::real AS agri_ih,
          ih.pasture::real AS pasture_ih,
          ih.forest::real AS forest_ih,
          ih.shrub::real AS shrub_ih,
          ih.grass::real AS grass_ih,
          ih.savanna::real AS savanna_ih,
          ih.barren::real AS barren_ih,
          ih.water::real AS water_ih ,
          nl_mean::real AS nlights_mean,
          nl_sd::real AS nlights_sd,
          nl_min::real AS nlights_min,
          nl_max::real AS nlights_max,
          nl_cal_mean::real AS nlights_calib_mean
   FROM cshapes
   FULL OUTER JOIN borderdist USING(gid, year)
   FULL OUTER JOIN pop_gpw USING(gid, year)
   FULL OUTER JOIN excluded_epr USING(gid, year)
   FULL OUTER JOIN nordhaus USING(gid, year)
   FULL OUTER JOIN petroleum_y USING(gid, year)
   FULL OUTER JOIN diamonds_y USING(gid, year)
   FULL OUTER JOIN gold_y USING(gid, year)
   FULL OUTER JOIN gems_y USING(gid, year)
   FULL OUTER JOIN drugs_y USING(gid, year)
   FULL OUTER JOIN precip_gpcp AS gpcp USING(gid, year)
   FULL OUTER JOIN precip_gpcc AS gpcc USING(gid, year)
   FULL OUTER JOIN temp AS temp USING(gid, year)
   FULL OUTER JOIN drought_spi AS dspi USING(gid, year)
   FULL OUTER JOIN drought_speibase AS dspeibase USING(gid, year)
   FULL OUTER JOIN drought_speigdm AS dspeigdm USING(gid, year)
   FULL OUTER JOIN irrigation AS irrig USING(gid, year)
   FULL OUTER JOIN pop_hyde USING(gid, year)
   FULL OUTER JOIN isamhyde AS ih USING(gid, year)
   FULL OUTER JOIN nightlights USING(gid, year) );

 -- Add indexes

CREATE INDEX finalyearly_idx_1 ON finalyearly USING BTREE(gid);
CREATE INDEX finalyearly_idx_2 ON finalyearly USING BTREE(year);
CREATE INDEX finalyearly_idx_3 ON finalyearly USING BTREE(gid, year);
ANALYZE finalyearly;





