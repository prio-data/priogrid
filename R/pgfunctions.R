# ================================
# Useful utility functions for
# working with PRIOGrid
# ================================

#' Get column and row number based on PRIO-GRID ID with ncol and nrow.
#'
#' @param gid An integer cell id.
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @return A vector with components
#' \itemize{
#'   \item X Column number.
#'   \item Y Row number.
#' }
#' @examples
#' pg <- create_pg_indices(3, 3)
#' getXY(5, 3, 3)
getXY <- function(gid, ncol, nrow){
  rownum <- nrow - ceiling(gid/ncol) + 1

  colnum <- gid %% ncol
  if(colnum == 0){colnum = ncol}

  return(c("X"=colnum, "Y"=rownum))
}

#' Create matrix with index numbering conventions as for PRIO-GRID.
#'
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @return A ncol*nrow matrix with integer indices.
#' @examples
#' pg <- create_pg_indices(3, 3)
#' assertthat::are_equal(pg[1,1], (ncol*nrow)-ncol+1)
#' assertthat::are_equal(pg[nrow, 1], 1)
#' assertthat::are_equal(pg[nrow, ncol], ncol)
#' assertthat::are_equal(pg[1, ncol], (ncol*nrow))
create_pg_indices <- function(ncol, nrow){
  # To create PRIO-GRID, swap ncol and nrow, load index in reverse order, and
  # rotate 90 degrees once.
  rotate <- function(x) t(apply(x, 2, rev))
  pg <- rotate(matrix(rev(1:(ncol*nrow)), nrow=ncol, ncol=nrow))

  return(pg)
}


#' Get first order neighbors in PRIO-GRID.
#' TODO: does this work as cell2nb in spdep? Must probably map to PG indices?
#'
#' @param x The GID to the cell that you would like to find the neighbors of.
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @return If asmat=TRUE: A matrix of the neighbors, If asmat=FALSE: A vector of the neighbors.
#' @examples
#' pg <- create_pg_indices(5, 5)
#' pgneighbors(1, ncol=5, nrow=5)
pgneighbors <- function(x, ncol, nrow, asmat=TRUE){

  farright <- ncol*1:nrow
  farleft <- c(1, farright[-length(farright)]+1)
  toprow <- ncol*nrow - ncol:1 + 1
  bottomrow <- 1:ncol

  #all(farleft + ncol-1 == farright
  if (x %in% farright){
    # x on the far right side
    uppern <- c(x+ncol-1, x+ncol, x+1)
    midn <- c(x-1, x-ncol+1)
    lown <- c(x-ncol-1, x-ncol, x-(2*ncol)+1)
  } else if (x %in% farleft){
    # x on the far left side
    uppern <- c(x+(2*ncol)-1, x+ncol, x+ncol+1)
    midn <- c(x+ncol-1, x+1)
    lown <- c(x-1, x-ncol, x-ncol+1)
  } else {
    uppern <- c(x+ncol-1, x+ncol, x+ncol+1)
    midn <- c(x-1, x+1)
    lown <- c(x-ncol-1, x-ncol, x-ncol+1)
  }

  if(asmat==TRUE){
    midn <- c(midn[1], NA, midn[2])
    if(x %in% toprow){
      return(rbind(midn, lown))
    } else if(x %in% bottomrow){
      return(rbind(uppern, midn))
    } else{
      return(rbind(uppern, midn, lown))
    }

  } else {
    if(x %in% toprow){
      return(c(midn, lown))
    } else if(x %in% bottomrow){
      return(c(uppern, midn))
    } else{
      return(c(uppern, midn, lown))
    }
  }
}

pgneighbors_v <- Vectorize(pgneighbors, vectorize.args=c("x"))

#' Get first order neighbors in PRIO-GRID, vectorized version.
#'
#' @param gids The GIDs to the cells that you would like to find the neighbors of.
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @return A dataframe with a list of neighbors for each gid in gids.
#' @export
pgneighbors_nnb <- function(gids, ncol, nrow){
  nnb <- pgneighbors_v(gids, ncol, nrow)
  df <- as.data.frame(unlist(lapply(format(nnb, scientific=F), paste, collapse=" ")))
  names(df) <- c("neighbors")
  return(df)
}


#' Get nth-order neighbors in PRIO-GRID, vectorized version.
#'
#' @param gid The GIDs to the cells that you would like to find the neighbors of.
#' @param order A number, the n-th order.
#' @param ncol The number of columns in the grid.
#' @param nrow The number of rows in the grid.
#' @param include_self Whether or not to also return the center gids
#' @return A dataframe with a list of neighbors for each gid in gids.
#' @export
nth_order_pgneighbors <- function(gid, order, ncol, nrow, include_self = FALSE){
  gids <- gid
  for(i in 1:order){
    gids <- unique(as.vector(unlist(gids)))
    gids <- pgneighbors_v(gids, ncol, nrow)
  }
  gids <- sort(unique(as.numeric(as.vector(unlist(gids)))))

  if(include_self == FALSE){
    gids <- gids[gids != gid]
  }

  return(gids)
}

#' Blank GRID
#' Returns a blank grid with default PRIOGrid dimensions
#'
#' @param ncol Number of columns, defaults to prio_ncol
#' @param nrow Number of rows, defaults to prio_nrow
#' @param crs CRS string, defaults to prio_crs
#'
#' @return A blank grid
#' @export
prio_blank_grid <- function(ncol = FALSE, nrow = FALSE, crs = FALSE, extent = FALSE){
   if(!ncol){
      ncol <- priogrid::prio_ncol()
   }
   if(!nrow){
      nrow <- priogrid::prio_nrow()
   }
   if(!crs){
      crs <- priogrid::prio_crs()
   }
   if(!extent){
      extent <- priogrid::prio_extent()
   }

   pg <- create_pg_indices(ncol,nrow)
   pg <- raster::raster(pg, crs = crs)
   raster::extent(pg) <- extent
   names(pg) <- "pgid"
   pg
}

#' Polygonize grid
#'
#' Returns a polygon version of PRIOGrid, meaning a grid of square polygons
#'
#' @param rastergrid A raster
#'
#' @return A polygon grid
prio_polygonize_grid <- function(rastergrid){
   poly <- spex::polygonize(rastergrid)
   poly$pgid <- poly$layer
   poly$layer <- NULL
   poly$row <- as.integer(ceiling(poly$pgid/720))
   poly$col <- as.integer(poly$pgid %% 720)
   poly
}

#' Raster layers
#'
#' Returns a list of n rasters where n = length(unique(values(raster)))
#' (Might be) useful for partitioning a raster into separate instances for
#' more effective computation.
#TODO i think this might actually already be a function raster::layerize?

prio_raster_layers <- function(raster){
   unique_values <- unique(raster::values(raster))
   unique_values <- unique_values[!is.na(unique_values)]

   lapply(unique_values, function(x){
      copy <- raster
      values(copy) <- ifelse(values(copy) == x,values(copy),NA)
      copy
   })
}


#' Raster points
#'
#' Converts a raster to a set of points, optionally
#' removing missing values.
#' @return An sf with column val = values(raster)

raster_points <- function(raster, na.rm = TRUE){
   cids <- 1:length(raster)
   raster_values <- values(raster)
   if(na.rm){
      cids <- cids[!is.na(raster_values)]
      raster_values <- raster_values[!is.na(raster_values)]
   }

   pts <- lapply(cids, function(cid){
      st_point(xyFromCell(raster,cid))
      })

   tibble::tibble(val = raster_values) %>%
     st_sf(geometry = pts)
}

#' raster_to_table
#'
#' Converts a raster to a tibble with x, y, mydate and raster value
#' @return A tibble with columns x, y, mydate, and value
raster_to_table <- function(raster){
  df <- raster::rasterToPoints(raster)
  df <- dplyr::as_tibble(df)
  df <- tidyr::gather(df, key = "mydate", value = "value", -x, -y)
  df$mydate <- sub("X", "", df$mydate)
  df$mydate <- lubridate::ymd(df$mydate)
  return(df)
}
