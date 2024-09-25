get_grid_spacing <- function(dom) {
  ll_proj <- c(
    "lalo","longlat", "latlong", "rot_longlat", "rot_latlong", "RotLatLon"
  )
  if (!dom$projection$proj %in% ll_proj) {
    return(dom$dx)
  }

  estimate_grid_spacing_from_ll(dom)
}


# get the grid spacing at the centre of the domain
estimate_grid_spacing_from_ll <- function(dom) {
  dom_ext <- harpCore::domain_extent(dom)

  centre_lon <- dom_ext$clonlat[1]
  centre_lat <- dom_ext$clonlat[2]

  lon_range <- c(centre_lon - dom_ext$dx / 2, centre_lon + dom_ext$dx / 2)
  lat_range <- c(centre_lat - dom_ext$dy / 2, centre_lat + dom_ext$dy / 2)

  dx <- haversine(lon_range[1], lon_range[2], centre_lat, centre_lat)
  dy <- haversine(centre_lon, centre_lon, lat_range[1], lat_range[2])

  mean(c(dx, dy))
}

haversine <- function(lon1, lon2, lat1, lat2) {
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180

  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180

  a <- sin(dphi / 2) ^ 2 + cos(phi1) * cos(phi2) * sin(dlam / 2) ^ 2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  round(6371000 * c)
}

geolist_to_iris_cube <- function(geo, valid_dttm) {

  DimCoord <- reticulate::import(
    "iris.coords", delay_load = TRUE, convert = FALSE
  )$DimCoord
  Cube <- reticulate::import(
    "iris.cube", delay_load = TRUE, convert = FALSE
  )$Cube
  np_stack <- reticulate::import(
    "numpy", delay_load = TRUE, convert = FALSE
  )$stack

  dom     <- harpCore::get_domain(geo)
  dom_ext <- meteogrid::DomainExtent(dom)
  x       <- seq(dom_ext$x0, dom_ext$x1, dom_ext$dx)
  y       <- seq(dom_ext$y0, dom_ext$y1, dom_ext$dy)

  x_dims <- DimCoord(
    reticulate::np_array(x),
    standard_name = "projection_x_coordinate",
    units         = "m"
  )

  y_dims <- DimCoord(
    reticulate::np_array(y),
    standard_name = "projection_y_coordinate",
    units         = "m"
  )

  time_dims <- DimCoord(
    reticulate::np_array(valid_dttm),
    standard_name = "time",
    units = "seconds since 1970-01-01 00:00:00 +00:00"
  )

  Cube(
    reticulate::np_array(np_stack(geo)),
    dim_coords_and_dims = list(
      list(time_dims, 0),
      list(x_dims, 1),
      list(y_dims, 2)
    )
  )
}

as_int <- function(x) {
  if (!is.null(x)) {
    x <- as.integer(x)
  }
  x
}

check_harp_df <- function(x, data_col, time_col) {


  if (!inherits(x, "harp_grid_df")) {
    cli::cli_abort(c(
      "Invlaid class for {.arg field_data}",
      "i" = paste(
        "{.arg field_data} should be a data frame",
        "with class {.cls harp_grid_df}"
      ),
      "x" = "You supplied an object of class {.cls class(x)}."
    ), call = rlang::caller_env())
  }

  if (!is.element(data_col, colnames(x))) {
    geolist_cols <- names(which(vapply(x, harpCore::is_geolist, logical(1))))
    if (length(geolist_cols) != 1) {
      cli::cli_abort(
        "{.arg data_col} = \"{data_col}\" not found in {.arg field_data}",
        call = rlang::caller_env()
      )
    }

    cli::cli_warn(c(
      "{.arg data_col} = \"{data_col}\" not found in {.arg field_data}",
      "i" = "Column \"{geolist_cols}\" is a {.cls geolist}.",
      "i" = "Setting {.arg data_col} = \"{geolist_cols}\""
    ))

    data_col <- geolist_cols
  }

  if (!is.element(time_col, colnames(x))) {
    time_cols <- names(which(vapply(x, is_posix, logical(1))))
    if (length(time_cols) != 1) {
      cli::cli_abort(
        "{.arg dttm_col} = \"{time_col}\" not found in {.arg field_data}",
        call = rlang::caller_env()
      )
    }

    cli::cli_warn(c(
      "{.arg data_col} = \"{time_col}\" not found in {.arg field_data}",
      "i" = "Column \"{time_cols}\" is class {.cls POSIXt}.",
      "i" = "Setting {.arg dttm_col} = \"{time_cols}\""
    ))

    time_col <- time_cols
  }

  list(data_col, time_col)

}

is_posix <- function(x) inherits(x, "POSIXt")
