################################################################################
### Feature Detection ##########################################################
################################################################################

#' harp wrapper for the tobac `feature_detection_multithreshold()` function
#'
#' This function is used to detect features on a field based on contiguous
#' regions. The regions are above / below a threshold depending on the value of
#' the `target` argument. Note that 3d features are not yet implemented for this
#' wrapper.
#'
#' @param field_data A `harp_grid_df` data frame such as one returned by the
#'   *harpIO* functions
#'   [harpIO::read_grid()] with `data_frame = TRUE`, [harpIO::read_forecast()],
#'   or [harpIO::read_analysis()].
#' @param thresholds  Threshold values used to select target regions to track.
#'   The feature detection is inclusive of the threshold value(s), i.e. values
#'   greater/less than or equal are included in the target region. The `target`
#'   argument controls whether the detection is based on less than or greater
#'   than the threshold(s).
#' @param data_col <[`tidy-select`][dplyr_tidy_select]> The column in
#'   `field_data` containing the fields to be used to detect features in. Should
#'   be a `<geolist>` column. If the named column is not found in `field_data`,
#'   but `field_data` contains 1 `<geolist>` column, that `<geolist>` column is
#'   used.
#' @param dttm_col <[`tidy-select`][dplyr_tidy_select]> The column in
#'   `field_data` containing the date-times to be used for the time dimension.
#'   Can be numeric with units in Unix time (seconds since 1970-01-01 00:00:00),
#'   or a `<POSIXt>`column. If the named column is not found in `field_data`,
#'   but `field_data` contains 1 `<POSIXt>` column, that `<POSIXt>` column is
#'   used.
#' @param target Flag to determine if tracking is targeting minima or maxima in
#'   the data. Should be "max" or "min". Default is "max".
#' @param position_threshold Flag to choose the method to be used for the
#'   setting the position of the tracked feature. Can be one of "centre",
#'   "extreme", or "weighted_diff". Default is ‘centre’, though "weighted_diff"
#'   is often preferable for atmospheric features.
#' @param sigma_threshold Standard deviation for initial filtering step. Default
#'   is 0.5.
#' @param n_erosion_threshold Number of pixels by which to erode the identified
#'   features. Default is 0.
#' @param n_min_threshold Minimum number of identified contiguous pixels for a
#'   feature to be detected. Default is 0.
#' @param min_distance Minimum distance between detected features (in metres).
#'   Default is 0.
#' @param feature_number_start Feature id to start with. Default is 1.
#' @param pbc_flag  Sets whether to use periodic boundaries, and if so in which
#'   directions. "none" means that we do not have periodic boundaries "hdim_1"
#'   means that we are periodic along hdim1, "hdim_2" means that we are periodic
#'   along hdim2 and "both" means that we are periodic along both horizontal
#'   dimensions
#' @param vertical_coord Name of the vertical coordinate. If `NULL`, tries to
#'   auto-detect. It looks for the coordinate or the dimension name
#'   corresponding to the string.
#' @param vertical_axis The vertical axis number of the data. If `NULL`, uses
#'   `vertical_coord` to determine axis. This must be >=0.
#' @param detect_subset Whether to run feature detection on only a subset of the
#'   data. If this is not `NULL`, it should be a named list and it will subset
#'   the grid that we run feature detection on to the range specified for each
#'   axis specified. The format of this list is: `list(axis-number = c(start,
#'   end))`, where axis-number is the number of the axis to subset, start is
#'   inclusive, and end is exclusive. For example, if your data are oriented as
#'   (time, z, y, x) and you want to only detect on values between z levels 10
#'   and 29, you would set: `list("1" = c(10, 30))`. __Note that this is not
#'   tested.__
#' @param wavelength_filtering  Minimum and maximum wavelength for horizontal
#'   spectral filtering in metres as a 2 element vector. Default is `NULL`.
#' @param dz Constant vertical grid spacing (metres). If not specified and
#'   the input is 3D, this function requires that altitude is available in the
#'   features input. If you specify a value here, this function assumes that it
#'   is the constant z spacing between points, even if `z_coordinate_name` is
#'   specified.
#' @param strict_thresholding If `TRUE`, a feature can only be detected if all
#'   previous thresholds have been met. Default is `FALSE`.
#'
#' @return A data frame of detected features.
#' @export
#'
#' @importFrom zeallot %<-%
detect_features_multithreshold <- function(
  field_data,
  thresholds,
  data_col           = "gridded_data",
  dttm_col           = "valid_dttm",
  target             = c("max", "min"),
  position_threshold = c(
    "centre",
    "extreme",
    "weighted_diff",
    "weighted_abs"
  ),
  sigma_threshold      = 0.5,
  n_erosion_threshold  = 0,
  n_min_threshold      = 0,
  min_distance         = 0,
  feature_number_start = 1,
  pbc_flag             = c("none", "h_dim1", "h_dim2", "both"),
  vertical_coord       = NULL,
  vertical_axis        = NULL,
  detect_subset        = NULL,
  wavelength_filtering = NULL,
  dz                   = NULL,
  strict_thresholding  = FALSE
) {

  data_col <- rlang::enquo(data_col)
  time_col <- rlang::enquo(dttm_col)

  data_col_name <- rlang::as_name(data_col)
  time_col_name <- rlang::as_name(time_col)

  c(data_col_name, time_col_name) %<-% check_harp_df(
    field_data, data_col_name, time_col_name
  )

  target <- match.arg(target)
  target <- switch(
    target,
    "min" = "minimum",
    "max" = "maximum"
  )

  position_threshold <- match.arg(position_threshold)
  if (position_threshold == "centre") {
    position_threshold <- "center"
  }

  pbc_flag <- match.arg(pbc_flag)

  n_erosion_threshold  <- as_int(n_erosion_threshold)
  n_min_threshold      <- as_int(n_min_threshold)
  feature_number_start <- as_int(feature_number_start)
  vertical_axis        <- as_int(vertical_axis)

  if (!is.element(data_col_name, colnames(field_data))) {
    possible_cols <- glue::glue_collapse(
      colnames(field_data)[vapply(field_data, harpCore::is_geolist, logical(1))],
      sep = ", ", last = " or "
    )
    cli::cli_abort(c(
      "{.arg data_col} is not a column in {.arg field_data}.",
      "x" = "You supplied data_col = {data_col_name}.",
      "i" = "{.arg data_col} must be one of {possible_cols}."
    ))
  }

  if (!is.element(time_col_name, colnames(field_data))) {
    is_POSIX <- function(x) inherits(x, "POSIXt")
    possible_cols <- glue::glue_collapse(
      colnames(field_data)[vapply(field_data, is_POSIX, logical(1))],
      sep = ", ", last = " or "
    )
    cli::cli_abort(c(
      "{.arg data_col} is not a column in {.arg field_data}.",
      "x" = "You supplied data_col = {data_col_name}.",
      "i" = "{.arg data_col} must be one of {possible_cols}."
    ))
  }

  dx <- get_grid_spacing(harpCore::get_domain(field_data[[data_col_name]]))

  res <- reticulate::py_to_r(reticulate::py_suppress_warnings(
    tobac$feature_detection_multithreshold(
      geolist_to_iris_cube(
        field_data[[data_col_name]], as.numeric(field_data[[time_col_name]])
      ),
      dx,
      threshold            = thresholds,
      target               = target,
      position_threshold   = position_threshold,
      sigma_threshold      = sigma_threshold,
      n_erosion_threshold  = n_erosion_threshold,
      n_min_threshold      = n_min_threshold,
      min_distance         = min_distance,
      feature_number_start = feature_number_start,
      PBC_flag             = pbc_flag,
      vertical_coord       = vertical_coord,
      vertical_axis        = vertical_axis,
      detect_subset        = detect_subset,
      wavelength_filtering = wavelength_filtering,
      dz                   = dz,
      strict_thresholding  = strict_thresholding
    )
  ))

  res$frame   <- as.integer(res$frame)
  res$idx     <- as.integer(res$idx)
  res$feature <- as.integer(res$feature)

  tibble::as_tibble(res)
}


################################################################################
### Segmentation ###############################################################
################################################################################

#' harp wrapper for tobac `segmentation.segmentation_2d()` function
#'
#' @inheritParams detect_features_multithreshold
#'
#' @param features A features data frame, as returned by
#'   \code{\link{detect_features_multithreshold()}}.
#' @param threshold Threshold for the watershedding field to be used for the
#'   mask.
#' @param level Levels at which to seed the cells for the watershedding
#'   algorithm. Default is `NULL`.
#' @param method Flag determining the algorithm to use (currently only
#'   "watershed" is implemented).
#' @param max_distance Maximum distance from a marker allowed to be classified
#'   as belonging to that cell in metres. Default is `NULL`.
#' @param seed_3d_flag Seed 3D field at feature positions with either the full
#'   column (default) or a box of user-set size.
#' @param statistic Default is `NULL`. Parameter to calculate bulk statistics
#'   within feature detection. Named list with callable function(s) to apply
#'   over the region of each detected feature and the name of the statistics to
#'   appear in the feature output data frame. The functions should be the values
#'  and the names of the metric the keys (e.g. list(mean = np.mean)). This
#'  probably doesn't work yet as there needs to be a translation between R and
#'  Python functions here.
#'
#' @return A named list with:
#'   * segments: A data frame with a `geolist` column with 2d fields on the
#'     same domain as the input data with integer values denoting each segment.
#'   * features: Feature data frame including the number of cells in the
#'     segmented area of the feature at the time step.
#' @export
#'
segment_2d <- function(
  features,
  field_data,
  threshold,
  data_col     = "gridded_data",
  dttm_col     = "valid_dttm",
  target       = c("max", "min"),
  level        = NULL,
  method       = "watershed",
  max_distance = NULL,
  pbc_flag     = c("none", "h_dim1", "h_dim2", "both"),
  seed_3d_flag = c("column", "box"),
  statistic    = NULL
) {

  data_col <- rlang::enquo(data_col)
  time_col <- rlang::enquo(dttm_col)

  data_col_name <- rlang::as_name(data_col)
  time_col_name <- rlang::as_name(time_col)

  c(data_col_name, time_col_name) %<-% check_harp_df(
    field_data, data_col_name, time_col_name
  )

  target <- match.arg(target)
  target <- switch(
    target,
    "min" = "minimum",
    "max" = "maximum"
  )

  pbc_flag <- match.arg(pbc_flag)

  seed_3d_flag <- match.arg(seed_3d_flag)

  dx <- get_grid_spacing(harpCore::get_domain(field_data[[data_col_name]]))

  segmentation <- reticulate::py_to_r(tobac$segmentation_2D(
    features,
    geolist_to_iris_cube(
      field_data[[data_col_name]], as.numeric(field_data[[time_col_name]])
    ),
    dx,
    threshold    = threshold,
    target       = target,
    level        = level,
    method       = method,
    max_distance = max_distance,
    PBC_flag     = pbc_flag,
    seed_3D_flag = seed_3d_flag,
    statistic    = statistic
  ))

  names(segmentation) <- c("segments", "features")

  segmentation$segments <- tibble::tibble(
    valid_dttm = harpCore::unixtime_to_dttm(
      as.numeric(field_data[[time_col_name]])
    ),
    segmentation_mask = harpCore::geolist(lapply(
      seq_len(nrow(field_data)),
      function(i) harpCore::geofield(
        segmentation$segments$data[i, ,],
        domain = harpCore::get_domain(field_data[[data_col_name]])
      )
    ))
  )

  segmentation$features <- tibble::tibble(segmentation$features)

  segmentation

}

################################################################################
### Tracking ###################################################################
################################################################################

#' harp wrapper to the tobac function `tracking.linking_trackpy()`.
#'
#' @inheritParams detect_features_multithreshold
#' @param features A features data frame as returned by
#'   \code{\link{detect_features_multithreshold()}} or
#'   \code{\link{segment_2d()}}
#' @param dz Constant vertical grid spacing (meters), optional. If not specified
#'   and the input is 3D, this function requires that vertical_coord is
#'   available in the features input. If you specify a value here, this function
#'  assumes that it is the constant z spacing between points, even if
#'  `vertical_coord` is specified.
#' @param d_max Maximum search range in metres. Only one of d_max, or v_max can
#'   be set. Default is `NULL`.
#' @param subnetwork_size Maximum size of subnetwork for linking. This parameter
#'   should be adjusted when using adaptive search. Usually a lower value is
#'   desired in that case. For a more in depth explanation have look
#'   [here](https://soft-matter.github.io/trackpy/v0.5.0/tutorial/adaptive-search.html).
#'   If `NULL`, 30 is used for regular search and 15 for adaptive search.
#'   Default is `NULL`.
#' @param v_max Speed at which features are allowed to move in m/s. Only one of
#'   d_max or v_max can be set. Default is `NULL`.
#' @param memory Number of output timesteps features allowed to vanish for to
#'   still be considered tracked. Default is 0. _Warning_ :: This parameter
#'   should be used with caution, as it can lead to erroneous trajectory
#'   linking, espacially for data with low time resolution.
#' @param stubs Minimum number of timesteps of a tracked cell to be reported.
#'   Default is 1
#' @param time_cell_min Minimum length in time that a cell must be tracked for
#'   to be considered a valid cell in seconds. Default is `NULL`.
#' @param order Order of polynomial used to extrapolate trajectory into gaps and
#'   on start and end point. Default is 1.
#' @param extrapolate Number or timesteps to extrapolate trajectories.
#'   _Currently unused_. Default is 0.
#' @param method_linking Flag choosing method used for trajectory linking. Can
#'   be "predict" or "random". Default is ‘predict’. _Note_: the default in
#'   tobac is "random" though they typically encourage users to use ‘predict’.
#' @param adaptive_step Reduce search range by multiplying it by this factor.
#'   Needs to be used in combination with `adaptive_stop`. Default is `NULL`.
#' @param adaptive_stop  If not `NULL, when encountering an oversize subnet,
#'   retry by progressively reducing `search_range` by multiplying with
#'   `adaptive_step` until the subnet is solvable. If search_range becomes <=
#'   `adaptive_stop`, give up and raise a SubnetOversizeException. Needs to be
#'   used in combination with `adaptive_step`. Default is `NULL`.
#' @param cell_number_start Cell number for first tracked cell. Default is 1.
#' @param cell_number_unassigned Number to set the unassigned/non-tracked cells
#'   to. Default is -1.
#' @param vertical_coord Name of the vertical coordinate. The vertical
#'   coordinate used must be metres. If `NULL, it tries to auto-detect. It looks
#'   for the coordinate or the dimension name corresponding to the string. To
#'   use `dz`, set this to `NULL`.
#' @param min_h1 Minimum hdim_1 value, required when `pbc_flag` is "hdim_1" or
#'   "both".
#' @param max_h1 Maximum hdim_1 value, required when `pbc_flag` is "hdim_1" or
#'   "both".
#' @param min_h2 Minimum hdim_2 value, required when `pbc_flag` is "hdim_2" or
#'   "both".
#' @param max_h2 Maximum hdim_2 value, required when `pbc_flag` is "hdim_2" or
#'   "both".
#'
#' @return Data frame of the linked features, containing the column "cell", with
#'   integers indicating the affiliation of a feature to a specific track, and
#'   the variable "time_cell" with the time, in seconds' the cell has already
#'   existed.
#' @export
#'
link_tracks <- function(
  features,
  field_data,
  dz                     = NULL,
  data_col               = "gridded_data",
  dttm_col               = "valid_dttm",
  d_max                  = NULL,
  subnetwork_size        = NULL,
  v_max                  = NULL,
  memory                 = 0,
  stubs                  = 1,
  time_cell_min          = NULL,
  order                  = 1,
  extrapolate            = 0,
  method_linking         = c("predict", "random"),
  adaptive_step          = NULL,
  adaptive_stop          = NULL,
  cell_number_start      = 1,
  cell_number_unassigned = -1,
  vertical_coord         = NULL,
  min_h1                 = NULL,
  max_h1                 = NULL,
  min_h2                 = NULL,
  max_h2                 = NULL,
  pbc_flag               = c("none", "hdim_1", "hdim_2", "both")
) {

  data_col <- rlang::enquo(data_col)
  time_col <- rlang::enquo(dttm_col)

  data_col_name <- rlang::as_name(data_col)
  time_col_name <- rlang::as_name(time_col)

  c(data_col_name, time_col_name) %<-% check_harp_df(
    field_data, data_col_name, time_col_name
  )

  method_linking <- match.arg(method_linking)
  pbc_flag       <- match.arg(pbc_flag)

  memory                 <- as_int(memory)
  stubs                  <- as_int(stubs)
  order                  <- as_int(order)
  extrapolate            <- as_int(extrapolate)
  cell_number_start      <- as_int(cell_number_start)
  cell_number_unassigned <- as_int(cell_number_unassigned)
  subnetwork_size        <- as_int(subnetwork_size)
  min_h1                 <- as_int(min_h1)
  max_h1                 <- as_int(max_h1)
  min_h2                 <- as_int(min_h2)
  max_h2                 <- as_int(max_h2)

  dx <- get_grid_spacing(harpCore::get_domain(field_data[[data_col_name]]))

  tracks <- tobac$linking_trackpy(
    features,
    geolist_to_iris_cube(
      field_data[[data_col_name]], as.numeric(field_data[[time_col_name]])
    ),
    mean(diff(as.numeric(field_data[[time_col_name]]))),
    dx,
    dz                     = dz,
    d_max                  = d_max,
    d_min                  = NULL,
    subnetwork_size        = subnetwork_size,
    v_max                  = v_max,
    memory                 = memory,
    stubs                  = stubs,
    time_cell_min          = time_cell_min,
    order                  = order,
    extrapolate            = extrapolate,
    method_linking         = method_linking,
    adaptive_step          = adaptive_step,
    adaptive_stop          = adaptive_stop,
    cell_number_start      = cell_number_start,
    cell_number_unassigned = cell_number_unassigned,
    vertical_coord         = vertical_coord,
    min_h1                 = min_h1,
    max_h1                 = max_h1,
    min_h2                 = min_h2,
    max_h2                 = max_h2,
    PBC_flag               = pbc_flag
  )

  np_int32 <- reticulate::import(
    "numpy", delay_load = TRUE, convert = FALSE
  )$int32

  tracks[["time_cell"]] = np_int32(tracks[["time_cell"]]$dt$seconds)

  tracks <- tibble::tibble(reticulate::py_to_r(tracks))
  tracks$valid_dttm <- as.POSIXct(tracks$timestr, tz = "UTC")

  tracks <- tracks[!colnames(tracks) %in% c("time", "timestr")]

  colnames(tracks) <- suppressWarnings(harpCore::psub(
    colnames(tracks),
    c("^projection_", "_coordinate$"),
    c("", ""),
    exact = FALSE
  ))

  attr(tracks, "domain") <- harpCore::get_domain(field_data[[data_col_name]])

  tracks

}

