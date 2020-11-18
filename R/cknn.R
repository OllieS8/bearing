
#' Find comparable properties using clustered nearest neighbors
#'
#' @description Sales comparables are recent sales which have the same
#'   (or very similar) characteristics as a target (unsold) property. They are
#'   frequently used by assessors, real estate agents, and appraisers to
#'   determine the fair market value of a home. However, finding comparable
#'   properties at scale can be difficult.
#'
#'   This function can be used to quickly find comparables for any number of
#'   unsold properties. It can also be used more generally to find similar
#'   properties that are nearby each other, regardless of whether or not they
#'   sold.
#'
#'   See the \href{https://ccao-data-science---modeling.gitlab.io/packages/assessr/articles/finding-sales-comps.html}{documentation site}
#'   for example usage.
#'
#' @details The \code{cknn} algorithm works in two stages:
#'
#'   1. Divide the full set of sales into \code{m} clusters according to each
#'   property's characteristics. This mimics the process of market segmentation
#'   or separating properties into different classes. This clustering is done
#'   using the k-prototypes function \code{\link[clustMixType]{kproto}} from
#'   the \href{https://cran.r-project.org/web/packages/clustMixType/index.html}{clustMixType} library.
#'   See \href{https://journal.r-project.org/archive/2018/RJ-2018-048/RJ-2018-048.pdf}{the clustMixType whitepaper}
#'   for more information.
#'
#'   2. For each property \code{i}, find the \code{k} nearest neighbors within
#'   \code{i}'s cluster, minimizing the distance over planar coordinates and
#'   Euclidean distance to all cluster centers, This is accomplished with the
#'   fast kNN function from \code{\link[dbscan]{kNN}}.
#'
#'   Options for inputs to \code{var_weights} include:
#'
#'   - A named list with names corresponding to column names in the input data.
#'   Names not included in the list are assumed to have a value of 1. These
#'   named values are multiplied by the variance estimates created by
#'   \code{\link[clustMixType]{lambdaest}}. Higher values will weight variables
#'   more heavily during clustering.
#'
#'   - A \code{p} long unnamed vector, where \code{p} is equal to the number
#'   columns in the input data. These weights are not multiplied by the variance
#'   estimates created by \code{\link[clustMixType]{lambdaest}}.
#'
#'   - A single unnamed numeric value. This value trades off the relative
#'   importance of numeric versus categorical variables. Higher values will
#'   more heavily weight categorical variables, while a value of 0 replicates
#'   standard k-means (numerics only).
#'
#'   - A \code{NULL} value. This uses the default estimates produced by
#'   \code{\link[clustMixType]{lambdaest}}. All variables are weighted equally.
#'
#' @note Input data should be thoroughly cleaned. Outliers in numeric vectors
#'   and factors with rare levels can both affect clustering performance.
#'   Outlier values should be removed. Rare factor levels should be collapsed
#'   into a single level or removed.
#'
#' @param data A data frame containing the variables to cluster on. Should
#'   contain both numerics and factors. Numerics should be unscaled. Lat/lon
#'   should NOT be included.
#' @param lon A numeric vector of longitude values, reprojected into planar
#'   coordinates specific to the target area. See
#'   \href{https://geocompr.robinlovelace.net/reproj-geo-data.html}{here} for
#'   details on reprojection using R.
#' @param lat A numeric vector of latitude values, reprojected into planar
#'   coordinates specific to the target area. See
#'   \href{https://geocompr.robinlovelace.net/reproj-geo-data.html}{here} for
#'   details on reprojection using R.
#' @param m The number of clusters to create using the
#'   \code{\link[clustMixType]{kproto}} function.
#' @param k The number of nearest neighbors to return for each row of
#'   input data.
#' @param l Hyperparameter representing the trade-off between distance and
#'   characteristics in kNN matching. Must be >= 0 and <= 1. Value equal to 1
#'   will match on distance only, while value equal to 0 will disregard distance
#'   and match on characteristics only. Default 0.5 (equal weight).
#' @param var_weights Value(s) passed to \code{lambda} input of
#'   \code{\link[clustMixType]{kproto}}. See details.
#' @param keep_data Logical for whether original data should be included in the
#'   returned object.
#' @param ... Arguments passed on to \code{\link[clustMixType]{kproto}},
#'   most commonly \code{iter.max}.
#'
#' @return Object of class \code{cknn} containing:
#' @return \item{kproto}{\code{\link[clustMixType]{kproto}} object containing
#'   clusters, centroids, etc.}
#' @return \item{knn}{List of \code{k} nearest neighbors for each row in the
#'   input data.}
#' @return \item{knn_idx}{Lookup for translating in-cluster index positions to
#'   row indices from the input data. Used by predict method.}
#' @return \item{lon}{Unaltered input longitude vector. Used by predict method
#'   for scaling new input data.}
#' @return \item{lat}{Unaltered input latitude vector. Used by predict method
#'   for scaling new input data.}
#'@return \item{var_weights}{Unaltered variable weights used to construct the
#'   cknn model.}
#' @return \item{m}{Number of clusters created by
#'   \code{\link[clustMixType]{kproto}}.}
#' @return \item{k}{Number of nearest neighbors returned by
#'   \code{\link[dbscan]{kNN}}.}
#' @return \item{l}{Hyperparameter used for distance/characteristics trade-off.}
#' @return \item{data}{Unaltered input data frame. Used by predict method for
#'   scaling new input data. Only returned if \code{keep_data} is \code{TRUE}.}
#'
#' @md
#' @family cknn
#' @importFrom utils capture.output
#' @export
cknn <- function(data, lon, lat, m = 5, k = 10, l = 0.5,
                 var_weights = NULL, keep_data = TRUE, ...) {

  # Basic error handling and expected input checking
  stopifnot(
    is.data.frame(data),
    ncol(data) > 1,
    nrow(data) > 1,
    all(!is.na(lon)) & all(!is.na(lat)),
    is.numeric(lon) & length(lon) == nrow(data),
    is.numeric(lat) & length(lat) == nrow(data),
    is.numeric(m) & m > 1,
    is.numeric(k) & k > 1,
    is.numeric(l) & l >= 0 & l <= 1,
    is.numeric(var_weights) | is.null(var_weights),
    is.logical(keep_data)
  )

  # Stop if any cols are not numeric or factor
  correct_cols <- unlist(lapply(data, function(x) is.factor(x) | is.numeric(x)))
  if (!all(correct_cols)) {
    stop("One or more columns is not numeric or factor type\n")
  }

  # Warn if factors contain levels with rare values (less than 0.5% of vals)
  fct_cols <- unlist(lapply(data, is.factor))
  chk_levels <- function(x) any((table(droplevels(x)) / nrow(data)) < 0.005)
  if (any(sapply(data[fct_cols], chk_levels))) {
    warning("One or more factor columns contains rare levels, see ?cknn for details\n") # nolint
  }

  # Stop if missing values are present
  if (any(sapply(data, function(x) sum(is.na(x))))) {
    stop("Missing values present in the input data\n")
  }

  # Copy data for later output with the cknn object, rescale numeric columns
  # in-place to be between 0 and 1
  if (keep_data) data_unscaled <- data else data_unscaled <- NULL
  num_cols <- which(unlist(lapply(data, is.numeric)))
  data[num_cols] <- lapply(data[num_cols], rescale)

  # Setup variable weights for kproto using pre-calculated lambdas multiplied
  # by relative weights from var_weights. Weight = 1 if not specified. If var
  # weights is a single value, then this is passed to kproto function directly
  var_c <- rep(1, ncol(data))
  if (length(names(var_weights)) > 0 & any(!names(var_weights) %in% names(data))) { # nolint
    stop("All named values in var_weights must be present in data\n")
  } else if (!is.null(var_weights) & length(names(var_weights)) > 0) {
    var_c[which(names(data) %in% names(var_weights))] <- var_weights
    invisible(capture.output(
      lambdas <- clustMixType::lambdaest(data, outtype = "vector") * var_c
    ))
  } else if (length(var_weights) == 1 | length(var_weights) == ncol(data)) {
    lambdas <- var_weights
  } else {
    invisible(capture.output(
      lambdas <- clustMixType::lambdaest(data, outtype = "numeric")
    ))
  }

  # Create m clusters, where m is determined by the user using the
  # elbow method, index or some other validation function.
  invisible(capture.output(
    kproto_clusts <- clustMixType::kproto(
      x = data,
      lambda = lambdas,
      k = m,
      keep.data = FALSE,
      ...
    )
  ))

  # Print m, k, and l values
  message("Creating clusters with: m = ", m, ", k = ", k, ", l = ", l, "\n")

  # Create a matrix of distance data for kNN. NOTE, the input data MUST be
  # rescaled according to the original data, otherwise kNN will not work
  dist_data <- data.frame(
    lon = rescale(lon - mean(lon), from = demean_coords(lon, lat)) * l,
    lat = rescale(lat - mean(lat), from = demean_coords(lon, lat)) * l,
    dist = apply(kproto_clusts$dists, 2, rescale) * (1 - l),
    row.names = seq_len(nrow(data))
  )

  # For each cluster m, find the K nearest neighbors for every sale. If the
  # number of sales in m is less than K, just return the members of m.
  # kNN function returns index positions relative to each cluster m, not the
  # whole input dataset. Inputs to KNN MUST BE SCALED TO BE WITHIN SAME RANGE
  knn_clusts <- lapply(
    split(dist_data, kproto_clusts$cluster),
    function(m_cluster) {
      nrd <- nrow(m_cluster)
      if (nrd > k) {
        # If m > k, return k nearest neighbors
        list(dbscan::kNN(m_cluster, k = k, sort = FALSE)$id)
      } else {
        # Otherwise, return the members of m in a m * k matrix
        m_out <- matrix(
          rep(c(seq(1:nrd), rep(NA, k - nrd)), nrd),
          nrow = nrd,
          ncol = k,
          byrow = TRUE
        )
        rownames(m_out) <- rownames(m_cluster)
        colnames(m_out) <- as.character(seq_len(ncol(m_out)))
        list(m_out)
      }
    }
  )

  # Create a N * K matrix where each row is a sale and each column is an index
  # position for a nearest neighbor within m. Convert this to a list where each
  # element is a sale with k nearest neighbors in a vector
  knn_lst <- as.list(data.frame(t(do.call(
    rbind, unlist(knn_clusts, recursive = FALSE)
  ))))

  # Create an ordered vector of cluster membership c(1, 1, 1, 2, 2, 2 ... K)
  # That corresponds to the cluster membership of each element of knn_lst. This
  # works due to the nature of the split() function, which first orders, then
  # splits
  clusters <- rep(
    sort(unique(kproto_clusts$cluster)),
    table(kproto_clusts$cluster)
  )

  # Create a lookup table with the goal of converting the within-cluster row
  # indices of knn_lst to row indices relative to the input dataset
  knn_idx <- data.frame(
    input_data_idx = as.numeric(gsub("\\D+", "", names(knn_lst))),
    clust_name = clusters
  )

  # Map over each cluster and index to retrieve the row indices of the original
  # dataset
  data_idx <- as.list(data.frame(mapply(
    function(x, y) knn_idx$input_data_idx[knn_idx$clust_name == y][x],
    x = knn_lst,
    y = clusters,
    SIMPLIFY = TRUE
  )))

  # Output a list with the results from the kproto object and the knn function
  out <- list(
    kproto = kproto_clusts,
    knn = data_idx[order(as.numeric(gsub("\\D+", "", names(data_idx))))],
    knn_idx = knn_idx,
    lon = lon,
    lat = lat,
    var_weights = var_weights,
    m = m,
    k = k,
    l = l
  )
  if (keep_data) out <- c(out, list(data = data_unscaled))
  attr(out, "class") <- "cknn"

  return(out)
}




#' recombines knn output with original data
#'
#' @param initial_data original data frame
#' @param knn_object knn object from cknn function
#'
#' @return returns original data frame with knn information
#' @export
#'
#' @examples
recombine_data_knn <- function(initial_data, knn_object){
  initial_data %>%
    dplyr::mutate(
      m = knn_object$kproto$cluster,
      knn = knn_object$knn,
    ) %>%
    dplyr::rename(Latitude = lat,
           Longitude = lon)
}



#' return subject cluster
#'
#' @param df data frame with subject property and cluster information
#'
#' @return returns the subject cluster
#' @export
#'
#' @examples
get_subj_cluster <- function(df, subject_apn = NULL){
  tryCatch({
    subject_apn <- get('subject_apn')
  },
  error=function(cond){
    message('Error: Must run function define_subject before leaflet plot')
  })

  (df %>%
      dplyr::filter(APN == subject_apn) %>%
      dplyr::select(m))[[1]]
}

#' Return nearest neighbours data
#'
#' @param df data frame with nearest neighbours information
#' @param subject_apn subject_apn - default = NULL for Rmd
#'
#' @return returns data frame with just nearest neighbours and subject properties data
#' @export
#'
#' @examples
get_nn <- function(df, subject_apn = NULL){
  tryCatch({
    subject_apn <- get('subject_apn')
  },
  error=function(cond){
    message('Error: Must run function define_subject before leaflet plot')
  })

  nn <- (df %>%
           dplyr::filter(APN == subject_apn) %>%
           dplyr::select(knn))[[1]] %>%
    unlist()

  rbind(df[nn,],
        df %>%
          dplyr::filter(APN == subject_apn))

}

#' Finding the optimum number of clusters
#'
#' @param df data frame
#' @param m_values m (number of clusters) values to test
#' @param n_start number of repetitive computations of kproto with random initializations
#'
#' @return returns optimum number of clusters according to the silhouette index.
#' @export
#'
#' @examples
optimal_m <- function(df, m_values = 3:15, n_start = 5){
  clustMixType::validation_kproto(method = "silhouette",
                                  data = df,
                                  k = m_values,
                                  nstart = n_start,
                                  verbose = FALSE)$k_opt
}

