#' GEKS
#'
#' Takes a time series of prices and id's and returns an index using the GEKS
#' method.
#'
#' The function takes vectors for each of the inputs. It is important to note
#' that the \code{times} argument must be of numeric or Date class. This is because
#' the order of the dates matters. \cr \cr
#' The function also has the capability to run in parallel, using the
#' \code{num_cores} argument. Note that for smaller datasets using non-parallel
#' code is often faster than using parallelisation due to the overhead
#' associated with dividing the job across multiple processors.
#'
#' @param times vector of the times at which price observations were made. \cr
#' NOTE: \code{times} must be of class Date or numeric.
#' @param price vector of log of prices at the given time
#' @param id vector of distinct identification number of consumer goods
#' @param features required if calculating 'impute-tornqvist'. Data.frame of features.
#' @param window_length single number for length of windows of the data that
#' regressions are fit on
#' @param quantity vector of quantities
#' @param splice_pos The position on which to splice the windows together.
#' This can be a number from 1 to window_length or any of
#' c("window", "half","movement", "mean").
#' @param index_method A character string to select the index number method.
#' Valid index number methods are fisher, tornqvist, impute-tronqvist, jevons. The default is tornqvist. (see ?IndexNumR::GEKSIndex)
#' @param num_cores Number of cores to use for parallel computation.
#' Convention is parallel::detectCores()-1 on local machines
#' @return The function returns a list of 3 items:
#'     \describe{
#'         \item{\strong{geks}}{a dataframe of the GEKS index}
#'         \item{\strong{fixed_effects}}{a dataframe of the unspliced coefficients
#'         of the fixed effects model. The number of rows in the data frame is
#'         \code{window_length} * the number of windows in the data}
#'          \item{\strong{diagnostics}}{a dataframe of diagnostic results
#'          produced during model calculation. The diagnostics dataframe has the
#'          following columns:}
#'          \describe{
#'               \item{\strong{contrib_rids_pc}}{the \% of ids which exist in
#'               the window and contribute to the index i.e. the id has at least
#'               2 prices in the window}
#'               \item{\strong{contrib_rids_nm}}{the number of ids which exist
#'               in the window that contribute}
#'               \item{\strong{total_rids_in_data}}{number of ids in the entire
#'                dataset}
#'               \item{\strong{total_rids_in_window}}{number of ids which have
#'               any prices in the window}
#'               \item{\strong{num_records_in_window}}{The number of rows of
#'               data in this window}
#'               \item{\strong{QU_1st}}{25\% of the id's have less than this many
#'               prices in the window}
#'               \item{\strong{gm_mean_entries_per_rid}}{the geometric mean
#'               number of prices per id in this window}
#'               \item{\strong{mean_entries_per_rid}}{the mean number of prices
#'                per rid in this window}
#'               \item{\strong{median_entries_per_rid}}{the median number of
#'               prices per rid in this window}
#'               \item{\strong{QU_3rd}}{75\% of the id's have less than this many
#'               prices in the window}
#'               \item{\strong{window_id}}{identifier for the window}
#'               }
#'               }
#' @examples
#' GEKS(times = turvey$month,
#' price = turvey$price,
#' id = turvey$commodity,
#' window_length = 5,
#' quantity = turvey$quantity,
#' splice_pos = "mean",
#' num_cores = NULL)
#'
#' GEKS(times = turvey$month,
#' price = turvey$price,
#' id = turvey$commodity,
#' window_length = 5,
#' quantity = turvey$quantity,
#' splice_pos = "mean",
#' num_cores = 2)
#'
#' GEKS(times = turvey$month,
#' price = turvey$price,
#' id = turvey$commodity,
#' window_length = 5,
#' quantity = NULL,
#' index_method = "jevons",
#' splice_pos = "mean",
#' num_cores = NULL)
#'
#' @export
#' @import dplyr
#' @import foreach
#' @importFrom methods is
#' @importFrom stats median quantile relevel
#' @importFrom utils head setTxtProgressBar tail txtProgressBar
GEKS <-  function(times, price, id, features=NULL, window_length, quantity = NULL,
                  splice_pos = "mean",
                  index_method = "tornqvist",
                  num_cores = NULL) {


  timer <- Sys.time() # Get the current time

  # check arguments are all legit
  # times will be converted to numeric if it is a date, and a date_flag is
  # returned so that the times can be coerced back into a date before returning
  c(times, price, id, quantity, window_length, splice_pos) %=%
    check_inputs (times = times,
                  price = price,
                  id = id,
                  quantity = quantity,
                  window_length = window_length,
                  splice_pos = splice_pos)


  # make a data frame from all of the inputs
  prices.df <- data.frame(times = times,
                          price = price,
                          quantity = quantity,
                          id = id)

  if(is.null(features)&index_method=="impute-tornqvist"){
    stop("You must provide a data frame of features
         and the id when calculating impute-tornqvist")
  }else if(index_method!="impute-tornqvist"){
    features <- id
  }

  if(index_method=="impute-tornqvist"){
  prices.df <- cbind(prices.df,features)
  }
  # It is essential that the data frame is sorted by date
  # use an if because sorting is slow, but testing is fast
  if (is.unsorted(prices.df$times)){
    prices.df <- prices.df [order(prices.df$times), ]
  }

  # Now add a new column which will be used as the time variable.
  # this is to allow the input to be either Dates of numeric
  prices.df <- prices.df %>%
    mutate(times_index = as.numeric(as.factor(times)))

  # Get the indexes of the start of each window
  window_st_days <- get_window_st_days (dframe = prices.df,
                                        window_length = window_length)

  num_windows <- length(window_st_days)
  cat("Number of windows:", num_windows, "\n")

  # Only run in parallel if num_cores not NULL, and doSNOW can be loaded
  if (!is.null(num_cores) &
      (requireNamespace("doSNOW", quietly = TRUE))&
      (requireNamespace("snow", quietly = TRUE))) {
    # Starting Parallel =======================================================
    cat("Initialising cores for parallel...\n")
    cores <- num_cores
    cl <- snow::makeSOCKcluster(cores)
    doSNOW::registerDoSNOW(cl)

    rqd_packs <- c("dplyr", "MatrixModels")
    rqd_data <- c("FE_model", "lmfun", "get_win_dates")

    pb <- txtProgressBar(min = 1, max = num_windows, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress) # Start the PB

    fe_ind_and_diag <- foreach(i = 1:num_windows,
                               .packages = rqd_packs,
                               .options.snow = opts,
                               .export = rqd_data) %dopar% {
                                 FE_model (st_date = window_st_days[i],
                                           dframe = prices.df,
                                           features = colnames(features),
                                           window_length = window_length,
                                           index_method = index_method)}

    # fe_ind_and_diag is a list of lists. unpack into 2 sperate lists of vectors
    fe_indexes <-  lapply(fe_ind_and_diag, "[[", 1)
    diagnostics <- lapply(fe_ind_and_diag, "[[", 2)

    close(pb) # End The PB
    snow::stopCluster(cl)
  } else {
    # Non Parallel Code ======================================================
    if (!is.null(num_cores)){
      warning("You have selected a number of cores, but GEKS is running ",
              "non-Parallel code. Have you installed the doSNOW package?")
    }
    # Initialise the list
    fe_indexes <-  vector("list", length(window_st_days))
    diagnostics <- vector("list", length(window_st_days))
    # Make a pb
    pb <- txtProgressBar(min = 0, max = length(window_st_days), style = 3)

    for (i in 1:length(window_st_days)){

      c(fe_indexes[[i]],
        diagnostics[[i]]) %=% FE_model(st_date = window_st_days[i],
                                       dframe = prices.df,
                                       features = colnames(features),
                                       window_length = window_length,
                                       index_method = index_method)

      setTxtProgressBar(pb, i)
    }
    close(pb)
  }


  # Convert fe_indexes from a list of data.frames to a wide dataframe
  fe_indexes <- as.data.frame(fe_indexes)

  # Diagnostics from list to long df, and add a column for window_id
  # diagnostics <- data.frame()
  diagnostics <- bind_rows(diagnostics) %>%
    mutate(window_id = 1:length(diagnostics))

  cat("\nFE model complete. Splicing results together\n")

  # Convert back fom log price
  # fe_indexes <- exp (fe_indexes)
  # Add in a row of 1's for the baseline month
  # fe_indexes <- rbind (rep(1, each = ncol(fe_indexes)), fe_indexes)

  # fe_list is a list of each window's fixed effects index
  fe_list <- get_fe_list (fe_indexes = fe_indexes,
                          dframe = prices.df,
                          window_st_days = window_st_days,
                          window_length = window_length)

  # Make the GEKS from the fe_list
  geks_df <- get_geks_df (fe_list = fe_list,
                          window_length = window_length,
                          splice_pos = splice_pos)


  cat("\nFinished. It took", round(Sys.time() - timer, 2), "seconds\n\n")

  # Wrap the output in a list and return
  list(geks = geks_df,
       fixed_effects = bind_rows(fe_list),
       diagnostics = diagnostics)
}
