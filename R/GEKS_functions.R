FE_model <- function(st_date, dframe,features = NULL, window_length,index_method) {


  # Calculate GEKS on every window and extract the coefficients
  # for the time values only
  # Arguments:
  #     st_date - a date indicating the start day of the window
  #     dframe - a data frame of prices
  #     window_length - the length of the window
  # Returns:
  #     index for given window and associated diagnostics in the form of a list

  # Get the dates of each day in this window
  win_dates <- get_win_dates(st_date, window_length)

  # Subset dframe by the dates in this window
  dframe_win <- dframe[dframe$times_index %in%
                             win_dates, ]



  # Convert times into numeric continuous starting from 1
  dframe_win$times_n <- as.numeric(as.factor(dframe_win$times))

  #https://github.com/grahamjwhite/IndexNumR
  #This function does handle its own splicing and there are defaults
  #however because the dframe going in is exact length of window
  #splicing does not occur at this stage, it happens in get_geks_df

  if(index_method=="impute-tornqvist"){
    # browser()
    fe_coefs <- GEKSIndex(dframe_win,
                          pvar = "price",
                          qvar = "weight",
                          pervar = "times_n",
                          prodID = features,
                          unique_prodID = "id",
                          window = window_length,
                          indexMethod = index_method)
  }else{
    fe_coefs <- GEKSIndex(dframe_win,
                          pvar = "price",
                          qvar = "weight",
                          pervar = "times_n",
                          prodID = "id",
                          window = window_length,
                          indexMethod = index_method)
  }

  diagnostics <- get_diagnostics(dframe_win)

  # fe_coefs should equal the length of window
  if ((length(fe_coefs)) != window_length){
    # TODO Should this be an error?
    stop(paste("The number of time coefficients does not match the window",
               "length? Check that your times are correct\n"))
  }

  list(data.frame(fe_coefs),
       diagnostics)
}

get_diagnostics <- function (dframe){

  # contrib_rids_pc - the % of ids which exist in the window that contribute
  # contrib_rids_nm - the number of ids which exist in the window that contribute
  # weight_captured - proportion of total weight captured by those with more than 1 observation
  # total_rids_in_data - number of rids in the entire dataset
  # total_rids_in_window - number of rids which exist in the window
  # num_records_in_window - The quantity of data in this window
  # QU_1st - 25% of the id's have less than this many entries in the window
  # gm_mean_entries_per_rid - the geometric mean number of entries per rid
  # mean_entries_per_rid - the mean number of entries per rid
  # median_entries_per_rid - the median number of entries per rid
  # QU_3rd - 25% of the id's have less than this many entries in the window

  entries_per_rid <- dframe %>%
    droplevels() %>%
    group_by(id) %>%
    summarise(n = n()) %>%
    pull(n)

  captured_weight <- dframe %>%
    droplevels() %>%
    group_by(id)%>%
    filter(n() > 1)%>%
    ungroup() %>%
    summarise(weight = sum(weight))%>%
    pull(weight)

  total_weight <- dframe %>%
    ungroup() %>%
    summarise(weight = sum(weight)) %>%
    pull(weight)


  data.frame(contrib_rids_pc = mean(entries_per_rid > 1)*100,
             weight_captured = captured_weight/total_weight,
             contrib_rids_nm = sum(entries_per_rid > 1),
             total_rids_in_data = nlevels(dframe$id),
             total_rids_in_window = length(entries_per_rid),
             num_records_in_window = nrow(dframe),
             QU_1st = quantile(entries_per_rid, probs = .25),
             gm_mean_entries_per_rid = gm_mean(entries_per_rid),
             mean_entries_per_rid = mean(entries_per_rid),
             median_entries_per_rid = median(entries_per_rid),
             QU_3rd = quantile(entries_per_rid, probs = .75))

}

get_fe_list <- function (fe_indexes,
                         dframe,
                         window_st_days,
                         window_length) {
  # Takes the fe outputs and makes a list with of dataframes containing
  # indexes and correct dates
  #
  # Arguments:
  #     FEindxes - is a matrix of dim(window_length, num_windows) containing all
  #         window indexes
  # Returns:
  #     fe_list - a list of dataframes. Length is number of windows.
  #         nrow of each data frame is window length.
  #         The data frames contain:
  #             FE indexes for each window
  #             the window id (numeric)
  #             the dates

  fe_list <- list()

  # Loop over each window and construct the DF
  for (i in 1:ncol(fe_indexes)){
    # get the times_indexes for the current window
    times_temp <- get_win_dates(window_st_days[i],
                                window_length = window_length)
    # Convert theses to times, and keep only unique values
    times_temp <- dframe$times[dframe$times_index %in% times_temp]
    times_temp <- unique(times_temp)

    # Make the df for this list entry
    fe_list[[i]] <- data.frame(
      price_date = times_temp,
      fe_indexes = fe_indexes[, i],
      window_id = i)

    # row names are junk due to: times as factors, and rbinding rows
    rownames(fe_list[[i]]) <- c()
  }

  fe_list
}


get_geks_df <- function (fe_list, window_length, splice_pos) {
  # Splice the windows together to produce a continuous time series of indexes
  # Arguments:
  #   fe_list - output from get_fe_list. See that  function for details
  #   window_length -  window_length - the number of time_units per window
  #   splice_pos - the index on which to splice the windows. Can also be
  #       'mean' which is the geometric mean of all possible values of splice_pos


  # Initialise the df with a 1 for the first time period
  last_date <- tail(fe_list[[1]]$price_date, 1)

  geks <- data.frame(price_date = last_date,
                     fe_indexes = 1,
                     window_id = 1)

  # Loop over the windows, starting at the second window.
  # Cannot splice on the first
  for (i in 2: length(fe_list)){
    old_window <- fe_list[[i - 1]]$fe_indexes
    new_window <- fe_list[[i]]$fe_indexes

    # Get the previous GEKS index value
    old_geks <- geks$fe_indexes[nrow(geks)]

    update_factor <- splice_update (old_window,
                                    new_window,
                                    splice_pos = splice_pos)

    # Get the "new" GEKS index value
    new_geks <- old_geks * update_factor

    # buid up the new row
    # price_date is the last date in the current window
    last_date <- tail(fe_list[[i]]$price_date, 1)

    new_row <- data.frame(price_date = last_date,
                          fe_indexes = new_geks,
                          window_id = i)

    geks <- rbind(geks, new_row)
  }

  return (geks)
}


splice_update <- function (win_old, win_new, splice_pos){
  # Calculate the update factor for splicing two windows
  # Arguments
  #   win_old - the indexes for the previous window
  #   win_new - the indexes for the current window
  #   splice_pos - an integer for the time period on which to splice
  #     the windows. Can also be 'mean' or 'window' for different splice types
  #
  # Returns
  #   update_factor -  a single number which is the splice update factor

  stopifnot(length(win_old) == length(win_new))
  w <- length(win_old)
  # As the old window starts 1 entery earlier in time than the new window,
  # adding a NaN to the start of the new window makes the indexes of the 2
  # windows align. This value is never used in the calculations
  win_new <- c(NaN, win_new)

  # Variable names chosen to follow notation in IndexNumR Package
  # https://cran.r-project.org/web/packages/IndexNumR/vignettes/indexnumr.html
  Pw_new <- win_new[w]
  Pw1_new <- win_new[w + 1]
  Pw_old  <- win_old[w]

  if (splice_pos == "mean") {
    t_accum = c()
    for (t in seq(from = 2, to = w - 1, by = 1)) {
      Pt1_new <- win_new[t + 1]
      Pt1_old <- win_old[t + 1]
      t_accum <- c(t_accum, ((Pw1_new / Pt1_new) / (Pw_old / Pt1_old)))
    }
    update_factor <- gm_mean(t_accum)

  }else if (splice_pos == "movement") {
    update_factor <- (Pw1_new/Pw_new)

  } else if(splice_pos == "geomean"){
    t_accum <- c() # Accumulator for the t loop
    for (t in seq(from = 2, to = w-1, by = 1)) {
      Pt_new <- win_new[t]
      Pt_old <- win_old[t]

      t_accum <- c(t_accum, ((Pw_new / Pt_new) / (Pw_old / Pt_old)))
    }
    update_factor <- (Pw1_new / Pw_new) * gm_mean(t_accum)
  }

  else {
    Pn_new <- win_new[splice_pos]
    Pn_old <- win_old[splice_pos]
    update_factor <- (Pw1_new / Pn_new) / (Pw_old / Pn_old )
  }


  return (update_factor)
}


ITRYGEKS_t <- function(p0,p1,q0,q1,f0,f1,id0,id1){


  #Modify expenditure shares
  # browser()

  mod_q <- data.frame(period=c(rep(0,length(id0)),rep(1,length(id1))),
                      id=c(id0,id1),
                      q=c(q0,q1),
                      stringsAsFactors = F)

  mod_q <- mod_q%>%
    mutate(qsum = sum(q))%>%
    group_by(period)%>%
    mutate(pqsum = sum(q))%>%
    group_by(id)%>%
    mutate(pq = q/pqsum)%>%
    mutate(count = n())%>%
    mutate(exp_share = ifelse(count==1,
                              sum(q)/(qsum/2), # (exp of product)/(total exp/2 periods (average))
                              (mean(pq)))) # Take the average exp share

  if(any(mod_q$count>2)){
    stop("An ID in a given period is appearing more than once. Aggregate your input dataframe first so there
         is only 1 observation in each period for a given ID.")
  }
  p <- log(c(p0,p1)) #log price
  q <- mod_q$exp_share #c(q0,q1)
  f <- bind_rows(f0,f1)
  id <- c(id0,id1)
  #Tidy up of features
  f <- f%>%mutate_if(is.character,as.factor) #Convert to factor if character
  f[,which(sapply(f,nlevels)==1)] <- NULL #Remove if only 1 level of factor
  f <- f[,!apply(f,2,function(x)all(is.na(x)))] #Drop where whole column is NA

  period <- c(rep(0,length(p0)),rep(1,length(p1)))
  period <- as.factor(period)


  model_df <- data.frame(p = p,
                         id = id,
                         f = f,
                         period = period)%>%
    droplevels()

  # Regression doesn't work if there is only 1 item in the time window.
  model_df[,which(sapply(model_df,nlevels)==1)] <- NULL #Final check of factors with 1 level

  #log price
  glm_formula <- p ~ .

  # Run the regression
  all_coefs <-  coef(lm(glm_formula,
                        weights = q,
                        data = model_df))

  # There are coefficients returned for each time period, and each product.
  # we are only interested in change of price wrt time - so only keep theses
  # coefficients. Theses rownames start with timefact
  rows_keep <- grepl(".*period.*", names(all_coefs))

  tpd_coef <-  all_coefs[rows_keep]

  return(exp(tpd_coef))

}

fisher_t <- function(p0,p1,q0,q1){
  las <- fixed_t(p0,p1,q0)
  pas <- fixed_t(p0,p1,q1)
  return(sqrt((las*pas)))
}

fixed_t <- function(p0,p1,q){
  return(sum(p1*q)/sum(p0*q))
}

tornqvist_t <- function(p0,p1,q0,q1){
  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)
  s0 <- (p0*q0)/exp0
  s1 <- (p1*q1)/exp1
  return(prod((p1/p0)^(0.5*(s0+s1))))
}

# IndexNumR: a package for index number computation
# Copyright (C) 2018 Graham J. White (g.white@unswalumni.com)
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.

#' GEKS_w
#'
#' Function to compute a GEKS index over a window.
#' The window size is assumed to be the number of periods available in pervar.
#' This is not exposed to the user because GEKSIndex calls this
#' @keywords internal
GEKS_w <- function(x,pvar,qvar,pervar,indexMethod="tornqvist",prodID,
                   unique_prodID = NULL,
                   sample="matched"){



  # get the window length
  window <- max(x[[pervar]]) - min(x[[pervar]]) + 1

  # initialise some matrices
  # matrix of all price indices using each month as the base period
  pindices <- matrix(0, nrow = window, ncol = window)

  # get the vector of period indices that are inside the window
  pi <- unique(x[[pervar]])

  # initialise a vector for storing NA pair information
  naPairs <- character()

  # for every period in the window...
  for(j in 1:window){

    # for every period in the window...
    for(k in 1:window){
      # if j=k then the index is 1
      if(j==k){
        pindices[j,k] <- 1
      }
      # if we're below the diagonal, then use symmetry to
      # save computation time
      else if(j>k){
        pindices[j,k] <- 1/pindices[k,j]
      }
      else {
        # set the period pi(j) = base period
        xt0 <- x[x[[pervar]] == pi[j],]
        # set the period pi(k) = period '1'
        xt1 <- x[x[[pervar]] == pi[k],]

        if(indexMethod=="impute-tornqvist"){
          sample <- "not matched"

          #Checks that df when impute-tornqvist
          #Not when not

        }

        # if user asked for matching, get matched samples
        if(sample=="matched"){
          xt1 <- xt1[xt1[[prodID]] %in% unique(xt0[[prodID]]),]
          xt0 <- xt0[xt0[[prodID]] %in% unique(xt1[[prodID]]),]
        }

        # set the price index element to NA if there are no
        # matches
        if(nrow(xt1)==0){
          pindices[j,k] <- NA
          naPairs <- paste0(naPairs, paste0("(",j,",",k,")"), collapse = ",")
        }
        else{
          # set the price and quantity vectors
          p0 <- xt0[[pvar]]
          p1 <- xt1[[pvar]]
          q0 <- xt0[[qvar]]
          q1 <- xt1[[qvar]]

          if(indexMethod=="impute-tornqvist"){

            f0 <- prodID[x[[pervar]] == pi[j],]
            f1 <- prodID[x[[pervar]] == pi[k],]
            id0 <- xt0[[unique_prodID]]
            id1 <- xt1[[unique_prodID]]

          }
          # calculate the price index for 'base' period j and 'next' period k
          # browser()
          switch(tolower(indexMethod),
                 fisher = {pindices[j,k] <- fisher_t(p0,p1,q0,q1)},
                 tornqvist = {pindices[j,k] <- tornqvist_t(p0,p1,q0,q1)},
                 'impute-tornqvist' = {pindices[j,k] <- ITRYGEKS_t(p0,p1,q0,q1,f0,f1,id0,id1)})
          # cat(j,k,pindices[j,k],'\n')
        }

      }
    }
  }
  # compute the geometric mean of each column of the price indices matrix
  pgeo <- apply(pindices, 2, geomean, na.rm = TRUE)

  # normalise to the first period
  pgeo <- pgeo/pgeo[1]

  return(list(pgeo=pgeo, naPairs=naPairs))
}

#' Compute a GEKS multilateral index
#'
#' A function to calculate a GEKS multilateral price index
#'
#' @param x A dataframe containing price, quantity, a time period identifier
#' and a product identifier. It must have column names.
#' @param pvar A character string for the name of the price variable
#' @param qvar A character string for the name of the quantity variable
#' @param prodID A character string for the name of the product identifier, or vector of names of features when calculating
#' 'impute-tornqvist'
#' @param unique_prodID A character string for the name of the unqiue product identifier
#' @param pervar A character string for the name of the time variable. This variable
#' must contain integers starting at period 1 and increasing in increments of 1 period.
#' There may be observations on multiple products for each time period.
#' @param indexMethod A character string to select the index number method. Valid index
#' number methods are fisher or tornqvist. The default is tornqvist.
#' @param sample A character string specifying whether matching is to be performed.
#' The default is to use matching.
#' If sample=matched then any products that are not present in comparison periods
#' are removed prior to estimating the index for those periods.
#' @param window An integer specifying the length of the GEKS window.
#' @param splice A character string specifying the splicing method. Valid methods are
#' window, movement or mean. The default is mean.
#' @details The splicing methods are used to update the price index when new data become
#' available without changing prior index values. The window and movement splicing methods
#' first calculate an 'update factor' by calculating the ratio of the final index value
#' in the new GEKS window to some base period and then multiply the relevant old GEKS
#' index value by the update factor. Alternatives are "window","movement","half", and "mean".
#' See the package vignette for more information.
#' @examples
#' # compute a GEKS mutlilateral index with mean splicing
#' GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "tornqvist", window=11, splice = "mean")
#'
#' # compute a GEKS multilateral index with window splicing and the Fisher index method
#' GEKSIndex(CES_sigma_2, pvar = "prices", qvar = "quantities", pervar = "time",
#' prodID = "prodID", indexMethod = "fisher", window=11, splice = "mean")
#'
#' @references Ivancic, L., W.E. Diewert and K.J. Fox (2011), "Scanner Data,
#' Time Aggregation and the Construction of Price Indexes", Journal of
#' Econometrics 161, 24-35.
#' @export
GEKSIndex <- function(x,pvar,qvar,pervar,
                      indexMethod="tornqvist",
                      prodID,
                      unique_prodID = NULL,
                      sample="matched",window=13,splice="mean"){

  if(is.null(unique_prodID)&indexMethod=="impute-tornqvist"){
    stop("You must provide prodID as a vector of column names of features
         and the unique_prodID when calculating impute-tornqvist")
  }else if(indexMethod!="impute-tornqvist"){
    unique_prodID <- prodID
  }

  if(indexMethod=="impute-tornqvist"&any(!prodID%in%colnames(x))){
    stop("Some of the colnames in provided prodID do not exist in x?")
  }


  # check that only valid index methods are chosen
  if(!(tolower(indexMethod) %in% c("fisher","tornqvist","impute-tornqvist"))){
    stop("Not a valid index number method.")
  }

  # check valid column names are given
  colNameCheck <- checkNames(x, c(pvar, qvar, pervar, unique_prodID))
  if(colNameCheck$result == FALSE){
    stop(colNameCheck$message)
  }

  # check that the time period variable is continuous
  timeCheck <- isContinuous(x[[pervar]])
  if(timeCheck$result == FALSE){
    stop(paste("The time period variable is not continuous.",
               "Missing periods:", timeCheck$missing))
  }

  # check that columns are the right class
  x <- checkTypes(x, pvar, qvar, pervar)

  # get the number of periods
  n <- max(x[[pervar]],na.rm = TRUE)
  if(n<window){
    stop("The window length exceeds the number of periods in the data")
  }

  # browser()
  # sort the dataset by time period and product ID
  x <- x[order(x[[pervar]], x[[unique_prodID]]),]

  # initialise some matrices
  # final price index
  pGEKS <- matrix(0, nrow = n, ncol = 1)

  # first estimate a GEKS index for the first (window) observations
  # subset the window of data to use
  xWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,]

  # call GEKS_w on first window
  if(indexMethod=="impute-tornqvist"){

    prodIDWindow <- x[x[[pervar]] >= 1 & x[[pervar]] <= window,prodID]
    prodIDWindow <- prodIDWindow%>%droplevels()

    tempGEK <- GEKS_w(xWindow,pvar,qvar,pervar,indexMethod,prodIDWindow,unique_prodID,
                      sample)
  }else{
    # call GEKS_w on first window
    tempGEK <- GEKS_w(xWindow,pvar,qvar,pervar,indexMethod,prodID,
                      sample)
  }

  pGEKS[1:window,1] <- tempGEK$pgeo

  # initiate a vector of warnings for NAs
  if(length(tempGEK$naPairs) > 0){
    naWarn <- paste0("1 to ",window,": ",tempGEK$naPairs,"\n")
  }else{
    naWarn <- character()
  }

  # if there were periods where there were no overlapping products then
  # print a warning
  if(length(naWarn) > 0){
    warning(paste0("The following windows contained bilateral comparisons where no overlapping products were found: \n",
                   "Window: Pairs \n",naWarn))
  }

  return(pGEKS)
}

