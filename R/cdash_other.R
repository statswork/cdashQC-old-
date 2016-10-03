

##########################################################################################
#                                   USEFUL FUNCTIONS
#
##########################################################################################
# read as many data sets as needed.
#
# @title race and ethnicity indicator, age.
# @param data_vector  the name list of the data sets to be read
# @param data_dir  where the data sets are stored
# @return datasets  the data sets have been loaded
# @export a list, each corresponding to a specific data set
#
#
# read_all_data <- function(data_vector, data_dir){
#   data_name <- c("ae", "bd", "cm", "cr", "cv", "dm", "ds", "eg", "ex", "hlt", "ie", "lb_cq",
#                  "lbd", "mh", "ml", "pe", "su", "ti", "ur", "vs", "xd")
#   data <- list()
#   ids <- which(data_name %in% data_vector)
#   for(i in 1:length(ids)){
#     per_data <- read_sas(paste(data_dir, data_name[ids[i]], ".sas7bdat", sep = ""))
#     data[[i]] <-  per_data
#   }
#   names(data) <- data_vector
#   return(data)
# }


# create a cumalative count for each variable
# so I can label "QTCF" as fig1QTCF, ... fig4QTCF, and fig1PR, fig2PR
cumcount <- function(vec_c)
{
  vec_c <- as.character(vec_c)
  seqs <- rep(0, length(vec_c))
  items <- unique(vec_c)
  tab <- table(vec_c)
  for (k in 1:length(items))
  {
    c1 <- as.numeric(tab[k])
    seqs[vec_c == names(tab)[k]] <- 1:c1
  }
  return(seqs)
}



#' label variables by threshold
#'
#' @title label the variables of interest.
#' @param data  the dataset to be flagged.
#' @param threshold  a data frame returned by \code{create_threshold()}.
#' @param included  the dataset included from sas
#' @param oor     set to be True when flagging if a < c1 or a > c2; set to be FALSE when flagging if c1 < a < c2.
#' @return aet the data
#' @export
#'
label_var <- function(data, thresh, prefix = "flg", oor= F){

  n_var <- nrow(thresh)

  seqs <- cumcount(thresh[, 1])
  for (i in 1:n_var)
  {
    col_num <- which(trimws(toupper(names(data)))== toupper(thresh[i, 1]))  # column to be flagged


    # flg only extreme values?
    if (oor){  # flag if a <  c1 or a > c2

      message("flagging all variables for out of range")

      # for lower bound
      if (thresh[i, 4]){
        ind_low <- data[, col_num] <= thresh[i, 2]
      }
      else { ind_low <- data[, col_num] < thresh[i, 2] }

      # for upper bound
      if (thresh[i, 5]){
        ind_upper <- data[, col_num] >= thresh[i, 3]
      }
      else { ind_upper <- data[, col_num] >= thresh[i, 3] }


      row_num <- c(which(ind_low), which(ind_upper))
      ncols <- ncol(data) + 1
      data[, ncols] <- ""
      data[row_num, ncols] <- as.character(thresh[i, 6])
    }


    else {   # flag when c1 < a < c2
      message("flagging all variables for in beween values")

      # for lower bound
      if (thresh[i, 4]){
        ind_low <- data[, col_num] >= thresh[i, 2]
      }
      else { ind_low <- data[, col_num] > thresh[i, 2] }

      # for upper bound
      if (thresh[i, 5]){
        ind_upper <- data[, col_num] <= thresh[i, 3]
      }
      else { ind_upper <- data[, col_num] < thresh[i, 3] }

      row_num <- intersect(which(ind_low), which(ind_upper))
      ncols <- ncol(data) + 1
      data[, ncols] <- ""
      data[row_num, ncols] <- as.character(thresh[i, 6])
    }

    names(data)[ncols] <- paste(prefix, seqs[i], tolower(thresh[i, 1]), sep = "")

  }
  return(data)
}





#' remove empty rows and empty columns of a data set
#'
#' @title remove empty rows and empty columns of a data set.
#' @param data  the dataset
#' @param return_truncated_data Controls the output.
#' @return a data frame with empty rows and empty columns removed,  or a list containing the following elements.
#' \item{keep_rows}{a vector of \code{TRUE} (for keeping row) or \code{FALSE} (otherwise)}
#' \item{keep_rows}{a vector of \code{TRUE} (for keeping column) or \code{FALSE} (otherwise)}
#' @export

keep_non_empty <- function(data, pattern = NA, return_truncated_data = F){

  col_total <- ncol(data)
  row_total <- nrow(data)
  if(is.na(pattern)){             # if want to remove NA
    if (col_total == 1) {         # if there is only one column
      keep_rows <- !is.na(data)
    }
    else {
      keep_rows <- rowSums(is.na(data)) < col_total
    }
    if(row_total == 1){
      keep_cols <- !is.na(as.vector(data))
    }
    else {
      keep_cols <- colSums(is.na(data)) < row_total
    }
  }

  else {     # if want to remove pattern , for example "" blank space
    data_new <- apply(data, 2, trimws)
    if (col_total == 1) {     # if there is only one column
      keep_rows <- data_new == pattern
    }
    else {
      keep_rows <- rowSums(data_new == pattern) < col_total
    }
    if(row_total == 1){
      keep_cols <- data_new == pattern
    }
    else {
      keep_cols <- colSums(data_new == pattern) < row_total
    }
  }


  if(return_truncated_data){
    data_result <- data.frame(data[keep_rows, keep_cols])
    names(data_result) <- names(data)[keep_cols]
    return(data_result)
  }
  else{
    return(list(keep_rows = keep_rows, keep_cols = keep_cols))
  }
}




#' create threshold data frame
#'
#' @title create threshold table.
#' @param flgvar  the variable to be flagged
#' @param lower   lower bound. Set to be \code{-Inf} if none.
#' @param upper  upper bound. Set to be \code{Inf} if none.
#' @param include_lower whether the lower bound should be included, TRUE by default
#' @param include_upper whether the upper bound should be included, FALSE by default
#' @param add2existing  \code{TRUE} or \code{FALSE}. If \code{TRUE}, must specify thresh.
#' @param thresh  a data frame with three columns: first column, the variable to be flagged, second column, lower bound (if any); third column, upper bound (if any).
#' @return a data frame
#' @export
#' @examples
#' a <- create_threshold(flagvar = "QTCF", lower = 0, upper = 20,, include_lower = F, include_upper = T,  flg_label = "^")
#' b <- create_threshold(flagvar = "PR",  upper = 430, flg_label = "*", add2existing = T, thresh = a)
#' c <- create_threshold(flagvar = "QRs",  lower = 100, flg_label = "%",add2existing = T, thresh = b)
#' print(a); print(b);print(c);


create_threshold <- function(flagvar, lower = -Inf, upper = Inf,
                             include_lower = T, include_upper = F,
                             flg_label = "#", add2existing= F, thresh = NULL){
  if (!add2existing)
  {
    thresh1 <- data.frame(variable = flagvar, lower = lower, upper = upper,
                          include_lower = include_lower,
                          include_upper = include_upper,
                          flg_label=flg_label)
  }
  else {
    nx <- nrow(thresh)
    newthresh <- data.frame(variable = flagvar, lower = lower, upper = upper,
                            include_lower = include_lower,
                            include_upper = include_upper,
                            flg_label=flg_label)
    names(newthresh) <- names(thresh)
    thresh1 <- rbind(thresh, newthresh)
  }
  return(thresh1)
}


