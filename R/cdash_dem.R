
#' create necessary variable for the demographic summary tables and listing.
#'
#' @title race and ethnicity indicator, age.
#' @param dm  the dataset dm read from sas
#' @param ex  the dataset ex read from sas
#' @return a data frame with additional columns listed as follows
#' \item{race}{the race of the subject}
#' \item{ethnic}{has two levels, "NOT HISPANIC OR LATINO" and "HISPANIC OR LATINO".}
#' \item{EX_TRT_C}{the treatment groups}
#' \item{ptno}{convert CLIENTID to numerical values of subject number}
#' \item{age}{Age calculated from start date of treatment}
#' @export

find_race <- function(dm, ex){
  sort_col <- sort(names(dm))
  dm <- data.frame(dm[, sort_col])

  # combine the race categories, if some one has multiple race attribute
  race_category <- sort(c("WHTE", "BLCK", "AMERIND", "HAWAIIAN", "ASIAN", "OTHER"))
  race_name <- sort(c('White','Black or African American','American Indian/Alaska Native',
                 'Native Hawaiian/Pacific Islander','Asian','Other Race'))
  col_id <- which(names(dm) %in% race_category)
  race_matrix <- dm[, col_id]=="YES"
  race_sum <- apply(race_matrix, 1, sum)
  race <- c()
  for ( i in 1:nrow(race_matrix)){
      if (race_sum[i] == 1) {  # if this subject belongs to a single race
        race[i] <-  race_name[which(race_matrix[i, ])] }
      else if (race_sum[i] > 1) { # if the subject belongs to multi-race
        race[i] <- paste(race_name[which(race_matrix[i, ])], sep= ",", collapse = ", ")
      }
      else race[i] <- ""
    }
  dm$race <- race

  dm$ptno <- as.numeric(dm$CLIENTID)

  # # create ethnicity variable if not already exist
  if (!( "ETHNIC" %in% (names(dm) ))) {  # not exist
      id1 <- dm$HISPANIC == "" # obs have empty value of HISPANIC
      dm$ethnic[!id1] <- ifelse(trimws(dm$HISPANIC[!id1]) %in%
                                c("NOT HISPANIC OR LATINO", "NO"),
                    "NOT HISPANIC OR LATINO", "HISPANIC OR LATINO")
      dm$ethnic[id1] <- ""
  }

  # get the treatment information
  select_ex <- which(names(ex) %in% c("CLIENTID", "EX_TRT_C", "EX_STDAT"))
  dm <- left_join(dm, ex[, select_ex], by = "CLIENTID")

  # calculate age by EX_STDAT (start date of treatement)
  span <- time_length(interval(ymd(dm$BRTHDAT), ymd(dm$EX_STDAT)), "year")
  dm$age <- floor(span)


  return(dm)
}



#' extract BMI, weight and height from vs.sas7bat.
#'
#' @title read BMI Weight and Height info from Admission stage.
#' @param  vs  the vs sas data
#' @return a data frame containing BMI, WEIGHT and HEIGHT from admission stage
#'
#' @export

w_h_bmi <- function(vs){

  vs$VS_TEST[trimws(toupper(vs$VS_TEST)) == "BODY MASS INDEX"] <- "BMI"
  vs$ptno <- as.numeric(vs$CLIENTID)

  # select the variables of interest
  row_vs <- which(toupper(vs$PERIOD) == "SCREEN" &
                    toupper(vs$VS_TEST) %in%
                    c('BMI','WEIGHT','HEIGHT','ELBOW','FRAME'))


  #
  col_vs <- names(vs) %in%  c("ptno", "VS_REU_R","VS_RES_R", "VS_TEST", "VS_DAT")

  vs1 <- vs[row_vs, col_vs] %>% arrange(VS_RES_R)
  vs1$VS_RES_R <- as.numeric(vs1$VS_RES_R)

  vs2 <- dcast(vs1, ptno + VS_DAT ~ VS_TEST, value.var = "VS_RES_R")

  return(vs2)
}


# get the summary statistics
get_summary <- function(var, categorical = F, name  = "race"){

  if (categorical)  {
    n <- length(var)
    frequency <- table(var)
    percent <- frequency/n

    result <- data.frame(trait = rep(name, length(frequency)),
                         Type = names(frequency),
                         frequency = as.vector(frequency),
                         percent=as.vector(percent) )

  }
  else {
  result <- data.frame(trait =name,
                       n = length(var),
                       mean = mean(var),
                       sd = sd(var),
                       minimum = min(var),
                       median = median(var),
                       maximum =  max(var) )
  }
  return(result)
}



#' Summarize the demographic data
#'
#' @title demographic summary.
#' @param dm the dm data set
#' @param ex the ex data set
#' @param vs the vs data set
#' @param bytrt  whether to summarize them by treatment, default TRUE.
#' @return a list
#' \item{categorical}{summary for categorical variables, such as race and gender}
#' \item{continuous}{summary statistics for continuous variables, such as BMI and age}
#' @export


dem_summary <- function(dm, ex, vs, bytrt = F){
  vsdm <-find_race(dm, ex) %>% inner_join(w_h_bmi(vs) , by = "ptno")
  treatment <- unique(vsdm$EX_TRT_C)
  result <- list()


  if (bytrt) {
    for (i in 1:length(treatment))
    {
      obs <- vsdm$EX_TRT_C==treatment[i]
      # for categorical
      race_sum <- get_summary(vsdm$race[obs], categorical = T, name = "Race")
      gender_sum <- get_summary(vsdm$SEX[obs], categorical = T, name = "Gender")

      cat_sum <- rbind(race_sum, gender_sum)
      # for continuous
      bmi <- get_summary(vsdm$BMI[obs], name = "BMI")
      height <- get_summary(vsdm$HEIGHT[obs], name = "Height")
      weight <- get_summary(vsdm$WEIGHT[obs], name = "Weight")
      span = time_length(interval(ymd(vsdm$BRTHDAT[obs]), ymd(vsdm$VS_DAT[obs])), "year")
      age <- get_summary(floor(span), name = "Age")  #floor function truncate age to integer
      continous_sum <- rbind(bmi, height, weight, age) %>%
        melt(id ="trait", value.name = "value", variable.name = "Type")%>% arrange(trait)

      res <- bind_rows(cat_sum, continous_sum)

      result[[treatment[i]]] <- res
    }

  }

  else {
    obs <- 1:nrow(vsdm)
    # for categorical
    race_sum <- get_summary(vsdm$race[obs], categorical = T, name = "Race")
    gender_sum <- get_summary(vsdm$SEX[obs], categorical = T, name = "Gender")
    cat_sum <- rbind(race_sum, gender_sum)
    # for continuous
    bmi <- get_summary(vsdm$BMI[obs], name = "BMI")
    height <- get_summary(vsdm$HEIGHT[obs], name = "Height")
    weight <- get_summary(vsdm$WEIGHT[obs], name = "Weight")
    age <- get_summary(vsdm$age[obs], name = "Age")
    continous_sum <- rbind(bmi, height, weight, age) %>%
        melt(id ="trait", value.name = "value", variable.name = "Type")%>% arrange(trait)


    # combine for output
   result <- bind_rows(cat_sum, continous_sum)
  }


    return(result)
}



##
#' Summarize the demographic data
#'
#' @title demographics listing.
#' @param dm the dm data set
#' @param ex the ex data set
#' @param vs the vs data set
#' @param bytrt  whether to summarize them by treatment, default TRUE.
#' @return a data frame
#' @export

dem_listing <- function(dm, ex, vs, bytrt = T){

  vsdm <-find_race(dm, ex) %>% inner_join(w_h_bmi(vs) , by = "ptno")
  treatment <- unique(vsdm$EX_TRT_C)

  # variables to be displayed
  var_select <- c("ptno", "BRTHDAT", "age", "SEX_D", "race", "ethnic", "HEIGHT", "WEIGHT", "BMI")
  # decide the order the variables to be displayed
  o1 <- c()
  for(i in 1:length(var_select)) {
    o1[i] <- which(names(vsdm)== var_select[i])
  }

  result <- list()

  if (bytrt){
    for ( k in 1:length(treatment)) {
      res <- data.frame(vsdm[vsdm$EX_TRT_C==treatment[k], o1])
      res <- res[order(res$ptno), ]
      result[[treatment[k]]] <- res
    }
  }
  else
  {
    result <- data.frame(vsdm[, o1])
  }


  return(result)
}




