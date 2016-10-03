#' new_create_included.
#'
#' @title do what new_create_included does.
#' @param dm  the dataset dm read from sas
#' @param ex  the dataset ex read from sas
#' @return a data frame with additional columns listed as follows
#' \item{race}{the race of the subject}
#' \item{ethnic}{has two levels, "NOT HISPANIC OR LATINO" and "HISPANIC OR LATINO".}
#' \item{EX_TRT_C}{the treatment groups}
#' \item{ptno}{convert CLIENTID to numerical values of subject number}
#' \item{age}{Age calculated from start date of treatment}
#' @export
#'
#'

new_create_included <- function(ex){

  med <- ex %>% mutate(ptno = as.numeric(CLIENTID),
                       ex_trt = substr(EX_TRT_C, 4, 4),   # get the treatment lable
                       pern = as.numeric(gsub("A|B|C|D|E|F", "", ex$PERIOD)) # remove A or B or C, ect.
                        )

  medseq <- med %>% arrange(ptno, pern, EX_TRT_C) %>% distinct()

  # total number of period
  totper <- length(unique(medseq$pern))
  tottrt <- length(unique(medseq$EX_TRT_C))

  seqtest <- dcast(medseq, ptno ~ EX_TRT_C, value.var="pern")
}
