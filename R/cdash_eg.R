################################################################################################
#                                               ECGs
#
################################################################################################


#' dcast the ecg variables
#' 
#' @title create variable columns for the ECGs parameters.
#' @param eg  the dataset dm read from sas
#' @return eg1 the transposed data 
#' @export
#' 

create_eg <- function(eg){
  
   # do the transpose
   eg_value <- eg %>% filter(trimws(EG_TEST) != "OVERALL INTERPRETATION") %>%
     select(CLIENTID, DAY, HOUR, EG_TEST, EG_ORRES, EG_DAT, EG_TIM, PERIOD) %>% # choose columns
     mutate(EG_ORRES = as.numeric(EG_ORRES)) %>%   # charactor to numerical
     dcast(CLIENTID + PERIOD +  DAY + HOUR + EG_DAT + EG_TIM ~ EG_TEST, value.var = "EG_ORRES") %>% # transpose
     arrange(CLIENTID, DAY, HOUR, EG_DAT, EG_TIM)  # sort them 
   
   # get the specifications
   eg_spec <- eg %>% filter(trimws(EG_TEST) ==  "OVERALL INTERPRETATION") %>%
          select(CLIENTID,  DAY, HOUR, EG_DAT, EG_TIM, EG_ORR_D, EG_SPEC) %>%
          arrange(CLIENTID, DAY, HOUR, EG_DAT, EG_TIM)
  
    eg1 <- inner_join(eg_value, eg_spec, 
              by = c("CLIENTID", "EG_DAT", "EG_TIM", "HOUR", "DAY") ) %>%
            arrange(CLIENTID, EG_DAT, EG_TIM, HOUR, DAY)
   
    return(eg1)
}





