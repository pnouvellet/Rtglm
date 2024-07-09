#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
prep_glm <- function(I_incid, si_distr){
  # reframe data
  data_infer <- data.frame(t = seq(1,nrow(I_incid)), 
                           incidence = rowSums(I_incid),
                           Oi = EpiEstim::overall_infectivity(incid = I_incid,
                                                              si_distr =  si_distr))
  # correct incidence when Oi is 0 to NA
  f <- which(data_infer$Oi ==0)
  if(length(f)>0){
    data_infer$Oi[f] <- NA
  }
  
  data_infer$log_Oi <- log(data_infer$Oi)
  
  return(data_infer)
}