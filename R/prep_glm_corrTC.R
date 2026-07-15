#' Initial preparation of the input for Rt.glm
#'
#' This function construct a dataframe including the time (time-step used in the input), 
#' incidence (local + imported, see below), and the overall infectivity (calculated as
#' the sum of past incidence weighted by the serial interval distribution).
#'
#' @param I_incid A dataframe of non-negative integers with two columns, so that
#'              \code{I_incid$local} contains the incidence of cases due to local transmission
#'              and \code{I_incid$imported} contains the incidence of imported cases (with
#'              \code{I_incid$local + I_incid$imported} the total incidence).
#'
#' @param si_distr  a vector containing
#'              the discrete serial interval distribution(s) used for estimation. 
#'              This is equivalent to the input in EpiEstim when using the "non_parametric_si"
#'              method.
#'
#' @return A dataframe including the time (t), 
#'              incidence (incidence), the overall infectivity (Oi), and its log-transform (log_Oi).
#'
#' @export
#' 
#' 
prep_glm_corrTC <- function(I_incid, si_distr, rho, gamma){
  
  # reframe data and get overall infectivity
  # data_infer <- data.frame(t = seq(1,nrow(I_incid)), 
  #                          incidence = rowSums(I_incid),
  #                          Oi = EpiEstim::overall_infectivity(incid = I_incid,
  #                                                             si_distr =  si_distr))
  data_infer <- data.frame(t = seq(1,nrow(I_incid)), 
                           incidence = rowSums(I_incid),
                           Oi = NA)
  # get overall infectivity corrected
  T <- nrow(data_infer)
  for (t in seq(2, T)) {
    data_infer$Oi[t] <- sum( si_distr[seq_len(t)] * data_infer$incidence[seq(t, 1)] *
                               1/gamma[seq(t, 1)] * 1/rho[seq(t, 1)] , na.rm = TRUE)
  }
  data_infer$Oi <- rho * data_infer$Oi * temp$gamma
  
  # correct incidence when Oi is 0 to NA
  f <- which(data_infer$Oi ==0)
  if(length(f)>0){
    data_infer$Oi[f] <- NA
  }
  
  # save the log-transformed infectivity
  data_infer$log_Oi <- log(data_infer$Oi)
  
  return(data_infer)
}