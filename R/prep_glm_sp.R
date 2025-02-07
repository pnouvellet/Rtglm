#' Initial preparation of the input for a spatial Rt.glm
#'
#' This function construct a dataframe including the time (time-step used in the input), 
#' incidence (local + imported, see below), and the overall infectivity (calculated as
#' the sum of past incidence weighted by the serial interval distribution).
#'
#' @param I_incid A list of dataframes, with one dataframe per location.
#'              As for the input of 'prep_glm', each dataframe containsnon-negative 
#'              integers with two columns, so that
#'              \code{I_incid$local} contains the incidence of cases due to local transmission
#'              and \code{I_incid$imported} contains the incidence of imported cases (with
#'              \code{I_incid$local + I_incid$imported} the total incidence).
#'
#' @param si_distr  a vector containing
#'              the discrete serial interval distribution(s) used for estimation. 
#'              This is equivalent to the input in EpiEstim when using the "non_parametric_si"
#'              method.
#'              
#' @param x,y  two vectors, each of the same length as the I_incid list (number of locations) containing 
#'              the coordinates of each locations.
#'
#' @return A dataframe including the time (t), the location and its coordinates,
#'              incidence (incidence), the overall infectivity (Oi), and its log-transform (log_Oi).
#'
#' @export
prep_glm_sp <- function(I_incid, si_distr,x,y){
  
  data_infer<- c()
  for (i in 1:length(x)){
    # reframe data
    data <- data.frame(t = seq(1,nrow(I_incid[[i]])), 
                       incidence = rowSums(I_incid[[i]]),
                       Oi = EpiEstim::overall_infectivity(incid = rowSums(I_incid[[i]]),
                                                          si_distr =  si_distr),
                       x = x[i],
                       y = y[i],
                       loc = i)
    
    #remove initialisation
    f <- which(I_incid[[i]]$imported > 0)
    data <- data[-f,]
    
    data_infer <- rbind(data_infer,data)
  }
  
  
  # correct incidence when Oi is 0 to NA
  f <- which(data_infer$Oi ==0)
  if(length(f)>0){
    data_infer$Oi[f] <- NA
  }
  
  data_infer$log_Oi <- log(data_infer$Oi)
  
  return(data_infer)
}