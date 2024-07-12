#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
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
    f <- which(I_incid[[i]]$local == 0)
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