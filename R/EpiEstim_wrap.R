#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
EpiEstim_wrap <- function(I_incid, si_distr, t_window, overlap, mean_prior, std_prior){
  
  t_max <- nrow(I_incid)
  # time window
  t_start <- seq(2, t_max-(t_window-1),by = 1)
  if (overlap == FALSE) {
    t_start <- t_start[seq(1,length(t_start),by = t_window)]
  }
  t_end <- t_start + (t_window - 1)    
  
  config <- make_config(list(si_distr = data$si_distr,
                             t_start = t_start,
                             t_end = t_end,
                             mean_prior = 1,
                             std_prior = 1))
  
  temp <- estimate_R(incid = I_incid, 
                     method="non_parametric_si",
                     config = config)
  
  temp <- temp$R[,c(1,2,3,4,5,8,11)]
  
  # add time and make continuous when non-overlapping
  if(overlap == TRUE){
    temp$t <- (temp$t_start+temp$t_end)/2
  }else{
    a = temp[rep(1,t_window),]
    for(k in 2:nrow(temp)){
      a = rbind(a,temp[rep(k,t_window),])
    }
    a$t <- seq(a$t_start[1],tail(a$t_end,1))
    temp <- a
  }
  names(temp) <- c("t_start","t_end","Mean","Std","low_Quantile",
                   "Median","high_Quantile","t")
  res <- list(config = config, Rt = temp)
  return(res)
}