#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
EpiEstim_wrap <- function(I_incid, si_distr, t_ini, mean_prior = 5, std_prior = 5){
  
  t_max <- nrow(I_incid)

  config <- make_config(list(si_distr = si_distr,
                             t_start = t_ini+1,
                             t_end = t_ini + t_max,
                             mean_prior = mean_prior,
                             std_prior = std_prior))
  
  temp <- estimate_R(incid = I_incid, 
                     method="non_parametric_si",
                     config = config)
  
  temp <- temp$R[,c(1,2,3,4,5,8,11)]
  
  # add time 
  temp$t <- (temp$t_start+temp$t_end)/2
  
  names(temp) <- c("t_start","t_end","Mean","Std","low_Quantile",
                   "Median","high_Quantile","t")
  
  res <- list(config = config, 
              Rt = temp)
  
  return(res)
}