#' Wrapper to run the gam version of EpiEstim with spatial smooth
#'
#' Run the gam equivalent of EpiEstim with spatial smoothing. currently the function is setup for 
#' a single time window and a poisson distribution.
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
#' @param x,y  two vectors, each of the same length as the I_incid list (number of locations) containing 
#'              the coordinates of each locations.
#'              
#' @return A list including all results, including:
#' 
#' @returns model: the output of the gam model (equivalent to the output when running mgcv::gam);
#'              
#' @returns Rt: a dataframe, including the time step for estimation; the mean, lower and higher 
#'              quantile (95% CI), and standard deviation in the estimated Rt at each time 
#'              step (center of each time-window).
#'              
#'              
#' @export
gam_Rt_sp_wrap <- function(I_incid, si_distr, x, y){
  
  data_infer <- prep_glm_sp(I_incid, si_distr, x, y)
  
  # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  m_gam <- mgcv::gam(incidence ~  s(x,y) + offset(log_Oi), 
                     data = data_infer, family = poisson(link = "log"))
  k_check <- mgcv::k.check(m_gam)[4]
  # if(k_check<0.05){
  #   k_basis <- 5
  #   k_check <- 0
  #   while((k_check<0.05) + (k_basis<(nrow(data_infer)-1)) == 2){
  #     k_basis <- min(c(k_basis*2,nrow(data_infer)-1))
  #     # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  #     m_gam <- mgcv::gam(incidence ~ 0 + s(x,y, k=k_basis) + offset(log_Oi), 
  #                        data = data_infer, family = poisson(link = "log"))
  #     k_check <- mgcv::k.check(m_gam)[4]
  #   }
  # }
  # 
  # mgcv::k.check(m_gam)
  # summary(m_gam)
  # plot(m_gam)
 
  data_infer <- pred_Rtglm(model = m_gam, 
                           newdata = data_infer)
  #remove duplicate
  for(i in 1:length(unique(data_infer$loc))){
    f <- which(data_infer$loc %in% unique(data_infer$loc)[i])
    if(length(f)>1){
      data_infer <- data_infer[-f[-1],]
    }
  }
  
  res <- list(model = m_gam,
              Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile",'Std')])
  
  return(res)
  
}