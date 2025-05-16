#' Wrapper to run the gam version of EpiEstim with temporal smooth
#'
#' Run the gam equivalent of EpiEstim.with temporal smoothing.
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
#' @param dist  distribution assumed for gam, currently support 'poisson' (default) or 'nb' 
#'              for negative binomial (see mgcv::nb(link = "log")).
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
gam_Rt_wrap <- function(I_incid, si_distr, dist = 'poisson'){
  
  data_infer <- prep_glm(I_incid, si_distr)
  
  # run the gam, ensuring p_value of the k.check is above 0.05 (see mgcv::k.check) 
  # or the dimension of the basis used to represent the smooth term (k) is lower
  # than the number of data point available
  k_basis <- 5
  k_check <- 0
  # while((k_check<1) + (k_basis<(nrow(data_infer)-1)) == 2){
  while((k_check<0.05) + (k_basis<(sum(!is.na(data_infer$Oi))-1)) == 2){
    k_basis <- min(c(k_basis*2,sum(!is.na(data_infer$Oi))-1))
    if(dist == 'nb'){
      m_gam <- mgcv::gam(incidence ~ 0 + s(t, k=k_basis) + offset(log_Oi), 
                         data = data_infer, family = mgcv::tw(link = "log"))
    }else{
      m_gam <- mgcv::gam(incidence ~ 0 + s(t, k=k_basis) + offset(log_Oi), 
                           data = data_infer, family = poisson(link = "log"))
    }
    # k_check <- mgcv::k.check(m_gam)[3]
    k_check <- mgcv::k.check(m_gam)[4]
    
  }
  mgcv::k.check(m_gam)
  summary(m_gam)
 
  data_infer <- pred_Rtglm(model = m_gam, 
                           newdata = data_infer)
  
  res <- list(model = m_gam,
              Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile",'Std')])
  
  return(res)
  
}