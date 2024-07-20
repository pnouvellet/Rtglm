#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
gam_Rt_wrap <- function(I_incid, si_distr, dist = 'poisson'){
  
  data_infer <- prep_glm(I_incid, si_distr)
  
  # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  k_basis <- 5
  k_check <- 0
  # while((k_check<1) + (k_basis<(nrow(data_infer)-1)) == 2){
  while((k_check<0.05) + (k_basis<(sum(!is.na(data_infer$Oi))-1)) == 2){
    k_basis <- min(c(k_basis*2,sum(!is.na(data_infer$Oi))-1))
    if(dist == 'nb'){
      m_gam <- mgcv::gam(incidence ~ 0 + s(t, k=k_basis) + offset(log_Oi), 
                         data = data_infer, family = mgcv::nb(link = "log"))
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
                           newdata = data_infer,
                           t_window = t_window)
  
  res <- list(model = m_gam,
              Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile",'Std')])
  
  return(res)
  
}