#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
gam_Rt_sp_wrap <- function(I_incid, si_distr, x, y){
  
  data_infer <- prep_glm_sp(I_incid, si_distr, x, y)
  
  # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  m_gam <- mgcv::gam(incidence ~ 0 + s(x,y) + offset(log_Oi), 
                     data = data_infer, family = poisson(link = "log"))
  k_check <- mgcv::k.check(m_gam)[4]
  if(k_check<0.05){
    k_basis <- 5
    k_check <- 0
    while((k_check<0.05) + (k_basis<(nrow(data_infer)-1)) == 2){
      k_basis <- min(c(k_basis*2,nrow(data_infer)-1))
      # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
      m_gam <- mgcv::gam(incidence ~ 0 + s(x,y, k=k_basis) + offset(log_Oi), 
                         data = data_infer, family = poisson(link = "log"))
      k_check <- mgcv::k.check(m_gam)[4]
    }
  }
  
  mgcv::k.check(m_gam)
  summary(m_gam)
  plot(m_gam)
 
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