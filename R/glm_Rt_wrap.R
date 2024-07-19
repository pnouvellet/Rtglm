#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
glm_Rt_wrap <- function(I_incid, si_distr, t_window, overlap){
  
  # reframe data
  data_infer <- prep_glm(I_incid, si_distr)
  
  # time window
  data_infer <- prep_glm_tWindow(data_infer, t_window, overlap)
  
  # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  # poi_glm <- glm(incidence ~ 0 + tw + offset(log_Oi), 
  #                data = data_infer, family = poisson(link = "log"))
  # 
  # pred <- predict.glm(object = poi_glm, 
  #                     newdata = data_infer,
  #                     type = 'link', 
  #                     se.fit = TRUE)
  poi_glm <- mgcv::gam(incidence ~ 0 + tw + offset(log_Oi), 
                        data = data_infer, family = poisson(link = "log"))

  data_infer <- pred_Rtglm(model = poi_glm, 
                           newdata = data_infer,
                           overlap = overlap)
  
  
  res <- list(config = list(t_window, overlap), 
              model = poi_glm,
              Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile",'Std')])

  return(res)
  
}