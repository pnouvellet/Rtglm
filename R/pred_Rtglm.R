#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
#' 
pred_Rtglm <- function(model, newdata, overlap = FALSE){
  pred <- mgcv::predict.gam(object = poi_glm, 
                            newdata = data_infer,
                            type = 'link', 
                            se.fit = TRUE)
  
  # storing values to df
  data_infer$Mean <- exp(pred$fit) / data_infer$Oi
  data_infer$low_Quantile <- exp((pred$fit) - 1.96*pred$se.fit) / data_infer$Oi
  data_infer$high_Quantile <- exp((pred$fit) + 1.96*pred$se.fit) / data_infer$Oi
  data_infer$Std <- pred$se.fit
  
  # readjust data.frame to return the center of time window
  if(overlap==TRUE){
    data_infer <- data_infer[seq(1+ceiling(t_window/2),
                                 nrow(data_infer), 
                                 by = t_window),]
  }
  
  return(data_infer)
}