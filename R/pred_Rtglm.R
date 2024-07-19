#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
#' 
pred_Rtglm <- function(model, newdata, t_window, overlap = FALSE){
  
  pred <- mgcv::predict.gam(object = model, 
                            newdata = newdata,
                            type = 'link', 
                            se.fit = TRUE)
  
  # storing values to df
  newdata$Mean <- exp(pred$fit) / newdata$Oi
  newdata$low_Quantile <- exp((pred$fit) - 1.96*pred$se.fit) / newdata$Oi
  newdata$high_Quantile <- exp((pred$fit) + 1.96*pred$se.fit) / newdata$Oi
  newdata$Std <- pred$se.fit
  
  # readjust data.frame to return the center of time window
  if(overlap==TRUE){
    newdata <- newdata[seq(1+ceiling(t_window/2),
                                 nrow(newdata), 
                                 by = t_window),]
  }
  
  return(newdata)
}