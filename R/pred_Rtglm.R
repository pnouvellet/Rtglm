#' Output the predictions for Rt 
#'
#' This functions translate the results of the glm back into Rt estimations.
#'
#'
#' @param model: the output of the glm model (equivalent to the output when running mgcv::gam);   
#'
#' @param newdata A dataframe output of the Rt.glm function 'prep_glm_tWindow'. It includes the time (t), 
#'              incidence (incidence), the overall infectivity (Oi), its log-transform (log_Oi) and the time-windows (tw).
#'              
#' @param t_window  a single integer characterising the time-window to be used. During
#'              a time-window, the Rt is assumed to be constant. Time-windows may be overlapping 
#'              or not (see overlap parameter)
#'    
#' @param overlap  a logical, TRUE or FALSE, indicating whether time-window used for estimation
#'              should overlap. by default, overlap is set at FALSE.
#'   
#'   
#' @returns newdata: a dataframe, same as input but with added estimates: the mean, lower and higher 
#'              quantile (95% CI), and standard deviation in the estimated Rt at each time 
#'              step (center of each time-window when overlapping).
#'              
#'                                   
#' @export
#' 
pred_Rtglm <- function(model, newdata, t_window, overlap){
  
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