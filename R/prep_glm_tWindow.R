#' Finish the preparation for Rt.glm
#'
#' Integrate information of the incidence, serial interval with configuration for the time-window settings.
#'
#' @param data_infer A dataframe output of the Rt.glm function 'prep_glm'. It includes the time (t), 
#'              incidence (incidence), the overall infectivity (Oi), and its log-transform (log_Oi).
#'              
#' @param t_window  a single integer characterising the time-window to be used. During
#'              a time-window, the Rt is assumed to be constant. Time-windows may be overlapping 
#'              or not (see overlap parameter)
#'              
#' @param overlap  a logical, TRUE or FALSE, indicating whether time-window used for estimation
#'              should overlap. by default, overlap is set at FALSE.
#'              
#'              
#' @return A dataframe similar to the data_infer input but with added columns for the time-window 'tw'. 
#'              If overlap = TRUE, row of incidence will be duplicated accordingly. E.g. assuming an 
#'              overlapping time-window of 3 days, days 2 and 3 would overlap between time window 1
#'              (including incidence on days 1,2 and 3) and time window 2 (including incidence on 
#'              days 2,3 and 4).
#'              
#' @export
#' 
prep_glm_tWindow <- function(data_infer, t_window, overlap){
  
  # define maximum time
  t_max <- nrow(data_infer)
  # define start and end of time windows
  t_start <- seq(2, t_max-(t_window-1),by = 1)
  if (overlap == FALSE) {
    t_start <- t_start[seq(1,length(t_start),by = t_window)]
  }
  t_end <- t_start + (t_window - 1)  
  
  # make fix factor for time windows
  tw <- c(NA, rep(1:length(t_start),each = t_window))
  if(overlap==FALSE){
    data_infer$tw <- as.character(c(tw, rep(max(tw, na.rm=TRUE)+1,
                                            nrow(data_infer)-length(tw))))
  }else{ # when overlapping
    temp <- data_infer[1,]
    for(k in 1:length(t_start)){
      temp <- rbind(temp,
                    data_infer[t_start[k]:t_end[k],])
    }
    temp$tw <- as.character(tw)
    data_infer <- temp
  }
  
  # correct tw when no information on overall infectivity
  f <- which(is.na(data_infer$Oi))
  if(length(f)>0){
    data_infer$tw[f] <- NA
  }
  
  return(data_infer)
  
}