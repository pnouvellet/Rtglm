#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
#' 
prep_glm_tWindow <- function(data_infer, t_window, overlap){
  
  t_max <- nrow(data_infer)
  # time window
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
  }else{ # overlapping
    temp <- data_infer[1,]
    for(k in 1:length(t_start)){
      temp <- rbind(temp,
                    data_infer[t_start[k]:t_end[k],])
    }
    temp$tw <- as.character(tw)
    data_infer <- temp
  }
  
  f <- which(is.na(data_infer$Oi))
  if(length(f)>0){
    data_infer$tw[f] <- NA
  }
  
  return(data_infer)
  
}