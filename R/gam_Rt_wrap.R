#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
#'
#' @export
gam_Rt_wrap <- function(I_incid, si_distr){
  
  # overlap <- FALSE
  # 
  # t_max <- nrow(I_incid)
  # # time window
  # t_start <- seq(2, t_max-(t_window-1),by = 1)
  # if (overlap == FALSE) {
  #   t_start <- t_start[seq(1,length(t_start),by = t_window)]
  # }
  # t_end <- t_start + (t_window - 1)    
  
  # reframe data
  data_infer <- data.frame(t = seq(1,nrow(I_incid)), 
                           incidence = rowSums(I_incid),
                           Oi = EpiEstim::overall_infectivity(incid = I_incid,
                                                              si_distr =  si_distr))
  # correct incidence when Oi is 0 to NA
  f <- which(data_infer$Oi ==0)
  if(length(f)>0){
    data_infer$Oi[f] <- NA
  }
  
  data_infer$log_Oi <- log(data_infer$Oi)
  
  # # make fix factor for time windows
  # tw <- c(NA, rep(1:length(t_start),each = t_window))
  # if(overlap==FALSE){
  #   data_infer$tw <- as.character(c(tw, rep(max(tw, na.rm=TRUE)+1,
  #                                           nrow(data_infer)-length(tw))))
  # }else{ # overlapping
  #   temp <- data_infer[1,]
  #   for(k in 1:length(t_start)){
  #     temp <- rbind(temp,
  #                   data_infer[t_start[k]:t_end[k],])
  #   }
  #   temp$tw <- as.character(tw)
  #   data_infer <- temp
  #   
  # }
  # f <- which(is.na(data_infer$Oi))
  # if(length(f)>0){
  #   data_infer$tw[f] <- NA
  # }
  
  # coefficient in the above is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  k_basis <- 5
  k_check <- 0
  while(k_check<1){
    k_basis <- k_basis*2
    poi_gam <- mgcv::gam(incidence ~ 0 + s(t, k=k_basis) + offset(log_Oi), 
                         data = data_infer, family = poisson(link = "log"))
    k_check <- mgcv::k.check(poi_gam)[3]
    
  }
  mgcv::k.check(poi_gam)
  summary(poi_gam)
  # summary(poi_glm)
  # summary(poi_glm2)
  pred <- mgcv::predict.gam(object = poi_gam, 
                            newdata = data_infer,
                            type = 'link', 
                            se.fit = TRUE)
  
  # storing values to df
  data_infer$Mean <- exp(pred$fit) / data_infer$Oi
  data_infer$low_Quantile <- exp((pred$fit) - 1.96*pred$se.fit) / data_infer$Oi
  data_infer$high_Quantile <- exp((pred$fit) + 1.96*pred$se.fit) / data_infer$Oi
  # if(overlap==TRUE){
  #   data_infer <- data_infer[seq(1+ceiling(t_window/2),
  #                                nrow(data_infer), 
  #                                by = t_window),]
  # }
  # plot(Rt$t, Rt$Rt)
  # lines(data_infer$t,data_infer$Mean)
  
  res <- list(Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile")])
  
  return(res)
  
}