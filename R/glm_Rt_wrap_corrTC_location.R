#' Wrapper to run the glm version of EpiEstim with correction
#'
#' Run the glm equivalent of EpiEstim, with correction for testing capacity and censoring for multiple locations
#'generally same as the glm_Rt_wrap function, but with some addition
#'
#'
#' @param I_incid a list of dataframe of non-negative integers with two columns, so that
#'              \code{I_incid$local} contains the incidence of cases due to local transmission
#'              and \code{I_incid$imported} contains the incidence of imported cases (with
#'              \code{I_incid$local + I_incid$imported} the total incidence).
#'              there is one dataframe per location.
#'
#' @param rho  a matrix containing
#'              the proportion of T/S over time, should have the same number of row as I_incdid, and column as number of locations
#'
#' @param gamma  to account for censoring, probability that a case (S or T or T+) is already in the database.
#'                As for rho, should be a matrix with the same number of row as I_incdid, and column as number of locations
#'              
#' @param location  a vector of location names, the length should be the same as number of columns in rho/gamma and the dataframes in the I_incid list
#'
#' @return A list including all results, including:
#' 
#' @returns config: a record of the configuration, i.e. a list including the parameters
#'              t_window and overlap
#' 
#' @returns model: the output of the glm model (equivalent to the output when running mgcv::gam);
#'              
#' @returns Rt: a dataframe, including the time step for estimation; the mean, lower and higher 
#'              quantile (95% CI), and standard deviation in the estimated Rt at each time 
#'              step (center of each time-window).
#'
#'
#' @export
glm_Rt_wrap_corrTC <- function(I_incid, rho, gamma, location, si_distr, t_window, overlap = FALSE){
  
  if (location==1){
    # reframe data and make overal infectivity
    data_infer <- prep_glm_corrTC(I_incid, si_distr, rho, gamma)
  }else{
    data_infer <- c()
    for (i in 1:location){
      temp <- prep_glm_corrTC(I_incid[[i]], si_distr, rho[,i], gamma[,i])
      temp$location <- paste0('location_',i)
      data_infer <- rbind(data_infer, temp)
    }
  }
  
  # prepare data to include time window information
  data_infer <- prep_glm_tWindow(data_infer, t_window, overlap)
  
  # run glm version of EPiEstim (assuming Poisson distribution as default)
  # coefficient in the below is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  m_glm <- mgcv::gam(incidence ~ 0 + tw*location + offset(log_Oi), 
                     data = data_infer, family = poisson(link = "log"))
  
  # run the prediction
  data_infer <- pred_Rtglm(model = m_glm, 
                           newdata = data_infer,
                           t_window = t_window,
                           overlap = overlap)
  
  # save the results of the run
  res <- list(config = list(t_window, overlap), 
              model = m_glm,
              Rt = data_infer[,c( "t","Mean","low_Quantile","high_Quantile",'Std')])
  
  return(res)
  
}