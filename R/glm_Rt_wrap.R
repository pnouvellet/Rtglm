#' Wrapper to run the glm version of EpiEstim
#'
#' Run the glm equivalent of EpiEstim.
#'
#' @param I_incid A dataframe of non-negative integers with two columns, so that
#'              \code{I_incid$local} contains the incidence of cases due to local transmission
#'              and \code{I_incid$imported} contains the incidence of imported cases (with
#'              \code{I_incid$local + I_incid$imported} the total incidence).
#'
#' @param si_distr  a vector containing
#'              the discrete serial interval distribution(s) used for estimation. 
#'              This is equivalent to the input in EpiEstim when using the "non_parametric_si"
#'              method.
#'
#' @param t_window  a single integer characterising the time-window to be used. During
#'              a time-window, the Rt is assumed to be constant. Time-windows may be overlapping 
#'              or not (see overlap parameter)
#'              
#' @param overlap  a logical, TRUE or FALSE, indicating whether time-window used for estimation
#'              should overlap. by default, overlap is set at FALSE.
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
glm_Rt_wrap <- function(I_incid, si_distr, t_window, overlap = FALSE){
  
  # reframe data and make overal infectivity
  data_infer <- prep_glm(I_incid, si_distr)
  
  # prepare data to include time window information
  data_infer <- prep_glm_tWindow(data_infer, t_window, overlap)
  
  # run glm version of EPiEstim (assuming Poisson distribution as default)
  # coefficient in the below is equivalent to logI = log(Rt)+log(OI) -> Rt = exp(coeff)
  m_glm <- mgcv::gam(incidence ~ 0 + tw + offset(log_Oi), 
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