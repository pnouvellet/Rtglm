#' Kernel for simulated Rt
#'
#' This function, used in the proof of concept paper, allow the user to obtain spatially smoothed
#' simulated Rts. As described in the paper methods the Rts are simulated from multiple bivariate normal distributions
#' that are scaled to a given min and max.
#'
#' @param map a dataframe (usually will be a GIS object) with one row per location and
#'              containing centroid coordinates of each location. Centroid coordinates should be named
#'              map$cent.x and map$cent.y.
#'
#' @param mu a list which size correspond to the number of simuated normal kernels. Each element
#'              of the list is a 2D vector containing the coordinates (x,y) of 
#'              the central position of each normal kernel .
#'
#' @param sigma a vector of the same size as mu containing the variances
#'              for each of the simulated normal kernels.
#'              
#' @param Rt_min_max a dataframe containing Rt_min_max$min and Rt_min_max$min, as the minimum and maximum
#'              simulated Rts.
#'   
#' @return A vector R of simulated Rts with one Rt per location.
#'   
#'              
#' @export
#' 
kernel_rt <- function(map, mu, sigma, Rt_min_max){
  
  # mu list of same size as sigma!
  n_peak <- length(sigma)
  
  # spatial component of  force of infection
  mu_p <- mu[[1]]
  d <- dmvnorm(x = cbind(map$cent.x,map$cent.y), mean = mu_p, sigma = sigma[1]*diag(2))
  for (i in 2:n_peak){
    mu_p <- mu[[i]]
    d <- d + dmvnorm(x = cbind(map$cent.x,map$cent.y), mean = mu_p, sigma = sigma[i]*diag(2))
  }
  d <- d/sum(d) # normalise it
  
  # scale
  Rt_range <- Rt_min_max$max - Rt_min_max$min
  R <- d/max(d)*Rt_range + Rt_min_max$min
  
  return(R)
}
