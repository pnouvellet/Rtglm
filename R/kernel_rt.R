#' Hello World
#'
#' This is an example of how to create and document exported functions.
#'
#' @param input you should always document the paramters.
#'              Including the expected data type.
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
  map$R <- d/max(d)*Rt_range + Rt_min_max$min
  
  return(map)
}
