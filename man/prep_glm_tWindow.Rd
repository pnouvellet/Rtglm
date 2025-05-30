% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_glm_tWindow.R
\name{prep_glm_tWindow}
\alias{prep_glm_tWindow}
\title{Finish the preparation for Rt.glm}
\usage{
prep_glm_tWindow(data_infer, t_window, overlap)
}
\arguments{
\item{data_infer}{A dataframe output of the Rt.glm function 'prep_glm'. It includes the time (t), 
incidence (incidence), the overall infectivity (Oi), and its log-transform (log_Oi).}

\item{t_window}{a single integer characterising the time-window to be used. During
a time-window, the Rt is assumed to be constant. Time-windows may be overlapping 
or not (see overlap parameter)}

\item{overlap}{a logical, TRUE or FALSE, indicating whether time-window used for estimation
should overlap. by default, overlap is set at FALSE.}
}
\value{
A dataframe similar to the data_infer input but with added columns for the time-window 'tw'. 
             If overlap = TRUE, row of incidence will be duplicated accordingly. E.g. assuming an 
             overlapping time-window of 3 days, days 2 and 3 would overlap between time window 1
             (including incidence on days 1,2 and 3) and time window 2 (including incidence on 
             days 2,3 and 4).
}
\description{
Integrate information of the incidence, serial interval with configuration for the time-window settings.
}
