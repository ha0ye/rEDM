% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/block_GP.R
\name{block_gp}
\alias{block_gp}
\title{Perform generalized forecasting using Gaussian processes}
\usage{
block_gp(block, lib = c(1, NROW(block)), pred = lib, tp = 1, phi = 0,
  v_e = 0, eta = 0, fit_params = TRUE, columns = NULL,
  target_column = 1, stats_only = TRUE, save_covariance_matrix = FALSE,
  first_column_time = FALSE, silent = FALSE, ...)
}
\arguments{
\item{block}{either a vector to be used as the time series, or a 
data.frame or matrix where each column is a time series}

\item{lib}{a 2-column matrix (or 2-element vector) where each row specifes the 
first and last *rows* of the time series to use for attractor reconstruction}

\item{pred}{(same format as lib), but specifying the sections of the time 
series to forecast.}

\item{tp}{the prediction horizon (how far ahead to forecast)}

\item{phi}{length-scale parameter. see 'Details'}

\item{v_e}{noise-variance parameter. see 'Details'}

\item{eta}{signal-variance parameter. see 'Details'}

\item{fit_params}{specify whether to use MLE to estimate params over the lib}

\item{columns}{either a vector with the columns to use (indices or names), 
or a list of such columns}

\item{target_column}{the index (or name) of the column to forecast}

\item{stats_only}{specify whether to output just the forecast statistics or 
the raw predictions for each run}

\item{save_covariance_matrix}{specifies whether to include the full 
covariance matrix with the output (and forces the full output as if 
stats_only were set to FALSE)}

\item{first_column_time}{indicates whether the first column of the given 
block is a time column (and therefore excluded when indexing)}

\item{silent}{prevents warning messages from being printed to the R console}

\item{...}{other parameters. see 'Details'}
}
\value{
If stats_only, then a data.frame with components for the parameters 
  and forecast statistics:
\tabular{ll}{
  embedding \tab embedding\cr
  tp \tab prediction horizon\cr
  phi \tab length-scale parameter\cr
  v_e \tab noise-variance parameter\cr
  eta \tab signal-variance parameter\cr
  fit_params \tab whether params were fitted or not\cr
  num_pred \tab number of predictions\cr
  rho \tab correlation coefficient between observations and predictions\cr
  mae \tab mean absolute error\cr
  rmse \tab root mean square error\cr
  perc \tab percent correct sign\cr
  p_val \tab p-value that rho is significantly greater than 0 using Fisher's 
  z-transformation\cr
}
If stats_only is FALSE or save_covariance_matrix is TRUE, then there is an 
additional list-column variable:
\tabular{ll}{
  model_output \tab data.frame with columns for the time index, observations, 
    and mean-value for predictions\cr
}
If save_covariance_matrix is TRUE, then there is an additional list-column variable:
\tabular{ll}{
  covariance_matrix \tab covariance matrix for predictions\cr
}
}
\description{
\code{block_gp} uses multiple time series given as input to generate an 
attractor reconstruction, and then applies Gaussian process regression to 
approximate the dynamics and make forecasts. This method is the 
generalized version of \code{tde_gp}, which constructs the block from time 
lags of a time series to pass into this function.
}
\details{
The default parameters are set so that passing a vector as the only 
argument will use that vector to predict itself one time step ahead. If a 
matrix or data.frame is given as the only argument, the first column will be 
predicted (one time step ahead), using the remaining columns as the 
embedding. Rownames will be converted to numeric if possible to be used as 
the time index, otherwise 1:NROW will be used instead. The default lib and 
pred are to perform maximum likelihood estimation of the phi, v_e, and eta 
parameters over the whole time series, and return just the forecast 
statistics.

If phi, v_e, and eta parameters are given, all combinations of their values 
will be tried. If fit_params is also set to TRUE, these values will be the 
initial values for subsequent optimization of likelihood.

[[DETAILS ABOUT IMPLEMENTATION]]
}
\examples{
data("two_species_model")
block <- two_species_model[1:200,]
block_gp(block, columns = c("x", "y"), first_column_time = TRUE)
}