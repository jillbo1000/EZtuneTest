% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ez_benchmark.R
\name{ez_benchmark}
\alias{ez_benchmark}
\title{Tunes a model via tidymodels and returns}
\usage{
ez_benchmark(
  x,
  y,
  name = "Data",
  method = "svm",
  optimizer = "hjn",
  fast = TRUE,
  cross = NULL,
  loss = "default"
)
}
\arguments{
\item{x}{Matrix or data frame containing the dependent variables.}

\item{y}{Vector of responses. Can either be a factor or a numeric vector.}

\item{name}{Name of the dataset. Used to name the output file and
as an identifier within the output dataset.}

\item{method}{Model to be fit. Choices are "ada" for adaboost, "en" for
elastic net, "gbm" for gradient boosting machines, and "svm" for support
vector machines. This argument is passed to \code{eztune}. See
\code{\link{EZtune::eztune}} for more information.}

\item{optimizer}{Optimization method to be used. May be either "hjn" or
"ga". This argument is passed to \code{eztune}. See
\code{\link{EZtune::eztune}} for more information.}

\item{fast}{Value for \code{fast} argument in \code{eztune}. Options are
TRUE, FALSE, a positive integer, or a number between 0 and 1. See
\code{\link{EZtune::eztune}} for more information.}

\item{cross}{Value for \code{cross} argument in \code{eztune}.Options are
NULL or a positive integer to represent the number of folds. See
\code{\link{EZtune::eztune}} for more information.}

\item{loss}{Value for \code{loss} argument in \code{eztune}. Options are
"default", "class", "auc", "mse", or "mae". See
\code{\link{EZtune::eztune}} for more information.}
}
\value{

}
\description{
\code{ez_benchmark} tunes a model with eztune and returns a one row
\code{data.frame} with the results. \pkg{rsample} is used to split
the dataset into training and test datasets. The results returned
}
\seealso{
\code{\link{tm_benchmark}}, \code{\link{EZtune::eztune}}
}
