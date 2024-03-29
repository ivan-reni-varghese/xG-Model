#' Fertility dataset
#'
#' \code{fertility} 100 volunteers provide a semen sample analyzed according to
#' the WHO 2010 criteria. Sperm concentration are related to socio-demographic
#' data, environmental factors, health status, and life habits.
#'
#'
#' @format A data frame with 100 rows and 10 variables:
#' \describe{
#'   \item{season}{numeric, Season in which the analysis was performed.
#'   1) winter, 2) spring, 3) Summer, 4) fall. (-1, -0.33, 0.33, 1)}
#'   \item{age}{numeric, Age at the time of analysis. 18-36 (0, 1)}
#'   \item{childish_diseases}{numeric, Childish diseases (ie , chicken pox,
#'   measles, mumps, polio) 1) yes, 2) no. (0, 1)}
#'   \item{accident_or_serious_trauma}{numeric,Accident or serious trauma 1) yes,
#'   2) no. (0, 1)}
#'   \item{surgerical_intervention}{numeric, Surgical intervention 1) yes, 2
#'   ) no. (0, 1)}
#'   \item{high_fevers_last_year}{numeric, High fevers in the last year 1)
#'   less than three months ago, 2) more than three months ago, 3) no. (-1, 0, 1)}
#'   \item{alcohol_consumption}{numeric,Frequency of alcohol consumption 1)
#'   several times a day, 2) every day, 3) several times a week, 4) once a week,
#'   5) hardly ever or never (0, 1)}
#'   \item{smoking}{numeric, Smoking habit 1) never, 2) occasional 3) daily.
#'   (-1, 0, 1)}
#'   \item{sitting_hours}{numeric, Number of hours spent sitting per day ene-16
#'   (0, 1)}
#'   \item{diagnosis}{factor, Diagnosis normal (1), altered (2)}
#' }
#' @name fertility
#' @docType data
#' @usage data(fertility)
#'
#' @source Data from UCI \url{https://archive.ics.uci.edu/ml/datasets/Fertility}
#'
NULL
