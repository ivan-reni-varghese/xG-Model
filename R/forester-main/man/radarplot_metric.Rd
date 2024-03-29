% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_metrics.R
\name{radarplot_metric}
\alias{radarplot_metric}
\title{Plot scatter chart of one metric}
\usage{
radarplot_metric(score_frame, metric_name)
}
\arguments{
\item{score_frame}{The result of `score_model` function.}

\item{metric_name}{String describing the metric name.}
}
\description{
Plot scatter chart of one metric
}
\examples{
data(iris)
iris_bin <- iris[1:100,]
iris_bin$Species <- factor(iris_bin$Species)
type <- guess_type(iris_bin, 'Species')
preprocessed_data <- preprocessing(iris_bin, 'Species')
preprocessed_data <- preprocessed_data$data
split_data <-
  train_test_balance(preprocessed_data,
                     'Species',
                     type = type,
                     balance = FALSE)
train_data <-
  prepare_data(split_data$train,
               'Species',
               engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
               'catboost'))
test_data <-
  prepare_data(split_data$test,
               'Species',
               engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
               'catboost'),
               predict = TRUE,
               train = split_data$train)

model <-
  train_models(train_data,
               'Species',
               engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
               'catboost'),
               type = type)
predictions <-
  predict_models(model,
                 test_data,
                 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm',
                 'catboost'),
                 type = type)
score <-
  score_models(model,
               predictions,
               observed = split_data$test$Species,
               type = type)
radarplot_metric(score, 'f1')
}
