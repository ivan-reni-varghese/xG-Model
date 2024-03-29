#' Function \code{make_lightgbm} automates the process of applying LightGBM model
#' for a dataset and simutaneously creates explainer from the use of DALEX package.
#' The created explainer can be further processed by functions for explanations.
#'
#'
#' @param data data.frame, matrix, data.table or dgCMatrix - data will be used to run the XGBoost model. NOTE: data has to contain the target column.
#' @param target character: name of the target column, should be character and has to be a column name in data.
#' @param type character: defining the task. Two options are: "regression" and "classification", particularly, binary classification.
#' @param fill_na logical, default is FALSE. If TRUE, missing values in target column are removed, missing values in categorical columns are replaced by mode and
#' missing values in numeric columns are substituted by median of corresponding columns.
#' @param num_features numeric, default is NULL. Parameter indicates number of most important features, which are chosen from the train dataset. Automatically, those important
#' features will be kept in the train and test datasets.
#' @param tune logical. If TRUE, function will perform the hyperparameter tuning steps for each model inside.
#' @param tune_metric character, name of metric used for evaluating best model. For regression, options are: "mse", "rmse", "mad" and "r2".
#' For classification, options are: "auc", "recall", "precision", "f1" and "accuracy".
#' @param tune_iter number (default: 20) - total number of times the optimization step is to repeated. This argument is used when tune = TRUE.
#' @param label string indicating the name of the model. Might be usefull while comparing different models of the same type.
#'
#'
#' @return An object of the class \code{explainer} for LightGBM model with given data, target and defined type of problem.
#'
#'
#' @references Explanatory Model Analysis. Explore, Explain and Examine Predictive Models. \url{https://ema.drwhy.ai/}
#' @export
#' @importFrom stats predict
#' @importFrom utils head tail installed.packages methods
#'
#' @examples


make_lightgbm <- function(data, target, type, num_features = NULL, fill_na = TRUE, tune = FALSE, tune_metric = NULL, tune_iter=20, label = "LightGBM"){

  message("--- Creating LightGBM model ---")
  # Preparing data
  prepared_data <- prepare_data(data, target, type, fill_na = fill_na,
                                num_features = num_features)

  data <- prepared_data$data
  modifications <- prepared_data$modifications

  ### Data processing level 2/2
  # Data frame with transformed label_column:
  data_info <- data[, !(colnames(data) %in% target), drop = FALSE]

  # Using lgb.convert_with_rules. The function transforms the data into a fittable data
  data_info_rules <- lightgbm::lgb.convert_with_rules(data = data_info)
  data_encoded <- data_info_rules$data

  # Transform from dataframe to matrix:
  data_encoded_matrix <- as.matrix(data_encoded)

  if (!tune){
    # Convert to object used in LightGBM model:
    names_cat_vars <- names(data_info_rules$rules)
    dtrain <-
      lightgbm::lgb.Dataset(
        data  = data_encoded_matrix,
        label = data[, target],
        categorical_feature = as.vector(which(colnames(data_encoded) %in% names_cat_vars))
      )

  ### Creating Model:
    model <- lightgbm::lightgbm(
      data = dtrain,
      verbose = -1,
      learning_rate = 0.4,
      nrounds = 10,
      objective = ifelse(type == "regression", "regression", "binary"))
  } else {
    # Checking the metric
    message('--- Starting tuning process')
    tune_metric <- check_metric(tune_metric, type)

    data_tune <- data

  # Spliting data_train and data_validation:
    df_splitted <- split_data(data_tune, target, type)
    data_train <- df_splitted[[1]]
    data_val <- df_splitted[[2]]

  # Saving data for explainer
    data <- data_train

  # Extract y_val. Data_val without target column:
    y_val    <- data_val[[target]]
    data_val <- data_val[ ,!(colnames(data_val) %in% target), drop = FALSE]

  # Update label_column from data_train. Data_train without target column:
    label_column <- data_train[[target]]
    data_train <- data_train[, !(colnames(data_train) %in% target), drop = FALSE]

  # Using lgb.convert_with_rules. The function transforms the data into a fittable data
    data_train_tuned_rules <- lightgbm::lgb.convert_with_rules(data = data_train)
    data_encoded_tuned <- data_train_tuned_rules$data

  # Transform from dataframe to matrix:
    data_encoded_tuned_matrix <- as.matrix(data_encoded_tuned)

  # Convert to data_train object used in LightGBM:
    names_cat_tuned_vars <- names(data_train_tuned_rules$rules)

  # Encode data_val:
    data_val <- lightgbm::lgb.convert_with_rules(data = data_val, rules = data_train_tuned_rules$rules)$data
    data_val <- as.matrix(data_val)

  ### Tuning part:
    # Argument for metrics which should be minimized
    desc <- ifelse(tune_metric %in% c("mse", "rmse", "mad"), -1, 1)

    lightgbm_tune_fun <- function(learning_rate, num_leaves, bagging_fraction,
                                  max_depth, max_bin, min_data_in_leaf,
                                  min_sum_hessian_in_leaf,
                                  subsample,nrounds){
      # Creating LightGBM object
      dtrain <- lightgbm::lgb.Dataset(
          data = data_encoded_tuned_matrix,
          label = label_column,
          categorical_feature = as.vector(which(
            colnames(data_encoded_tuned) %in% names_cat_tuned_vars))
        )

     lightgbm_tune <- lightgbm::lightgbm(data = dtrain,
                                         learning_rate = learning_rate,
                                         num_leaves = num_leaves,
                                         bagging_fraction = bagging_fraction,
                                         max_depth = max_depth,
                                         max_bin = max_bin,
                                         min_data_in_leaf = min_data_in_leaf,
                                         min_sum_hessian_in_leaf = min_sum_hessian_in_leaf,
                                         subsample = subsample,
                                         nrounds = nrounds,
                                         verbose = -1,
                                         objective = ifelse(type == "regression", "regression", "binary")
      )
      if (type == "regression"){
        predicted <- predict(lightgbm_tune, data_val)
      }
      # If user wants to return probability of classes: remove ifelse statement.
      if (type == "classification"){
        predicted <- ifelse(predict(lightgbm_tune, data_val) >= 0.5, 1, 0)
      }

      score <- desc * calculate_metric(tune_metric, predicted, y_val)
      list(Score = score, Pred = predicted)
    }


    # Tuning process:
    tuned_lightgbm <- rBayesianOptimization::BayesianOptimization(lightgbm_tune_fun,
                                          bounds = list(nrounds = c(1L, 5000L),
                                                        learning_rate = c(0.01, 1.0),
                                                        num_leaves = c(24L,80L),
                                                        bagging_fraction = c(0.8, 1),
                                                        max_depth = c(1L,15L),
                                                        max_bin = c(20L,90L),
                                                        min_data_in_leaf = c(7L,63L),
                                                        min_sum_hessian_in_leaf = c(0,100),
                                                        subsample = c(0.1,1.0)),
                                          init_grid_dt = NULL,
                                          init_points = 10,
                                          n_iter = tune_iter,
                                          acq = "ucb",
                                          kappa = 2.576,
                                          eps = 0.0,
                                          verbose = TRUE)

    best_params <- tuned_lightgbm$Best_Par

    # Creating last LihgtGBM dataset
    dtrain <- lightgbm::lgb.Dataset(
      data = data_encoded_tuned_matrix,
      label = label_column,
      categorical_feature = as.vector(which(
        colnames(data_encoded_tuned) %in% names_cat_tuned_vars))
    )

    model <- lightgbm::lightgbm(data = dtrain,
                                nrounds = best_params["nrounds"],
                                learning_rate = best_params["learning_rate"],
                                num_leaves = best_params["num_leaves"],
                                bagging_fraction = best_params["bagging_fraction"],
                                max_depth = best_params["max_depth"],
                                max_bin = best_params["max_bin"],
                                min_data_in_leaf = best_params["min_data_in_leaf"],
                                min_sum_hessian_in_leaf = best_params["min_sum_hessian_in_leaf"],
                                subsample = best_params["subsample"],
                                objective = ifelse(type == "regression", "regression", "binary"),
                                verbose = -1
                                )
  }

  ### Creating predict function:
  lightgbm_predict <- function(object, newdata, rec = modifications) {
    # Changing data type to data frame
    newdata <- check_condition(newdata)

    newdata <- as.data.frame(recipes::bake(rec, newdata))

    newdata_encoded <- lightgbm::lgb.convert_with_rules(data = newdata,
                                                        rules = data_info_rules$rules)$data
    data_encoded_matrix_newdata <- as.matrix(newdata_encoded)

    return(predict(object, data_encoded_matrix_newdata))
  }

  ### Explainer from DALEX
  # For simplicity, take processed matrix from original data frame for explanation purpose:
  explainer_automate_lightgbm <- DALEX::explain(
    model,
    data = data[, which(colnames(data) != target), drop = FALSE],
    y = data[, target],
    predict_function = lightgbm_predict,
    label = label,
    type = type,
    verbose = 0
  )

  ### S3 objects
  explainer_automate_lightgbm$modifications <- modifications
  class(explainer_automate_lightgbm) <- c("forester_model", "explainer")
  return(explainer_automate_lightgbm)
}


