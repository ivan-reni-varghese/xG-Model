test_that('test-prepare_data', {
  # lisbon
  type <- guess_type(lisbon, 'Price')
  suppressWarnings(preprocessed_data <- preprocessing(lisbon, 'Price'))
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Price',
                       type = type,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'Price',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y = 'Price',
                 engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)

  expect_true(length(train_data) == 5)
  expect_true(is.data.frame(train_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(train_data$decision_tree))
  expect_true(class(train_data$lightgbm_data)[1] == 'lgb.Dataset')
  expect_true(class(train_data$catboost_data) == 'catboost.Pool')

  expect_true(length(test_data) == 5)
  expect_true(is.data.frame(test_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(test_data$decision_tree))
  expect_true('matrix' %in% class(test_data$lightgbm_data)[1])
  expect_true(class(test_data$catboost_data) == 'catboost.Pool')

  expect_identical(levels(train_data$ranger$Condition), levels(test_data$ranger$Condition))
  expect_identical(levels(train_data$ranger$PropertyType), levels(test_data$ranger$PropertyType))
  expect_true('other' %in% levels(train_data$ranger$Condition))
  expect_true('other' %in% levels(train_data$ranger$PropertyType))
  expect_true('other' %in% train_data$ranger$Condition)
  expect_true('other' %in% train_data$ranger$PropertyType)
  expect_true(dim(train_data$xgboost_data)[2] == dim(test_data$xgboost_data)[2])
  expect_identical(colnames(train_data$xgboost_data), colnames(test_data$xgboost_data))


  # iris
  type <- guess_type(iris,'Species')
  preprocessed_data <- preprocessing(iris, 'Species')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'Species',
                       type = type,
                       balance = FALSE)
  set.seed(123)
  train_data <-
    prepare_data(split_data$train,
                 y = 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y = 'Species',
                 engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)

  expect_true(length(train_data) == 5)
  expect_true(is.data.frame(train_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(train_data$decision_tree))
  expect_true(class(train_data$lightgbm_data)[1] == 'lgb.Dataset')
  expect_true(class(train_data$catboost_data) == 'catboost.Pool')

  expect_true(length(test_data) == 5)
  expect_true(is.data.frame(test_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(test_data$decision_tree))
  expect_true('matrix' %in% class(test_data$lightgbm_data)[1])
  expect_true(class(test_data$catboost_data) == 'catboost.Pool')

  expect_identical(levels(train_data$ranger$Species), levels(test_data$ranger$Species))
  expect_identical(levels(train_data$ranger$Species), c('setosa', 'versicolor', 'virginica'))
  expect_false('other' %in% levels(train_data$ranger$Petal.Width))
  expect_identical(colnames(train_data$xgboost_data), colnames(test_data$xgboost_data))

  # lymph
  type <- guess_type(lymph, 'class')
  preprocessed_data <- preprocessing(lymph, 'class')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'class',
                       type = type,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'class',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y = 'class',
                 engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)

  expect_true(length(train_data) == 5)
  expect_true(is.data.frame(train_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(train_data$decision_tree))
  expect_true(class(train_data$lightgbm_data)[1] == 'lgb.Dataset')
  expect_true(class(train_data$catboost_data) == 'catboost.Pool')

  expect_true(length(test_data) == 5)
  expect_true(is.data.frame(test_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(test_data$decision_tree))
  expect_true('matrix' %in% class(test_data$lightgbm_data)[1])
  expect_true(class(test_data$catboost_data) == 'catboost.Pool')

  expect_false('other' %in% levels(train_data$ranger$lym_nodes_dimin))
  expect_identical(colnames(train_data$xgboost_data), colnames(test_data$xgboost_data))

  # test

  type <- guess_type(testing_data, 'y')
  preprocessed_data <- preprocessing(testing_data, 'y')
  preprocessed_data <- preprocessed_data$data
  split_data <-
    train_test_balance(preprocessed_data,
                       y = 'y',
                       type = type,
                       balance = FALSE)
  set.seed(123)
  suppressWarnings(
    train_data <-
      prepare_data(split_data$train,
                   y = 'y',
                   engine = c('ranger', 'xgboost', 'decision_tree', 'lightgbm', 'catboost'))
  )
  set.seed(123)
  test_data <-
    prepare_data(split_data$test,
                 y = 'y',
                 engine = c('ranger', 'xgboost', 'decision_tree','lightgbm', 'catboost'),
                 predict = TRUE,
                 train = split_data$train)

  expect_true(length(train_data) == 5)
  expect_true(is.data.frame(train_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(train_data$decision_tree))
  expect_true(class(train_data$lightgbm_data)[1] == 'lgb.Dataset')
  expect_true(class(train_data$catboost_data) == 'catboost.Pool')

  expect_true(length(test_data) == 5)
  expect_true(is.data.frame(test_data$ranger))
  expect_true('matrix' %in% class(train_data$xgboost))
  expect_true(is.data.frame(test_data$decision_tree))
  expect_true('matrix' %in% class(test_data$lightgbm_data)[1])
  expect_true(class(test_data$catboost_data) == 'catboost.Pool')

  expect_identical(levels(train_data$ranger$Condition), levels(test_data$ranger$Condition))
  expect_identical(levels(train_data$ranger$PropertyType), levels(test_data$ranger$PropertyType))
  expect_true('other' %in% levels(train_data$ranger$X11))
  expect_true('other' %in% train_data$ranger$X11)
  expect_true(dim(train_data$xgboost_data)[2] == dim(test_data$xgboost_data)[2])
  expect_identical(colnames(train_data$xgboost_data), colnames(test_data$xgboost_data))
})
