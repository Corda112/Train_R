# ================================================================================
# AQI 時間序列預測模型訓練 - LightGBM 模組
# ================================================================================

# 載入必要套件
if(!requireNamespace("lightgbm", quietly = TRUE)) {
  stop("請安裝 lightgbm 套件: install.packages('lightgbm')")
}

if(!requireNamespace("Matrix", quietly = TRUE)) {
  stop("請安裝 Matrix 套件: install.packages('Matrix')")
}

cat("🌳 載入 LightGBM 模型模組...\n")

# ================================================================================
# 1. 資料預處理函數
# ================================================================================

#' 將3D陣列展平為2D矩陣 (適用於LightGBM)
#' @param x_array 3D陣列 [n_samples, seq_len, n_features]
#' @param features 特徵名稱向量
#' @param create_lag_names 是否創建滯後特徵名稱
#' @return 展平後的矩陣和特徵名稱
flatten_for_lgbm <- function(x_array, features, create_lag_names = TRUE) {
  if(!is.array(x_array) || length(dim(x_array)) != 3) {
    stop("x_array 必須是3維陣列")
  }
  
  n_samples <- dim(x_array)[1]
  seq_len <- dim(x_array)[2]
  n_features <- dim(x_array)[3]
  
  # 展平陣列: [n_samples, seq_len * n_features]
  x_flat <- array(x_array, dim = c(n_samples, seq_len * n_features))
  
  # 創建特徵名稱
  if(create_lag_names) {
    flat_feature_names <- c()
    for(i in 1:n_features) {
      for(j in 1:seq_len) {
        lag_hours <- seq_len - j  # 滯後小時數 (0表示當前時刻)
        feature_name <- paste0(features[i], "_lag", lag_hours)
        flat_feature_names <- c(flat_feature_names, feature_name)
      }
    }
  } else {
    flat_feature_names <- paste0("feature_", 1:(seq_len * n_features))
  }
  
  # 轉換為矩陣
  x_matrix <- as.matrix(x_flat)
  colnames(x_matrix) <- flat_feature_names
  
  return(list(
    matrix = x_matrix,
    feature_names = flat_feature_names,
    original_shape = dim(x_array)
  ))
}

#' 創建LightGBM資料集
#' @param x_matrix 特徵矩陣
#' @param y_vector 目標向量
#' @param feature_names 特徵名稱
#' @param categorical_features 類別特徵索引
#' @return LightGBM資料集物件
create_lgb_dataset <- function(x_matrix, y_vector, feature_names = NULL, categorical_features = NULL) {
  if(nrow(x_matrix) != length(y_vector)) {
    stop("特徵矩陣行數與目標向量長度不一致")
  }
  
  # 處理NA值
  if(any(is.na(y_vector))) {
    warning("目標向量包含NA值，將被移除")
    valid_idx <- !is.na(y_vector)
    x_matrix <- x_matrix[valid_idx, , drop = FALSE]
    y_vector <- y_vector[valid_idx]
  }
  
  # 檢查是否需要轉換為稀疏矩陣
  sparsity <- sum(x_matrix == 0, na.rm = TRUE) / length(x_matrix)
  if(sparsity > 0.5 && nrow(x_matrix) > 1000) {
    cat("  檢測到稀疏矩陣 (稀疏度:", round(sparsity * 100, 1), "%)，轉換為稀疏格式\n")
    x_matrix <- Matrix::Matrix(x_matrix, sparse = TRUE)
  }
  
  # 創建LightGBM資料集
  lgb_data <- lightgbm::lgb.Dataset(
    data = x_matrix,
    label = y_vector,
    feature_name = feature_names,
    categorical_feature = categorical_features
  )
  
  return(lgb_data)
}

# ================================================================================
# 2. 模型訓練函數
# ================================================================================

#' 訓練LightGBM模型
#' @param train_dataset 訓練資料集 (aqi_dataset物件)
#' @param val_dataset 驗證資料集 (aqi_dataset物件，可選)
#' @param params LightGBM參數列表
#' @param verbose 是否顯示詳細資訊
#' @return 訓練好的模型物件
train_lgbm <- function(train_dataset, val_dataset = NULL, params = LGBM_PARAMS, verbose = TRUE) {
  if(!inherits(train_dataset, "aqi_dataset")) {
    stop("train_dataset 必須是 aqi_dataset 物件")
  }
  
  if(verbose) {
    cat("🌳 開始訓練 LightGBM 模型...\n")
    cat("  訓練樣本數:", format(train_dataset$n_windows, big.mark = ","), "\n")
    cat("  特徵數量:", train_dataset$n_features, "\n")
    cat("  序列長度:", train_dataset$seq_len, "\n")
  }
  
  start_time <- Sys.time()
  
  # 展平訓練資料
  train_flat <- flatten_for_lgbm(train_dataset$x, train_dataset$features)
  
  if(verbose) {
    cat("  展平後特徵數:", length(train_flat$feature_names), "\n")
  }
  
  # 創建LightGBM訓練資料集
  lgb_train <- create_lgb_dataset(
    x_matrix = train_flat$matrix,
    y_vector = train_dataset$y,
    feature_names = train_flat$feature_names
  )
  
  # 處理驗證資料集
  lgb_val <- NULL
  valids <- list(train = lgb_train)
  
  if(!is.null(val_dataset)) {
    if(!inherits(val_dataset, "aqi_dataset")) {
      stop("val_dataset 必須是 aqi_dataset 物件")
    }
    
    if(verbose) {
      cat("  驗證樣本數:", format(val_dataset$n_windows, big.mark = ","), "\n")
    }
    
    # 展平驗證資料
    val_flat <- flatten_for_lgbm(val_dataset$x, val_dataset$features)
    
    # 檢查特徵一致性
    if(!identical(train_flat$feature_names, val_flat$feature_names)) {
      stop("訓練集和驗證集的特徵不一致")
    }
    
    lgb_val <- create_lgb_dataset(
      x_matrix = val_flat$matrix,
      y_vector = val_dataset$y,
      feature_names = val_flat$feature_names
    )
    
    valids$val <- lgb_val
  }
  
  # 準備訓練參數
  train_params <- params
  
  # 設定評估指標
  if(is.null(train_params$metric)) {
    train_params$metric <- "rmse"
  }
  
  # 設定early stopping
  callbacks <- list()
  if(!is.null(train_params$early_stopping_rounds) && !is.null(lgb_val)) {
    callbacks <- list(lightgbm::lgb.early_stop(train_params$early_stopping_rounds))
  }
  
  # 訓練模型
  if(verbose) {
    cat("  開始訓練...\n")
  }
  
  tryCatch({
    model <- lightgbm::lgb.train(
      params = train_params,
      data = lgb_train,
      valids = valids,
      callbacks = callbacks,
      verbose = ifelse(verbose, 1, -1)
    )
    
    end_time <- Sys.time()
    training_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    if(verbose) {
      cat("✅ 模型訓練完成\n")
      cat("  訓練時間:", round(training_time, 2), "分鐘\n")
      cat("  最佳迭代:", model$best_iter, "\n")
      
      # 顯示最終評估結果
      if(!is.null(model$record_evals)) {
        train_score <- tail(model$record_evals$train[[train_params$metric]]$eval, 1)
        cat("  訓練", toupper(train_params$metric), ":", round(train_score, 4), "\n")
        
        if("val" %in% names(model$record_evals)) {
          val_score <- tail(model$record_evals$val[[train_params$metric]]$eval, 1)
          cat("  驗證", toupper(train_params$metric), ":", round(val_score, 4), "\n")
        }
      }
    }
    
    # 創建模型包裝物件
    lgbm_model <- list(
      model = model,
      feature_names = train_flat$feature_names,
      original_features = train_dataset$features,
      n_features_original = train_dataset$n_features,
      seq_len = train_dataset$seq_len,
      data_type = train_dataset$data_type,
      params = train_params,
      training_time = training_time,
      best_iteration = model$best_iter,
      trained_at = end_time
    )
    
    class(lgbm_model) <- c("aqi_lgbm_model", "list")
    
    return(lgbm_model)
    
  }, error = function(e) {
    stop("LightGBM 訓練失敗: ", e$message)
  })
}

#' 打印LightGBM模型摘要
#' @param x aqi_lgbm_model 物件
print.aqi_lgbm_model <- function(x, ...) {
  cat("AQI LightGBM 模型\n")
  cat("=================\n")
  cat("資料類型:", x$data_type, "\n")
  cat("原始特徵數:", x$n_features_original, "\n")
  cat("展平特徵數:", length(x$feature_names), "\n")
  cat("序列長度:", x$seq_len, "\n")
  cat("最佳迭代:", x$best_iteration, "\n")
  cat("訓練時間:", round(x$training_time, 2), "分鐘\n")
  cat("訓練時間:", format(x$trained_at, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # 顯示主要參數
  cat("\n主要參數:\n")
  cat("  學習率:", x$params$learning_rate, "\n")
  cat("  樹葉數:", x$params$num_leaves, "\n")
  cat("  最大深度:", x$params$max_depth, "\n")
  cat("  目標函數:", x$params$objective, "\n")
}

# ================================================================================
# 3. 模型預測函數
# ================================================================================

#' 使用LightGBM模型進行預測
#' @param model aqi_lgbm_model 物件
#' @param test_dataset 測試資料集 (aqi_dataset物件)
#' @param num_iteration 使用的迭代次數 (NULL表示使用最佳迭代)
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lgbm <- function(model, test_dataset, num_iteration = NULL, verbose = TRUE) {
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("model 必須是 aqi_lgbm_model 物件")
  }
  
  if(!inherits(test_dataset, "aqi_dataset")) {
    stop("test_dataset 必須是 aqi_dataset 物件")
  }
  
  if(verbose) {
    cat("🔮 使用 LightGBM 模型進行預測...\n")
    cat("  測試樣本數:", format(test_dataset$n_windows, big.mark = ","), "\n")
  }
  
  # 檢查特徵一致性
  if(!identical(test_dataset$features, model$original_features)) {
    stop("測試資料的特徵與模型訓練時的特徵不一致")
  }
  
  # 展平測試資料
  test_flat <- flatten_for_lgbm(test_dataset$x, test_dataset$features)
  
  # 檢查展平後的特徵名稱一致性
  if(!identical(test_flat$feature_names, model$feature_names)) {
    stop("展平後的特徵名稱與模型不一致")
  }
  
  # 設定預測迭代次數
  if(is.null(num_iteration)) {
    num_iteration <- model$best_iteration
  }
  
  if(verbose) {
    cat("  使用迭代次數:", num_iteration, "\n")
  }
  
  # 執行預測
  tryCatch({
    predictions <- predict(
      model$model,
      test_flat$matrix,
      num_iteration = num_iteration
    )
    
    if(verbose) {
      cat("✅ 預測完成\n")
      cat("  預測範圍: [", round(min(predictions), 2), ", ", round(max(predictions), 2), "]\n")
      cat("  預測均值:", round(mean(predictions), 2), "\n")
    }
    
    return(predictions)
    
  }, error = function(e) {
    stop("LightGBM 預測失敗: ", e$message)
  })
}

#' 批次預測函數
#' @param model aqi_lgbm_model 物件
#' @param x_matrix 特徵矩陣
#' @param batch_size 批次大小
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lgbm_batch <- function(model, x_matrix, batch_size = 10000, verbose = TRUE) {
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("model 必須是 aqi_lgbm_model 物件")
  }
  
  n_samples <- nrow(x_matrix)
  
  if(n_samples <= batch_size) {
    # 小批次，直接預測
    return(predict(model$model, x_matrix, num_iteration = model$best_iteration))
  }
  
  if(verbose) {
    cat("🔮 執行批次預測...\n")
    cat("  總樣本數:", format(n_samples, big.mark = ","), "\n")
    cat("  批次大小:", format(batch_size, big.mark = ","), "\n")
  }
  
  # 分批預測
  predictions <- numeric(n_samples)
  n_batches <- ceiling(n_samples / batch_size)
  
  for(i in 1:n_batches) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, n_samples)
    
    batch_x <- x_matrix[start_idx:end_idx, , drop = FALSE]
    batch_pred <- predict(model$model, batch_x, num_iteration = model$best_iteration)
    
    predictions[start_idx:end_idx] <- batch_pred
    
    if(verbose && i %% 10 == 0) {
      cat("  完成批次:", i, "/", n_batches, "\n")
    }
  }
  
  if(verbose) {
    cat("✅ 批次預測完成\n")
  }
  
  return(predictions)
}

# ================================================================================
# 4. 特徵重要度分析
# ================================================================================

#' 獲取特徵重要度
#' @param model aqi_lgbm_model 物件
#' @param importance_type 重要度類型 ("split", "gain")
#' @param top_n 返回前N個重要特徵 (NULL表示全部)
#' @return 特徵重要度資料框
get_feature_importance <- function(model, importance_type = "gain", top_n = NULL) {
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("model 必須是 aqi_lgbm_model 物件")
  }
  
  # 獲取重要度
  importance <- lightgbm::lgb.importance(
    model = model$model,
    percentage = TRUE
  )
  
  # 篩選重要度類型
  if(importance_type == "gain") {
    importance <- importance[order(-importance$Gain), ]
    importance$Importance <- importance$Gain
  } else if(importance_type == "split") {
    importance <- importance[order(-importance$Frequency), ]
    importance$Importance <- importance$Frequency
  } else {
    stop("不支援的重要度類型: ", importance_type)
  }
  
  # 限制返回數量
  if(!is.null(top_n) && nrow(importance) > top_n) {
    importance <- importance[1:top_n, ]
  }
  
  # 添加原始特徵資訊
  importance$Original_Feature <- sapply(importance$Feature, function(feat_name) {
    # 提取原始特徵名稱 (移除_lagX後綴)
    gsub("_lag\\d+$", "", feat_name)
  })
  
  importance$Lag_Hours <- sapply(importance$Feature, function(feat_name) {
    # 提取滯後小時數
    lag_match <- regexpr("_lag(\\d+)$", feat_name)
    if(lag_match > 0) {
      lag_str <- regmatches(feat_name, lag_match)
      return(as.numeric(gsub("_lag", "", lag_str)))
    } else {
      return(0)
    }
  })
  
  return(importance)
}

#' 儲存特徵重要度
#' @param importance 特徵重要度資料框
#' @param file_path 儲存路徑
#' @param format 儲存格式 ("csv", "rds")
save_feature_importance <- function(importance, file_path, format = "csv") {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  if(format == "csv") {
    write.csv(importance, file_path, row.names = FALSE)
  } else if(format == "rds") {
    saveRDS(importance, file_path)
  } else {
    stop("不支援的格式: ", format)
  }
  
  cat("✅ 特徵重要度已儲存:", file_path, "\n")
}

#' 分析特徵重要度模式
#' @param importance 特徵重要度資料框
#' @return 分析結果列表
analyze_feature_importance <- function(importance) {
  # 按原始特徵聚合重要度
  feature_summary <- aggregate(
    Importance ~ Original_Feature,
    data = importance,
    FUN = sum
  )
  feature_summary <- feature_summary[order(-feature_summary$Importance), ]
  
  # 按滯後時間聚合重要度
  lag_summary <- aggregate(
    Importance ~ Lag_Hours,
    data = importance,
    FUN = sum
  )
  lag_summary <- lag_summary[order(lag_summary$Lag_Hours), ]
  
  # 找出最重要的時間窗口
  top_lags <- head(lag_summary[order(-lag_summary$Importance), ], 10)
  
  return(list(
    top_features = feature_summary,
    lag_importance = lag_summary,
    top_time_windows = top_lags,
    total_features = nrow(importance),
    top_10_coverage = sum(head(importance$Importance, 10))
  ))
}

# ================================================================================
# 5. 模型儲存與載入
# ================================================================================

#' 儲存LightGBM模型
#' @param model aqi_lgbm_model 物件
#' @param file_path 儲存路徑 (不含副檔名)
#' @param save_importance 是否同時儲存特徵重要度
save_lgbm_model <- function(model, file_path, save_importance = TRUE) {
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("model 必須是 aqi_lgbm_model 物件")
  }
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # 儲存完整模型物件
  model_path <- paste0(file_path, "_model.rds")
  saveRDS(model, model_path)
  
  # 儲存LightGBM原生模型
  lgb_path <- paste0(file_path, "_lgb.txt")
  lightgbm::lgb.save(model$model, lgb_path)
  
  # 儲存特徵重要度
  if(save_importance) {
    importance <- get_feature_importance(model, importance_type = "gain")
    importance_path <- paste0(file_path, "_importance.csv")
    save_feature_importance(importance, importance_path)
    
    # 儲存重要度分析
    analysis <- analyze_feature_importance(importance)
    analysis_path <- paste0(file_path, "_analysis.rds")
    saveRDS(analysis, analysis_path)
  }
  
  cat("✅ LightGBM 模型已儲存:\n")
  cat("  模型物件:", model_path, "\n")
  cat("  LightGBM檔:", lgb_path, "\n")
  if(save_importance) {
    cat("  特徵重要度:", importance_path, "\n")
    cat("  重要度分析:", analysis_path, "\n")
  }
}

#' 載入LightGBM模型
#' @param file_path 模型路徑 (不含副檔名)
#' @return aqi_lgbm_model 物件
load_lgbm_model <- function(file_path) {
  model_path <- paste0(file_path, "_model.rds")
  
  if(!file.exists(model_path)) {
    stop("模型檔案不存在: ", model_path)
  }
  
  model <- readRDS(model_path)
  
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("載入的物件不是 aqi_lgbm_model 類型")
  }
  
  cat("✅ LightGBM 模型載入完成:", model_path, "\n")
  return(model)
}

# ================================================================================
# 6. 模型診斷函數
# ================================================================================

#' 診斷LightGBM模型
#' @param model aqi_lgbm_model 物件
#' @param test_dataset 測試資料集
#' @return 診斷結果列表
diagnose_lgbm_model <- function(model, test_dataset = NULL) {
  if(!inherits(model, "aqi_lgbm_model")) {
    stop("model 必須是 aqi_lgbm_model 物件")
  }
  
  diagnosis <- list()
  
  # 基本資訊
  diagnosis$basic_info <- list(
    data_type = model$data_type,
    n_features = length(model$feature_names),
    seq_len = model$seq_len,
    best_iteration = model$best_iteration,
    training_time = model$training_time
  )
  
  # 訓練歷史
  if(!is.null(model$model$record_evals)) {
    diagnosis$training_history <- model$model$record_evals
  }
  
  # 特徵重要度
  diagnosis$feature_importance <- get_feature_importance(model, top_n = 20)
  diagnosis$importance_analysis <- analyze_feature_importance(diagnosis$feature_importance)
  
  # 如果提供測試資料，進行預測診斷
  if(!is.null(test_dataset)) {
    predictions <- predict_lgbm(model, test_dataset, verbose = FALSE)
    evaluation <- evaluate_predictions(test_dataset$y, predictions)
    diagnosis$test_performance <- evaluation
  }
  
  class(diagnosis) <- c("aqi_lgbm_diagnosis", "list")
  return(diagnosis)
}

#' 打印LightGBM診斷結果
#' @param x aqi_lgbm_diagnosis 物件
print.aqi_lgbm_diagnosis <- function(x, ...) {
  cat("AQI LightGBM 模型診斷\n")
  cat("=====================\n")
  
  # 基本資訊
  cat("📊 基本資訊:\n")
  cat("  資料類型:", x$basic_info$data_type, "\n")
  cat("  特徵數量:", x$basic_info$n_features, "\n")
  cat("  序列長度:", x$basic_info$seq_len, "\n")
  cat("  最佳迭代:", x$basic_info$best_iteration, "\n")
  cat("  訓練時間:", round(x$basic_info$training_time, 2), "分鐘\n\n")
  
  # 特徵重要度
  if(!is.null(x$importance_analysis)) {
    cat("🔍 特徵重要度分析:\n")
    cat("  前10特徵覆蓋率:", round(x$importance_analysis$top_10_coverage, 1), "%\n")
    
    cat("  最重要特徵:\n")
    top_features <- head(x$importance_analysis$top_features, 5)
    for(i in 1:nrow(top_features)) {
      cat("    ", i, ".", top_features$Original_Feature[i], 
          " (", round(top_features$Importance[i], 1), "%)\n")
    }
    
    cat("  最重要時間窗口:\n")
    top_lags <- head(x$importance_analysis$top_time_windows, 3)
    for(i in 1:nrow(top_lags)) {
      cat("    ", top_lags$Lag_Hours[i], "小時前", 
          " (", round(top_lags$Importance[i], 1), "%)\n")
    }
  }
  
  # 測試性能
  if(!is.null(x$test_performance)) {
    cat("\n🎯 測試集性能:\n")
    cat("  RMSE:", round(x$test_performance$rmse, 4), "\n")
    cat("  MAE:", round(x$test_performance$mae, 4), "\n")
    cat("  R²:", round(x$test_performance$r2, 4), "\n")
  }
}

cat("✅ LightGBM 模型模組載入完成\n") 