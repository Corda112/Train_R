# ================================================================================
# AQI 時間序列預測模型訓練 - LightGBM 模型模組 (優化版)
# ================================================================================

# 載入必要套件
if(!requireNamespace("lightgbm", quietly = TRUE)) {
  stop("請安裝 lightgbm 套件: install.packages('lightgbm')")
}

if(!requireNamespace("Matrix", quietly = TRUE)) {
  stop("請安裝 Matrix 套件: install.packages('Matrix')")
}

library(lightgbm)
library(Matrix)

cat("🌳 載入 LightGBM 模型模組 (優化版)...\n")

# ================================================================================
# 1. 資料預處理 - 展平與欄位名稱生成
# ================================================================================

#' 生成可回溯的特徵欄位名稱
#' @param features 原始特徵名稱向量
#' @param seq_len 序列長度 (預設72)
#' @param lag_format 滯後格式 ("lag" 或 "hour")
#' @return 展平後的欄位名稱向量
generate_flattened_feature_names <- function(features, seq_len = 72, lag_format = "lag") {
  flattened_names <- c()
  
  for(feat in features) {
    if(lag_format == "lag") {
      # 格式: feature_lag71, feature_lag70, ..., feature_lag0
      lag_names <- paste0(feat, "_lag", (seq_len-1):0)
    } else if(lag_format == "hour") {
      # 格式: feature_h71, feature_h70, ..., feature_h0  
      lag_names <- paste0(feat, "_h", (seq_len-1):0)
    } else {
      # 格式: feature_t71, feature_t70, ..., feature_t0
      lag_names <- paste0(feat, "_t", (seq_len-1):0)
    }
    
    flattened_names <- c(flattened_names, lag_names)
  }
  
  return(flattened_names)
}

#' 展平三維陣列為二維矩陣 (LightGBM專用)
#' @param x 三維陣列 [n_windows, seq_len, n_features]
#' @param features 特徵名稱向量
#' @param use_sparse 是否使用稀疏矩陣 (預設FALSE)
#' @param verbose 是否顯示詳細資訊
#' @return 包含矩陣和欄位名稱的列表
flatten_tensor_for_lgbm <- function(x, features, use_sparse = FALSE, verbose = TRUE) {
  if(!is.array(x) || length(dim(x)) != 3) {
    stop("x 必須是三維陣列 [n_windows, seq_len, n_features]")
  }
  
  n_windows <- dim(x)[1]
  seq_len <- dim(x)[2]
  n_features <- dim(x)[3]
  
  if(verbose) {
    cat("🔄 展平張量:", n_windows, "×", seq_len, "×", n_features, 
        "→", n_windows, "×", seq_len * n_features, "\n")
  }
  
  # 生成欄位名稱
  flattened_names <- generate_flattened_feature_names(features, seq_len, "lag")
  
  # 展平陣列 - 重新排列維度順序
  # 從 [n_windows, seq_len, n_features] 到 [n_windows, seq_len * n_features]
  flattened_matrix <- array(x, dim = c(n_windows, seq_len * n_features))
  
  # 轉換為適當的矩陣格式
  if(use_sparse) {
    if(verbose) cat("📦 轉換為稀疏矩陣...\n")
    flattened_matrix <- as(flattened_matrix, "dgCMatrix")
  } else {
    flattened_matrix <- as.matrix(flattened_matrix)
  }
  
  # 設定欄位名稱
  colnames(flattened_matrix) <- flattened_names
  
  if(verbose) {
    cat("✅ 展平完成\n")
    cat("  輸出維度:", paste(dim(flattened_matrix), collapse = " × "), "\n")
    cat("  矩陣類型:", class(flattened_matrix)[1], "\n")
    cat("  記憶體使用:", format(object.size(flattened_matrix), units = "MB"), "\n")
  }
  
  return(list(
    matrix = flattened_matrix,
    feature_names = flattened_names,
    original_features = features,
    seq_len = seq_len,
    n_features = n_features
  ))
}

# ================================================================================
# 2. LightGBM 訓練函數 (優化版)
# ================================================================================

#' 訓練 LightGBM 模型 (優化版)
#' @param train_dataset 訓練資料集
#' @param val_dataset 驗證資料集 (可選)
#' @param params LightGBM 參數列表
#' @param use_sparse 是否使用稀疏矩陣
#' @param save_checkpoint 是否保存檢查點
#' @param checkpoint_path 檢查點保存路徑
#' @param verbose 是否顯示詳細資訊
#' @return 訓練好的模型物件
train_lgbm <- function(train_dataset, val_dataset = NULL, params = LGBM_PARAMS,
                      use_sparse = FALSE, save_checkpoint = TRUE, 
                      checkpoint_path = NULL, verbose = TRUE) {
  
  if(verbose) {
    cat("🌳 開始訓練 LightGBM 模型...\n")
    cat("訓練樣本數:", train_dataset$n_windows, "\n")
    if(!is.null(val_dataset)) {
      cat("驗證樣本數:", val_dataset$n_windows, "\n")
    }
  }
  
  start_time <- Sys.time()
  
  # 設定隨機種子
  set.seed(RANDOM_SEEDS$lgbm)
  
  # 展平訓練資料
  train_flattened <- flatten_tensor_for_lgbm(
    train_dataset$x, 
    train_dataset$features, 
    use_sparse = use_sparse,
    verbose = verbose
  )
  
  # 創建 LightGBM 資料集
  if(verbose) cat("📊 創建 LightGBM 資料集...\n")
  
  lgb_train <- lgb.Dataset(
    data = train_flattened$matrix,
    label = train_dataset$y
  )
  
  # 處理驗證資料集
  lgb_val <- NULL
  val_flattened <- NULL
  if(!is.null(val_dataset)) {
    val_flattened <- flatten_tensor_for_lgbm(
      val_dataset$x,
      val_dataset$features,
      use_sparse = use_sparse,
      verbose = FALSE
    )
    
    lgb_val <- lgb.Dataset(
      data = val_flattened$matrix,
      label = val_dataset$y,
      reference = lgb_train
    )
  }
  
  # 準備訓練參數
  train_params <- params
  train_params$seed <- RANDOM_SEEDS$lgbm
  
  # 設定驗證資料
  valids <- list()
  if(!is.null(lgb_val)) {
    valids$val <- lgb_val
  }
  valids$train <- lgb_train
  
  # 訓練模型
  if(verbose) cat("🚀 開始模型訓練...\n")
  
  tryCatch({
    model <- lgb.train(
      params = train_params,
      data = lgb_train,
      valids = valids,
      verbose = if(verbose) 1 else -1,
      eval_freq = 50,
      early_stopping_rounds = train_params$early_stopping_rounds
    )
    
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    
    if(verbose) {
      cat("✅ LightGBM 訓練完成\n")
      cat("訓練時間:", round(training_time, 2), "分鐘\n")
      cat("最佳迭代:", model$best_iter, "\n")
      cat("最佳分數:", round(model$best_score, 4), "\n")
    }
    
    # 創建模型物件
    lgbm_model <- list(
      model = model,
      feature_info = train_flattened,
      training_params = train_params,
      training_time = training_time,
      best_iter = model$best_iter,
      best_score = model$best_score,
      data_type = train_dataset$data_type,
      created_at = Sys.time(),
      model_type = "lightgbm"
    )
    
    class(lgbm_model) <- c("aqi_lgbm_model", "list")
    
    # 保存檢查點
    if(save_checkpoint && !is.null(checkpoint_path)) {
      save_lgbm_checkpoint(lgbm_model, checkpoint_path, verbose = verbose)
    }
    
    return(lgbm_model)
    
  }, error = function(e) {
    cat("❌ LightGBM 訓練失敗:", e$message, "\n")
    stop(e)
  })
}

# ================================================================================
# 3. LightGBM 預測函數
# ================================================================================

#' LightGBM 模型預測
#' @param lgbm_model 訓練好的 LightGBM 模型
#' @param test_dataset 測試資料集
#' @param use_best_iter 是否使用最佳迭代次數
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lgbm <- function(lgbm_model, test_dataset, use_best_iter = TRUE, verbose = TRUE) {
  if(!inherits(lgbm_model, "aqi_lgbm_model")) {
    stop("lgbm_model 必須是 aqi_lgbm_model 物件")
  }
  
  if(verbose) {
    cat("🔮 執行 LightGBM 預測...\n")
    cat("測試樣本數:", test_dataset$n_windows, "\n")
  }
  
  # 展平測試資料
  test_flattened <- flatten_tensor_for_lgbm(
    test_dataset$x,
    test_dataset$features,
    use_sparse = FALSE,  # 預測時通常不需要稀疏矩陣
    verbose = FALSE
  )
  
  # 檢查特徵一致性
  if(!identical(test_flattened$feature_names, lgbm_model$feature_info$feature_names)) {
    warning("測試資料的特徵名稱與訓練模型不一致")
  }
  
  # 執行預測
  num_iteration <- if(use_best_iter) lgbm_model$best_iter else NULL
  
  predictions <- predict(
    lgbm_model$model,
    test_flattened$matrix,
    num_iteration = num_iteration
  )
  
  if(verbose) {
    cat("✅ 預測完成\n")
    cat("預測範圍:", round(min(predictions), 2), "~", round(max(predictions), 2), "\n")
  }
  
  return(predictions)
}

# ================================================================================
# 4. 特徵重要度分析
# ================================================================================

#' 獲取並分析特徵重要度
#' @param lgbm_model 訓練好的 LightGBM 模型
#' @param importance_type 重要度類型 ("gain", "split", "frequency")
#' @param top_n 顯示前N個重要特徵
#' @param save_path 保存路徑 (可選)
#' @param verbose 是否顯示詳細資訊
#' @return 特徵重要度資料框
analyze_feature_importance <- function(lgbm_model, importance_type = "gain", 
                                     top_n = 20, save_path = NULL, verbose = TRUE) {
  if(!inherits(lgbm_model, "aqi_lgbm_model")) {
    stop("lgbm_model 必須是 aqi_lgbm_model 物件")
  }
  
  if(verbose) {
    cat("📊 分析特徵重要度 (", importance_type, ")...\n")
  }
  
  # 獲取特徵重要度
  importance <- lgb.importance(
    lgbm_model$model,
    percentage = TRUE
  )
  
  # 解析特徵名稱
  importance$original_feature <- sapply(importance$Feature, function(x) {
    parts <- strsplit(x, "_lag|_h|_t")[[1]]
    if(length(parts) > 1) parts[1] else x
  })
  
  importance$lag_hour <- sapply(importance$Feature, function(x) {
    parts <- strsplit(x, "_lag|_h|_t")[[1]]
    if(length(parts) > 1) as.numeric(parts[2]) else 0
  })
  
  # 按重要度排序
  importance <- importance[order(-importance$Gain), ]
  
  if(verbose) {
    cat("✅ 特徵重要度分析完成\n")
    cat("總特徵數:", nrow(importance), "\n")
    cat("前", min(top_n, nrow(importance)), "個重要特徵:\n")
    
    top_features <- head(importance, top_n)
    for(i in 1:nrow(top_features)) {
      cat(sprintf("  %2d. %s (%.2f%%)\n", 
                  i, top_features$Feature[i], top_features$Gain[i]))
    }
  }
  
  # 保存結果
  if(!is.null(save_path)) {
    write.csv(importance, save_path, row.names = FALSE)
    if(verbose) cat("💾 特徵重要度已保存:", basename(save_path), "\n")
  }
  
  return(importance)
}

#' 分析原始特徵的總重要度
#' @param importance_df 特徵重要度資料框
#' @param top_n 顯示前N個原始特徵
#' @param verbose 是否顯示詳細資訊
#' @return 原始特徵重要度統計
analyze_original_feature_importance <- function(importance_df, top_n = 10, verbose = TRUE) {
  # 按原始特徵聚合重要度
  original_importance <- aggregate(
    Gain ~ original_feature,
    data = importance_df,
    FUN = sum
  )
  
  # 排序
  original_importance <- original_importance[order(-original_importance$Gain), ]
  
  if(verbose) {
    cat("📈 原始特徵重要度統計:\n")
    top_orig <- head(original_importance, top_n)
    for(i in 1:nrow(top_orig)) {
      cat(sprintf("  %2d. %s (%.2f%%)\n", 
                  i, top_orig$original_feature[i], top_orig$Gain[i]))
    }
  }
  
  return(original_importance)
}

# ================================================================================
# 5. 模型保存與載入
# ================================================================================

#' 保存 LightGBM 模型
#' @param lgbm_model LightGBM 模型物件
#' @param save_path 保存路徑 (不含副檔名)
#' @param save_importance 是否保存特徵重要度
#' @param verbose 是否顯示詳細資訊
save_lgbm_model <- function(lgbm_model, save_path, save_importance = TRUE, verbose = TRUE) {
  if(!inherits(lgbm_model, "aqi_lgbm_model")) {
    stop("lgbm_model 必須是 aqi_lgbm_model 物件")
  }
  
  # 創建保存目錄
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  
  # 保存完整模型物件
  model_path <- paste0(save_path, "_complete.rds")
  saveRDS(lgbm_model, model_path)
  
  # 保存 LightGBM 原生模型
  native_path <- paste0(save_path, "_native.txt")
  lgb.save(lgbm_model$model, native_path)
  
  # 保存特徵重要度
  if(save_importance) {
    importance_path <- paste0(save_path, "_importance.csv")
    importance <- analyze_feature_importance(
      lgbm_model, 
      save_path = importance_path,
      verbose = FALSE
    )
    
    # 保存原始特徵重要度
    orig_importance_path <- paste0(save_path, "_original_importance.csv")
    orig_importance <- analyze_original_feature_importance(importance, verbose = FALSE)
    write.csv(orig_importance, orig_importance_path, row.names = FALSE)
  }
  
  if(verbose) {
    cat("💾 LightGBM 模型已保存:\n")
    cat("  完整模型:", basename(model_path), "\n")
    cat("  原生模型:", basename(native_path), "\n")
    if(save_importance) {
      cat("  特徵重要度:", basename(importance_path), "\n")
      cat("  原始特徵重要度:", basename(orig_importance_path), "\n")
    }
  }
}

#' 載入 LightGBM 模型
#' @param model_path 模型路徑 (不含副檔名)
#' @param load_complete 是否載入完整模型 (否則只載入原生模型)
#' @param verbose 是否顯示詳細資訊
#' @return LightGBM 模型物件
load_lgbm_model <- function(model_path, load_complete = TRUE, verbose = TRUE) {
  if(load_complete) {
    complete_path <- paste0(model_path, "_complete.rds")
    if(!file.exists(complete_path)) {
      stop("完整模型檔案不存在: ", complete_path)
    }
    
    lgbm_model <- readRDS(complete_path)
    
    if(verbose) {
      cat("📥 載入完整 LightGBM 模型:", basename(complete_path), "\n")
      cat("  資料類型:", lgbm_model$data_type, "\n")
      cat("  最佳迭代:", lgbm_model$best_iter, "\n")
      cat("  創建時間:", format(lgbm_model$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
    }
    
    return(lgbm_model)
    
  } else {
    native_path <- paste0(model_path, "_native.txt")
    if(!file.exists(native_path)) {
      stop("原生模型檔案不存在: ", native_path)
    }
    
    model <- lgb.load(native_path)
    
    if(verbose) {
      cat("📥 載入原生 LightGBM 模型:", basename(native_path), "\n")
    }
    
    return(model)
  }
}

# ================================================================================
# 6. 檢查點功能
# ================================================================================

#' 保存 LightGBM 檢查點
#' @param lgbm_model LightGBM 模型物件
#' @param checkpoint_path 檢查點路徑
#' @param verbose 是否顯示詳細資訊
save_lgbm_checkpoint <- function(lgbm_model, checkpoint_path, verbose = TRUE) {
  dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
  
  checkpoint_data <- list(
    model = lgbm_model,
    timestamp = Sys.time(),
    status = "completed"
  )
  
  saveRDS(checkpoint_data, checkpoint_path)
  
  if(verbose) {
    cat("💾 LightGBM 檢查點已保存:", basename(checkpoint_path), "\n")
  }
}

#' 載入 LightGBM 檢查點
#' @param checkpoint_path 檢查點路徑
#' @param verbose 是否顯示詳細資訊
#' @return 檢查點資料
load_lgbm_checkpoint <- function(checkpoint_path, verbose = TRUE) {
  if(!file.exists(checkpoint_path)) {
    stop("檢查點檔案不存在: ", checkpoint_path)
  }
  
  checkpoint_data <- readRDS(checkpoint_path)
  
  if(verbose) {
    cat("📥 載入 LightGBM 檢查點:", basename(checkpoint_path), "\n")
    cat("  時間戳:", format(checkpoint_data$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
    cat("  狀態:", checkpoint_data$status, "\n")
  }
  
  return(checkpoint_data)
}

# ================================================================================
# 7. 工具函數
# ================================================================================

#' 檢查 LightGBM 模型健康狀態
#' @param lgbm_model LightGBM 模型物件
#' @return 健康檢查結果
check_lgbm_model_health <- function(lgbm_model) {
  if(!inherits(lgbm_model, "aqi_lgbm_model")) {
    return(list(is_healthy = FALSE, issues = "不是有效的 aqi_lgbm_model 物件"))
  }
  
  issues <- c()
  
  # 檢查必要組件
  required_components <- c("model", "feature_info", "training_params")
  missing_components <- setdiff(required_components, names(lgbm_model))
  if(length(missing_components) > 0) {
    issues <- c(issues, paste("缺少組件:", paste(missing_components, collapse = ", ")))
  }
  
  # 檢查模型物件
  if(is.null(lgbm_model$model)) {
    issues <- c(issues, "模型物件為空")
  }
  
  # 檢查特徵資訊
  if(is.null(lgbm_model$feature_info$feature_names)) {
    issues <- c(issues, "缺少特徵名稱資訊")
  }
  
  is_healthy <- length(issues) == 0
  
  return(list(
    is_healthy = is_healthy,
    issues = if(length(issues) > 0) issues else "模型狀態良好"
  ))
}

cat("✅ LightGBM 模型模組 (優化版) 載入完成\n") 