# ================================================================================
# AQI 時間序列預測模型訓練 - 資料切分模組
# ================================================================================

cat("✂️  載入資料切分模組...\n")

# ================================================================================
# 1. 時序切分核心函數
# ================================================================================

#' 時間序列交叉驗證切分 (符合規劃要求)
#' @param dataset aqi_dataset 物件
#' @param test_ratio 測試集比例 (預設0.2)
#' @param val_ratio 驗證集比例 (預設0.1，從剩餘資料中切分)
#' @param method 切分方法 ("sequential", "stratified")
#' @param verbose 是否顯示詳細資訊
#' @return 切分索引物件，包含 train_idx, val_idx, test_idx
time_cv <- function(dataset, 
                   test_ratio = 0.2,
                   val_ratio = 0.1,
                   method = "sequential",
                   verbose = TRUE) {
  
  # 計算訓練集比例
  train_ratio <- 1.0 - test_ratio - val_ratio
  
  if(train_ratio <= 0) {
    stop("訓練集比例必須大於0，請調整test_ratio和val_ratio")
  }
  
  # 驗證輸入
  if(!inherits(dataset, "aqi_dataset")) {
    stop("dataset 必須是 aqi_dataset 物件")
  }
  
  if(abs(train_ratio + val_ratio + test_ratio - 1.0) > 1e-6) {
    stop("切分比例總和必須等於1.0")
  }
  
  n_samples <- dataset$n_windows
  
  if(verbose) {
    cat("📊 執行時序資料切分...\n")
    cat("  總樣本數:", format(n_samples, big.mark = ","), "\n")
    cat("  切分比例:", train_ratio, "/", val_ratio, "/", test_ratio, "\n")
    cat("  切分方法:", method, "\n")
  }
  
  # 計算各集合大小
  n_train <- floor(n_samples * train_ratio)
  n_val <- floor(n_samples * val_ratio)
  n_test <- n_samples - n_train - n_val  # 剩餘的分配給測試集
  
  if(method == "sequential") {
    # 順序切分：保持時間順序
    train_idx <- 1:n_train
    val_idx <- (n_train + 1):(n_train + n_val)
    test_idx <- (n_train + n_val + 1):n_samples
    
  } else if(method == "stratified") {
    # 分層切分：基於目標變數分佈
    y_values <- dataset$y
    
    # 將目標變數分成若干層
    n_strata <- min(10, floor(n_samples / 100))  # 最多10層，每層至少100個樣本
    y_quantiles <- quantile(y_values, probs = seq(0, 1, length.out = n_strata + 1), na.rm = TRUE)
    strata <- cut(y_values, breaks = y_quantiles, include.lowest = TRUE, labels = FALSE)
    
    train_idx <- c()
    val_idx <- c()
    test_idx <- c()
    
    # 對每一層進行切分
    for(stratum in 1:n_strata) {
      stratum_indices <- which(strata == stratum)
      n_stratum <- length(stratum_indices)
      
      if(n_stratum > 0) {
        # 保持時間順序的前提下進行分層切分
        stratum_indices <- sort(stratum_indices)
        
        n_train_stratum <- floor(n_stratum * train_ratio)
        n_val_stratum <- floor(n_stratum * val_ratio)
        n_test_stratum <- n_stratum - n_train_stratum - n_val_stratum
        
        train_idx <- c(train_idx, stratum_indices[1:n_train_stratum])
        val_idx <- c(val_idx, stratum_indices[(n_train_stratum + 1):(n_train_stratum + n_val_stratum)])
        test_idx <- c(test_idx, stratum_indices[(n_train_stratum + n_val_stratum + 1):n_stratum])
      }
    }
    
    # 重新排序以保持時間順序
    train_idx <- sort(train_idx)
    val_idx <- sort(val_idx)
    test_idx <- sort(test_idx)
    
  } else {
    stop("不支援的切分方法: ", method)
  }
  
  # 驗證切分結果
  if(length(train_idx) == 0 || length(val_idx) == 0 || length(test_idx) == 0) {
    stop("切分結果包含空集合")
  }
  
  if(length(unique(c(train_idx, val_idx, test_idx))) != n_samples) {
    stop("切分結果索引不完整或重複")
  }
  
  # 創建切分物件
  split_result <- list(
    train_idx = train_idx,
    val_idx = val_idx,
    test_idx = test_idx,
    
    # 統計資訊
    n_train = length(train_idx),
    n_val = length(val_idx),
    n_test = length(test_idx),
    n_total = n_samples,
    
    # 實際比例
    actual_train_ratio = length(train_idx) / n_samples,
    actual_val_ratio = length(val_idx) / n_samples,
    actual_test_ratio = length(test_idx) / n_samples,
    
    # 切分參數
    method = method,
    requested_ratios = c(train_ratio, val_ratio, test_ratio),
    
    # 目標變數統計
    train_y_stats = list(
      mean = mean(dataset$y[train_idx], na.rm = TRUE),
      sd = sd(dataset$y[train_idx], na.rm = TRUE),
      min = min(dataset$y[train_idx], na.rm = TRUE),
      max = max(dataset$y[train_idx], na.rm = TRUE)
    ),
    val_y_stats = list(
      mean = mean(dataset$y[val_idx], na.rm = TRUE),
      sd = sd(dataset$y[val_idx], na.rm = TRUE),
      min = min(dataset$y[val_idx], na.rm = TRUE),
      max = max(dataset$y[val_idx], na.rm = TRUE)
    ),
    test_y_stats = list(
      mean = mean(dataset$y[test_idx], na.rm = TRUE),
      sd = sd(dataset$y[test_idx], na.rm = TRUE),
      min = min(dataset$y[test_idx], na.rm = TRUE),
      max = max(dataset$y[test_idx], na.rm = TRUE)
    ),
    
    # 元資料
    split_time = Sys.time(),
    data_type = dataset$data_type
  )
  
  class(split_result) <- c("aqi_split", "list")
  
  if(verbose) {
    print(split_result)
  }
  
  return(split_result)
}

#' 打印切分結果摘要
#' @param x aqi_split 物件
print.aqi_split <- function(x, ...) {
  cat("AQI 時間序列資料切分結果\n")
  cat("========================\n")
  cat("資料類型:", x$data_type, "\n")
  cat("切分方法:", x$method, "\n")
  cat("總樣本數:", format(x$n_total, big.mark = ","), "\n\n")
  
  cat("📊 各集合統計:\n")
  cat("  訓練集:", format(x$n_train, big.mark = ","), 
      "個樣本 (", round(x$actual_train_ratio * 100, 1), "%)\n")
  cat("  驗證集:", format(x$n_val, big.mark = ","), 
      "個樣本 (", round(x$actual_val_ratio * 100, 1), "%)\n")
  cat("  測試集:", format(x$n_test, big.mark = ","), 
      "個樣本 (", round(x$actual_test_ratio * 100, 1), "%)\n\n")
  
  cat("🎯 目標變數分佈:\n")
  cat("  訓練集: 均值=", round(x$train_y_stats$mean, 2), 
      ", 標準差=", round(x$train_y_stats$sd, 2), 
      ", 範圍=[", round(x$train_y_stats$min, 2), ",", round(x$train_y_stats$max, 2), "]\n")
  cat("  驗證集: 均值=", round(x$val_y_stats$mean, 2), 
      ", 標準差=", round(x$val_y_stats$sd, 2), 
      ", 範圍=[", round(x$val_y_stats$min, 2), ",", round(x$val_y_stats$max, 2), "]\n")
  cat("  測試集: 均值=", round(x$test_y_stats$mean, 2), 
      ", 標準差=", round(x$test_y_stats$sd, 2), 
      ", 範圍=[", round(x$test_y_stats$min, 2), ",", round(x$test_y_stats$max, 2), "]\n")
  
  cat("\n切分時間:", format(x$split_time, "%Y-%m-%d %H:%M:%S"), "\n")
}

# ================================================================================
# 2. 資料集提取函數
# ================================================================================

#' 根據切分索引提取訓練集
#' @param dataset aqi_dataset 物件
#' @param split aqi_split 物件
#' @return 訓練集 aqi_dataset 物件
extract_train_set <- function(dataset, split) {
  if(!inherits(dataset, "aqi_dataset") || !inherits(split, "aqi_split")) {
    stop("輸入物件類型錯誤")
  }
  
  train_dataset <- create_dataset(
    x = dataset$x[split$train_idx, , , drop = FALSE],
    y = dataset$y[split$train_idx],
    features = dataset$features,
    data_type = paste0(dataset$data_type, "_train"),
    metadata = list(
      original_dataset = dataset$data_type,
      split_method = split$method,
      n_original = dataset$n_windows,
      indices = split$train_idx,
      extracted_at = Sys.time()
    )
  )
  
  return(train_dataset)
}

#' 根據切分索引提取驗證集
#' @param dataset aqi_dataset 物件
#' @param split aqi_split 物件
#' @return 驗證集 aqi_dataset 物件
extract_val_set <- function(dataset, split) {
  if(!inherits(dataset, "aqi_dataset") || !inherits(split, "aqi_split")) {
    stop("輸入物件類型錯誤")
  }
  
  val_dataset <- create_dataset(
    x = dataset$x[split$val_idx, , , drop = FALSE],
    y = dataset$y[split$val_idx],
    features = dataset$features,
    data_type = paste0(dataset$data_type, "_val"),
    metadata = list(
      original_dataset = dataset$data_type,
      split_method = split$method,
      n_original = dataset$n_windows,
      indices = split$val_idx,
      extracted_at = Sys.time()
    )
  )
  
  return(val_dataset)
}

#' 根據切分索引提取測試集
#' @param dataset aqi_dataset 物件
#' @param split aqi_split 物件
#' @return 測試集 aqi_dataset 物件
extract_test_set <- function(dataset, split) {
  if(!inherits(dataset, "aqi_dataset") || !inherits(split, "aqi_split")) {
    stop("輸入物件類型錯誤")
  }
  
  test_dataset <- create_dataset(
    x = dataset$x[split$test_idx, , , drop = FALSE],
    y = dataset$y[split$test_idx],
    features = dataset$features,
    data_type = paste0(dataset$data_type, "_test"),
    metadata = list(
      original_dataset = dataset$data_type,
      split_method = split$method,
      n_original = dataset$n_windows,
      indices = split$test_idx,
      extracted_at = Sys.time()
    )
  )
  
  return(test_dataset)
}

#' 一次性提取所有資料集
#' @param dataset aqi_dataset 物件
#' @param split aqi_split 物件
#' @return 包含train, val, test的列表
extract_all_sets <- function(dataset, split) {
  return(list(
    train = extract_train_set(dataset, split),
    val = extract_val_set(dataset, split),
    test = extract_test_set(dataset, split)
  ))
}

# ================================================================================
# 3. 交叉驗證支援
# ================================================================================

#' 時間序列交叉驗證切分
#' @param dataset aqi_dataset 物件
#' @param n_folds 摺數
#' @param horizon 預測時間範圍
#' @param gap 訓練和測試之間的間隔
#' @param verbose 是否顯示詳細資訊
#' @return 交叉驗證切分列表
time_series_cv_split <- function(dataset, n_folds = 5, horizon = 1, gap = 0, verbose = TRUE) {
  if(!inherits(dataset, "aqi_dataset")) {
    stop("dataset 必須是 aqi_dataset 物件")
  }
  
  n_samples <- dataset$n_windows
  
  if(verbose) {
    cat("🔄 執行時間序列交叉驗證切分...\n")
    cat("  總樣本數:", format(n_samples, big.mark = ","), "\n")
    cat("  摺數:", n_folds, "\n")
    cat("  預測範圍:", horizon, "\n")
    cat("  間隔:", gap, "\n")
  }
  
  # 計算每摺的大小
  min_train_size <- floor(n_samples * 0.3)  # 最小訓練集大小
  test_size <- floor((n_samples - min_train_size) / n_folds)
  
  if(test_size < horizon) {
    stop("資料量不足以進行", n_folds, "摺交叉驗證")
  }
  
  cv_splits <- list()
  
  for(fold in 1:n_folds) {
    # 計算當前摺的測試集範圍
    test_end <- n_samples - (n_folds - fold) * test_size
    test_start <- test_end - test_size + 1
    
    # 訓練集結束位置（考慮間隔）
    train_end <- test_start - gap - 1
    
    if(train_end < min_train_size) {
      warning("摺", fold, "的訓練集過小，跳過")
      next
    }
    
    train_idx <- 1:train_end
    test_idx <- test_start:test_end
    
    # 從訓練集中分出驗證集（最後20%）
    val_size <- floor(length(train_idx) * 0.2)
    val_start <- length(train_idx) - val_size + 1
    
    val_idx <- train_idx[val_start:length(train_idx)]
    train_idx <- train_idx[1:(val_start - 1)]
    
    cv_split <- list(
      fold = fold,
      train_idx = train_idx,
      val_idx = val_idx,
      test_idx = test_idx,
      n_train = length(train_idx),
      n_val = length(val_idx),
      n_test = length(test_idx)
    )
    
    cv_splits[[fold]] <- cv_split
    
    if(verbose) {
      cat("  摺", fold, ": 訓練=", length(train_idx), 
          ", 驗證=", length(val_idx), 
          ", 測試=", length(test_idx), "\n")
    }
  }
  
  if(verbose) {
    cat("✅ 交叉驗證切分完成:", length(cv_splits), "摺\n")
  }
  
  return(cv_splits)
}

# ================================================================================
# 4. 切分品質評估
# ================================================================================

#' 評估切分品質
#' @param dataset aqi_dataset 物件
#' @param split aqi_split 物件
#' @return 品質評估結果
evaluate_split_quality <- function(dataset, split) {
  if(!inherits(dataset, "aqi_dataset") || !inherits(split, "aqi_split")) {
    stop("輸入物件類型錯誤")
  }
  
  # 提取各集合的目標變數
  y_train <- dataset$y[split$train_idx]
  y_val <- dataset$y[split$val_idx]
  y_test <- dataset$y[split$test_idx]
  
  # 計算分佈相似性（使用KS檢驗）
  ks_train_val <- ks.test(y_train, y_val)
  ks_train_test <- ks.test(y_train, y_test)
  ks_val_test <- ks.test(y_val, y_test)
  
  # 計算統計量差異
  mean_diff_train_val <- abs(mean(y_train, na.rm = TRUE) - mean(y_val, na.rm = TRUE))
  mean_diff_train_test <- abs(mean(y_train, na.rm = TRUE) - mean(y_test, na.rm = TRUE))
  
  sd_diff_train_val <- abs(sd(y_train, na.rm = TRUE) - sd(y_val, na.rm = TRUE))
  sd_diff_train_test <- abs(sd(y_train, na.rm = TRUE) - sd(y_test, na.rm = TRUE))
  
  # 檢查時間順序
  time_order_valid <- all(diff(split$train_idx) > 0) && 
                     all(diff(split$val_idx) > 0) && 
                     all(diff(split$test_idx) > 0) &&
                     max(split$train_idx) < min(split$val_idx) &&
                     max(split$val_idx) < min(split$test_idx)
  
  quality_result <- list(
    # 分佈相似性
    ks_tests = list(
      train_val = list(statistic = ks_train_val$statistic, p_value = ks_train_val$p.value),
      train_test = list(statistic = ks_train_test$statistic, p_value = ks_train_test$p.value),
      val_test = list(statistic = ks_val_test$statistic, p_value = ks_val_test$p.value)
    ),
    
    # 統計量差異
    mean_differences = list(
      train_val = mean_diff_train_val,
      train_test = mean_diff_train_test
    ),
    
    sd_differences = list(
      train_val = sd_diff_train_val,
      train_test = sd_diff_train_test
    ),
    
    # 時間順序檢查
    time_order_valid = time_order_valid,
    
    # 整體品質評分 (0-1, 越高越好)
    quality_score = calculate_split_quality_score(ks_train_val$p.value, ks_train_test$p.value,
                                                  mean_diff_train_val, mean_diff_train_test,
                                                  time_order_valid)
  )
  
  return(quality_result)
}

#' 計算切分品質評分
#' @param ks_p1 KS檢驗p值1
#' @param ks_p2 KS檢驗p值2  
#' @param mean_diff1 均值差異1
#' @param mean_diff2 均值差異2
#' @param time_valid 時間順序是否有效
#' @return 品質評分 (0-1)
calculate_split_quality_score <- function(ks_p1, ks_p2, mean_diff1, mean_diff2, time_valid) {
  # KS檢驗評分 (p值越大越好，表示分佈越相似)
  ks_score <- (ks_p1 + ks_p2) / 2
  
  # 均值差異評分 (差異越小越好)
  mean_score <- 1 / (1 + (mean_diff1 + mean_diff2) / 2)
  
  # 時間順序評分
  time_score <- ifelse(time_valid, 1, 0)
  
  # 綜合評分
  overall_score <- (ks_score * 0.4 + mean_score * 0.4 + time_score * 0.2)
  
  return(pmax(0, pmin(1, overall_score)))
}

# ================================================================================
# 5. 便利函數
# ================================================================================

#' 快速時序切分（使用預設參數）
#' @param dataset aqi_dataset 物件
#' @param verbose 是否顯示詳細資訊
#' @return 切分結果和提取的資料集
quick_time_split <- function(dataset, verbose = TRUE) {
  # 使用配置文件中的預設比例
  split_result <- time_split(
    dataset = dataset,
    train_ratio = SPLIT_CONFIG$train_ratio,
    val_ratio = SPLIT_CONFIG$val_ratio,
    test_ratio = SPLIT_CONFIG$test_ratio,
    method = "sequential",
    verbose = verbose
  )
  
  # 提取所有資料集
  datasets <- extract_all_sets(dataset, split_result)
  
  return(list(
    split = split_result,
    datasets = datasets
  ))
}

#' 檢查切分結果的時間連續性
#' @param split aqi_split 物件
#' @return 連續性檢查結果
check_time_continuity <- function(split) {
  # 檢查各集合內部的連續性
  train_continuous <- all(diff(split$train_idx) == 1)
  val_continuous <- all(diff(split$val_idx) == 1)
  test_continuous <- all(diff(split$test_idx) == 1)
  
  # 檢查集合間的順序
  train_val_order <- max(split$train_idx) < min(split$val_idx)
  val_test_order <- max(split$val_idx) < min(split$test_idx)
  
  # 檢查是否有間隔
  train_val_gap <- min(split$val_idx) - max(split$train_idx) - 1
  val_test_gap <- min(split$test_idx) - max(split$val_idx) - 1
  
  return(list(
    internal_continuity = list(
      train = train_continuous,
      val = val_continuous,
      test = test_continuous
    ),
    inter_set_order = list(
      train_val = train_val_order,
      val_test = val_test_order
    ),
    gaps = list(
      train_val = train_val_gap,
      val_test = val_test_gap
    ),
    overall_valid = train_continuous && val_continuous && test_continuous &&
                   train_val_order && val_test_order
  ))
}

cat("✅ 資料切分模組載入完成\n") 