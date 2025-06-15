# ================================================================================
# AQI 時間序列預測模型訓練 - 評估模組
# ================================================================================

cat("📊 載入模型評估模組...\n")

# ================================================================================
# 1. 核心評估指標函數
# ================================================================================

#' 計算均方根誤差 (RMSE)
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return RMSE值
calculate_rmse <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
  
  if(sum(valid_idx) == 0) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  rmse <- sqrt(mean((y_true_clean - y_pred_clean)^2))
  return(rmse)
}

#' 計算平均絕對誤差 (MAE)
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return MAE值
calculate_mae <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
  
  if(sum(valid_idx) == 0) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  mae <- mean(abs(y_true_clean - y_pred_clean))
  return(mae)
}

#' 計算平均絕對百分比誤差 (MAPE)
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return MAPE值 (百分比)
calculate_mape <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值和零值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred) & y_true != 0
  
  if(sum(valid_idx) == 0) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  mape <- mean(abs((y_true_clean - y_pred_clean) / y_true_clean)) * 100
  return(mape)
}

#' 計算對稱平均絕對百分比誤差 (SMAPE)
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return SMAPE值 (百分比)
calculate_smape <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
  
  if(sum(valid_idx) == 0) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  denominator <- (abs(y_true_clean) + abs(y_pred_clean)) / 2
  # 避免除零
  valid_denom <- denominator != 0
  
  if(sum(valid_denom) == 0) {
    return(NA)
  }
  
  smape <- mean(abs(y_true_clean[valid_denom] - y_pred_clean[valid_denom]) / denominator[valid_denom]) * 100
  return(smape)
}

#' 計算決定係數 (R²)
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return R²值
calculate_r2 <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
  
  if(sum(valid_idx) == 0) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  if(length(y_true_clean) < 2) {
    return(NA)
  }
  
  ss_res <- sum((y_true_clean - y_pred_clean)^2)
  ss_tot <- sum((y_true_clean - mean(y_true_clean))^2)
  
  if(ss_tot == 0) {
    return(ifelse(ss_res == 0, 1, 0))
  }
  
  r2 <- 1 - (ss_res / ss_tot)
  return(r2)
}

#' 計算皮爾森相關係數
#' @param y_true 真實值
#' @param y_pred 預測值
#' @return 相關係數
calculate_correlation <- function(y_true, y_pred) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  # 移除NA值
  valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
  
  if(sum(valid_idx) < 2) {
    return(NA)
  }
  
  y_true_clean <- y_true[valid_idx]
  y_pred_clean <- y_pred[valid_idx]
  
  correlation <- cor(y_true_clean, y_pred_clean, method = "pearson")
  return(correlation)
}

# ================================================================================
# 2. 綜合評估函數
# ================================================================================

#' 計算所有評估指標
#' @param y_true 真實值
#' @param y_pred 預測值
#' @param metrics 要計算的指標列表
#' @return 評估結果列表
evaluate_predictions <- function(y_true, y_pred, metrics = EVAL_METRICS) {
  if(length(y_true) != length(y_pred)) {
    stop("y_true 和 y_pred 長度不一致")
  }
  
  results <- list()
  
  # 基本統計
  n_total <- length(y_true)
  n_valid <- sum(!is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred))
  n_missing <- n_total - n_valid
  
  results$basic_stats <- list(
    n_total = n_total,
    n_valid = n_valid,
    n_missing = n_missing,
    missing_ratio = n_missing / n_total
  )
  
  # 計算各項指標
  if("rmse" %in% metrics) {
    results$rmse <- calculate_rmse(y_true, y_pred)
  }
  
  if("mae" %in% metrics) {
    results$mae <- calculate_mae(y_true, y_pred)
  }
  
  if("mape" %in% metrics) {
    results$mape <- calculate_mape(y_true, y_pred)
  }
  
  if("smape" %in% metrics) {
    results$smape <- calculate_smape(y_true, y_pred)
  }
  
  if("r2" %in% metrics) {
    results$r2 <- calculate_r2(y_true, y_pred)
  }
  
  if("correlation" %in% metrics) {
    results$correlation <- calculate_correlation(y_true, y_pred)
  }
  
  # 殘差統計
  if(n_valid > 0) {
    valid_idx <- !is.na(y_true) & !is.na(y_pred) & is.finite(y_true) & is.finite(y_pred)
    residuals <- y_true[valid_idx] - y_pred[valid_idx]
    
    results$residual_stats <- list(
      mean = mean(residuals),
      sd = sd(residuals),
      min = min(residuals),
      max = max(residuals),
      q25 = quantile(residuals, 0.25),
      median = median(residuals),
      q75 = quantile(residuals, 0.75)
    )
  }
  
  # 評估時間
  results$evaluation_time <- Sys.time()
  
  class(results) <- c("aqi_evaluation", "list")
  return(results)
}

#' 打印評估結果
#' @param x aqi_evaluation 物件
print.aqi_evaluation <- function(x, ...) {
  cat("AQI 模型評估結果\n")
  cat("================\n")
  
  # 基本統計
  cat("📊 基本統計:\n")
  cat("  總樣本數:", format(x$basic_stats$n_total, big.mark = ","), "\n")
  cat("  有效樣本數:", format(x$basic_stats$n_valid, big.mark = ","), "\n")
  cat("  缺失樣本數:", format(x$basic_stats$n_missing, big.mark = ","), 
      "(", round(x$basic_stats$missing_ratio * 100, 2), "%)\n\n")
  
  # 評估指標
  cat("🎯 評估指標:\n")
  if(!is.null(x$rmse)) {
    cat("  RMSE:", round(x$rmse, 4), "\n")
  }
  if(!is.null(x$mae)) {
    cat("  MAE:", round(x$mae, 4), "\n")
  }
  if(!is.null(x$mape)) {
    cat("  MAPE:", round(x$mape, 2), "%\n")
  }
  if(!is.null(x$smape)) {
    cat("  SMAPE:", round(x$smape, 2), "%\n")
  }
  if(!is.null(x$r2)) {
    cat("  R²:", round(x$r2, 4), "\n")
  }
  if(!is.null(x$correlation)) {
    cat("  相關係數:", round(x$correlation, 4), "\n")
  }
  
  # 殘差統計
  if(!is.null(x$residual_stats)) {
    cat("\n📈 殘差統計:\n")
    cat("  均值:", round(x$residual_stats$mean, 4), "\n")
    cat("  標準差:", round(x$residual_stats$sd, 4), "\n")
    cat("  範圍: [", round(x$residual_stats$min, 2), ", ", round(x$residual_stats$max, 2), "]\n")
    cat("  四分位數: [", round(x$residual_stats$q25, 2), ", ", 
        round(x$residual_stats$median, 2), ", ", round(x$residual_stats$q75, 2), "]\n")
  }
  
  cat("\n評估時間:", format(x$evaluation_time, "%Y-%m-%d %H:%M:%S"), "\n")
}

# ================================================================================
# 3. 模型比較函數
# ================================================================================

#' 比較多個模型的評估結果
#' @param evaluation_list 評估結果列表
#' @param model_names 模型名稱
#' @return 比較結果表格
compare_models <- function(evaluation_list, model_names = NULL) {
  if(length(evaluation_list) == 0) {
    stop("評估結果列表為空")
  }
  
  if(is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(evaluation_list))
  }
  
  if(length(model_names) != length(evaluation_list)) {
    stop("模型名稱數量與評估結果數量不一致")
  }
  
  # 提取指標
  metrics <- c("rmse", "mae", "mape", "smape", "r2", "correlation")
  comparison_table <- data.frame(Model = model_names)
  
  for(metric in metrics) {
    values <- sapply(evaluation_list, function(eval_result) {
      if(metric %in% names(eval_result)) {
        return(eval_result[[metric]])
      } else {
        return(NA)
      }
    })
    comparison_table[[toupper(metric)]] <- values
  }
  
  # 添加排名
  for(metric in c("RMSE", "MAE", "MAPE", "SMAPE")) {
    if(metric %in% names(comparison_table)) {
      rank_col <- paste0(metric, "_Rank")
      comparison_table[[rank_col]] <- rank(comparison_table[[metric]], na.last = "keep")
    }
  }
  
  for(metric in c("R2", "CORRELATION")) {
    if(metric %in% names(comparison_table)) {
      rank_col <- paste0(metric, "_Rank")
      comparison_table[[rank_col]] <- rank(-comparison_table[[metric]], na.last = "keep")
    }
  }
  
  # 計算綜合評分
  rank_cols <- grep("_Rank$", names(comparison_table), value = TRUE)
  if(length(rank_cols) > 0) {
    comparison_table$Overall_Score <- rowMeans(comparison_table[rank_cols], na.rm = TRUE)
    comparison_table$Overall_Rank <- rank(comparison_table$Overall_Score, na.last = "keep")
  }
  
  class(comparison_table) <- c("aqi_model_comparison", "data.frame")
  return(comparison_table)
}

#' 打印模型比較結果
#' @param x aqi_model_comparison 物件
print.aqi_model_comparison <- function(x, ...) {
  cat("AQI 模型比較結果\n")
  cat("================\n\n")
  
  # 顯示主要指標
  main_cols <- c("Model", "RMSE", "MAE", "MAPE", "R2")
  available_cols <- intersect(main_cols, names(x))
  
  if(length(available_cols) > 1) {
    cat("📊 主要指標比較:\n")
    print(x[available_cols])
    cat("\n")
  }
  
  # 顯示排名
  if("Overall_Rank" %in% names(x)) {
    cat("🏆 綜合排名:\n")
    ranked_models <- x[order(x$Overall_Rank), c("Model", "Overall_Score", "Overall_Rank")]
    print(ranked_models)
    cat("\n")
    
    # 最佳模型
    best_model <- ranked_models$Model[1]
    cat("🥇 最佳模型:", best_model, "\n")
  }
}

# ================================================================================
# 4. 評估品質檢查
# ================================================================================

#' 檢查評估結果品質
#' @param evaluation 評估結果物件
#' @return 品質檢查結果
check_evaluation_quality <- function(evaluation) {
  if(!inherits(evaluation, "aqi_evaluation")) {
    stop("輸入必須是 aqi_evaluation 物件")
  }
  
  issues <- list()
  warnings <- list()
  
  # 檢查缺失值比例
  if(evaluation$basic_stats$missing_ratio > 0.1) {
    issues <- c(issues, paste("缺失值比例過高:", round(evaluation$basic_stats$missing_ratio * 100, 2), "%"))
  } else if(evaluation$basic_stats$missing_ratio > 0.05) {
    warnings <- c(warnings, paste("缺失值比例較高:", round(evaluation$basic_stats$missing_ratio * 100, 2), "%"))
  }
  
  # 檢查樣本數量
  if(evaluation$basic_stats$n_valid < 100) {
    issues <- c(issues, paste("有效樣本數過少:", evaluation$basic_stats$n_valid))
  } else if(evaluation$basic_stats$n_valid < 1000) {
    warnings <- c(warnings, paste("有效樣本數較少:", evaluation$basic_stats$n_valid))
  }
  
  # 檢查指標異常值
  if(!is.null(evaluation$r2) && evaluation$r2 < 0) {
    issues <- c(issues, paste("R²為負值:", round(evaluation$r2, 4)))
  }
  
  if(!is.null(evaluation$mape) && evaluation$mape > 100) {
    warnings <- c(warnings, paste("MAPE過高:", round(evaluation$mape, 2), "%"))
  }
  
  if(!is.null(evaluation$correlation) && abs(evaluation$correlation) < 0.1) {
    warnings <- c(warnings, paste("相關係數過低:", round(evaluation$correlation, 4)))
  }
  
  # 檢查殘差分佈
  if(!is.null(evaluation$residual_stats)) {
    residual_mean <- abs(evaluation$residual_stats$mean)
    residual_sd <- evaluation$residual_stats$sd
    
    if(residual_mean > residual_sd * 0.1) {
      warnings <- c(warnings, paste("殘差均值偏離零:", round(residual_mean, 4)))
    }
  }
  
  return(list(
    is_good_quality = length(issues) == 0,
    issues = issues,
    warnings = warnings,
    n_issues = length(issues),
    n_warnings = length(warnings)
  ))
}

# ================================================================================
# 5. 評估結果儲存與載入
# ================================================================================

#' 儲存評估結果
#' @param evaluation 評估結果物件
#' @param file_path 儲存路徑
#' @param format 儲存格式 ("rds", "csv", "json")
save_evaluation <- function(evaluation, file_path, format = "rds") {
  if(!inherits(evaluation, "aqi_evaluation")) {
    stop("輸入必須是 aqi_evaluation 物件")
  }
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  if(format == "rds") {
    saveRDS(evaluation, file_path)
    
  } else if(format == "csv") {
    # 轉換為表格格式
    eval_df <- data.frame(
      Metric = character(),
      Value = numeric(),
      stringsAsFactors = FALSE
    )
    
    # 添加主要指標
    main_metrics <- c("rmse", "mae", "mape", "smape", "r2", "correlation")
    for(metric in main_metrics) {
      if(!is.null(evaluation[[metric]])) {
        eval_df <- rbind(eval_df, data.frame(Metric = toupper(metric), Value = evaluation[[metric]]))
      }
    }
    
    write.csv(eval_df, file_path, row.names = FALSE)
    
  } else if(format == "json") {
    if(!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("需要安裝 jsonlite 套件以支援 JSON 格式")
    }
    
    # 簡化物件以便JSON序列化
    simple_eval <- list(
      rmse = evaluation$rmse,
      mae = evaluation$mae,
      mape = evaluation$mape,
      smape = evaluation$smape,
      r2 = evaluation$r2,
      correlation = evaluation$correlation,
      basic_stats = evaluation$basic_stats,
      residual_stats = evaluation$residual_stats,
      evaluation_time = as.character(evaluation$evaluation_time)
    )
    
    jsonlite::write_json(simple_eval, file_path, pretty = TRUE)
    
  } else {
    stop("不支援的格式: ", format)
  }
  
  cat("✅ 評估結果已儲存:", file_path, "\n")
}

#' 載入評估結果
#' @param file_path 檔案路徑
#' @return 評估結果物件
load_evaluation <- function(file_path) {
  if(!file.exists(file_path)) {
    stop("檔案不存在: ", file_path)
  }
  
  file_ext <- tools::file_ext(file_path)
  
  if(file_ext == "rds") {
    evaluation <- readRDS(file_path)
    if(!inherits(evaluation, "aqi_evaluation")) {
      warning("載入的物件不是 aqi_evaluation 類型")
    }
    return(evaluation)
    
  } else {
    stop("目前僅支援載入 RDS 格式的評估結果")
  }
}

# ================================================================================
# 6. 批次評估函數
# ================================================================================

#' 批次評估多個預測結果
#' @param predictions_list 預測結果列表
#' @param y_true 真實值
#' @param model_names 模型名稱
#' @param save_dir 儲存目錄 (可選)
#' @return 批次評估結果
batch_evaluate <- function(predictions_list, y_true, model_names = NULL, save_dir = NULL) {
  if(length(predictions_list) == 0) {
    stop("預測結果列表為空")
  }
  
  if(is.null(model_names)) {
    model_names <- paste0("Model_", seq_along(predictions_list))
  }
  
  cat("🔄 執行批次評估...\n")
  
  evaluation_results <- list()
  
  for(i in seq_along(predictions_list)) {
    model_name <- model_names[i]
    y_pred <- predictions_list[[i]]
    
    cat("  評估模型:", model_name, "\n")
    
    tryCatch({
      eval_result <- evaluate_predictions(y_true, y_pred)
      evaluation_results[[model_name]] <- eval_result
      
      # 儲存個別結果
      if(!is.null(save_dir)) {
        save_path <- file.path(save_dir, paste0(model_name, "_evaluation.rds"))
        save_evaluation(eval_result, save_path)
      }
      
    }, error = function(e) {
      cat("    ⚠️  評估失敗:", e$message, "\n")
      evaluation_results[[model_name]] <- NULL
    })
  }
  
  # 生成比較結果
  if(length(evaluation_results) > 1) {
    comparison <- compare_models(evaluation_results, names(evaluation_results))
    
    if(!is.null(save_dir)) {
      comparison_path <- file.path(save_dir, "model_comparison.csv")
      write.csv(comparison, comparison_path, row.names = FALSE)
      cat("📊 模型比較結果已儲存:", comparison_path, "\n")
    }
  } else {
    comparison <- NULL
  }
  
  cat("✅ 批次評估完成\n")
  
  return(list(
    evaluations = evaluation_results,
    comparison = comparison,
    model_names = names(evaluation_results)
  ))
}

cat("✅ 模型評估模組載入完成\n") 