# ================================================================================
# AQI 模型解析與可解釋性分析模組 (最簡版)
# ================================================================================

suppressPackageStartupMessages({
  library(data.table)
  if(requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    HAS_GGPLOT <- TRUE
  } else {
    HAS_GGPLOT <- FALSE
    cat("⚠️ ggplot2 未安裝，將跳過圖表生成\n")
  }
})

# ================================================================================
# 1. 統一載入層
# ================================================================================

#' 掃描模型輸出目錄
#' @param models_dir 模型目錄路徑
#' @return 模型檔案資訊表
scan_model_outputs <- function(models_dir = "model_outputs/models/") {
  if(!dir.exists(models_dir)) {
    stop("模型目錄不存在: ", models_dir)
  }
  
  cat("📂 掃描模型輸出目錄:", models_dir, "\n")
  
  # 掃描所有完整模型檔案
  complete_files <- list.files(models_dir, pattern = "_complete\\.rds$", full.names = TRUE)
  
  if(length(complete_files) == 0) {
    stop("未找到任何完整模型檔案 (*_complete.rds)")
  }
  
  # 解析檔案名稱
  models_info <- data.table()
  
  for(file_path in complete_files) {
    file_name <- basename(file_path)
    
    # 解析檔案名稱: {model_type}_{dataset_type}_{station_name}_{timestamp}_complete.rds
    parts <- strsplit(tools::file_path_sans_ext(file_name), "_")[[1]]
    
    if(length(parts) >= 4) {
      model_type <- parts[1]
      dataset_type <- parts[2]
      station_name <- paste(parts[3:(length(parts)-2)], collapse = "_")
      timestamp <- parts[length(parts)-1]
      
      # 構建路徑前綴
      path_prefix <- file.path(models_dir, paste(model_type, dataset_type, station_name, timestamp, sep = "_"))
      
      # 檢查相關檔案是否存在
      importance_file <- paste0(path_prefix, "_importance.csv")
      original_importance_file <- paste0(path_prefix, "_original_importance.csv")
      
      models_info <- rbindlist(list(models_info, data.table(
        id = paste(model_type, dataset_type, station_name, timestamp, sep = "_"),
        model_type = model_type,
        dataset_type = dataset_type,
        station_name = station_name,
        timestamp = timestamp,
        path_prefix = path_prefix,
        complete_file = file_path,
        importance_file = if(file.exists(importance_file)) importance_file else NA,
        original_importance_file = if(file.exists(original_importance_file)) original_importance_file else NA,
        exists_importance = file.exists(importance_file),
        exists_original_importance = file.exists(original_importance_file)
      )))
    }
  }
  
  cat("✅ 掃描完成:", nrow(models_info), "個模型\n")
  cat("  LightGBM:", sum(models_info$model_type == "lgbm"), "個\n")
  cat("  LSTM:", sum(models_info$model_type == "lstm"), "個\n")
  
  return(models_info)
}

#' 創建模型註冊表
#' @param models_info 模型資訊表
#' @param output_path 輸出路徑
#' @return 擴展的模型註冊表
create_model_registry <- function(models_info, output_path = "model_outputs/explain/model_registry.tsv") {
  cat("📋 創建模型註冊表...\n")
  
  # 創建輸出目錄
  output_dir <- dirname(output_path)
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  registry <- models_info[, .(id, model_type, dataset_type, station_name, timestamp, path_prefix)]
  
  # 添加模型詳細資訊欄位
  registry$n_features <- NA_integer_
  registry$best_iter_epoch <- NA_integer_
  registry$train_rmse <- NA_real_
  registry$test_rmse <- NA_real_
  registry$model_size_mb <- NA_real_
  registry$has_importance <- FALSE
  registry$has_original_importance <- FALSE
  
  # 載入每個模型的詳細資訊
  for(i in 1:nrow(registry)) {
    tryCatch({
      complete_file <- models_info[i, complete_file]
      model_obj <- readRDS(complete_file)
      
      # 提取模型資訊
      if(registry[i, model_type] == "lgbm") {
        if(!is.null(model_obj$model) && !is.null(model_obj$model$params) && !is.null(model_obj$model$params$num_feature)) {
          registry$n_features[i] <- as.integer(model_obj$model$params$num_feature)
        }
        if(!is.null(model_obj$model) && !is.null(model_obj$model$best_iter)) {
          registry$best_iter_epoch[i] <- as.integer(model_obj$model$best_iter)
        }
        if(!is.null(model_obj$metrics)) {
          if(!is.null(model_obj$metrics$train_rmse)) {
            registry$train_rmse[i] <- as.numeric(model_obj$metrics$train_rmse)
          }
          if(!is.null(model_obj$metrics$test_rmse)) {
            registry$test_rmse[i] <- as.numeric(model_obj$metrics$test_rmse)
          }
        }
        registry$has_importance[i] <- models_info[i, exists_importance]
        registry$has_original_importance[i] <- models_info[i, exists_original_importance]
        
      } else if(registry[i, model_type] == "lstm") {
        if(!is.null(model_obj$architecture) && !is.null(model_obj$architecture$input_size)) {
          registry$n_features[i] <- as.integer(model_obj$architecture$input_size)
        }
        if(!is.null(model_obj$training_info) && !is.null(model_obj$training_info$best_epoch)) {
          registry$best_iter_epoch[i] <- as.integer(model_obj$training_info$best_epoch)
        }
        if(!is.null(model_obj$metrics)) {
          if(!is.null(model_obj$metrics$train_rmse)) {
            registry$train_rmse[i] <- as.numeric(model_obj$metrics$train_rmse)
          }
          if(!is.null(model_obj$metrics$test_rmse)) {
            registry$test_rmse[i] <- as.numeric(model_obj$metrics$test_rmse)
          }
        }
      }
      
      # 檔案大小
      file_info <- file.info(complete_file)
      if(!is.na(file_info$size)) {
        registry$model_size_mb[i] <- round(file_info$size / 1024^2, 2)
      }
      
    }, error = function(e) {
      cat("⚠️ 處理模型失敗:", registry[i, id], "-", e$message, "\n")
    })
    
    # 顯示進度
    if(i %% 10 == 0 || i == nrow(registry)) {
      cat("  進度:", i, "/", nrow(registry), "\n")
    }
  }
  
  # 保存註冊表
  fwrite(registry, output_path, sep = "\t")
  
  cat("✅ 模型註冊表已保存:", output_path, "\n")
  cat("  總模型數:", nrow(registry), "\n")
  
  valid_rmse <- registry[!is.na(test_rmse), test_rmse]
  if(length(valid_rmse) > 0) {
    cat("  平均測試RMSE:", round(mean(valid_rmse), 4), "\n")
  }
  
  return(registry)
}

# ================================================================================
# 2. LightGBM 解釋流程 (簡化版)
# ================================================================================

#' LightGBM 特徵重要度分析 (簡化版)
#' @param model_info 單個模型資訊
#' @param output_dir 輸出目錄
#' @return 重要度分析結果
analyze_lgbm_importance_simple <- function(model_info, output_dir = "model_outputs/explain/") {
  cat("🌳 分析 LightGBM 重要度:", model_info$id, "\n")
  
  results <- list()
  
  # 原始特徵重要度
  if(!is.na(model_info$original_importance_file) && file.exists(model_info$original_importance_file)) {
    importance_orig <- fread(model_info$original_importance_file)
    
    # Top-30 原始特徵
    top_orig <- head(importance_orig[order(-total_gain)], 30)
    results$original_importance <- top_orig
    
    # 保存重要度摘要
    summary_file <- file.path(output_dir, paste0("importance_summary_", model_info$id, ".csv"))
    fwrite(top_orig, summary_file)
    
    cat("  ✅ 原始重要度:", nrow(importance_orig), "個特徵\n")
    cat("  📁 摘要已保存:", basename(summary_file), "\n")
  }
  
  return(results)
}

# ================================================================================
# 3. 統整報告生成 (簡化版)
# ================================================================================

#' 生成簡化版解釋報告
#' @param registry 模型註冊表
#' @param output_dir 輸出目錄
generate_simple_report <- function(registry, output_dir = "model_outputs/explain/") {
  cat("📊 生成簡化版解釋報告...\n")
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 生成Markdown報告
  report_file <- file.path(output_dir, "explanation_report_simple.md")
  
  # 計算統計資訊
  total_models <- nrow(registry)
  lgbm_models <- sum(registry$model_type == "lgbm")
  lstm_models <- sum(registry$model_type == "lstm")
  dataset_types <- unique(registry$dataset_type)
  
  valid_rmse <- registry[!is.na(test_rmse), test_rmse]
  avg_rmse <- if(length(valid_rmse) > 0) mean(valid_rmse) else NA
  
  best_model <- registry[which.min(test_rmse)]
  
  report_content <- c(
    "# AQI 模型解釋性分析報告 (簡化版)",
    "",
    paste("**生成時間**:", Sys.time()),
    "",
    "## 模型總覽",
    "",
    paste("- 總模型數:", total_models),
    paste("- LightGBM模型:", lgbm_models),
    paste("- LSTM模型:", lstm_models),
    paste("- 資料集類型:", paste(dataset_types, collapse = ", ")),
    ifelse(is.na(avg_rmse), "- 平均測試RMSE: 無法計算", paste("- 平均測試RMSE:", round(avg_rmse, 4))),
    ifelse(nrow(best_model) == 0, "- 最佳模型: 無法確定", 
           paste("- 最佳模型:", best_model$id, "(RMSE:", round(best_model$test_rmse, 4), ")")),
    "",
    "## 性能統計",
    ""
  )
  
  # 添加性能統計表
  if(nrow(registry[!is.na(test_rmse)]) > 0) {
    perf_stats <- registry[!is.na(test_rmse), .(
      count = .N,
      mean_rmse = round(mean(test_rmse), 4),
      min_rmse = round(min(test_rmse), 4),
      max_rmse = round(max(test_rmse), 4)
    ), by = .(dataset_type, model_type)]
    
    report_content <- c(report_content,
      "| 資料集類型 | 模型類型 | 模型數 | 平均RMSE | 最小RMSE | 最大RMSE |",
      "|-----------|----------|--------|----------|----------|----------|"
    )
    
    for(i in 1:nrow(perf_stats)) {
      row <- perf_stats[i]
      report_content <- c(report_content,
        sprintf("| %s | %s | %d | %.4f | %.4f | %.4f |",
          row$dataset_type, row$model_type, row$count,
          row$mean_rmse, row$min_rmse, row$max_rmse)
      )
    }
  }
  
  report_content <- c(report_content,
    "",
    "## 特徵重要度分析",
    "",
    "LightGBM模型的特徵重要度分析結果已保存為CSV檔案。",
    "",
    "## 檔案說明",
    "",
    "- `model_registry.tsv`: 完整的模型註冊表",
    "- `importance_summary_*.csv`: 各模型的特徵重要度摘要",
    "- `explanation_report_simple.md`: 本報告",
    "",
    "---",
    "",
    "*本報告由AQI模型解釋系統自動生成*"
  )
  
  # 寫入檔案
  writeLines(report_content, report_file)
  
  cat("✅ 簡化版報告已保存:", report_file, "\n")
}

# ================================================================================
# 4. 主要管線函數 (簡化版)
# ================================================================================

#' 執行簡化版解釋性分析管線
#' @param models_dir 模型目錄
#' @param output_dir 輸出目錄
#' @param verbose 詳細輸出
run_simple_explanation_pipeline <- function(models_dir = "model_outputs/models/", 
                                           output_dir = "model_outputs/explain/",
                                           verbose = TRUE) {
  
  pipeline_start_time <- Sys.time()
  
  if(verbose) {
    cat("🔍 ================================================================================\n")
    cat("🚀 開始執行簡化版模型解釋性分析管線\n")
    cat("================================================================================\n")
  }
  
  # 1. 統一載入層
  if(verbose) cat("📂 步驟1: 掃描模型輸出...\n")
  models_info <- scan_model_outputs(models_dir)
  
  if(verbose) cat("📋 步驟2: 創建模型註冊表...\n")
  registry <- create_model_registry(models_info, file.path(output_dir, "model_registry.tsv"))
  
  # 2. 簡化版重要度分析
  if(verbose) cat("🌳 步驟3: 分析LightGBM重要度...\n")
  lgbm_models <- registry[model_type == "lgbm" & has_original_importance == TRUE]
  
  if(nrow(lgbm_models) > 0) {
    for(i in 1:nrow(lgbm_models)) {
      model_info <- lgbm_models[i]
      analyze_lgbm_importance_simple(model_info, output_dir)
    }
  } else {
    cat("  ⚠️ 未找到具有重要度檔案的LightGBM模型\n")
  }
  
  # 3. 生成簡化版報告
  if(verbose) cat("📊 步驟4: 生成簡化版報告...\n")
  generate_simple_report(registry, output_dir)
  
  pipeline_end_time <- Sys.time()
  total_time <- as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins"))
  
  if(verbose) {
    cat("\n🎉 ================================================================================\n")
    cat("✅ 簡化版解釋性分析管線執行完成！\n")
    cat("⏱️  總耗時:", round(total_time, 2), "分鐘\n")
    cat("📊 分析模型數:", nrow(registry), "\n")
    cat("📁 輸出目錄:", output_dir, "\n")
    cat("================================================================================\n")
  }
  
  return(list(
    registry = registry,
    execution_time = total_time
  ))
}

cat("✅ 簡化版模型解析與可解釋性分析模組載入完成\n") 