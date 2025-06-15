#!/usr/bin/env Rscript
# ================================================================================
# AQI 模型訓練系統 - 主執行腳本
# ================================================================================
# 
# 使用方法:
#   Rscript run_aqi_training.R                    # 執行完整訓練 (LightGBM + LSTM)
#   Rscript run_aqi_training.R --models lgbm      # 只訓練 LightGBM
#   Rscript run_aqi_training.R --models lstm      # 只訓練 LSTM
#   Rscript run_aqi_training.R --max-files 5      # 限制每種資料類型最多5個檔案
#
# ================================================================================

cat("🚀 AQI 模型訓練系統啟動\n")
cat("================================================================================\n")

# 載入主要pipeline
source("model_src/pipeline.R")

# 解析命令列參數
args <- commandArgs(trailingOnly = TRUE)

# 預設參數
models <- c("lgbm", "lstm")
max_files <- NULL
verbose <- TRUE

# 解析參數
if (length(args) > 0) {
  i <- 1
  while (i <= length(args)) {
    if (args[i] == "--models") {
      if (i + 1 <= length(args)) {
        models <- unlist(strsplit(args[i + 1], ","))
        i <- i + 2
      } else {
        stop("--models 參數需要指定模型類型")
      }
    } else if (args[i] == "--max-files") {
      if (i + 1 <= length(args)) {
        max_files <- as.numeric(args[i + 1])
        i <- i + 2
      } else {
        stop("--max-files 參數需要指定數字")
      }
    } else if (args[i] == "--quiet") {
      verbose <- FALSE
      i <- i + 1
    } else {
      warning("未知參數: ", args[i])
      i <- i + 1
    }
  }
}

# 顯示配置
if (verbose) {
  cat("📋 訓練配置:\n")
  cat("  模型類型:", paste(models, collapse = ", "), "\n")
  cat("  最大檔案數:", ifelse(is.null(max_files), "無限制", max_files), "\n")
  cat("  詳細輸出:", verbose, "\n")
  cat("================================================================================\n")
}

# 執行訓練
tryCatch({
  
  # 檢查環境
  if (verbose) {
    cat("🔍 檢查訓練環境...\n")
    check_training_environment()
  }
  
  # 執行完整pipeline
  result <- run_full_pipeline(
    models = models,
    max_files = max_files,
    verbose = verbose
  )
  
  # 顯示結果摘要
  if (verbose) {
    cat("\n================================================================================\n")
    cat("🎉 訓練完成！\n")
    cat("⏱️  總執行時間:", round(result$total_time, 2), "分鐘\n")
    cat("📊 處理結果:\n")
    
    for (dtype in names(result$results)) {
      if (!is.null(result$results[[dtype]])) {
        successful_models <- 0
        total_files <- length(result$results[[dtype]])
        
        for (file_result in result$results[[dtype]]) {
          if (!is.null(file_result$models) && length(file_result$models) > 0) {
            successful_models <- successful_models + length(file_result$models)
          }
        }
        
        cat("  ", toupper(dtype), ":", total_files, "檔案,", successful_models, "個成功模型\n")
      }
    }
    
    cat("💾 結果已儲存至 model_outputs/ 目錄\n")
    cat("================================================================================\n")
  }
  
}, error = function(e) {
  cat("❌ 訓練失敗:", e$message, "\n")
  cat("請檢查:\n")
  cat("  1. 資料檔案是否存在於 data/ 目錄\n")
  cat("  2. 所有必要套件是否已安裝\n")
  cat("  3. 系統記憶體是否足夠\n")
  quit(status = 1)
})

cat("✅ 程式執行完成\n") 