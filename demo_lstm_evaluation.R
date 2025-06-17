#!/usr/bin/env Rscript
# ================================================================================
# AQI LSTM 模型專用評估系統
# ================================================================================

cat("🧠 AQI LSTM 模型評估系統啟動...\n")
cat("📅 執行時間:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# ================================================================================
# 1. LSTM 專用配置
# ================================================================================

configure_lstm_mode <- function() {
  cat("\n⚙️ 設定 LSTM 評估模式...\n")
  
  # 核心設定
  Sys.setenv(SHAP_MODE = Sys.getenv("SHAP_MODE", "fast"))
  Sys.setenv(AQI_FULL_EVAL = "true")
  Sys.setenv(MAX_MEMORY_GB = "55")
  
  # 並行處理設定
  if(.Platform$OS.type != "windows") {
    Sys.setenv(USE_PARALLEL = "true")
    Sys.setenv(PARALLEL_CORES = "16")
    cat("   🚀 並行處理: 啟用 (16 核心)\n")
  } else {
    Sys.setenv(USE_PARALLEL = "false")
    cat("   ⚠️ 並行處理: 停用 (Windows 不支援)\n")
  }
  
  cat("✅ LSTM 評估模式配置完成:\n")
  cat("   🧠 專注於: LSTM 模型分析\n")
  cat("   🔥 SHAP 模式:", Sys.getenv("SHAP_MODE"), "\n")
  cat("   💾 記憶體控制: 55GB 限制\n")
}

# ================================================================================
# 2. LSTM 模型篩選和分析
# ================================================================================

analyze_lstm_models <- function(max_models = NULL) {
  cat("\n🧠 開始 LSTM 模型分析...\n")
  
  # 載入分析模組
  source("model_src/explainer.R")
  
  # 生成註冊表
  registry <- generate_model_registry()
  
  # 篩選 LSTM 模型
  lstm_models <- registry[model_type == "lstm"]
  
  if(nrow(lstm_models) == 0) {
    cat("❌ 沒有找到 LSTM 模型\n")
    return(NULL)
  }
  
  cat("✅ 找到", nrow(lstm_models), "個 LSTM 模型\n")
  
  # 檢查有效模型
  valid_lstm <- lstm_models[!is.na(test_rmse)]
  cat("📊 有效 LSTM 模型 (有 RMSE):", nrow(valid_lstm), "個\n")
  
  if(nrow(valid_lstm) == 0) {
    cat("⚠️ 所有 LSTM 模型都沒有有效的 RMSE 值\n")
    cat("💡 將嘗試分析前", min(10, nrow(lstm_models)), "個 LSTM 模型...\n")
    models_to_analyze <- head(lstm_models, min(10, nrow(lstm_models)))
  } else {
    # 按 RMSE 排序
    valid_lstm <- valid_lstm[order(test_rmse)]
    if(!is.null(max_models)) {
      models_to_analyze <- head(valid_lstm, max_models)
    } else {
      models_to_analyze <- valid_lstm
    }
  }
  
  cat("🎯 將分析", nrow(models_to_analyze), "個 LSTM 模型\n")
  
  # 顯示要分析的模型
  if(nrow(models_to_analyze) > 0) {
    cat("\n📋 分析清單:\n")
    for(i in 1:min(5, nrow(models_to_analyze))) {
      model <- models_to_analyze[i]
      rmse_text <- if(!is.na(model$test_rmse)) paste("RMSE:", round(model$test_rmse, 4)) else "RMSE: NA"
      cat(sprintf("   %d. %s (%s)\n", i, model$model_id, rmse_text))
    }
    if(nrow(models_to_analyze) > 5) {
      cat(sprintf("   ... 還有 %d 個模型\n", nrow(models_to_analyze) - 5))
    }
  }
  
  # 執行分析
  results <- run_model_analysis(models_to_analyze, n_top_models = nrow(models_to_analyze))
  
  return(list(
    models_analyzed = models_to_analyze,
    results = results,
    total_lstm = nrow(lstm_models),
    valid_lstm = nrow(valid_lstm)
  ))
}

# ================================================================================
# 3. 混合模型分析 (LGBM + LSTM)
# ================================================================================

analyze_mixed_models <- function(max_lgbm = 10, max_lstm = 10) {
  cat("\n⚖️ 開始混合模型分析 (LGBM + LSTM)...\n")
  
  # 載入分析模組
  source("model_src/explainer.R")
  
  # 生成註冊表
  registry <- generate_model_registry()
  
  # 分別篩選模型
  lgbm_models <- registry[model_type == "lgbm" & !is.na(test_rmse)][order(test_rmse)]
  lstm_models <- registry[model_type == "lstm" & !is.na(test_rmse)][order(test_rmse)]
  
  # 取前 N 個最佳模型
  top_lgbm <- head(lgbm_models, max_lgbm)
  top_lstm <- head(lstm_models, max_lstm)
  
  # 合併分析
  mixed_models <- rbind(top_lgbm, top_lstm)
  
  cat("📊 混合分析統計:\n")
  cat("   🌳 LGBM 模型:", nrow(top_lgbm), "個\n")
  cat("   🧠 LSTM 模型:", nrow(top_lstm), "個\n")
  cat("   📈 總計:", nrow(mixed_models), "個\n")
  
  if(nrow(mixed_models) == 0) {
    cat("❌ 沒有有效模型可分析\n")
    return(NULL)
  }
  
  # 執行分析
  results <- run_model_analysis(mixed_models, n_top_models = nrow(mixed_models))
  
  return(list(
    models_analyzed = mixed_models,
    results = results,
    lgbm_count = nrow(top_lgbm),
    lstm_count = nrow(top_lstm)
  ))
}

# ================================================================================
# 4. 主程式
# ================================================================================

main_lstm <- function(mode = "lstm_only", max_models = 20) {
  cat("🧠 LSTM 評估主程式啟動\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    # 1. 配置模式
    configure_lstm_mode()
    
    # 2. 根據模式執行分析
    if(mode == "lstm_only") {
      cat("\n🎯 模式: 純 LSTM 分析\n")
      results <- analyze_lstm_models(max_models)
    } else if(mode == "mixed") {
      cat("\n🎯 模式: 混合模型分析\n")
      results <- analyze_mixed_models(max_lgbm = 10, max_lstm = max_models)
    } else {
      stop("❌ 不支援的模式: ", mode)
    }
    
    if(is.null(results)) {
      cat("❌ 分析失敗或沒有可分析的模型\n")
      return(NULL)
    }
    
    # 3. 生成摘要
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    
    cat("\n🎉 LSTM 評估完成！\n")
    cat("⏱️ 總執行時間:", round(total_time, 2), "分鐘\n")
    
    if(mode == "lstm_only") {
      cat("🧠 分析 LSTM 模型:", results$total_lstm, "個 (有效:", results$valid_lstm, "個)\n")
    } else {
      cat("🌳 LGBM 模型:", results$lgbm_count, "個\n")
      cat("🧠 LSTM 模型:", results$lstm_count, "個\n")
    }
    
    # 清理資源
    cleanup_memory(verbose = TRUE)
    
    return(results)
    
  }, error = function(e) {
    cat("\n❌ LSTM 評估失敗:\n")
    cat("錯誤訊息:", e$message, "\n")
    return(NULL)
  })
}

# ================================================================================
# 執行選項
# ================================================================================

# 從命令列參數獲取模式和模型數
args <- commandArgs(trailingOnly = TRUE)
mode <- if(length(args) > 0) args[1] else "lstm_only"
max_models <- if(length(args) > 1) as.numeric(args[2]) else 20

cat("\n💡 使用方法:\n")
cat("  Rscript demo_lstm_evaluation.R [模式] [最大模型數]\n")
cat("  模式選項:\n")
cat("    lstm_only - 只分析 LSTM 模型 (預設)\n")
cat("    mixed     - 混合分析 LGBM + LSTM\n")
cat("  範例:\n")
cat("    Rscript demo_lstm_evaluation.R lstm_only 30\n")
cat("    Rscript demo_lstm_evaluation.R mixed 15\n")
cat("\n")

if(!interactive()) {
  # 腳本模式執行
  cat("🎯 執行模式:", mode, "| 最大模型數:", max_models, "\n")
  results <- main_lstm(mode, max_models)
} else {
  # 互動模式
  cat("💡 LSTM 評估系統已載入，執行 main_lstm('lstm_only', 20) 開始分析\n")
} 