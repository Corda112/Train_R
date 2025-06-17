#!/usr/bin/env Rscript
# ================================================================================
# 全量評估模式示範腳本 (增強版) - 包含記憶體控制
# ================================================================================
# 使用方法:
#   Rscript demo_full_evaluation_enhanced.R
#   或者:
#   Rscript --vanilla demo_full_evaluation_enhanced.R

cat("🚀 AQI 模型全量評估系統 (增強版) 啟動...\n")
cat("📅 執行時間:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# ================================================================================
# 1. 系統檢查
# ================================================================================

check_system_requirements <- function() {
  cat("\n🔍 檢查系統需求...\n")
  
  # 檢查 R 版本
  r_version <- paste(R.version$major, R.version$minor, sep = ".")
  cat("R 版本:", r_version, "\n")
  
  # 檢查記憶體
  if(.Platform$OS.type == "windows") {
    mem_info <- system("wmic OS get TotalVisibleMemorySize,FreePhysicalMemory /value", intern = TRUE)
    mem_lines <- mem_info[nzchar(mem_info)]
    
    total_kb <- as.numeric(gsub(".*=", "", mem_lines[grep("TotalVisibleMemorySize", mem_lines)]))
    free_kb <- as.numeric(gsub(".*=", "", mem_lines[grep("FreePhysicalMemory", mem_lines)]))
    
    total_gb <- total_kb / (1024^2)
    free_gb <- free_kb / (1024^2)
    
    cat(sprintf("系統記憶體: %.1f GB 總計, %.1f GB 可用 (%.1f%%)\n", 
               total_gb, free_gb, (free_gb/total_gb)*100))
    
    if(free_gb < 2.0) {
      warning("⚠️ 可用記憶體不足 2GB，建議關閉其他程式")
      return(FALSE)
    }
  }
  
  # 檢查必要套件
  required_packages <- c("data.table", "ggplot2", "lightgbm")
  optional_packages <- c("torch", "plotly", "htmlwidgets", "knitr")
  
  missing_required <- setdiff(required_packages, rownames(installed.packages()))
  if(length(missing_required) > 0) {
    stop("❌ 缺少必要套件: ", paste(missing_required, collapse = ", "))
  }
  
  missing_optional <- setdiff(optional_packages, rownames(installed.packages()))
  if(length(missing_optional) > 0) {
    cat("⚠️ 缺少選用套件 (部分功能可能不可用):", paste(missing_optional, collapse = ", "), "\n")
  }
  
  cat("✅ 系統需求檢查完成\n")
  return(TRUE)
}

# ================================================================================
# 2. 載入模組
# ================================================================================

load_modules <- function() {
  cat("\n📦 載入分析模組...\n")
  
  # 檢查模組檔案
  module_files <- c(
    "model_src/explainer.R",
    "model_src/config.R"
  )
  
  missing_modules <- module_files[!file.exists(module_files)]
  if(length(missing_modules) > 0) {
    stop("❌ 找不到必要模組: ", paste(missing_modules, collapse = ", "))
  }
  
  # 載入模組
  tryCatch({
    source("model_src/explainer.R")
    cat("✅ 模組載入完成\n")
  }, error = function(e) {
    stop("❌ 模組載入失敗: ", e$message)
  })
}

# ================================================================================
# 3. 全量評估模式選項
# ================================================================================

configure_full_evaluation <- function() {
  cat("\n⚙️ 設定全量評估模式...\n")
  
  # 設定環境變數
  Sys.setenv(AQI_FULL_EVAL = "true")
  Sys.setenv(ANALYSIS_MODE = "full")
  
  cat("✅ 已啟用全量評估模式\n")
  cat("📊 將分析所有測站的 LGBM 和 LSTM 模型\n")
  cat("⏱️ 預估執行時間: 20-60 分鐘 (依系統效能而定)\n")
  
  # 提示記憶體使用
  cat("\n💡 記憶體使用提示:\n")
  cat("   - 系統將自動監控記憶體使用狀況\n")
  cat("   - 當可用記憶體低於 2GB 時會自動清理\n")
  cat("   - 當可用記憶體低於 1.5GB 時會跳過剩餘分析\n")
  cat("   - 建議執行期間不要開啟其他大型程式\n")
}

# ================================================================================
# 4. 執行全量評估
# ================================================================================

run_full_evaluation <- function() {
  cat("\n🚀 開始全量評估...\n")
  
  start_time <- Sys.time()
  
  tryCatch({
    # 生成模型註冊表
    cat("\n📋 生成完整模型註冊表...\n")
    registry <- generate_model_registry()
    
    if(nrow(registry) == 0) {
      stop("❌ 沒有找到任何模型檔案")
    }
    
    cat(sprintf("✅ 找到 %d 個模型\n", nrow(registry)))
    cat(sprintf("   - LGBM: %d 個\n", sum(registry$model_type == "lgbm")))
    cat(sprintf("   - LSTM: %d 個\n", sum(registry$model_type == "lstm")))
    cat(sprintf("   - 有效 RMSE: %d 個\n", sum(!is.na(registry$test_rmse))))
    
    # 執行 7 區塊分析
    cat("\n📊 執行完整的 7 區塊分析...\n")
    results <- run_model_analysis(registry, n_top_models = nrow(registry))
    
    # 生成全量評估摘要
    cat("\n📋 生成全量評估摘要...\n")
    full_summary <- generate_full_evaluation_summary(registry, "analysis_outputs")
    
    end_time <- Sys.time()
    total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
    
    cat("\n🎉 全量評估完成！\n")
    cat(sprintf("⏱️ 總執行時間: %.1f 分鐘\n", total_time))
    
    return(list(
      registry = registry,
      results = results,
      full_summary = full_summary,
      execution_time = total_time
    ))
    
  }, error = function(e) {
    cat("\n❌ 全量評估過程中發生錯誤:\n")
    cat("錯誤訊息:", e$message, "\n")
    cat("建議檢查:\n")
    cat("  1. 系統記憶體是否充足\n")
    cat("  2. 模型檔案是否完整\n")
    cat("  3. 磁碟空間是否充足\n")
    return(NULL)
  })
}

# ================================================================================
# 5. 結果摘要
# ================================================================================

summarize_results <- function(results) {
  if(is.null(results)) {
    cat("\n❌ 沒有結果可以摘要\n")
    return(invisible(NULL))
  }
  
  cat("\n📊 全量評估結果摘要:\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # 模型數量統計
  registry <- results$registry
  cat(sprintf("📈 模型統計:\n"))
  cat(sprintf("   總模型數: %d\n", nrow(registry)))
  cat(sprintf("   LGBM 模型: %d\n", sum(registry$model_type == "lgbm")))
  cat(sprintf("   LSTM 模型: %d\n", sum(registry$model_type == "lstm")))
  cat(sprintf("   有效模型: %d\n", sum(!is.na(registry$test_rmse))))
  
  # 最佳模型
  best_models <- registry[!is.na(test_rmse)][order(test_rmse)][1:5]
  cat("\n🏆 前 5 名最佳模型 (RMSE):\n")
  for(i in 1:min(5, nrow(best_models))) {
    model <- best_models[i]
    cat(sprintf("   %d. %s (%s) - RMSE: %.4f\n", 
               i, model$model_id, model$model_type, model$test_rmse))
  }
  
  # 輸出檔案位置
  cat("\n📁 輸出檔案位置:\n")
  output_files <- c(
    "analysis_outputs/full_evaluation_summary.md",
    "analysis_outputs/full_evaluation_combined_ranking.csv",
    "analysis_outputs/full_evaluation_lgbm_ranking.csv",
    "analysis_outputs/full_evaluation_lstm_ranking.csv",
    "analysis_outputs/full_evaluation_station_summary.csv",
    "analysis_outputs/registry/all_models_registry.csv"
  )
  
  for(file in output_files) {
    if(file.exists(file)) {
      size_kb <- round(file.info(file)$size / 1024, 1)
      cat(sprintf("   ✅ %s (%.1f KB)\n", file, size_kb))
    } else {
      cat(sprintf("   ❌ %s (未生成)\n", file))
    }
  }
  
  cat("\n💡 使用建議:\n")
  cat("   1. 查看 full_evaluation_summary.md 了解整體分析結果\n")
  cat("   2. 開啟 *_ranking.csv 檔案進行詳細比較\n")
  cat("   3. 檢查 analysis_outputs/lgbm/ 目錄下的圖表\n")
  cat("   4. 導入 Excel 進行進一步分析\n")
}

# ================================================================================
# 6. 主程式
# ================================================================================

main <- function() {
  cat("🔄 AQI 全量評估系統啟動\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  # 1. 系統檢查
  if(!check_system_requirements()) {
    cat("❌ 系統需求不符，程式終止\n")
    return(invisible(NULL))
  }
  
  # 2. 載入模組
  load_modules()
  
  # 3. 設定全量評估
  configure_full_evaluation()
  
  # 確認執行
  cat("\n❓ 確認要執行全量評估嗎？這可能需要 20-60 分鐘...\n")
  cat("   如果是自動執行，將在 10 秒後開始\n")
  
  # 自動模式或手動確認
  if(interactive()) {
    response <- readline("輸入 'y' 確認執行，其他鍵取消: ")
    if(tolower(response) != "y") {
      cat("❌ 用戶取消執行\n")
      return(invisible(NULL))
    }
  } else {
    # 非互動模式，等待 10 秒
    for(i in 10:1) {
      cat(sprintf("\r⏳ %d 秒後開始執行...", i))
      Sys.sleep(1)
    }
    cat("\n")
  }
  
  # 4. 執行全量評估
  results <- run_full_evaluation()
  
  # 5. 摘要結果
  summarize_results(results)
  
  cat("\n🎯 全量評估系統執行完成\n")
  return(invisible(results))
}

# ================================================================================
# 執行主程式
# ================================================================================

if(!interactive()) {
  # 腳本模式執行
  results <- main()
} else {
  # 互動模式執行
  cat("💡 全量評估系統已載入，執行 main() 開始分析\n")
} 