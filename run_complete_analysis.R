#!/usr/bin/env Rscript
# ================================================================================
# AQI模型解釋性分析系統 - 完整分析執行腳本
# ================================================================================
# 功能: 整合模型解釋、比較分析、報告生成
# 作者: AQI分析系統
# 版本: 3.0
# 更新: 2024-12-19
# ================================================================================

cat("🎯 AQI模型解釋性分析系統 v3.0\n")
cat("================================================================================\n")

# ================================================================================
# 1. 載入必要套件和模組
# ================================================================================

# 檢查並安裝缺失套件
required_packages <- c("data.table", "ggplot2", "lightgbm", "torch")
optional_packages <- c("SHAPforxgboost")

for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("⚠️ 安裝缺失套件:", pkg, "\n")
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# 可選套件（SHAP分析用）
shap_available <- FALSE
if(require("SHAPforxgboost", quietly = TRUE)) {
  shap_available <- TRUE
  cat("✅ SHAP分析功能可用\n")
} else {
  cat("⚠️ SHAP套件未安裝，將跳過SHAP分析\n")
}

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(lightgbm)
  library(torch)
})

# 載入分析模組
source("model_src/explainer_advanced.R")
source("model_src/lstm_explainer.R")

cat("✅ 所有模組載入完成\n\n")

# ================================================================================
# 2. 主要分析函數
# ================================================================================

#' 執行完整模型分析
#' @param output_dir 輸出目錄
#' @param max_models 最大分析模型數量
#' @param include_comparison 是否包含模型比較
#' @return 分析結果
run_complete_analysis <- function(output_dir = "analysis_outputs/", max_models = 10, include_comparison = TRUE) {
  
  cat("🚀 開始完整模型分析流程...\n")
  cat("📁 輸出目錄:", output_dir, "\n")
  cat("📊 最大分析模型數:", max_models, "\n")
  cat("🔍 包含模型比較:", include_comparison, "\n\n")
  
  # 確保輸出目錄存在
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✅ 創建輸出目錄:", output_dir, "\n")
  }
  
  # ================================================================================
  # 步驟1: 掃描和註冊模型
  # ================================================================================
  cat("📋 步驟1: 掃描和註冊模型...\n")
  
  # 掃描模型輸出
  models_info <- scan_model_outputs("model_outputs/models/")
  
  # 創建模型註冊表
  registry <- create_model_registry(models_info)
  
  if(nrow(registry) == 0) {
    cat("❌ 未找到任何模型，請檢查模型目錄\n")
    return(NULL)
  }
  
  cat("✅ 找到", nrow(registry), "個模型\n")
  cat("   - LightGBM:", sum(registry$model_type == "lgbm"), "個\n")
  cat("   - LSTM:", sum(registry$model_type == "lstm"), "個\n")
  cat("   - 可分析:", sum(registry$has_importance == "TRUE"), "個\n\n")
  
  # ================================================================================
  # 步驟2: 執行模型解釋分析
  # ================================================================================
  cat("🔍 步驟2: 執行模型解釋分析...\n")
  
  analysis_results <- list()
  analyzed_count <- 0
  
  # 分析LightGBM模型
  lgbm_models <- registry[model_type == "lgbm" & has_importance == "TRUE"]
  if(nrow(lgbm_models) > 0) {
    cat("🌳 分析LightGBM模型...\n")
    
    for(i in 1:min(max_models, nrow(lgbm_models))) {
      model_info <- lgbm_models[i]
      cat("   分析模型:", model_info$id, "\n")
      
             tryCatch({
         # 使用進階LightGBM分析
         lgbm_result <- analyze_lgbm_advanced(
           model_info,
           output_dir,
           enable_shap = shap_available
         )
         
         analysis_results[[paste0("lgbm_", model_info$id)]] <- lgbm_result
         analyzed_count <- analyzed_count + 1
         
       }, error = function(e) {
         cat("   ⚠️ 分析失敗:", e$message, "\n")
       })
    }
  }
  
  # 分析LSTM模型
  lstm_models <- registry[model_type == "lstm"]
  if(nrow(lstm_models) > 0) {
    cat("🧠 分析LSTM模型...\n")
    
    for(i in 1:min(max_models, nrow(lstm_models))) {
      model_info <- lstm_models[i]
      cat("   分析模型:", model_info$id, "\n")
      
             tryCatch({
         lstm_result <- analyze_lstm_advanced(
           model_info,
           output_dir
         )
         
         analysis_results[[paste0("lstm_", model_info$id)]] <- lstm_result
         analyzed_count <- analyzed_count + 1
         
       }, error = function(e) {
         cat("   ⚠️ 分析失敗:", e$message, "\n")
       })
    }
  }
  
  cat("✅ 完成", analyzed_count, "個模型的解釋分析\n\n")
  
  # ================================================================================
  # 步驟3: 模型比較分析
  # ================================================================================
  comparison_results <- NULL
  if(include_comparison) {
    cat("🔍 步驟3: 執行模型比較分析...\n")
    
    tryCatch({
      comparison_results <- analyze_model_comparison(registry, output_dir)
      cat("✅ 模型比較分析完成\n\n")
    }, error = function(e) {
      cat("⚠️ 模型比較分析失敗:", e$message, "\n\n")
    })
  }
  
  # ================================================================================
  # 步驟4: 生成報告
  # ================================================================================
  cat("📊 步驟4: 生成分析報告...\n")
  
  # 生成基本HTML報告
  basic_report <- generate_enhanced_html_report(registry, analysis_results, NULL, output_dir)
  cat("✅ 基本HTML報告:", basic_report, "\n")
  
  # 生成增強版HTML報告（包含比較）
  if(!is.null(comparison_results)) {
    enhanced_report <- generate_enhanced_html_report(
      registry, 
      analysis_results, 
      comparison_results, 
      output_dir
    )
    cat("✅ 增強版HTML報告:", enhanced_report, "\n")
  }
  
  # 生成Markdown報告
  md_report <- generate_markdown_report(registry, output_dir)
  cat("✅ Markdown報告:", md_report, "\n")
  
  # ================================================================================
  # 步驟5: 生成總結
  # ================================================================================
  cat("\n📋 分析總結:\n")
  cat("================================================================================\n")
  cat("📊 總模型數:", nrow(registry), "\n")
  cat("🔍 已分析模型:", analyzed_count, "\n")
  cat("🌳 LightGBM模型:", sum(registry$model_type == "lgbm"), "個\n")
  cat("🧠 LSTM模型:", sum(registry$model_type == "lstm"), "個\n")
  cat("✅ 可分析率:", round(sum(registry$has_importance == "TRUE") / nrow(registry) * 100, 1), "%\n")
  cat("📁 輸出目錄:", output_dir, "\n")
  
  # 列出生成的檔案
  output_files <- list.files(output_dir, full.names = FALSE)
  cat("\n📄 生成檔案 (", length(output_files), "個):\n")
  for(file in head(output_files, 10)) {
    cat("   -", file, "\n")
  }
  if(length(output_files) > 10) {
    cat("   ... 還有", length(output_files) - 10, "個檔案\n")
  }
  
  cat("\n🎉 完整分析流程執行完成！\n")
  cat("================================================================================\n")
  
  # 返回結果
  return(list(
    registry = registry,
    analysis_results = analysis_results,
    comparison_results = comparison_results,
    output_dir = output_dir,
    analyzed_count = analyzed_count
  ))
}

#' 快速分析模式（僅分析前幾個模型）
#' @param max_models 最大分析模型數
#' @param output_dir 輸出目錄
quick_analysis <- function(max_models = 3, output_dir = "quick_analysis/") {
  cat("⚡ 快速分析模式\n")
  return(run_complete_analysis(output_dir, max_models, include_comparison = FALSE))
}

#' 深度分析模式（包含所有功能）
#' @param output_dir 輸出目錄
deep_analysis <- function(output_dir = "deep_analysis/") {
  cat("🔬 深度分析模式\n")
  return(run_complete_analysis(output_dir, max_models = 20, include_comparison = TRUE))
}

# ================================================================================
# 3. 命令行執行
# ================================================================================

# 檢查是否為命令行執行
if(!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if(length(args) == 0) {
    # 預設執行完整分析
    cat("🎯 執行預設完整分析...\n")
    result <- run_complete_analysis()
    
  } else if(args[1] == "quick") {
    # 快速分析
    cat("⚡ 執行快速分析...\n")
    result <- quick_analysis()
    
  } else if(args[1] == "deep") {
    # 深度分析
    cat("🔬 執行深度分析...\n")
    result <- deep_analysis()
    
  } else {
    cat("❌ 未知參數:", args[1], "\n")
    cat("可用選項: quick, deep\n")
    cat("或直接執行進行預設分析\n")
  }
}

cat("✅ 完整分析系統載入完成\n")
cat("💡 使用方法:\n")
cat("   - run_complete_analysis()  # 完整分析\n")
cat("   - quick_analysis()         # 快速分析\n")
cat("   - deep_analysis()          # 深度分析\n") 