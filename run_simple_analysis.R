#!/usr/bin/env Rscript
# ================================================================================
# AQI模型解釋性分析系統 - 簡化版
# ================================================================================
# 功能: 直接使用現有檔案結構進行分析
# 版本: 1.0
# 更新: 2024-12-19
# ================================================================================

cat("🎯 AQI模型解釋性分析系統 - 簡化版\n")
cat("================================================================================\n")

# ================================================================================
# 1. 載入必要套件
# ================================================================================
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

cat("✅ 基本套件載入完成\n\n")

# ================================================================================
# 2. 掃描模型檔案
# ================================================================================

scan_models_simple <- function(models_dir = "model_outputs/models/") {
  cat("📂 掃描模型目錄:", models_dir, "\n")
  
  # 掃描所有檔案
  all_files <- list.files(models_dir, full.names = TRUE)
  
  # 分類檔案
  lgbm_complete <- grep("lgbm.*_complete\\.rds$", all_files, value = TRUE)
  lstm_complete <- grep("lstm.*_complete\\.rds$", all_files, value = TRUE)
  importance_files <- grep("_importance\\.csv$", all_files, value = TRUE)
  original_importance <- grep("_original_importance\\.csv$", all_files, value = TRUE)
  
  cat("✅ 找到檔案:\n")
  cat("   - LightGBM完整模型:", length(lgbm_complete), "個\n")
  cat("   - LSTM完整模型:", length(lstm_complete), "個\n")
  cat("   - 特徵重要度檔案:", length(importance_files), "個\n")
  cat("   - 原始重要度檔案:", length(original_importance), "個\n")
  
  return(list(
    lgbm_models = lgbm_complete,
    lstm_models = lstm_complete,
    importance_files = importance_files,
    original_importance = original_importance
  ))
}

# ================================================================================
# 3. 分析LightGBM特徵重要度
# ================================================================================

analyze_lgbm_importance_simple <- function(importance_file, model_id, output_dir = "analysis_outputs/") {
  cat("🌳 分析LightGBM重要度:", model_id, "\n")
  
  if(!file.exists(importance_file)) {
    cat("   ❌ 重要度檔案不存在\n")
    return(NULL)
  }
  
  # 讀取重要度數據
  importance_data <- fread(importance_file)
  
  # 檢查數據結構
  if(!"Feature" %in% names(importance_data)) {
    cat("   ⚠️ 重要度檔案格式不正確\n")
    return(NULL)
  }
  
  # 取前20個重要特徵
  top_features <- head(importance_data[order(-Gain)], 20)
  
  # 創建圖表
  p <- ggplot(top_features, aes(x = reorder(Feature, Gain), y = Gain)) +
    geom_col(fill = "darkgreen", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("特徵重要度分析 -", model_id),
      x = "特徵名稱",
      y = "Gain重要度",
      caption = "前20個最重要特徵"
    ) +
    theme_classic() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12)
    )
  
  # 保存圖表
  plot_file <- file.path(output_dir, paste0("importance_", gsub("[^A-Za-z0-9_-]", "_", model_id), ".png"))
  ggsave(plot_file, p, width = 12, height = 8, dpi = 300, bg = "white")
  
  # 保存數據
  data_file <- file.path(output_dir, paste0("importance_data_", gsub("[^A-Za-z0-9_-]", "_", model_id), ".csv"))
  fwrite(top_features, data_file)
  
  cat("   ✅ 分析完成，圖表:", basename(plot_file), "\n")
  
  return(list(
    data = top_features,
    plot_file = plot_file,
    data_file = data_file
  ))
}

# ================================================================================
# 4. 分析LSTM模型
# ================================================================================

analyze_lstm_simple <- function(model_file, model_id, output_dir = "analysis_outputs/") {
  cat("🧠 分析LSTM模型:", model_id, "\n")
  
  if(!file.exists(model_file)) {
    cat("   ❌ 模型檔案不存在\n")
    return(NULL)
  }
  
  tryCatch({
    # 載入模型
    model_obj <- readRDS(model_file)
    
    # 提取基本資訊
    info <- list(
      model_id = model_id,
      best_val_loss = model_obj$best_val_loss %||% NA,
      training_time = model_obj$training_time %||% NA,
      input_size = model_obj$input_size %||% NA,
      seq_len = model_obj$seq_len %||% NA
    )
    
    # 創建簡單的性能圖表
    if(!is.null(model_obj$training_history)) {
      history <- model_obj$training_history
      
      if(is.data.frame(history) && nrow(history) > 0) {
        p <- ggplot(history, aes(x = epoch)) +
          geom_line(aes(y = train_loss, color = "訓練損失"), linewidth = 1) +
          geom_line(aes(y = val_loss, color = "驗證損失"), linewidth = 1) +
          labs(
            title = paste("LSTM訓練歷史 -", model_id),
            x = "訓練週期",
            y = "損失值",
            color = "類型"
          ) +
          theme_classic() +
          theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            legend.position = "bottom"
          )
        
        # 保存圖表
        plot_file <- file.path(output_dir, paste0("lstm_history_", gsub("[^A-Za-z0-9_-]", "_", model_id), ".png"))
        ggsave(plot_file, p, width = 10, height = 6, dpi = 300, bg = "white")
        
        info$plot_file <- plot_file
      }
    }
    
    cat("   ✅ 分析完成\n")
    return(info)
    
  }, error = function(e) {
    cat("   ❌ 分析失敗:", e$message, "\n")
    return(NULL)
  })
}

# ================================================================================
# 5. 生成簡單報告
# ================================================================================

generate_simple_report <- function(results, output_dir = "analysis_outputs/") {
  cat("📊 生成分析報告...\n")
  
  # 統計結果
  lgbm_count <- sum(sapply(results$lgbm_results, function(x) !is.null(x)))
  lstm_count <- sum(sapply(results$lstm_results, function(x) !is.null(x)))
  
  # 創建HTML報告
  html_content <- paste0(
    '<!DOCTYPE html>
<html lang="zh-TW">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>AQI模型分析報告 - 簡化版</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 20px; }
        .header { text-align: center; color: #2c3e50; }
        .stats { display: flex; justify-content: space-around; margin: 20px 0; }
        .stat-card { background: #f8f9fa; padding: 20px; border-radius: 8px; text-align: center; }
        .stat-number { font-size: 2em; font-weight: bold; color: #3498db; }
        .file-list { margin: 20px 0; }
        .file-item { margin: 5px 0; padding: 5px; background: #ecf0f1; border-radius: 4px; }
    </style>
</head>
<body>
    <div class="header">
        <h1>🎯 AQI模型分析報告</h1>
        <p>生成時間: ', Sys.time(), '</p>
    </div>
    
    <div class="stats">
        <div class="stat-card">
            <div class="stat-number">', lgbm_count, '</div>
            <div>LightGBM分析</div>
        </div>
        <div class="stat-card">
            <div class="stat-number">', lstm_count, '</div>
            <div>LSTM分析</div>
        </div>
        <div class="stat-card">
            <div class="stat-number">', lgbm_count + lstm_count, '</div>
            <div>總分析數</div>
        </div>
    </div>
    
    <h2>📁 生成檔案</h2>
    <div class="file-list">'
  )
  
  # 列出生成的檔案
  output_files <- list.files(output_dir, pattern = "\\.(png|csv)$", full.names = FALSE)
  for(file in output_files) {
    html_content <- paste0(html_content, 
      '<div class="file-item">📄 ', file, '</div>')
  }
  
  html_content <- paste0(html_content,
    '    </div>
</body>
</html>')
  
  # 保存報告
  report_file <- file.path(output_dir, "analysis_report.html")
  writeLines(html_content, report_file, useBytes = TRUE)
  
  cat("✅ HTML報告已生成:", report_file, "\n")
  return(report_file)
}

# ================================================================================
# 6. 主要執行函數
# ================================================================================

run_simple_analysis <- function(output_dir = "analysis_outputs/", max_models = 5) {
  cat("🚀 開始簡化分析流程...\n")
  cat("📁 輸出目錄:", output_dir, "\n")
  cat("📊 最大分析模型數:", max_models, "\n\n")
  
  # 確保輸出目錄存在
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✅ 創建輸出目錄\n")
  }
  
  # 掃描模型
  models <- scan_models_simple()
  
  # 分析LightGBM模型
  lgbm_results <- list()
  if(length(models$importance_files) > 0) {
    cat("🌳 分析LightGBM特徵重要度...\n")
    
    for(i in 1:min(max_models, length(models$importance_files))) {
      importance_file <- models$importance_files[i]
      model_id <- gsub(".*/(.*?)_importance\\.csv$", "\\1", importance_file)
      
      result <- analyze_lgbm_importance_simple(importance_file, model_id, output_dir)
      lgbm_results[[model_id]] <- result
    }
  }
  
  # 分析LSTM模型
  lstm_results <- list()
  if(length(models$lstm_models) > 0) {
    cat("🧠 分析LSTM模型...\n")
    
    for(i in 1:min(max_models, length(models$lstm_models))) {
      model_file <- models$lstm_models[i]
      model_id <- gsub(".*/(.*?)_complete\\.rds$", "\\1", model_file)
      
      result <- analyze_lstm_simple(model_file, model_id, output_dir)
      lstm_results[[model_id]] <- result
    }
  }
  
  # 生成報告
  results <- list(
    lgbm_results = lgbm_results,
    lstm_results = lstm_results
  )
  
  report_file <- generate_simple_report(results, output_dir)
  
  # 總結
  cat("\n📋 分析總結:\n")
  cat("================================================================================\n")
  cat("🌳 LightGBM分析:", length(lgbm_results), "個\n")
  cat("🧠 LSTM分析:", length(lstm_results), "個\n")
  cat("📊 HTML報告:", report_file, "\n")
  cat("📁 輸出目錄:", output_dir, "\n")
  
  output_files <- list.files(output_dir, full.names = FALSE)
  cat("📄 生成檔案:", length(output_files), "個\n")
  
  cat("\n🎉 簡化分析完成！\n")
  cat("================================================================================\n")
  
  return(results)
}

# ================================================================================
# 7. 輔助函數
# ================================================================================

# 空值合併運算符
`%||%` <- function(x, y) if(is.null(x)) y else x

# ================================================================================
# 8. 命令行執行
# ================================================================================

if(!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  if(length(args) == 0) {
    # 預設執行
    result <- run_simple_analysis()
  } else if(args[1] == "quick") {
    # 快速模式
    result <- run_simple_analysis(max_models = 3)
  } else if(args[1] == "full") {
    # 完整模式
    result <- run_simple_analysis(max_models = 10)
  } else {
    cat("❌ 未知參數:", args[1], "\n")
    cat("可用選項: quick, full\n")
  }
}

cat("✅ 簡化分析系統載入完成\n")
cat("💡 使用方法:\n")
cat("   - run_simple_analysis()           # 標準分析\n")
cat("   - run_simple_analysis(max_models=3)  # 快速分析\n")
cat("   - run_simple_analysis(max_models=10) # 完整分析\n") 