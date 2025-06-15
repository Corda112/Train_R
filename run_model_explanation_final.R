#!/usr/bin/env Rscript
# ================================================================================
# AQI 模型解釋性分析執行腳本 (最終版本)
# ================================================================================

# 載入必要套件
suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
})

# 設定命令列參數
option_list <- list(
  make_option(c("--models-dir"), 
              type = "character",
              default = "model_outputs/models/",
              help = "模型檔案目錄 [預設: model_outputs/models/]"),
  make_option(c("--output-dir"), 
              type = "character",
              default = "model_outputs/explain/",
              help = "解釋結果輸出目錄 [預設: model_outputs/explain/]"),
  make_option(c("--max-models"),
              type = "integer", 
              default = NULL,
              help = "最大分析模型數量 (測試用)"),
  make_option(c("--analysis-type"),
              type = "character",
              default = "registry",
              help = "分析類型: registry, importance, full [預設: registry]"),
  make_option(c("--verbose"), 
              action = "store_true",
              default = FALSE,
              help = "顯示詳細輸出")
)

# 解析參數
opt_parser <- OptionParser(option_list = option_list, 
                          description = "AQI 模型解釋性分析系統")
opt <- parse_args(opt_parser)

# 驗證分析類型
valid_types <- c("registry", "importance", "full")
if(!opt$`analysis-type` %in% valid_types) {
  cat("❌ 無效的分析類型:", opt$`analysis-type`, "\n")
  cat("📝 有效選項:", paste(valid_types, collapse = ", "), "\n")
  quit(status = 1)
}

# 顯示啟動資訊
cat("🔍 ================================================================================\n")
cat("🚀 AQI 模型解釋性分析系統啟動 (最終版本)\n")
cat("================================================================================\n")

# 載入解釋模組
cat("📥 載入模型解釋模組...\n")
tryCatch({
  source("model_src/explainer_minimal.R")
  cat("✅ 簡化版解釋模組載入完成\n")
}, error = function(e) {
  cat("❌ 解釋模組載入失敗:", e$message, "\n")
  quit(status = 1)
})

# 檢查輸入目錄
if(!dir.exists(opt$`models-dir`)) {
  cat("❌ 模型目錄不存在:", opt$`models-dir`, "\n")
  quit(status = 1)
}

# 創建輸出目錄
if(!dir.exists(opt$`output-dir`)) {
  dir.create(opt$`output-dir`, recursive = TRUE)
  cat("📁 創建輸出目錄:", opt$`output-dir`, "\n")
}

# 顯示配置資訊
cat("📋 解釋分析配置:\n")
cat("  模型目錄:", opt$`models-dir`, "\n")
cat("  輸出目錄:", opt$`output-dir`, "\n")
cat("  分析類型:", opt$`analysis-type`, "\n")
if(!is.null(opt$`max-models`)) {
  cat("  最大模型數:", opt$`max-models`, "\n")
}
cat("  詳細輸出:", opt$verbose, "\n")

# 執行解釋分析
cat("================================================================================\n")

tryCatch({
  
  if(opt$`analysis-type` == "registry") {
    # 僅生成模型註冊表
    cat("📋 僅生成模型註冊表...\n")
    models_info <- scan_model_outputs(opt$`models-dir`)
    
    # 限制模型數量
    if(!is.null(opt$`max-models`) && opt$`max-models` < nrow(models_info)) {
      models_info <- head(models_info, opt$`max-models`)
      cat("⚠️ 限制分析前", opt$`max-models`, "個模型\n")
    }
    
    registry <- create_model_registry(models_info, file.path(opt$`output-dir`, "model_registry.tsv"))
    cat("✅ 模型註冊表生成完成\n")
    
  } else if(opt$`analysis-type` == "importance") {
    cat("📊 特徵重要度分析尚未實作於簡化版本\n")
    cat("💡 請使用 analysis-type=registry 或安裝完整套件環境\n")
    
  } else if(opt$`analysis-type` == "full") {
    cat("🔍 完整解釋分析尚未實作於簡化版本\n")
    cat("💡 請使用 analysis-type=registry 或安裝完整套件環境\n")
    
  } else {
    cat("⚠️ 未知的分析類型:", opt$`analysis-type`, "\n")
  }
  
}, error = function(e) {
  cat("❌ 解釋分析執行失敗:", e$message, "\n")
  cat("📍 錯誤追蹤:", toString(sys.calls()), "\n")
  quit(status = 1)
})

cat("🎉 ================================================================================\n")
cat("✅ 模型解釋性分析完成！\n")
cat("📁 結果已保存至:", opt$`output-dir`, "\n")
cat("================================================================================\n") 