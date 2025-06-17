# ================================================================================
# 模型分析執行腳本
# ================================================================================
#
# 使用方式:
# 1. 確保已經完成模型訓練，且模型檔案位於 `model_outputs/models` 目錄下。
# 2. 標準模式（Top-3 LSTM 分析）: `Rscript --vanilla run_analysis.R`
# 3. 全量評估模式（所有測站）: `Rscript --vanilla run_analysis.R --full`
#
# 功能:
# - 載入統一的模型分析模組 `model_src/explainer.R`。
# - 掃描模型目錄，生成一個包含所有模型詳細資訊的註冊表。
# - 基於測試集 RMSE 對模型進行排序。
# - 標準模式：對表現最好的 3 個 LSTM 和部分 LightGBM 模型執行分析。
# - 全量模式：對所有測站的 LSTM 和 LightGBM 模型執行完整評估。
# - 將分析結果 (圖表、數據) 保存至 `analysis_outputs` 目錄。
#
# ================================================================================

cat("🚀 開始執行模型分析流程...\n")

# --- 檢查執行模式 ---
args <- commandArgs(trailingOnly = TRUE)
full_mode <- "--full" %in% args

if (full_mode) {
  cat("🔥 全量評估模式：將處理所有測站的 LSTM & LGBM 模型\n")
  cat("⚠️  注意：此模式需要較長執行時間（預估 10-30 分鐘）\n")
} else {
  cat("📊 標準分析模式：處理 Top-3 LSTM 與代表性 LGBM 模型\n")
}

# --- 1. 載入分析模組 ---
source("model_src/explainer.R")

# --- 2. 設定全域分析參數 ---
if (full_mode) {
  # 全量模式：設定大數值讓所有模型都被處理
  Sys.setenv("ANALYSIS_TOP_LSTM" = "999")
  Sys.setenv("ANALYSIS_MODE" = "full")
} else {
  # 標準模式：保持原有限制
  Sys.setenv("ANALYSIS_TOP_LSTM" = "3")
  Sys.setenv("ANALYSIS_MODE" = "standard")
}

# --- 3. 生成模型註冊表 ---
# 此函數會掃描模型目錄，讀取元數據，並返回一個詳細的 data.table
registry <- generate_model_registry(
  models_dir = "model_outputs/models",
  analysis_dir = "analysis_outputs"
)

# --- 4. 執行分析 ---
# 如果註冊表成功生成且包含模型，則執行相應模式的分析
if (exists("registry") && nrow(registry) > 0) {
  
  if (full_mode) {
    cat("\n🔥 執行全量評估模式...\n")
    
    # 執行完整的 7 大分析區塊，設為處理更多模型
    analysis_results <- run_model_analysis(
      registry = registry,
      n_top_models = 999  # 大數值確保處理所有模型
    )
    
    # 額外生成全量評估報告
    cat("\n📋 生成全量評估摘要報告...\n")
    generate_full_evaluation_summary(registry)
    
  } else {
    cat("\n📊 執行標準分析模式...\n")
    
    # run_model_analysis 會自動排序並選出最佳模型進行分析
    analysis_results <- run_model_analysis(
      registry = registry,
      n_top_models = 5 
    )
  }
  
  cat("\n🎉 分析流程執行完畢！\n")
  cat("請查看 `analysis_outputs` 目錄下的結果。\n")
  
  if (full_mode) {
    cat("📁 全量評估結果位於:\n")
    cat("  - analysis_outputs/full_evaluation_summary.csv\n")
    cat("  - analysis_outputs/ig_lstm_*（所有測站）\n")
  }
  
} else {
  cat("❌ 未能生成模型註冊表或註冊表為空，分析中止。\n")
} 