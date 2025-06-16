# ================================================================================
# 模型分析執行腳本
# ================================================================================
#
# 使用方式:
# 1. 確保已經完成模型訓練，且模型檔案位於 `model_outputs/models` 目錄下。
# 2. 在 R 環境中直接執行此腳本: `source("run_analysis.R")`
#
# 功能:
# - 載入統一的模型分析模組 `model_src/explainer.R`。
# - 掃描模型目錄，生成一個包含所有模型詳細資訊的註冊表。
# - 基於測試集 RMSE 對模型進行排序。
# - 對表現最好的 N 個 LightGBM 模型執行深入分析 (如特徵重要度)。
# - 將分析結果 (圖表、數據) 保存至 `analysis_outputs` 目錄。
#
# ================================================================================

cat("🚀 開始執行模型分析流程...\n")

# --- 1. 載入分析模組 ---
source("model_src/explainer.R")

# --- 2. 生成模型註冊表 ---
# 此函數會掃描模型目錄，讀取元數據，並返回一個詳細的 data.table
registry <- generate_model_registry(
  models_dir = "model_outputs/models",
  analysis_dir = "analysis_outputs"
)

# --- 3. 執行分析 ---
# 如果註冊表成功生成且包含模型，則對 Top 5 模型進行分析
if (exists("registry") && nrow(registry) > 0) {
  
  # run_model_analysis 會自動排序並選出最佳模型進行分析
  analysis_results <- run_model_analysis(
    registry = registry,
    n_top_models = 5 
  )
  
  cat("\n🎉 分析流程執行完畢！\n")
  cat("請查看 `analysis_outputs` 目錄下的結果。\n")
  
} else {
  cat("❌ 未能生成模型註冊表或註冊表為空，分析中止。\n")
} 