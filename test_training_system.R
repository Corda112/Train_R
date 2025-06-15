# ================================================================================
# AQI 模型訓練系統 - 測試腳本
# ================================================================================

cat("🧪 AQI 模型訓練系統測試\n")
cat("========================\n")

# 載入主要管線
tryCatch({
  source("model_src/pipeline.R")
  cat("✅ 管線載入成功\n")
}, error = function(e) {
  cat("❌ 管線載入失敗:", e$message, "\n")
  stop("無法載入訓練管線")
})

# ================================================================================
# 1. 環境檢查
# ================================================================================

cat("\n🔍 執行環境檢查...\n")
check_training_environment()

# ================================================================================
# 2. 快速測試 - 使用小檔案
# ================================================================================

cat("\n🚀 執行快速測試...\n")
cat("使用 Separate 資料類型，僅載入 2 個檔案進行測試\n")

# 測試 LightGBM
cat("\n📊 測試 LightGBM 模型...\n")
tryCatch({
  lgbm_result <- quick_train(
    data_type = "separate", 
    model_type = "lgbm", 
    max_files = 2
  )
  
  if(!is.null(lgbm_result)) {
    cat("✅ LightGBM 測試成功\n")
    cat("  RMSE:", round(lgbm_result$evaluations$lgbm$rmse, 4), "\n")
    cat("  R²:", round(lgbm_result$evaluations$lgbm$r2, 4), "\n")
  }
  
}, error = function(e) {
  cat("❌ LightGBM 測試失敗:", e$message, "\n")
})

# 測試 LSTM (如果 torch 可用)
if(requireNamespace("torch", quietly = TRUE)) {
  cat("\n🧠 測試 LSTM 模型...\n")
  tryCatch({
    lstm_result <- quick_train(
      data_type = "separate", 
      model_type = "lstm", 
      max_files = 2
    )
    
    if(!is.null(lstm_result)) {
      cat("✅ LSTM 測試成功\n")
      cat("  RMSE:", round(lstm_result$evaluations$lstm$rmse, 4), "\n")
      cat("  R²:", round(lstm_result$evaluations$lstm$r2, 4), "\n")
    }
    
  }, error = function(e) {
    cat("❌ LSTM 測試失敗:", e$message, "\n")
  })
} else {
  cat("⚠️  跳過 LSTM 測試 (torch 套件未安裝)\n")
}

# ================================================================================
# 3. 系統狀態檢查
# ================================================================================

cat("\n📋 檢查系統輸出...\n")

# 檢查模型輸出目錄
model_dirs <- list.dirs("model_outputs", recursive = FALSE)
if(length(model_dirs) > 0) {
  cat("✅ 模型輸出目錄已創建:", length(model_dirs), "個\n")
  for(dir in model_dirs) {
    files <- list.files(dir, recursive = TRUE)
    cat("  ", basename(dir), ":", length(files), "個檔案\n")
  }
} else {
  cat("⚠️  尚未創建模型輸出目錄\n")
}

# 檢查記憶體使用
cat("\n💾 記憶體使用情況:\n")
mem_info <- gc()
cat("  已使用記憶體:", round(sum(mem_info[, 2]), 1), "MB\n")

# 檢查 GPU 記憶體 (如果可用)
if(requireNamespace("torch", quietly = TRUE) && torch::cuda_is_available()) {
  gpu_mem <- check_gpu_memory()
}

cat("\n✅ 系統測試完成\n")
cat("========================\n")

# ================================================================================
# 4. 使用指南
# ================================================================================

cat("\n📖 使用指南\n")
cat("==========\n")
cat("1. 快速測試單一模型:\n")
cat("   result <- quick_train('separate', 'lgbm', max_files = 5)\n\n")

cat("2. 訓練單一資料類型的所有模型:\n")
cat("   result <- train_single_data_type('separate', c('lgbm', 'lstm'))\n\n")

cat("3. 批次訓練所有資料類型:\n")
cat("   results <- train_all_data_types()\n\n")

cat("4. 僅訓練 LightGBM 模型:\n")
cat("   results <- train_all_data_types(models = 'lgbm')\n\n")

cat("5. 檢查訓練環境:\n")
cat("   check_training_environment()\n\n")

cat("📝 注意事項:\n")
cat("- 大檔案 (Combine*) 僅使用第一個區塊進行訓練\n")
cat("- LSTM 模型需要 torch 套件，建議使用 GPU\n")
cat("- 所有結果會自動儲存到 model_outputs/ 目錄\n")
cat("- 可使用 max_files 參數限制小檔案的載入數量以節省時間\n")

cat("\n🎯 系統已準備就緒，可以開始訓練！\n") 