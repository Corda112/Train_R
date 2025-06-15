#!/usr/bin/env Rscript
# ================================================================================
# 進階模型解釋分析執行腳本
# 支援完整的 SHAP 分析、LSTM 解釋、HTML 報告等進階功能
# ================================================================================

library(optparse)
library(data.table)

# 命令列參數設定
option_list <- list(
  make_option(c("-m", "--models-dir"), type="character", 
              default="model_outputs/models_organized/",
              help="模型檔案目錄路徑 [預設: model_outputs/models_organized/]"),
  
  make_option(c("-o", "--output-dir"), type="character", 
              default="analysis_outputs/",
              help="分析結果輸出目錄 [預設: analysis_outputs/]"),

  make_option(c("-t", "--analysis-type"), type="character", 
              default="registry",
              help="分析類型: registry, importance, shap, full [預設: registry]"),
              
  make_option(c("-n", "--max-models"), type="integer", 
              default=NULL,
              help="最大分析模型數量 [預設: 全部]"),
              
  make_option(c("-f", "--filter-type"), type="character", 
              default=NULL,
              help="篩選模型類型: lgbm, lstm [預設: 全部]"),
              
  make_option(c("-s", "--enable-shap"), action="store_true", 
              default=FALSE,
              help="啟用 SHAP 分析 [預設: 停用]"),
              
  make_option(c("-r", "--enable-html"), action="store_true", 
              default=FALSE,
              help="生成 HTML 報告 [預設: 停用]"),
              
  make_option(c("-l", "--lgbm-only"), action="store_true", 
              default=FALSE,
              help="僅分析 LightGBM 模型"),
              
  make_option(c("-u", "--lstm-only"), action="store_true", 
              default=FALSE,
              help="僅分析 LSTM 模型"),
              
  make_option(c("-p", "--shap-samples"), type="integer", 
              default=1000,
              help="SHAP 分析樣本數量 [預設: 1000]"),
              
  make_option(c("-v", "--verbose"), action="store_true", 
              default=FALSE,
              help="詳細輸出"),
              
  make_option(c("-h", "--help"), action="store_true", 
              default=FALSE,
              help="顯示此幫助訊息")
)

# 解析命令列參數
opt <- parse_args(OptionParser(option_list=option_list, add_help_option=FALSE))

# 顯示幫助
if(opt$help) {
  cat("🔬 進階模型解釋分析系統\n")
  cat("================================================================================\n")
  cat("\n使用範例:\n")
  cat("  Rscript run_model_explanation_advanced.R --analysis-type registry --verbose\n")
  cat("  Rscript run_model_explanation_advanced.R --analysis-type importance --max-models 10\n")
  cat("  Rscript run_model_explanation_advanced.R --analysis-type shap --enable-shap --lgbm-only\n")
  cat("  Rscript run_model_explanation_advanced.R --analysis-type full --enable-html --max-models 5\n")
  cat("\n分析類型說明:\n")
  cat("  registry    - 生成模型註冊表\n")
  cat("  importance  - 特徵重要度分析\n") 
  cat("  shap        - SHAP 解釋性分析\n")
  cat("  full        - 完整分析（包含所有功能）\n")
  cat("================================================================================\n")
  quit(status=0)
}

# ================================================================================
# 主要執行邏輯
# ================================================================================

cat("🔄 ================================================================================\n")
cat("🚀 進階模型解釋分析系統啟動\n")
cat("================================================================================\n")
cat("📂 模型目錄:", opt$`models-dir`, "\n")
cat("📁 輸出目錄:", opt$`output-dir`, "\n")
cat("🔬 分析類型:", opt$`analysis-type`, "\n")

# 處理參數
filter_type <- opt$`filter-type`
if(opt$`lgbm-only`) filter_type <- "lgbm"
if(opt$`lstm-only`) filter_type <- "lstm"

if(!is.null(filter_type)) {
  cat("🎯 篩選類型:", filter_type, "\n")
}

if(!is.null(opt$`max-models`)) {
  cat("📊 最大模型數:", opt$`max-models`, "\n")
}

if(opt$`enable-shap`) {
  cat("🔍 SHAP 分析: 啟用 (樣本數:", opt$`shap-samples`, ")\n")
}

if(opt$`enable-html`) {
  cat("📄 HTML 報告: 啟用\n")
}

cat("================================================================================\n")

# ================================================================================
# 模組載入
# ================================================================================

cat("📦 載入分析模組...\n")

# 載入基礎模組
basic_module_loaded <- FALSE
tryCatch({
  source("model_src/explainer_minimal.R")
  basic_module_loaded <- TRUE
  cat("✅ 基礎模組載入成功\n")
}, error = function(e) {
  cat("❌ 基礎模組載入失敗:", e$message, "\n")
})

# 載入進階模組
advanced_module_loaded <- FALSE
tryCatch({
  source("model_src/explainer_advanced.R")
  advanced_module_loaded <- TRUE
  cat("✅ 進階模組載入成功\n")
}, error = function(e) {
  cat("⚠️ 進階模組載入失敗:", e$message, "\n")
  cat("  將使用基礎功能\n")
})

if(!basic_module_loaded) {
  cat("❌ 無法載入任何分析模組，退出\n")
  quit(status=1)
}

# ================================================================================
# 執行分析
# ================================================================================

# 創建輸出目錄
if(!dir.exists(opt$`output-dir`)) {
  dir.create(opt$`output-dir`, recursive = TRUE)
}

start_time <- Sys.time()

tryCatch({
  
  if(opt$`analysis-type` == "registry") {
    # ================================================================================
    # 模型註冊表分析
    # ================================================================================
    
    cat("\n🗂️ 執行模型註冊表分析...\n")
    
    models <- scan_models_minimal(
      models_dir = opt$`models-dir`,
      filter_type = filter_type,
      max_models = opt$`max-models`,
      verbose = opt$verbose
    )
    
    if(nrow(models) == 0) {
      cat("❌ 未找到任何模型檔案\n")
      quit(status=1)
    }
    
    # 生成註冊表
    registry_file <- file.path(opt$`output-dir`, "model_registry.tsv")
    fwrite(models, registry_file, sep = "\t")
    
    cat("✅ 模型註冊表已保存:", registry_file, "\n")
    cat("📊 總計模型數量:", nrow(models), "\n")
    
    if(opt$verbose) {
      cat("\n📋 前5個模型預覽:\n")
      print(head(models[, .(id, model_type, dataset_type, has_original_importance)], 5))
    }
    
  } else if(opt$`analysis-type` == "importance") {
    # ================================================================================
    # 特徵重要度分析
    # ================================================================================
    
    cat("\n📈 執行特徵重要度分析...\n")
    
    models <- scan_models_minimal(
      models_dir = opt$`models-dir`,
      filter_type = filter_type,
      max_models = opt$`max-models`,
      verbose = opt$verbose
    )
    
    if(nrow(models) == 0) {
      cat("❌ 未找到任何模型檔案\n")
      quit(status=1)
    }
    
    # 執行重要度分析
    importance_results <- analyze_feature_importance_batch(
      models = models,
      output_dir = opt$`output-dir`,
      verbose = opt$verbose
    )
    
    cat("✅ 特徵重要度分析完成\n")
    cat("📊 分析結果:", length(importance_results), "個模型\n")
    
  } else if(opt$`analysis-type` == "shap") {
    # ================================================================================
    # SHAP 解釋性分析
    # ================================================================================
    
    if(!advanced_module_loaded) {
      cat("❌ SHAP 分析需要進階模組，但載入失敗\n")
      quit(status=1)
    }
    
    cat("\n🔍 執行 SHAP 解釋性分析...\n")
    
    models <- scan_models_minimal(
      models_dir = opt$`models-dir`,
      filter_type = filter_type,
      max_models = opt$`max-models`,
      verbose = opt$verbose
    )
    
    if(nrow(models) == 0) {
      cat("❌ 未找到任何模型檔案\n")
      quit(status=1)
    }
    
    # 執行 SHAP 分析
    shap_results <- analyze_shap_batch(
      models = models,
      sample_size = opt$`shap-samples`,
      output_dir = opt$`output-dir`,
      verbose = opt$verbose
    )
    
    cat("✅ SHAP 分析完成\n")
    cat("📊 分析結果:", length(shap_results), "個模型\n")
    
  } else if(opt$`analysis-type` == "full") {
    # ================================================================================
    # 完整分析
    # ================================================================================
    
    cat("\n🔬 執行完整模型解釋性分析...\n")
    
    models <- scan_models_minimal(
      models_dir = opt$`models-dir`,
      filter_type = filter_type,
      max_models = opt$`max-models`,
      verbose = opt$verbose
    )
    
    if(nrow(models) == 0) {
      cat("❌ 未找到任何模型檔案\n")
      quit(status=1)
    }
    
    # 1. 模型註冊表
    cat("\n1️⃣ 生成模型註冊表...\n")
    registry_file <- file.path(opt$`output-dir`, "model_registry.tsv")
    fwrite(models, registry_file, sep = "\t")
    
    # 2. 特徵重要度分析
    cat("\n2️⃣ 特徵重要度分析...\n")
    importance_results <- analyze_feature_importance_batch(
      models = models,
      output_dir = opt$`output-dir`,
      verbose = opt$verbose
    )
    
    # 3. SHAP 分析（如果啟用且模組可用）
    if(opt$`enable-shap` && advanced_module_loaded) {
      cat("\n3️⃣ SHAP 解釋性分析...\n")
      shap_results <- analyze_shap_batch(
        models = models,
        sample_size = opt$`shap-samples`,
        output_dir = opt$`output-dir`,
        verbose = opt$verbose
      )
    }
    
    # 4. HTML 報告（如果啟用且模組可用）
    if(opt$`enable-html` && advanced_module_loaded) {
      cat("\n4️⃣ 生成 HTML 報告...\n")
      html_report <- generate_html_report(
        models = models,
        output_dir = opt$`output-dir`,
        include_shap = opt$`enable-shap`,
        verbose = opt$verbose
      )
    }
    
    cat("✅ 完整分析完成\n")
    
  } else {
    cat("❌ 未知的分析類型:", opt$`analysis-type`, "\n")
    cat("   支援的類型: registry, importance, shap, full\n")
    quit(status=1)
  }
  
}, error = function(e) {
  cat("❌ 分析過程發生錯誤:", e$message, "\n")
  if(opt$verbose) {
    cat("詳細錯誤資訊:\n")
    print(e)
  }
  quit(status=1)
})

# ================================================================================
# 完成報告
# ================================================================================

end_time <- Sys.time()
execution_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n🎉 ================================================================================\n")
cat("✅ 分析完成！\n")
cat("⏱️  執行時間:", round(execution_time, 2), "秒\n")
cat("📁 結果目錄:", opt$`output-dir`, "\n")
cat("================================================================================\n") 