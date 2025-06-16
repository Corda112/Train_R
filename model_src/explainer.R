# ================================================================================
# AQI 模型解析與可解釋性分析模組 (統一版)
# ================================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(htmlwidgets)
  library(DT)
  
  # 嘗試載入 IML，如果失敗則禁用 SHAP
  HAS_IML <- requireNamespace("iml", quietly = TRUE)
  if(HAS_IML) {
    suppressPackageStartupMessages(library(iml))
  } else {
    cat("⚠️ IML 套件未安裝，SHAP/LIME 分析功能將被禁用。\n")
  }
})

# ================================================================================
# 1. 模型掃描與註冊表生成
# ================================================================================

#' 掃描模型輸出目錄並生成註冊表
#' @param models_dir 模型根目錄 (e.g., "model_outputs/models")
#' @param analysis_dir 分析輸出目錄 (e.g., "analysis_outputs")
#' @return 包含所有模型詳細資訊的模型註冊表 (data.table)
generate_model_registry <- function(models_dir = "model_outputs/models",
                                    analysis_dir = "analysis_outputs") {
  
  registry_file <- file.path(analysis_dir, "registry", "model_registry.rds")
  dir.create(dirname(registry_file), recursive = TRUE, showWarnings = FALSE)
  
  cat("📂 正在掃描模型目錄:", models_dir, "\n")
  
  # 尋找所有代表完整模型的檔案
  model_files <- list.files(
    path = models_dir,
    pattern = "(_complete\\.rds|_final\\.pt)$",
    recursive = TRUE,
    full.names = TRUE
  )
  
  if (length(model_files) == 0) {
    warning("在指定的目錄中找不到任何模型檔案。")
    return(data.table())
  }
  
  cat("✅ 找到", length(model_files), "個模型檔案。\n")
  
  registry_list <- lapply(model_files, function(file_path) {
    tryCatch({
      file_info <- file.info(file_path)
      if (is.na(file_info$size) || file_info$size == 0) {
        warning(paste("檔案為空或無法讀取:", file_path))
        return(NULL)
      }
      
      model_type <- ifelse(grepl("\\.pt$", file_path), "lstm", "lgbm")
      
      # 提取基礎路徑 (不含後綴)
      base_path <- sub("(_complete\\.rds|_final\\.pt)$", "", file_path)
      base_name <- basename(base_path)
      
      parts <- strsplit(base_name, "_")[[1]]
      dataset_type <- parts[2]
      station <- if(length(parts) > 2) paste(parts[-(1:2)], collapse="_") else "all"
      model_id <- paste(model_type, dataset_type, station, sep = "_")
      
      # 讀取模型元數據
      test_rmse <- NA_real_
      if (model_type == "lgbm") {
        model_obj <- readRDS(file_path)
        test_rmse <- model_obj$evaluation$test_rmse
      } else { # lstm
        # 對於LSTM，我們只註冊路徑，不在此處加載模型以避免GPU記憶體問題
        # 假設評估文件與模型在相同目錄下
        eval_path <- file.path(dirname(file_path), paste0("lstm_evaluation.rds"))
        if(file.exists(eval_path)) {
            eval_obj <- readRDS(eval_path)
            test_rmse <- eval_obj$test_rmse
        }
      }
      
      data.table(
        model_id = model_id,
        model_type = model_type,
        dataset_type = dataset_type,
        station = station,
        file_path = normalizePath(file_path, mustWork = FALSE),
        base_path = normalizePath(base_path, mustWork = FALSE),
        test_rmse = test_rmse,
        model_size_mb = round(file_info$size / 1024^2, 2)
      )
    }, error = function(e) {
      warning(paste("處理檔案失敗:", file_path, "-", e$message))
      return(NULL)
    })
  })
  
  registry <- rbindlist(Filter(Negate(is.null), registry_list))
  
  if(nrow(registry) > 0) {
      # 保存註冊表
      saveRDS(registry, registry_file)
      fwrite(registry, sub("\\.rds", ".csv", registry_file))
      cat("✅ 模型註冊表已生成並保存至:", registry_file, "\n")
  }

  return(registry)
}


# ================================================================================
# 2. LightGBM 分析器
# ================================================================================

#' 分析單一LightGBM模型
#' @param model_info 來自註冊表的一行模型資訊
#' @param analysis_dir 分析結果的根目錄
#' @param enable_shap 是否執行 SHAP 分析 (如果 IML 套件可用)
analyze_lgbm_model <- function(model_info, 
                               analysis_dir = "analysis_outputs",
                               enable_shap = TRUE) {
                               
  if(model_info$model_type != "lgbm") {
      warning("此函數僅適用於 LightGBM 模型。")
      return(NULL)
  }
  
  cat("\n---\n")
  cat("🌳 開始分析 LightGBM 模型:", model_info$model_id, "\n")
  
  output_dir <- file.path(analysis_dir, "lgbm", model_info$model_id)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # --- 1. 載入模型和重要度數據 ---
  model_obj <- readRDS(model_info$file_path)
  
  importance_file <- paste0(model_info$base_path, "_original_importance.csv")
  if(!file.exists(importance_file)) {
      cat("  ❌ 找不到特徵重要度文件:", importance_file, "\n")
      return(NULL)
  }
  importance_data <- fread(importance_file)

  # --- 2. 生成並保存特徵重要度圖表 ---
  p_importance <- ggplot(head(importance_data, 30), aes(x = reorder(original_feature, Gain), y = Gain)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = paste("Feature Importance -", model_info$model_id), x = "Feature", y = "Total Gain") +
    theme_minimal()
  
  ggsave(file.path(output_dir, "feature_importance.png"), p_importance, width = 10, height = 8)
  cat("  ✅ 已保存特徵重要度圖表。\n")
  
  results <- list(importance_plot = p_importance)
  
  # --- 3. 執行 SHAP 分析 ---
  if(enable_shap && HAS_IML) {
      cat("  🔍 正在執行 SHAP 分析 (可能需要一些時間)...\n")
      tryCatch({
          # 準備 IML 所需的數據和預測函數
          predictor_data <- model_obj$feature_info$matrix
          
          predictor <- Predictor$new(
              model = model_obj$model, 
              data = as.data.frame(predictor_data), 
              y = model_obj$evaluation$test_actuals # 假設評估結果中有真實值
          )
          
          # 計算 SHAP 值
          shapley <- Shapley$new(predictor, x.interest = as.data.frame(predictor_data[1,]))
          results$shapley_plot <- plot(shapley)
          ggsave(file.path(output_dir, "shapley_example.png"), results$shapley_plot)
          cat("  ✅ 已保存單一樣本的 SHAP 圖。\n")

      }, error = function(e) {
          cat("  ⚠️ SHAP 分析失敗:", e$message, "\n")
      })
  }

  cat("🌳 模型分析完成:", model_info$model_id, "\n")
  return(results)
}

# ================================================================================
# 3. LSTM 分析器 (未來擴展)
# ================================================================================
# 此處可以添加 analyze_lstm_model 函數，用於處理梯度分析等

# ================================================================================
# 4. 主分析流程控制器
# ================================================================================
#' 執行完整的模型分析流程
#' @param registry 模型註冊表
#' @param n_top_models 要分析的最佳模型數量 (按 test_rmse)
run_model_analysis <- function(registry, n_top_models = 5) {
    
    if(nrow(registry) == 0) {
        cat("註冊表為空，無法進行分析。\n")
        return()
    }

    # 按 test_rmse 對模型進行排序
    setorder(registry, test_rmse)
    
    cat("\n🏆 將分析 Top", n_top_models, "個模型 (基於 Test RMSE)...\n")
    print(head(registry, n_top_models))

    top_models <- head(registry, n_top_models)

    all_results <- list()
    for(i in 1:nrow(top_models)) {
        model_info <- top_models[i, ]
        
        if(model_info$model_type == "lgbm") {
            analysis_results <- analyze_lgbm_model(model_info)
            all_results[[model_info$model_id]] <- analysis_results
        } else {
            cat("\n--- Skipping LSTM model:", model_info$model_id, "(analysis not yet implemented) ---\n")
        }
    }
    
    cat("\n✅ 所有模型的分析已完成。\n")
    return(all_results)
} 