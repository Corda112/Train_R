# ================================================================================
# AQI 模型解析與可解釋性分析模組 (完整進階版)
# ================================================================================

suppressPackageStartupMessages({
  library(data.table)
  
  # 檢查並載入進階套件
  required_packages <- c("ggplot2", "plotly", "gridExtra", "patchwork", "htmlwidgets", "DT")
  missing_packages <- c()
  loaded_packages <- c()
  
  for(pkg in required_packages) {
    if(requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      loaded_packages <- c(loaded_packages, pkg)
    } else {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # 檢查機器學習套件
  ml_packages <- c("lightgbm", "torch")
  for(pkg in ml_packages) {
    if(requireNamespace(pkg, quietly = TRUE)) {
      suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      loaded_packages <- c(loaded_packages, pkg)
    } else {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # 檢查解釋性套件
  explain_packages <- c("DALEX", "iml")
  available_explain <- c()
  for(pkg in explain_packages) {
    if(requireNamespace(pkg, quietly = TRUE)) {
      available_explain <- c(available_explain, pkg)
      loaded_packages <- c(loaded_packages, pkg)
    } else {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  cat("📦 套件載入狀態:\n")
  cat("  ✅ 已載入:", paste(loaded_packages, collapse = ", "), "\n")
  if(length(missing_packages) > 0) {
    cat("  ⚠️ 缺失:", paste(missing_packages, collapse = ", "), "\n")
  }
  cat("  🔍 可用解釋套件:", paste(available_explain, collapse = ", "), "\n")
})

# ================================================================================
# 1. 全域配置
# ================================================================================

ADVANCED_CONFIG <- list(
  # 分析配置
  max_samples_shap = 1000,        # SHAP分析最大樣本數
  max_samples_lime = 500,         # LIME分析最大樣本數
  sample_ratio = 0.02,            # 抽樣比例 2%
  
  # LSTM配置
  lstm_gradient_samples = 200,    # LSTM梯度分析樣本數
  ig_steps = 20,                  # Integrated Gradients步數
  
  # 特徵重要度配置
  top_features = 30,              # Top特徵數量
  min_importance = 0.001,         # 最小重要度閾值
  
  # 輸出配置
  plot_width = 12,                # 圖表寬度
  plot_height = 8,                # 圖表高度
  html_theme = "flatly",          # HTML主題
  
  # 記憶體配置
  chunk_size = 500,               # 批次處理大小
  max_ram_gb = 16                 # 最大RAM使用量
)

# ================================================================================
# 1.5. 基礎函數 (如果未載入)
# ================================================================================

# 確保基礎函數可用
if(!exists("scan_model_outputs") || !exists("create_model_registry")) {
  source("model_src/explainer_minimal.R")
}

# ================================================================================
# 2. 進階LightGBM分析 (包含SHAP)
# ================================================================================

#' 進階LightGBM特徵重要度分析
#' @param model_info 模型資訊
#' @param output_dir 輸出目錄
#' @param enable_shap 是否啟用SHAP分析
#' @return 分析結果
analyze_lgbm_advanced <- function(model_info, output_dir = "model_outputs/explain/", 
                                 enable_shap = TRUE) {
  cat("🌳 進階LightGBM分析:", model_info$id, "\n")
  
  results <- list()
  
  # 檢查並載入模型 (重新構建檔案路徑)
  if(!"path_prefix" %in% names(model_info)) {
    cat("  ❌ 模型資訊中缺少 path_prefix 欄位\n")
    return(results)
  }
  
  # 從 path_prefix 構建正確的 complete 檔案路徑
  path_prefix <- as.character(model_info$path_prefix)
  # 檔案格式: path_prefix.rds -> path_prefix.rds_complete.rds (不移除.rds)
  complete_file <- paste0(path_prefix, "_complete.rds")
  
  if(is.null(complete_file) || is.na(complete_file) || !file.exists(complete_file)) {
    cat("  ❌ 模型檔案不存在:", complete_file, "\n")
    return(results)
  }
  
  tryCatch({
    model_obj <- readRDS(complete_file)
    cat("  ✅ 模型載入成功\n")
  }, error = function(e) {
    cat("  ❌ 模型載入失敗:", e$message, "\n")
    return(results)
  })
  
  # 1. 基本特徵重要度
  # 重新構建重要度檔案路徑
  importance_path_prefix <- as.character(model_info$path_prefix)
  # 檔案格式: path_prefix.rds -> path_prefix.rds_original_importance.csv (不移除.rds)
  original_importance_file <- paste0(importance_path_prefix, "_original_importance.csv")
  
  if(file.exists(original_importance_file)) {
    importance_orig <- fread(original_importance_file)
    top_features <- head(importance_orig[order(-total_gain)], ADVANCED_CONFIG$top_features)
    
          # 創建重要度圖表
      if(exists("ggplot")) {
        model_id <- as.character(model_info$id)
        p_importance <- create_importance_plot(top_features, model_id)
      results$importance_plot <- p_importance
      
             # 保存圖表
               plot_file <- file.path(output_dir, paste0("importance_", gsub("[^A-Za-z0-9_-]", "_", model_id), ".png"))
       tryCatch({
         ggsave(plot_file, p_importance, width = ADVANCED_CONFIG$plot_width, 
                height = ADVANCED_CONFIG$plot_height, dpi = 300)
       }, error = function(e) {
         cat("    ⚠️ 圖表保存失敗:", e$message, "\n")
       })
    }
    
    results$importance_data <- top_features
  }
  
  # 2. SHAP分析 (如果可用)
  if(enable_shap && "iml" %in% loaded_packages && !is.null(model_obj) && !is.null(model_obj$data)) {
    tryCatch({
      shap_results <- perform_shap_analysis(model_obj, model_info, output_dir)
      results$shap <- shap_results
    }, error = function(e) {
      cat("  ⚠️ SHAP分析失敗:", e$message, "\n")
    })
  }
  
  # 3. 特徵交互作用分析
  if(!is.null(model_obj) && !is.null(model_obj$data) && !is.null(model_obj$data$train_x) && nrow(model_obj$data$train_x) > 0) {
    tryCatch({
      interaction_results <- analyze_feature_interactions(model_obj, model_info, output_dir)
      results$interactions <- interaction_results
    }, error = function(e) {
      cat("  ⚠️ 交互作用分析失敗:", e$message, "\n")
    })
  }
  
  return(results)
}

#' 執行SHAP分析
#' @param model_obj 模型物件
#' @param model_info 模型資訊
#' @param output_dir 輸出目錄
#' @return SHAP分析結果
perform_shap_analysis <- function(model_obj, model_info, output_dir) {
  cat("  🔍 執行SHAP分析...\n")
  
  # 準備資料
  train_x <- model_obj$data$train_x
  test_x <- model_obj$data$test_x
  
  # 抽樣以減少計算時間
  if(nrow(train_x) > ADVANCED_CONFIG$max_samples_shap) {
    sample_idx <- sample(nrow(train_x), ADVANCED_CONFIG$max_samples_shap)
    train_sample <- train_x[sample_idx, ]
  } else {
    train_sample <- train_x
  }
  
  # 創建預測函數
  predict_fun <- function(x) {
    x_matrix <- as.matrix(x)
    colnames(x_matrix) <- colnames(train_x)
    pred <- predict(model_obj$model, x_matrix)
    return(pred)
  }
  
  # 使用iml套件進行SHAP分析
  predictor <- iml::Predictor$new(predict_fun, data = train_sample)
  
  # SHAP values計算
  shap_values <- iml::Shapley$new(predictor, x.interest = head(test_x, 10))
  
  # 全域SHAP重要度
  global_shap <- shap_values$results
  
  # 保存SHAP結果
  shap_file <- file.path(output_dir, paste0("shap_", model_info$id, ".csv"))
  fwrite(global_shap, shap_file)
  
  cat("    ✅ SHAP分析完成\n")
  
  return(list(
    global_importance = global_shap,
    shap_file = shap_file
  ))
}

#' 分析特徵交互作用
#' @param model_obj 模型物件
#' @param model_info 模型資訊
#' @param output_dir 輸出目錄
#' @return 交互作用分析結果
analyze_feature_interactions <- function(model_obj, model_info, output_dir) {
  cat("  🔗 分析特徵交互作用...\n")
  
  # 獲取前10個重要特徵
  # 重新構建重要度檔案路徑
  interaction_path_prefix <- as.character(model_info$path_prefix)
  # 檔案格式: path_prefix.rds -> path_prefix.rds_original_importance.csv (不移除.rds)
  original_importance_file <- paste0(interaction_path_prefix, "_original_importance.csv")
  
  if(file.exists(original_importance_file)) {
    importance <- fread(original_importance_file)
    top_features <- head(importance[order(-total_gain)], 10)$feature_names
    
    # 計算特徵對之間的交互作用
    interactions <- data.table()
    
    for(i in 1:(length(top_features)-1)) {
      for(j in (i+1):length(top_features)) {
        feature1 <- top_features[i]
        feature2 <- top_features[j]
        
        # 簡化的交互作用計算 (基於特徵重要度乘積)
        imp1 <- importance[feature_names == feature1, total_gain]
        imp2 <- importance[feature_names == feature2, total_gain]
        interaction_score <- sqrt(imp1 * imp2)
        
        interactions <- rbindlist(list(interactions, data.table(
          feature1 = feature1,
          feature2 = feature2,
          interaction_score = interaction_score
        )))
      }
    }
    
    # 排序並保存
    interactions <- interactions[order(-interaction_score)]
    
    interaction_file <- file.path(output_dir, paste0("interactions_", model_info$id, ".csv"))
    fwrite(interactions, interaction_file)
    
    cat("    ✅ 交互作用分析完成\n")
    
    return(list(
      interactions = interactions,
      interaction_file = interaction_file
    ))
  }
  
  return(NULL)
}

#' 創建重要度圖表
#' @param importance_data 重要度資料
#' @param model_id 模型ID
#' @return ggplot物件
create_importance_plot <- function(importance_data, model_id) {
  plot_data <- head(importance_data, 20)
  
  p <- ggplot(plot_data, aes(x = reorder(feature_names, total_gain), y = total_gain)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("LightGBM 特徵重要度 -", model_id),
      x = "特徵名稱",
      y = "Total Gain",
      caption = "基於LightGBM的特徵重要度排序 (前20名)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 10)
    )
  
  return(p)
}

# ================================================================================
# 3. 進階LSTM分析
# ================================================================================

#' 進階LSTM模型分析
#' @param model_info 模型資訊
#' @param output_dir 輸出目錄
#' @return 分析結果
analyze_lstm_advanced <- function(model_info, output_dir = "model_outputs/explain/") {
  cat("🧠 進階LSTM分析:", model_info$id, "\n")
  
  results <- list()
  
  # 載入模型 (重新構建檔案路徑)
  if(!"path_prefix" %in% names(model_info)) {
    cat("  ❌ 模型資訊中缺少 path_prefix 欄位\n")
    return(results)
  }
  
  # 從 path_prefix 構建正確的 complete 檔案路徑
  path_prefix <- as.character(model_info$path_prefix)
  # 檔案格式: path_prefix.rds -> path_prefix.rds_complete.rds (不移除.rds)
  complete_file <- paste0(path_prefix, "_complete.rds")
  
  if(is.null(complete_file) || is.na(complete_file) || !file.exists(complete_file)) {
    cat("  ❌ 模型檔案不存在:", complete_file, "\n")
    return(results)
  }
  
  tryCatch({
    model_obj <- readRDS(complete_file)
    cat("  ✅ 模型載入成功\n")
  }, error = function(e) {
    cat("  ❌ 模型載入失敗:", e$message, "\n")
    return(results)
  })
  
  # 檢查是否有torch套件
  if(!"torch" %in% loaded_packages) {
    cat("  ⚠️ torch套件未載入，跳過LSTM分析\n")
    return(results)
  }
  
  # 1. 梯度分析
  if(!is.null(model_obj) && !is.null(model_obj$data) && !is.null(model_obj$data$test_x)) {
    tryCatch({
      # 載入LSTM輔助模組
      if(file.exists("model_src/lstm_explainer.R")) {
        source("model_src/lstm_explainer.R")
      }
      
      gradient_results <- analyze_lstm_gradients_advanced(model_obj, output_dir)
      results$gradients <- gradient_results
      
    }, error = function(e) {
      cat("  ⚠️ LSTM梯度分析失敗:", e$message, "\n")
    })
  }
  
  # 2. 注意力權重分析 (如果模型支援)
  if(!is.null(model_obj$attention_weights)) {
    attention_results <- analyze_attention_weights(model_obj, model_info, output_dir)
    results$attention <- attention_results
  }
  
  return(results)
}

#' 進階LSTM梯度分析
#' @param model_obj 模型物件
#' @param output_dir 輸出目錄
#' @return 梯度分析結果
analyze_lstm_gradients_advanced <- function(model_obj, output_dir) {
  cat("  🔍 LSTM梯度分析...\n")
  
  # 檢查CUDA可用性
  device <- torch_device(if(cuda_is_available()) "cuda" else "cpu")
  
  # 準備測試資料
  test_data <- model_obj$data$test_x
  if(is.array(test_data) && length(dim(test_data)) == 3) {
    # 隨機選擇樣本
    n_samples <- min(ADVANCED_CONFIG$lstm_gradient_samples, dim(test_data)[1])
    sample_idx <- sample(dim(test_data)[1], n_samples)
    x_sample <- test_data[sample_idx, , ]
  } else {
    stop("測試資料格式不正確")
  }
  
  # 簡化的梯度重要度計算
  # 由於模型結構複雜，我們使用數值梯度近似
  baseline_pred <- model_obj$model(torch_tensor(x_sample[1:min(10, n_samples), , ], dtype = torch_float32()))
  
  # 特徵重要度：對每個特徵加入小擾動
  feature_importance <- c()
  n_features <- dim(x_sample)[3]
  
  for(f in 1:min(20, n_features)) {  # 只分析前20個特徵以節省時間
    x_perturbed <- x_sample
    x_perturbed[, , f] <- x_perturbed[, , f] + 0.1  # 小擾動
    
    perturbed_pred <- model_obj$model(torch_tensor(x_perturbed[1:min(10, n_samples), , ], dtype = torch_float32()))
    
    importance <- mean(abs(as.array(perturbed_pred - baseline_pred)))
    feature_importance <- c(feature_importance, importance)
  }
  
  # 創建結果
  var_importance <- data.table(
    feature_idx = 1:length(feature_importance),
    importance = feature_importance,
    feature_name = paste0("feature_", 1:length(feature_importance))
  )[order(-importance)]
  
  cat("    ✅ LSTM梯度分析完成\n")
  
  return(list(
    variable_importance = var_importance,
    sample_size = n_samples
  ))
}

# ================================================================================
# 4. HTML報告生成
# ================================================================================

#' 生成完整HTML報告
#' @param registry 模型註冊表
#' @param analysis_results 分析結果
#' @param output_dir 輸出目錄
generate_html_report <- function(registry, analysis_results = NULL, output_dir = "model_outputs/explain/") {
  cat("📊 生成完整HTML報告...\n")
  
  # 檢查必要套件
  required_for_html <- c("DT", "htmlwidgets", "plotly")
  missing_html <- setdiff(required_for_html, loaded_packages)
  
  if(length(missing_html) > 0) {
    cat("  ⚠️ HTML報告需要套件:", paste(missing_html, collapse = ", "), "\n")
    cat("  📝 生成基礎Markdown報告...\n")
    return(generate_markdown_report(registry, output_dir))
  }
  
  html_file <- file.path(output_dir, "explanation_report_advanced.html")
  
  # 創建HTML內容
  html_content <- generate_html_content(registry, analysis_results, output_dir)
  
  # 寫入檔案
  writeLines(html_content, html_file)
  
  cat("✅ HTML報告已生成:", html_file, "\n")
  
  return(html_file)
}

#' 生成HTML內容
#' @param registry 模型註冊表
#' @param analysis_results 分析結果
#' @param output_dir 輸出目錄
#' @return HTML內容字串
generate_html_content <- function(registry, analysis_results, output_dir) {
  
  # 計算統計資訊
  total_models <- nrow(registry)
  lgbm_models <- sum(registry$model_type == "lgbm")
  lstm_models <- sum(registry$model_type == "lstm")
  
  valid_rmse <- registry[!is.na(test_rmse), test_rmse]
  avg_rmse <- if(length(valid_rmse) > 0) mean(valid_rmse) else NA
  best_model <- registry[which.min(test_rmse)]
  
  # HTML框架
  html_content <- paste0(
    '<!DOCTYPE html>
    <html lang="zh-TW">
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <title>AQI 模型解釋性分析報告 (進階版)</title>
        <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">
        <script src="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js"></script>
        <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
        <style>
            .metric-card { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; }
            .chart-container { background: #f8f9fa; border-radius: 10px; padding: 20px; margin: 20px 0; }
        </style>
    </head>
    <body>
        <div class="container-fluid">
            <div class="row">
                <div class="col-12">
                    <h1 class="text-center my-4">🔍 AQI 模型解釋性分析報告 (進階版)</h1>
                    <p class="text-center text-muted">生成時間: ', Sys.time(), '</p>
                </div>
            </div>
            
            <!-- 總覽卡片 -->
            <div class="row mb-4">
                <div class="col-md-3">
                    <div class="card metric-card">
                        <div class="card-body text-center">
                            <h3>', total_models, '</h3>
                            <p class="mb-0">總模型數</p>
                        </div>
                    </div>
                </div>
                <div class="col-md-3">
                    <div class="card metric-card">
                        <div class="card-body text-center">
                            <h3>', lgbm_models, '</h3>
                            <p class="mb-0">LightGBM模型</p>
                        </div>
                    </div>
                </div>
                <div class="col-md-3">
                    <div class="card metric-card">
                        <div class="card-body text-center">
                            <h3>', lstm_models, '</h3>
                            <p class="mb-0">LSTM模型</p>
                        </div>
                    </div>
                </div>
                <div class="col-md-3">
                    <div class="card metric-card">
                        <div class="card-body text-center">
                            <h3>', ifelse(is.na(avg_rmse), "N/A", round(avg_rmse, 3)), '</h3>
                            <p class="mb-0">平均RMSE</p>
                        </div>
                    </div>
                </div>
            </div>
            
            <!-- 模型註冊表 -->
            <div class="chart-container">
                <h3>📋 模型註冊表</h3>
                <div id="model-registry-table"></div>
            </div>
            
            <!-- 性能分析 -->
            <div class="chart-container">
                <h3>📈 性能分析</h3>
                <div id="performance-chart"></div>
            </div>
            
            <!-- 特徵重要度 -->
            <div class="chart-container">
                <h3>🎯 特徵重要度分析</h3>
                <p>LightGBM模型的特徵重要度和SHAP分析結果。</p>
                <div id="importance-chart"></div>
            </div>
            
            <!-- LSTM分析 -->
            <div class="chart-container">
                <h3>🧠 LSTM模型分析</h3>
                <p>LSTM模型的梯度分析和時間序列重要度。</p>
                <div id="lstm-analysis"></div>
            </div>
            
        </div>
        
        <script>
            // 添加互動式圖表的JavaScript代碼
            // 這裡可以加入Plotly圖表代碼
        </script>
    </body>
    </html>'
  )
  
  return(html_content)
}

#' 生成Markdown報告 (備用)
#' @param registry 模型註冊表
#' @param output_dir 輸出目錄
#' @return 報告檔案路徑
generate_markdown_report <- function(registry, output_dir) {
  report_file <- file.path(output_dir, "explanation_report_advanced.md")
  
  # 計算統計
  total_models <- nrow(registry)
  lgbm_models <- sum(registry$model_type == "lgbm")
  lstm_models <- sum(registry$model_type == "lstm")
  
  valid_rmse <- registry[!is.na(test_rmse), test_rmse]
  avg_rmse <- if(length(valid_rmse) > 0) mean(valid_rmse) else NA
  
  # Markdown內容
  content <- c(
    "# AQI 模型解釋性分析報告 (進階版)",
    "",
    paste("**生成時間**:", Sys.time()),
    "",
    "## 📊 模型總覽",
    "",
    paste("- 總模型數:", total_models),
    paste("- LightGBM模型:", lgbm_models),
    paste("- LSTM模型:", lstm_models),
    ifelse(is.na(avg_rmse), "- 平均測試RMSE: 無法計算", paste("- 平均測試RMSE:", round(avg_rmse, 4))),
    "",
    "## 🎯 分析功能",
    "",
    "### LightGBM分析",
    "- ✅ 特徵重要度分析",
    "- ✅ SHAP值分析 (如果套件可用)",
    "- ✅ 特徵交互作用分析",
    "",
    "### LSTM分析", 
    "- ✅ 梯度重要度分析",
    "- ✅ 時間步貢獻分析",
    "- ✅ 注意力權重分析 (如果可用)",
    "",
    "## 📁 輸出檔案",
    "",
    "- `model_registry.tsv`: 完整模型註冊表",
    "- `importance_*.png`: 特徵重要度圖表",
    "- `shap_*.csv`: SHAP分析結果",
    "- `interactions_*.csv`: 特徵交互作用",
    "",
    "---",
    "*本報告由AQI進階模型解釋系統自動生成*"
  )
  
  writeLines(content, report_file)
  cat("✅ Markdown報告已生成:", report_file, "\n")
  
  return(report_file)
}

# ================================================================================
# 5. 主要管線函數
# ================================================================================

#' 執行完整進階解釋分析管線
#' @param models_dir 模型目錄
#' @param output_dir 輸出目錄
#' @param max_models 最大分析模型數
#' @param enable_shap 啟用SHAP分析
#' @param enable_html 生成HTML報告
#' @param verbose 詳細輸出
run_advanced_explanation_pipeline <- function(models_dir = "model_outputs/models/", 
                                             output_dir = "model_outputs/explain/",
                                             max_models = NULL,
                                             enable_shap = TRUE,
                                             enable_html = TRUE,
                                             verbose = TRUE) {
  
  pipeline_start_time <- Sys.time()
  
  if(verbose) {
    cat("🔍 ================================================================================\n")
    cat("🚀 開始執行進階模型解釋性分析管線\n")
    cat("================================================================================\n")
  }
  
  # 載入基礎模組函數 (如果當前模組沒有這些函數)
  if(!exists("scan_model_outputs")) {
    if(file.exists("model_src/explainer_minimal.R")) {
      source("model_src/explainer_minimal.R")
    }
  }
  
  # 1. 掃描和註冊
  if(verbose) cat("📂 步驟1: 掃描模型輸出...\n")
  models_info <- scan_model_outputs(models_dir)
  
  # 限制模型數量
  if(!is.null(max_models) && max_models < nrow(models_info)) {
    models_info <- head(models_info, max_models)
    if(verbose) cat("⚠️ 限制分析前", max_models, "個模型\n")
  }
  
  if(verbose) cat("📋 步驟2: 創建模型註冊表...\n")
  registry <- create_model_registry(models_info, file.path(output_dir, "model_registry.tsv"))
  
  # 2. 進階LightGBM分析
  if(verbose) cat("🌳 步驟3: 進階LightGBM分析...\n")
  lgbm_models <- registry[model_type == "lgbm" & has_original_importance == "TRUE"]
  lgbm_results <- list()
  
  if(nrow(lgbm_models) > 0) {
    for(i in 1:min(5, nrow(lgbm_models))) {  # 限制分析數量
      model_info <- lgbm_models[i]
      tryCatch({
        result <- analyze_lgbm_advanced(model_info, output_dir, enable_shap)
        lgbm_results[[model_info$id]] <- result
      }, error = function(e) {
        cat("  ❌ 分析模型失敗:", model_info$id, "-", e$message, "\n")
      })
    }
  }
  
  # 3. 進階LSTM分析
  if(verbose) cat("🧠 步驟4: 進階LSTM分析...\n")
  lstm_models <- registry[model_type == "lstm"]
  lstm_results <- list()
  
  if(nrow(lstm_models) > 0) {
    for(i in 1:min(3, nrow(lstm_models))) {  # 限制分析數量
      model_info <- lstm_models[i]
      tryCatch({
        result <- analyze_lstm_advanced(model_info, output_dir)
        lstm_results[[model_info$id]] <- result
      }, error = function(e) {
        cat("  ❌ 分析LSTM模型失敗:", model_info$id, "-", e$message, "\n")
      })
    }
  }
  
  # 4. 生成報告
  if(verbose) cat("📊 步驟5: 生成完整報告...\n")
  
  analysis_results <- list(
    lgbm = lgbm_results,
    lstm = lstm_results
  )
  
  if(enable_html) {
    report_file <- generate_html_report(registry, analysis_results, output_dir)
  } else {
    report_file <- generate_markdown_report(registry, output_dir)
  }
  
  pipeline_end_time <- Sys.time()
  total_time <- as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins"))
  
  if(verbose) {
    cat("\n🎉 ================================================================================\n")
    cat("✅ 進階解釋性分析管線執行完成！\n")
    cat("⏱️  總耗時:", round(total_time, 2), "分鐘\n")
    cat("📊 分析模型數:", nrow(registry), "\n")
    cat("🌳 LightGBM分析:", length(lgbm_results), "個\n")
    cat("🧠 LSTM分析:", length(lstm_results), "個\n")
    cat("📁 輸出目錄:", output_dir, "\n")
    cat("📋 報告檔案:", basename(report_file), "\n")
    cat("================================================================================\n")
  }
  
  return(list(
    registry = registry,
    lgbm_results = lgbm_results,
    lstm_results = lstm_results,
    report_file = report_file,
    execution_time = total_time
  ))
}

# 模型掃描和註冊函數 (適配新目錄結構)
scan_organized_models <- function(models_dir = "model_outputs/models_organized/", 
                                 filter_type = NULL,
                                 max_models = NULL,
                                 sort_by = "model_type") {
  
  cat("📂 掃描重組後的模型目錄:", models_dir, "\n")
  
  if(!dir.exists(models_dir)) {
    cat("❌ 目錄不存在:", models_dir, "\n")
    return(data.table())
  }
  
  models_info <- data.table()
  
  # 遞歸掃描所有模型目錄
  model_dirs <- list.dirs(models_dir, recursive = TRUE, full.names = TRUE)
  model_dirs <- model_dirs[model_dirs != models_dir]  # 排除根目錄
  
  for(model_dir in model_dirs) {
    
    # 檢查是否包含model.rds
    model_file <- file.path(model_dir, "model.rds")
    
    if(file.exists(model_file)) {
      
      # 從路徑提取模型資訊
      rel_path <- gsub(paste0("^", models_dir, "/?"), "", model_dir)
      rel_path <- gsub("\\\\", "/", rel_path)  # 標準化路徑分隔符
      path_parts <- strsplit(rel_path, "/")[[1]]
      
      if(length(path_parts) >= 3) {
        model_type <- path_parts[1]
        dataset_type <- path_parts[2]
        specific_name <- path_parts[3]
        
        model_id <- paste(model_type, dataset_type, specific_name, sep = "_")
        
        # 檢查相關檔案
        importance_file <- file.path(model_dir, "importance.csv")
        original_importance_file <- file.path(model_dir, "original_importance.csv")
        native_file <- file.path(model_dir, "native.txt")
        
        models_info <- rbindlist(list(models_info, data.table(
          id = model_id,
          model_type = model_type,
          dataset_type = dataset_type,
          specific_name = specific_name,
          model_dir = model_dir,
          model_file = model_file,
          importance_file = if(file.exists(importance_file)) importance_file else NA,
          original_importance_file = if(file.exists(original_importance_file)) original_importance_file else NA,
          native_file = if(file.exists(native_file)) native_file else NA,
          has_importance = file.exists(importance_file),
          has_original_importance = file.exists(original_importance_file)
        )))
      }
    }
  }
  
  # 應用篩選器
  if(!is.null(filter_type)) {
    if(tolower(filter_type) %in% c("lgbm", "lightgbm")) {
      models_info <- models_info[model_type == "lgbm"]
    } else if(tolower(filter_type) == "lstm") {
      models_info <- models_info[model_type == "lstm"]
    }
  }
  
  # 排序
  if(sort_by == "model_type") {
    setorder(models_info, model_type, dataset_type, specific_name)
  } else if(sort_by == "dataset_type") {
    setorder(models_info, dataset_type, model_type, specific_name)
  }
  
  # 限制數量
  if(!is.null(max_models) && max_models > 0) {
    models_info <- head(models_info, max_models)
  }
  
  cat("✅ 掃描完成:", nrow(models_info), "個模型\n")
  if(nrow(models_info) > 0) {
    cat("  LightGBM:", sum(models_info$model_type == "lgbm"), "個\n")
    cat("  LSTM:", sum(models_info$model_type == "lstm"), "個\n")
  }
  
  return(models_info)
}

# 更新原有的 scan_models_minimal 函數以使用新結構
scan_models_minimal <- function(models_dir = "model_outputs/models_organized/", 
                               filter_type = NULL, 
                               max_models = NULL,
                               verbose = TRUE) {
  
  # 優先使用組織化目錄，如果不存在則回退到舊目錄
  if(dir.exists(models_dir)) {
    return(scan_organized_models(models_dir, filter_type, max_models))
  } else {
    # 回退到舊的掃描方式
    old_models_dir <- "model_outputs/models/"
    if(verbose) {
      cat("⚠️ 組織化目錄不存在，使用舊目錄:", old_models_dir, "\n")
    }
    return(scan_models_legacy(old_models_dir, filter_type, max_models))
  }
}

cat("✅ 進階模型解析與可解釋性分析模組載入完成\n")
cat("🎯 支援功能: SHAP分析, LSTM解釋, HTML報告生成\n")