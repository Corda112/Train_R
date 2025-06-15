# ================================================================================
# AQI 模型解析與可解釋性分析模組
# ================================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(plotly)
  library(gridExtra)
  library(patchwork)
  library(arrow)
  library(torch)
  library(lightgbm)
  library(DALEX)
  library(iml)
})

# ================================================================================
# 0. 全域配置
# ================================================================================

EXPLAIN_CONFIG <- list(
  # 抽樣配置
  max_samples_global = 10000,     # 全域解釋最大樣本數
  sample_ratio = 0.01,            # 抽樣比例 1%
  extreme_samples = 50,           # 極端值樣本數
  
  # IG配置
  ig_steps = 20,                  # Integrated Gradients 積分階數
  ig_baseline = "zero",           # 基線類型
  
  # 輸出配置
  top_features = 30,              # Top特徵數量
  interaction_pairs = 10,         # 交互作用對數
  
  # 記憶體配置
  chunk_size = 1000,              # 批次處理大小
  max_ram_gb = 8                  # 最大RAM使用量
)

# ================================================================================
# 1. 統一載入層
# ================================================================================

#' 掃描模型輸出目錄
#' @param models_dir 模型目錄路徑
#' @return 模型檔案資訊表
scan_model_outputs <- function(models_dir = "model_outputs/models/") {
  if(!dir.exists(models_dir)) {
    stop("模型目錄不存在: ", models_dir)
  }
  
  cat("📂 掃描模型輸出目錄:", models_dir, "\n")
  
  # 掃描所有完整模型檔案
  complete_files <- list.files(models_dir, pattern = "_complete\\.rds$", full.names = TRUE)
  
  if(length(complete_files) == 0) {
    stop("未找到任何完整模型檔案 (*_complete.rds)")
  }
  
  # 解析檔案名稱
  models_info <- data.table()
  
  for(file_path in complete_files) {
    file_name <- basename(file_path)
    
    # 移除_complete.rds後綴
    base_name <- gsub("_complete\\.rds$", "", file_name)
    
    # 改進的檔名解析: 識別模型類型和資料集類型
    parts <- strsplit(base_name, "_")[[1]]
    
    if(length(parts) >= 3) {
      model_type <- parts[1]  # lgbm 或 lstm
      dataset_type <- parts[2]  # separate, combine, separate_norm, combine_norm
      
      # 如果第三部分是"norm"，則合併dataset_type
      if(length(parts) >= 3 && parts[3] == "norm") {
        dataset_type <- paste(dataset_type, "norm", sep = "_")
        remaining_parts <- parts[4:length(parts)]
      } else {
        remaining_parts <- parts[3:length(parts)]
      }
      
      # 如果有"Nomorlization"字樣，跳過它
      if(length(remaining_parts) > 0 && remaining_parts[1] == "Nomorlization") {
        remaining_parts <- remaining_parts[2:length(remaining_parts)]
      }
      
      # 剩餘部分構成站點名稱和時間戳
      if(length(remaining_parts) >= 2) {
        # 最後兩個部分通常是站點代碼和時間戳
        station_parts <- remaining_parts[1:(length(remaining_parts)-1)]
        timestamp <- remaining_parts[length(remaining_parts)]
        station_name <- paste(station_parts, collapse = "_")
      } else if(length(remaining_parts) == 1) {
        station_name <- remaining_parts[1]
        timestamp <- "unknown"
      } else {
        station_name <- "unknown"
        timestamp <- "unknown"
      }
      
      # 生成唯一ID
      model_id <- paste(model_type, dataset_type, station_name, timestamp, sep = "_")
      
      # 構建路徑前綴 (不包含_complete)
      path_prefix <- file.path(models_dir, base_name)
      
      # 檢查相關檔案是否存在
      importance_file <- paste0(path_prefix, "_importance.csv")
      original_importance_file <- paste0(path_prefix, "_original_importance.csv")
      
      models_info <- rbindlist(list(models_info, data.table(
        id = model_id,
        model_type = model_type,
        dataset_type = dataset_type,
        station_name = station_name,
        timestamp = timestamp,
        path_prefix = path_prefix,
        complete_file = file_path,
        importance_file = if(file.exists(importance_file)) importance_file else NA,
        original_importance_file = if(file.exists(original_importance_file)) original_importance_file else NA,
        exists_importance = file.exists(importance_file),
        exists_original_importance = file.exists(original_importance_file)
      )))
    } else {
      cat("⚠️ 無法解析檔名:", file_name, "\n")
    }
  }
  
  cat("✅ 掃描完成:", nrow(models_info), "個模型\n")
  cat("  LightGBM:", sum(models_info$model_type == "lgbm"), "個\n")
  cat("  LSTM:", sum(models_info$model_type == "lstm"), "個\n")
  
  return(models_info)
}

#' 創建模型註冊表
#' @param models_info 模型資訊表
#' @param output_path 輸出路徑
#' @return 擴展的模型註冊表
create_model_registry <- function(models_info, output_path = "model_outputs/explain/model_registry.tsv") {
  cat("📋 創建模型註冊表...\n")
  
  # 創建輸出目錄
  output_dir <- dirname(output_path)
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  registry <- models_info[, .(id, model_type, dataset_type, station_name, timestamp, path_prefix)]
  
  # 添加模型詳細資訊
  registry[, `:=`(
    n_features = NA_integer_,
    best_iter_epoch = NA_integer_,
    train_rmse = NA_real_,
    test_rmse = NA_real_,
    model_size_mb = NA_real_,
    has_importance = FALSE,
    has_original_importance = FALSE
  )]
  
  # 載入每個模型的詳細資訊
  for(i in 1:nrow(registry)) {
    tryCatch({
      complete_file <- models_info[i, complete_file]
      model_obj <- readRDS(complete_file)
      
      # 提取模型資訊
      if(registry[i, model_type] == "lgbm") {
        registry[i, n_features := model_obj$model$params$num_feature]
        registry[i, best_iter_epoch := model_obj$model$best_iter]
        registry[i, train_rmse := model_obj$metrics$train_rmse]
        registry[i, test_rmse := model_obj$metrics$test_rmse]
        registry[i, has_importance := models_info[i, exists_importance]]
        registry[i, has_original_importance := models_info[i, exists_original_importance]]
      } else if(registry[i, model_type] == "lstm") {
        registry[i, n_features := model_obj$architecture$input_size]
        registry[i, best_iter_epoch := model_obj$training_info$best_epoch]
        registry[i, train_rmse := model_obj$metrics$train_rmse]
        registry[i, test_rmse := model_obj$metrics$test_rmse]
      }
      
      # 檔案大小
      registry[i, model_size_mb := round(file.info(complete_file)$size / 1024^2, 2)]
      
    }, error = function(e) {
      cat("⚠️ 處理模型失敗:", registry[i, id], "-", e$message, "\n")
    })
  }
  
  # 保存註冊表
  fwrite(registry, output_path, sep = "\t")
  
  # 同時保存feather格式（快速讀取）
  feather_path <- gsub("\\.tsv$", ".feather", output_path)
  arrow::write_feather(registry, feather_path)
  
  cat("✅ 模型註冊表已保存:", output_path, "\n")
  cat("  總模型數:", nrow(registry), "\n")
  cat("  平均測試RMSE:", round(mean(registry$test_rmse, na.rm = TRUE), 4), "\n")
  
  return(registry)
}

# ================================================================================
# 2. LightGBM 解釋流程
# ================================================================================

#' LightGBM 特徵重要度分析
#' @param model_info 單個模型資訊
#' @param output_dir 輸出目錄
#' @return 重要度分析結果
analyze_lgbm_importance <- function(model_info, output_dir = "model_outputs/explain/") {
  cat("🌳 分析 LightGBM 重要度:", model_info$id, "\n")
  
  results <- list()
  
  # 2-1: 展平重要度
  if(!is.na(model_info$importance_file) && file.exists(model_info$importance_file)) {
    importance_flat <- fread(model_info$importance_file)
    
    # Top-K 特徵
    top_flat <- head(importance_flat[order(-Gain)], EXPLAIN_CONFIG$top_features)
    results$flat_importance <- top_flat
    
    cat("  ✅ 展平重要度:", nrow(importance_flat), "個特徵\n")
  }
  
  # 2-2: 原始特徵重要度
  if(!is.na(model_info$original_importance_file) && file.exists(model_info$original_importance_file)) {
    importance_orig <- fread(model_info$original_importance_file)
    
    # Top-30 原始特徵
    top_orig <- head(importance_orig[order(-total_gain)], EXPLAIN_CONFIG$top_features)
    results$original_importance <- top_orig
    
    # 生成圖表
    p_bar <- create_importance_barplot(top_orig, model_info$id)
    results$importance_plot <- p_bar
    
    cat("  ✅ 原始重要度:", nrow(importance_orig), "個特徵\n")
  }
  
  return(results)
}

#' 創建特徵重要度長條圖
#' @param importance_data 重要度資料
#' @param model_id 模型ID
#' @return ggplot物件
create_importance_barplot <- function(importance_data, model_id) {
  if(nrow(importance_data) == 0) return(NULL)
  
  # 取前20個特徵避免圖表過於擁擠
  top_data <- head(importance_data, 20)
  
  p <- ggplot(top_data, aes(x = reorder(feature, total_gain), y = total_gain)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(
      title = paste("特徵重要度分析 -", model_id),
      x = "特徵名稱",
      y = "重要度 (Gain)",
      caption = paste("Top", nrow(top_data), "重要特徵")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10)
    )
  
  return(p)
}

#' LightGBM SHAP 值分析
#' @param model_info 模型資訊
#' @param data_sample 抽樣資料
#' @param output_dir 輸出目錄
#' @return SHAP分析結果
analyze_lgbm_shap <- function(model_info, data_sample, output_dir = "model_outputs/explain/") {
  cat("🔍 計算 SHAP 值:", model_info$id, "\n")
  
  tryCatch({
    # 載入模型
    model_obj <- readRDS(model_info$complete_file)
    lgb_model <- model_obj$model
    
    # 準備資料
    if(nrow(data_sample$x_flat) > EXPLAIN_CONFIG$max_samples_global) {
      sample_idx <- sample(nrow(data_sample$x_flat), EXPLAIN_CONFIG$max_samples_global)
      x_sample <- data_sample$x_flat[sample_idx, ]
      y_sample <- data_sample$y[sample_idx]
    } else {
      x_sample <- data_sample$x_flat
      y_sample <- data_sample$y
    }
    
    cat("  計算", nrow(x_sample), "個樣本的SHAP值...\n")
    
    # 計算SHAP值 (使用TreeSHAP)
    shap_values <- lgb.interprete(
      model = lgb_model,
      data = as.matrix(x_sample),
      idxset = 1:min(1000, nrow(x_sample)),  # 限制樣本數避免記憶體問題
      num_iteration = lgb_model$best_iter
    )
    
    # 全域SHAP重要度
    if(is.list(shap_values)) {
      global_shap <- data.table(
        feature = names(shap_values),
        mean_abs_shap = sapply(shap_values, function(x) mean(abs(x), na.rm = TRUE))
      )
      global_shap <- global_shap[order(-mean_abs_shap)]
    }
    
    results <- list(
      global_shap = global_shap,
      sample_size = nrow(x_sample)
    )
    
    cat("  ✅ SHAP計算完成\n")
    return(results)
    
  }, error = function(e) {
    cat("  ❌ SHAP計算失敗:", e$message, "\n")
    return(NULL)
  })
}

# ================================================================================
# 3. LSTM 解釋流程
# ================================================================================

#' LSTM Integrated Gradients 分析
#' @param model_info 模型資訊
#' @param data_sample 抽樣資料
#' @param output_dir 輸出目錄
#' @return IG分析結果
analyze_lstm_integrated_gradients <- function(model_info, data_sample, output_dir = "model_outputs/explain/") {
  cat("🧠 計算 Integrated Gradients:", model_info$id, "\n")
  
  tryCatch({
    # 載入模型
    model_obj <- readRDS(model_info$complete_file)
    
    # 重建模型結構
    model <- create_lstm_model(
      input_size = model_obj$architecture$input_size,
      hidden_size = model_obj$architecture$hidden_size,
      num_layers = model_obj$architecture$num_layers,
      dropout = model_obj$architecture$dropout
    )
    
    # 載入權重
    state_file <- gsub("_complete\\.rds$", "_state.pt", model_info$complete_file)
    if(file.exists(state_file)) {
      model$load_state_dict(torch_load(state_file))
    }
    
    model$eval()
    device <- torch_device(if(cuda_is_available()) "cuda" else "cpu")
    model$to(device = device)
    
    # 準備樣本
    n_samples <- min(EXPLAIN_CONFIG$max_samples_global * 0.05, 500)  # IG計算量大，使用更少樣本
    sample_idx <- sample(dim(data_sample$x)[1], n_samples)
    x_sample <- data_sample$x[sample_idx, , ]
    
    cat("  計算", n_samples, "個樣本的IG值...\n")
    
    # 計算IG
    ig_results <- compute_integrated_gradients(
      model = model,
      inputs = x_sample,
      steps = EXPLAIN_CONFIG$ig_steps,
      device = device
    )
    
    # 聚合結果
    # 變數重要度 (對所有時間步求平均)
    var_importance <- apply(abs(ig_results), c(1, 3), mean)  # [samples, features]
    global_var_importance <- apply(var_importance, 2, mean)  # [features]
    
    # 時間步重要度 (對所有特徵求平均)
    time_importance <- apply(abs(ig_results), c(1, 2), mean)  # [samples, timesteps]
    global_time_importance <- apply(time_importance, 2, mean)  # [timesteps]
    
    results <- list(
      variable_importance = data.table(
        feature_idx = 1:length(global_var_importance),
        importance = global_var_importance
      )[order(-importance)],
      timestep_importance = data.table(
        timestep = 1:length(global_time_importance),
        importance = global_time_importance
      ),
      sample_size = n_samples
    )
    
    cat("  ✅ IG計算完成\n")
    return(results)
    
  }, error = function(e) {
    cat("  ❌ IG計算失敗:", e$message, "\n")
    return(NULL)
  })
}

#' 計算 Integrated Gradients
#' @param model LSTM模型
#' @param inputs 輸入張量 [batch, seq_len, features]
#' @param steps 積分步數
#' @param device 設備
#' @return IG值 [batch, seq_len, features]
compute_integrated_gradients <- function(model, inputs, steps = 20, device = "cpu") {
  inputs_tensor <- torch_tensor(inputs, dtype = torch_float32())$to(device = device)
  inputs_tensor$requires_grad_(TRUE)
  
  # 基線 (全零)
  baseline <- torch_zeros_like(inputs_tensor)
  
  # 生成路徑
  alphas <- torch_linspace(0, 1, steps = steps)$to(device = device)
  
  gradients <- torch_zeros_like(inputs_tensor)
  
  for(i in 1:steps) {
    alpha <- alphas[i]
    interpolated <- baseline + alpha * (inputs_tensor - baseline)
    interpolated$requires_grad_(TRUE)
    
    # 前向傳播
    output <- model(interpolated)
    
    # 反向傳播
    grad_outputs <- torch_ones_like(output)
    grad <- torch_autograd_grad(
      outputs = output,
      inputs = interpolated,
      grad_outputs = grad_outputs,
      create_graph = FALSE,
      retain_graph = FALSE
    )[[1]]
    
    gradients <- gradients + grad
  }
  
  # 積分近似
  integrated_gradients <- (inputs_tensor - baseline) * gradients / steps
  
  return(as.array(integrated_gradients$cpu()))
}

#' 創建時間步貢獻圖
#' @param timestep_importance 時間步重要度資料
#' @param model_id 模型ID
#' @return ggplot物件
create_timestep_contribution_plot <- function(timestep_importance, model_id) {
  p <- ggplot(timestep_importance, aes(x = timestep, y = importance)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(color = "darkblue", size = 0.5) +
    labs(
      title = paste("時間步貢獻分析 -", model_id),
      x = "時間步 (小時前)",
      y = "平均重要度",
      caption = "顯示過去72小時各時間點對預測的影響程度"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    scale_x_continuous(breaks = seq(0, 72, 12))
  
  return(p)
}

# ================================================================================
# 4. 統整報告生成
# ================================================================================

#' 生成完整解釋報告
#' @param registry 模型註冊表
#' @param output_dir 輸出目錄
generate_explanation_report <- function(registry, output_dir = "model_outputs/explain/") {
  cat("📊 生成完整解釋報告...\n")
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # 創建報告結構
  report_sections <- list()
  
  # 1. 模型總覽
  report_sections$meta <- generate_model_meta_section(registry)
  
  # 2. 性能比較
  report_sections$performance <- generate_performance_comparison(registry)
  
  # 3. 特徵重要度匯總
  report_sections$importance <- generate_importance_summary(registry, output_dir)
  
  # 生成Markdown報告
  generate_markdown_report(report_sections, file.path(output_dir, "explanation_report.md"))
  
  # 生成HTML儀表板
  generate_html_dashboard(report_sections, file.path(output_dir, "dashboard.html"))
  
  cat("✅ 解釋報告生成完成:", output_dir, "\n")
}

#' 生成模型總覽部分
#' @param registry 模型註冊表
#' @return 總覽資料
generate_model_meta_section <- function(registry) {
  meta_summary <- list(
    total_models = nrow(registry),
    lgbm_models = sum(registry$model_type == "lgbm"),
    lstm_models = sum(registry$model_type == "lstm"),
    dataset_types = unique(registry$dataset_type),
    avg_test_rmse = mean(registry$test_rmse, na.rm = TRUE),
    best_model = registry[which.min(test_rmse), .(id, test_rmse)],
    total_size_mb = sum(registry$model_size_mb, na.rm = TRUE)
  )
  
  return(meta_summary)
}

#' 生成性能比較
#' @param registry 模型註冊表
#' @return 性能比較結果
generate_performance_comparison <- function(registry) {
  # 按資料集類型和模型類型分組比較
  performance_summary <- registry[!is.na(test_rmse), .(
    mean_rmse = mean(test_rmse),
    min_rmse = min(test_rmse),
    max_rmse = max(test_rmse),
    count = .N
  ), by = .(dataset_type, model_type)]
  
  # 創建比較圖
  p_performance <- ggplot(performance_summary, aes(x = dataset_type, y = mean_rmse, fill = model_type)) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_text(aes(label = round(mean_rmse, 3)), position = position_dodge(width = 0.9), vjust = -0.5) +
    labs(
      title = "模型性能比較",
      x = "資料集類型",
      y = "平均測試 RMSE",
      fill = "模型類型"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(list(
    summary = performance_summary,
    plot = p_performance
  ))
}

#' 生成重要度匯總
#' @param registry 模型註冊表
#' @param output_dir 輸出目錄
#' @return 重要度匯總結果
generate_importance_summary <- function(registry, output_dir) {
  importance_plots <- list()
  
  # 為每個LightGBM模型生成重要度圖
  lgbm_models <- registry[model_type == "lgbm" & has_original_importance == TRUE]
  
  for(i in 1:nrow(lgbm_models)) {
    model_info <- lgbm_models[i]
    
    if(file.exists(model_info$original_importance_file)) {
      importance_data <- fread(model_info$original_importance_file)
      
      if(nrow(importance_data) > 0) {
        plot_obj <- create_importance_barplot(importance_data, model_info$id)
        importance_plots[[model_info$id]] <- plot_obj
        
        # 保存個別圖表
        plot_file <- file.path(output_dir, paste0("importance_", model_info$id, ".png"))
        ggsave(plot_file, plot_obj, width = 10, height = 8, dpi = 300)
      }
    }
  }
  
  return(importance_plots)
}

#' 生成Markdown報告
#' @param report_sections 報告內容
#' @param output_file 輸出檔案
generate_markdown_report <- function(report_sections, output_file) {
  cat("📝 生成Markdown報告:", output_file, "\n")
  
  report_content <- c(
    "# AQI 模型解釋性分析報告",
    "",
    paste("**生成時間**:", Sys.time()),
    "",
    "## 模型總覽",
    "",
    paste("- 總模型數:", report_sections$meta$total_models),
    paste("- LightGBM模型:", report_sections$meta$lgbm_models),
    paste("- LSTM模型:", report_sections$meta$lstm_models),
    paste("- 資料集類型:", paste(report_sections$meta$dataset_types, collapse = ", ")),
    paste("- 平均測試RMSE:", round(report_sections$meta$avg_test_rmse, 4)),
    paste("- 最佳模型:", report_sections$meta$best_model$id, "(RMSE:", round(report_sections$meta$best_model$test_rmse, 4), ")"),
    paste("- 總模型大小:", round(report_sections$meta$total_size_mb, 1), "MB"),
    "",
    "## 性能比較",
    "",
    "### 各資料集與模型類型的RMSE表現",
    ""
  )
  
  # 添加性能表格
  if(!is.null(report_sections$performance$summary)) {
    perf_table <- report_sections$performance$summary
    report_content <- c(report_content,
      "| 資料集類型 | 模型類型 | 平均RMSE | 最小RMSE | 最大RMSE | 模型數 |",
      "|-----------|----------|----------|----------|----------|--------|"
    )
    
    for(i in 1:nrow(perf_table)) {
      row <- perf_table[i]
      report_content <- c(report_content,
        sprintf("| %s | %s | %.4f | %.4f | %.4f | %d |",
          row$dataset_type, row$model_type, row$mean_rmse, 
          row$min_rmse, row$max_rmse, row$count)
      )
    }
  }
  
  report_content <- c(report_content,
    "",
    "## 特徵重要度分析",
    "",
    paste("生成了", length(report_sections$importance), "個特徵重要度圖表。"),
    "",
    "詳細的特徵重要度分析請參考individual importance plots。",
    "",
    "---",
    "",
    "*本報告由AQI模型解釋系統自動生成*"
  )
  
  # 寫入檔案
  writeLines(report_content, output_file)
}

#' 生成HTML儀表板
#' @param report_sections 報告內容
#' @param output_file 輸出檔案
generate_html_dashboard <- function(report_sections, output_file) {
  cat("🌐 生成HTML儀表板:", output_file, "\n")
  
  # 基本HTML結構
  html_content <- c(
    "<!DOCTYPE html>",
    "<html lang='zh-TW'>",
    "<head>",
    "  <meta charset='UTF-8'>",
    "  <meta name='viewport' content='width=device-width, initial-scale=1.0'>",
    "  <title>AQI 模型解釋性分析儀表板</title>",
    "  <style>",
    "    body { font-family: Arial, sans-serif; margin: 20px; background-color: #f5f5f5; }",
    "    .container { max-width: 1200px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }",
    "    .header { text-align: center; color: #333; margin-bottom: 30px; }",
    "    .metrics { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 30px; }",
    "    .metric-card { background: #f8f9fa; padding: 15px; border-radius: 6px; text-align: center; }",
    "    .metric-value { font-size: 24px; font-weight: bold; color: #007bff; }",
    "    .metric-label { color: #666; margin-top: 5px; }",
    "    .section { margin-bottom: 30px; }",
    "    .section h2 { color: #333; border-bottom: 2px solid #007bff; padding-bottom: 10px; }",
    "    table { width: 100%; border-collapse: collapse; margin-top: 10px; }",
    "    th, td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }",
    "    th { background-color: #f8f9fa; font-weight: bold; }",
    "  </style>",
    "</head>",
    "<body>",
    "  <div class='container'>",
    "    <div class='header'>",
    "      <h1>🔍 AQI 模型解釋性分析儀表板</h1>",
    paste("      <p>生成時間:", Sys.time(), "</p>"),
    "    </div>",
    ""
  )
  
  # 添加指標卡片
  html_content <- c(html_content,
    "    <div class='metrics'>",
    "      <div class='metric-card'>",
    paste("        <div class='metric-value'>", report_sections$meta$total_models, "</div>"),
    "        <div class='metric-label'>總模型數</div>",
    "      </div>",
    "      <div class='metric-card'>",
    paste("        <div class='metric-value'>", length(report_sections$meta$dataset_types), "</div>"),
    "        <div class='metric-label'>資料集類型</div>",
    "      </div>",
    "      <div class='metric-card'>",
    paste("        <div class='metric-value'>", round(report_sections$meta$avg_test_rmse, 3), "</div>"),
    "        <div class='metric-label'>平均測試RMSE</div>",
    "      </div>",
    "      <div class='metric-card'>",
    paste("        <div class='metric-value'>", round(report_sections$meta$total_size_mb, 1), "MB</div>"),
    "        <div class='metric-label'>總模型大小</div>",
    "      </div>",
    "    </div>",
    ""
  )
  
  # 添加性能表格
  if(!is.null(report_sections$performance$summary)) {
    html_content <- c(html_content,
      "    <div class='section'>",
      "      <h2>📊 性能比較</h2>",
      "      <table>",
      "        <thead>",
      "          <tr><th>資料集類型</th><th>模型類型</th><th>平均RMSE</th><th>最小RMSE</th><th>最大RMSE</th><th>模型數</th></tr>",
      "        </thead>",
      "        <tbody>"
    )
    
    perf_table <- report_sections$performance$summary
    for(i in 1:nrow(perf_table)) {
      row <- perf_table[i]
      html_content <- c(html_content,
        sprintf("          <tr><td>%s</td><td>%s</td><td>%.4f</td><td>%.4f</td><td>%.4f</td><td>%d</td></tr>",
          row$dataset_type, row$model_type, row$mean_rmse, 
          row$min_rmse, row$max_rmse, row$count)
      )
    }
    
    html_content <- c(html_content,
      "        </tbody>",
      "      </table>",
      "    </div>"
    )
  }
  
  html_content <- c(html_content,
    "    <div class='section'>",
    "      <h2>📈 分析結果</h2>",
    "      <p>詳細的特徵重要度圖表和解釋性分析結果已保存在相應的檔案中。</p>",
    paste("      <p>共生成", length(report_sections$importance), "個特徵重要度分析圖表。</p>"),
    "    </div>",
    "  </div>",
    "</body>",
    "</html>"
  )
  
  # 寫入檔案
  writeLines(html_content, output_file)
}

# ================================================================================
# 4.5. SHAP 專用分析函數
# ================================================================================

#' 執行SHAP分析
#' @param model_info 模型資訊
#' @param output_dir 輸出目錄
run_shap_analysis <- function(model_info, output_dir = "model_outputs/explain/") {
  cat("🔍 SHAP分析:", model_info$id, "\n")
  
  if(model_info$model_type != "lgbm") {
    cat("  ⚠️ SHAP目前僅支援LightGBM模型\n")
    return(NULL)
  }
  
  # 創建模型專用目錄
  model_output_dir <- file.path(output_dir, model_info$id)
  if(!dir.exists(model_output_dir)) {
    dir.create(model_output_dir, recursive = TRUE)
  }
  
  tryCatch({
    # 載入模型
    model_obj <- readRDS(model_info$complete_file)
    lgb_model <- model_obj$model
    
    # 獲取訓練資料樣本 (如果可用)
    if(exists("train_data", model_obj) && nrow(model_obj$train_data) > 0) {
      # 抽樣用於SHAP計算
      sample_size <- min(nrow(model_obj$train_data), EXPLAIN_CONFIG$max_samples_global)
      sample_indices <- sample(nrow(model_obj$train_data), sample_size)
      X_sample <- model_obj$train_data[sample_indices, ]
      
      # 移除目標變數
      if("AQI_target" %in% colnames(X_sample)) {
        X_sample <- X_sample[, !colnames(X_sample) %in% "AQI_target", drop = FALSE]
      }
      
      cat("  📊 使用", sample_size, "個樣本進行SHAP分析\n")
      
      # 計算SHAP值
      shap_values <- lgb.shap(lgb_model, X_sample)
      
      # 保存SHAP結果
      shap_output_file <- file.path(model_output_dir, "shap_values.rds")
      saveRDS(shap_values, shap_output_file)
      
      # 生成SHAP摘要圖
      shap_summary_plot <- create_shap_summary_plot(shap_values, X_sample)
      ggsave(file.path(model_output_dir, "shap_summary.png"), 
             shap_summary_plot, width = 12, height = 8, dpi = 300)
      
      # 生成SHAP特徵重要度
      shap_importance <- create_shap_importance_table(shap_values, colnames(X_sample))
      fwrite(shap_importance, file.path(model_output_dir, "shap_importance.csv"))
      
      cat("  ✅ SHAP分析完成，結果保存至:", model_output_dir, "\n")
      
      return(list(
        shap_values = shap_values,
        importance = shap_importance,
        summary_plot = shap_summary_plot
      ))
      
    } else {
      cat("  ⚠️ 無法獲取訓練資料，跳過SHAP分析\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("  ❌ SHAP分析失敗:", e$message, "\n")
    return(NULL)
  })
}

#' 創建SHAP摘要圖
#' @param shap_values SHAP值矩陣
#' @param X_sample 樣本資料
create_shap_summary_plot <- function(shap_values, X_sample) {
  # 計算特徵重要度 (平均絕對SHAP值)
  feature_importance <- data.table(
    feature = colnames(X_sample),
    importance = colMeans(abs(shap_values))
  )[order(-importance)]
  
  # 取前20個重要特徵
  top_features <- head(feature_importance, 20)
  
  # 創建摘要圖
  p <- ggplot(top_features, aes(x = reorder(feature, importance), y = importance)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    coord_flip() +
    labs(
      title = "SHAP Feature Importance (Top 20)",
      x = "Feature",
      y = "Mean |SHAP value|"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 10)
    )
  
  return(p)
}

#' 創建SHAP重要度表格
#' @param shap_values SHAP值矩陣
#' @param feature_names 特徵名稱
create_shap_importance_table <- function(shap_values, feature_names) {
  importance_table <- data.table(
    feature = feature_names,
    mean_abs_shap = colMeans(abs(shap_values)),
    mean_shap = colMeans(shap_values),
    std_shap = apply(shap_values, 2, sd)
  )[order(-mean_abs_shap)]
  
  return(importance_table)
}

# ================================================================================
# 5. 主要管線函數
# ================================================================================

#' 執行完整解釋性分析管線
#' @param models_dir 模型目錄
#' @param output_dir 輸出目錄
#' @param max_models 最大分析模型數 (NULL表示分析全部)
#' @param verbose 詳細輸出
run_explanation_pipeline <- function(models_dir = "model_outputs/models/", 
                                   output_dir = "model_outputs/explain/",
                                   max_models = NULL,
                                   verbose = TRUE) {
  
  pipeline_start_time <- Sys.time()
  
  if(verbose) {
    cat("🔍 ================================================================================\n")
    cat("🚀 開始執行模型解釋性分析管線\n")
    cat("================================================================================\n")
  }
  
  # 1. 統一載入層
  if(verbose) cat("📂 步驟1: 掃描模型輸出...\n")
  models_info <- scan_model_outputs(models_dir)
  
  if(verbose) cat("📋 步驟2: 創建模型註冊表...\n")
  registry <- create_model_registry(models_info, file.path(output_dir, "model_registry.tsv"))
  
  # 限制分析模型數量
  if(!is.null(max_models) && max_models < nrow(registry)) {
    registry <- head(registry, max_models)
    if(verbose) cat("⚠️ 限制分析前", max_models, "個模型\n")
  }
  
  # 2. 模型解釋分析
  explanation_results <- list()
  
  for(i in 1:nrow(registry)) {
    model_info <- registry[i]
    
    if(verbose) {
      cat("\n", paste(rep("=", 60), collapse=""), "\n")
      cat("🔍 分析模型:", model_info$id, "\n")
      cat("  類型:", model_info$model_type, "| 資料:", model_info$dataset_type, "\n")
    }
    
    if(model_info$model_type == "lgbm") {
      # LightGBM 解釋流程
      lgbm_results <- analyze_lgbm_importance(model_info, output_dir)
      explanation_results[[model_info$id]] <- lgbm_results
      
    } else if(model_info$model_type == "lstm") {
      # LSTM 解釋流程 (需要原始資料，這裡先跳過)
      if(verbose) cat("  ⚠️ LSTM解釋需要原始資料，暫時跳過\n")
    }
    
    # 定期清理記憶體
    if(i %% 10 == 0) {
      gc()
      if(torch::cuda_is_available()) {
        torch::cuda_empty_cache()
      }
    }
  }
  
  # 3. 生成統整報告
  if(verbose) cat("\n📊 步驟3: 生成解釋報告...\n")
  generate_explanation_report(registry, output_dir)
  
  pipeline_end_time <- Sys.time()
  total_time <- as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins"))
  
  if(verbose) {
    cat("\n🎉 ================================================================================\n")
    cat("✅ 解釋性分析管線執行完成！\n")
    cat("⏱️  總耗時:", round(total_time, 2), "分鐘\n")
    cat("📊 分析模型數:", nrow(registry), "\n")
    cat("📁 輸出目錄:", output_dir, "\n")
    cat("================================================================================\n")
  }
  
  return(list(
    registry = registry,
    explanation_results = explanation_results,
    execution_time = total_time
  ))
}

cat("✅ 模型解析與可解釋性分析模組載入完成\n") 