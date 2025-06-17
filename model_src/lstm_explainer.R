# ================================================================================
# LSTM 模型解釋輔助模組
# ================================================================================

suppressPackageStartupMessages({
  library(torch)
  library(data.table)
  library(ggplot2)
})

#' 創建LSTM模型結構 (用於解釋分析)
#' @param input_size 輸入特徵數
#' @param hidden_size 隱藏層大小
#' @param num_layers 層數
#' @param dropout dropout率
#' @return LSTM模型
create_lstm_model <- function(input_size, hidden_size = 64, num_layers = 2, dropout = 0.2) {
  model <- nn_module(
    "AQI_LSTM",
    initialize = function(input_size, hidden_size, num_layers, dropout) {
      self$hidden_size <- hidden_size
      self$num_layers <- num_layers
      
      self$lstm <- nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
      
      self$fc <- nn_linear(hidden_size, 1)
      self$dropout <- nn_dropout(dropout)
    },
    
    forward = function(x) {
      lstm_out <- self$lstm(x)
      
      # 取最後一個時間步的輸出
      last_output <- lstm_out[[1]][, -1, ]  # [batch, hidden_size]
      
      # 應用dropout和全連接層
      out <- self$dropout(last_output)
      out <- self$fc(out)
      
      return(out)
    }
  )
  
  return(model(input_size, hidden_size, num_layers, dropout))
}

#' 分析 LSTM 梯度重要度
#' @param model_path LSTM 模型路徑
#' @param test_data 測試數據
#' @param n_samples 分析樣本數
#' @return 梯度重要度結果列表
analyze_lstm_gradients <- function(model_path, test_data = NULL, n_samples = 100) {
  # ⚠️ 警告：目前為模擬實現，正式使用需要真實計算
  # 真實實施需要:
  # 1. 載入 PyTorch 模型：torch::torch_load(paste0(model_path, "_state.pt"))
  # 2. 載入對應測試數據
  # 3. 設定 model$eval() 並啟用 grad
  # 4. 計算 input.grad：backward() 後取絕對值
  # 5. 聚合多筆樣本的梯度統計
  
  cat("⚠️ 注意：analyze_lstm_gradients 目前使用模擬數據\n")
  cat("  真實實施需要 PyTorch 環境、測試數據集與梯度計算\n")
  
  # 返回模擬數據結構（實際特徵名稱應從模型 metadata 獲取）
  list(
    variable_importance = data.table(
      feature_idx = 1:50,
      importance = abs(rnorm(50, 0, 1)) * exp(-((1:50)/10)^0.5),
      feature_name = paste0("Feature_", 1:50)  # 實際應使用真實特徵名
    )[order(-importance)],
    timestep_importance = data.table(
      timestep = 1:72,
      importance = abs(rnorm(72, 0, 0.5)) * exp(-((1:72)/24)^0.3),
      hour_before = 72:1
    )
  )
}

#' 創建LSTM變數重要度圖
#' @param var_importance 變數重要度資料
#' @param model_id 模型ID
#' @param top_n 顯示前N個特徵
#' @return ggplot物件
create_lstm_variable_plot <- function(var_importance, model_id, top_n = 20) {
  top_vars <- var_importance[1:min(top_n, nrow(var_importance))]
  
  p <- ggplot(top_vars, aes(x = reorder(feature_name, importance), y = importance)) +
    geom_col(fill = "steelblue", alpha = 0.7, color = "white", linewidth = 0.3) +
    coord_flip() +
    labs(
      title = sprintf("LSTM 變數重要度排名 - %s", model_id),
      subtitle = sprintf("Top %d 特徵梯度重要度", nrow(top_vars)),
      x = "特徵名稱",
      y = "梯度重要度",
      caption = "基於梯度絕對值計算的特徵重要度"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
      plot.title = element_text(size = 14, face = "bold", color = "black"),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, color = "black"),
      plot.caption = element_text(size = 9, color = "gray50")
    )
  
  return(p)
}

#' 創建LSTM時間步重要度圖
#' @param time_importance 時間重要度資料
#' @param model_id 模型ID
#' @return ggplot物件
create_lstm_timestep_plot <- function(time_importance, model_id) {
  p <- ggplot(time_importance, aes(x = timestep, y = importance)) +
    geom_line(color = "red", linewidth = 1.2, alpha = 0.8) +
    geom_point(color = "darkred", size = 2.5, alpha = 0.7) +
    scale_x_continuous(
      breaks = seq(0, max(time_importance$timestep), by = 12),
      labels = seq(0, max(time_importance$timestep), by = 12)
    ) +
    labs(
      title = sprintf("LSTM 時間步貢獻熱圖 - %s", model_id),
      subtitle = "不同時間步對預測結果的梯度貢獻度",
      x = "時間步 (小時前)",
      y = "梯度重要度",
      caption = "數值越高表示該時間步對當前預測影響越大"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
      plot.title = element_text(size = 14, face = "bold", color = "black"),
      plot.subtitle = element_text(size = 12, color = "gray30"),
      axis.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, color = "black"),
      plot.caption = element_text(size = 9, color = "gray50")
    )
  
  return(p)
}

#' 為單個樣本創建顯著性圖
#' @param grad_sample 單個樣本的梯度 [timesteps, features]
#' @param model_id 模型ID
#' @param sample_idx 樣本索引
#' @return ggplot物件
create_sample_saliency_map <- function(grad_sample, model_id, sample_idx = 1, top_features = 15) {
  # 準備資料
  n_timesteps <- dim(grad_sample)[1]
  n_features <- dim(grad_sample)[2]
  
  # 選擇最重要的特徵
  feature_importance <- apply(abs(grad_sample), 2, mean)
  top_feature_idx <- order(feature_importance, decreasing = TRUE)[1:min(top_features, n_features)]
  
  # 創建長格式資料
  plot_data <- data.table()
  for(f in top_feature_idx) {
    for(t in 1:n_timesteps) {
      plot_data <- rbindlist(list(plot_data, data.table(
        timestep = t,
        feature = paste0("F", f),
        saliency = grad_sample[t, f],
        hour_before = n_timesteps - t + 1
      )))
    }
  }
  
  # 創建熱圖
  p <- ggplot(plot_data, aes(x = hour_before, y = feature, fill = saliency)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_gradient2(
      low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
      midpoint = 0, name = "梯度值"
    ) +
    labs(
      title = paste("LSTM 顯著性圖 -", model_id),
      subtitle = paste("樣本", sample_idx, "的特徵-時間貢獻分析"),
      x = "小時前",
      y = "特徵",
      caption = "顏色深度表示該特徵在該時間點對預測的貢獻"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
      plot.subtitle = element_text(hjust = 0.5, size = 12, color = "gray30"),
      axis.text.y = element_text(size = 10, color = "black"),
      axis.text.x = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 12, color = "black"),
      plot.caption = element_text(size = 9, color = "gray50"),
      legend.position = "right",
      legend.title = element_text(size = 11, color = "black"),
      legend.text = element_text(size = 10, color = "black")
    ) +
    scale_x_continuous(breaks = seq(0, 72, 12))
  
  return(p)
}

#' 載入已訓練的LSTM模型用於解釋
#' @param model_path 模型完整檔案路徑
#' @return 載入的模型物件
load_lstm_for_explanation <- function(model_path) {
  if(!file.exists(model_path)) {
    stop("模型檔案不存在: ", model_path)
  }
  
  # 載入完整模型物件
  model_obj <- readRDS(model_path)
  
  # 重建模型結構
  model <- create_lstm_model(
    input_size = model_obj$architecture$input_size,
    hidden_size = model_obj$architecture$hidden_size,
    num_layers = model_obj$architecture$num_layers,
    dropout = 0  # 解釋時不使用dropout
  )
  
  # 載入權重
  state_file <- gsub("_complete\\.rds$", "_state.pt", model_path)
  if(file.exists(state_file)) {
    model$load_state_dict(torch_load(state_file))
    cat("✅ LSTM權重載入成功\n")
  } else {
    warning("LSTM權重檔案不存在，使用隨機權重")
  }
  
  return(list(
    model = model,
    architecture = model_obj$architecture,
    metadata = model_obj
  ))
}

cat("✅ LSTM解釋輔助模組載入完成\n") 