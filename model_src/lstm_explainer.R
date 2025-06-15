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

#' 簡化版梯度分析
#' @param model LSTM模型
#' @param sample_data 樣本資料
#' @param n_samples 分析樣本數
#' @return 梯度分析結果
analyze_lstm_gradients <- function(model, sample_data, n_samples = 100) {
  model$eval()
  device <- torch_device(if(cuda_is_available()) "cuda" else "cpu")
  model$to(device = device)
  
  # 準備樣本
  if(is.array(sample_data)) {
    # 隨機選擇樣本
    total_samples <- dim(sample_data)[1]
    selected_idx <- sample(total_samples, min(n_samples, total_samples))
    x_sample <- sample_data[selected_idx, , ]
  } else {
    stop("sample_data must be an array")
  }
  
  # 轉換為tensor
  x_tensor <- torch_tensor(x_sample, dtype = torch_float32())$to(device = device)
  x_tensor$requires_grad_(TRUE)
  
  # 前向傳播
  output <- model(x_tensor)
  
  # 反向傳播
  grad_outputs <- torch_ones_like(output)
  gradients <- torch_autograd_grad(
    outputs = output,
    inputs = x_tensor,
    grad_outputs = grad_outputs,
    create_graph = FALSE,
    retain_graph = FALSE
  )[[1]]
  
  # 轉換為CPU陣列
  grad_array <- as.array(gradients$cpu())
  
  # 計算重要度
  # 變數重要度：對時間維度求平均
  var_importance <- apply(abs(grad_array), c(1, 3), mean)  # [samples, features]
  global_var_importance <- apply(var_importance, 2, mean)  # [features]
  
  # 時間重要度：對特徵維度求平均
  time_importance <- apply(abs(grad_array), c(1, 2), mean)  # [samples, timesteps]  
  global_time_importance <- apply(time_importance, 2, mean)  # [timesteps]
  
  results <- list(
    variable_importance = data.table(
      feature_idx = 1:length(global_var_importance),
      importance = global_var_importance,
      feature_name = paste0("feature_", 1:length(global_var_importance))
    )[order(-importance)],
    
    timestep_importance = data.table(
      timestep = 1:length(global_time_importance),
      importance = global_time_importance,
      hour_before = length(global_time_importance):1
    ),
    
    sample_gradients = grad_array,
    sample_size = n_samples
  )
  
  return(results)
}

#' 創建LSTM變數重要度圖
#' @param var_importance 變數重要度資料
#' @param model_id 模型ID
#' @param top_n 顯示前N個特徵
#' @return ggplot物件
create_lstm_variable_plot <- function(var_importance, model_id, top_n = 20) {
  plot_data <- head(var_importance, top_n)
  
  p <- ggplot(plot_data, aes(x = reorder(feature_name, importance), y = importance)) +
    geom_col(fill = "darkgreen", alpha = 0.7) +
    coord_flip() +
    labs(
      title = paste("LSTM 特徵重要度分析 -", model_id),
      x = "特徵名稱", 
      y = "梯度重要度",
      caption = paste("基於梯度分析的前", top_n, "個重要特徵")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 9)
    )
  
  return(p)
}

#' 創建LSTM時間步重要度圖
#' @param time_importance 時間重要度資料
#' @param model_id 模型ID
#' @return ggplot物件
create_lstm_timestep_plot <- function(time_importance, model_id) {
  p <- ggplot(time_importance, aes(x = hour_before, y = importance)) +
    geom_line(color = "darkgreen", size = 1) +
    geom_point(color = "forestgreen", size = 0.8) +
    labs(
      title = paste("LSTM 時間步貢獻分析 -", model_id),
      x = "小時前",
      y = "梯度重要度", 
      caption = "顯示過去72小時各時間點對預測的影響程度"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    scale_x_continuous(breaks = seq(0, 72, 12)) +
    geom_vline(xintercept = c(1, 6, 12, 24), linetype = "dashed", alpha = 0.5, color = "gray")
  
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
    geom_tile() +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
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
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      axis.text.y = element_text(size = 8),
      legend.position = "right"
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