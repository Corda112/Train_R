# ================================================================================
# AQI 時間序列預測模型訓練 - LSTM 模組
# ================================================================================

# 載入必要套件
if(!requireNamespace("torch", quietly = TRUE)) {
  stop("請安裝 torch 套件: install.packages('torch')")
}

cat("🧠 載入 LSTM 模型模組...\n")

# ================================================================================
# 1. LSTM 網路架構定義
# ================================================================================

#' 定義LSTM網路架構
#' @param n_features 輸入特徵數
#' @param hidden_size 隱藏層大小
#' @param num_layers LSTM層數
#' @param dropout Dropout比例
#' @param bidirectional 是否使用雙向LSTM
#' @return LSTM網路模組
lstm_net <- torch::nn_module(
  "LSTMNet",
  
  initialize = function(n_features, hidden_size = 128, num_layers = 2, 
                       dropout = 0.2, bidirectional = FALSE) {
    self$n_features <- n_features
    self$hidden_size <- hidden_size
    self$num_layers <- num_layers
    self$bidirectional <- bidirectional
    
    # LSTM層
    self$lstm <- torch::nn_lstm(
      input_size = n_features,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = dropout,
      batch_first = TRUE,
      bidirectional = bidirectional
    )
    
    # 計算LSTM輸出維度
    lstm_output_size <- ifelse(bidirectional, hidden_size * 2, hidden_size)
    
    # 全連接層
    self$fc1 <- torch::nn_linear(lstm_output_size, hidden_size)
    self$dropout <- torch::nn_dropout(dropout)
    self$fc2 <- torch::nn_linear(hidden_size, 1)
    
    # 激活函數
    self$relu <- torch::nn_relu()
  },
  
  forward = function(x) {
    # x: [batch_size, seq_len, n_features]
    
    # LSTM前向傳播
    lstm_out <- self$lstm(x)
    
    # 取最後一個時間步的輸出
    # lstm_out[[1]]: [batch_size, seq_len, hidden_size * directions]
    last_output <- lstm_out[[1]][, -1, ]  # [batch_size, hidden_size * directions]
    
    # 全連接層
    out <- self$fc1(last_output)
    out <- self$relu(out)
    out <- self$dropout(out)
    out <- self$fc2(out)
    
    return(out)
  }
)

#' 創建LSTM模型
#' @param n_features 輸入特徵數
#' @param params LSTM參數列表
#' @return LSTM模型實例
create_lstm_model <- function(n_features, params = LSTM_PARAMS) {
  model <- lstm_net(
    n_features = n_features,
    hidden_size = params$hidden_size,
    num_layers = params$num_layers,
    dropout = params$dropout,
    bidirectional = params$bidirectional
  )
  
  return(model)
}

# ================================================================================
# 2. 資料預處理函數
# ================================================================================

#' 將資料集轉換為torch張量
#' @param dataset aqi_dataset 物件
#' @param device 設備 ("cpu", "cuda")
#' @return torch張量列表
dataset_to_torch <- function(dataset, device = "cpu") {
  if(!inherits(dataset, "aqi_dataset")) {
    stop("dataset 必須是 aqi_dataset 物件")
  }
  
  # 轉換為torch張量
  x_tensor <- torch::torch_tensor(dataset$x, dtype = torch::torch_float32())
  y_tensor <- torch::torch_tensor(dataset$y, dtype = torch::torch_float32())
  
  # 移動到指定設備
  if(device != "cpu") {
    x_tensor <- x_tensor$to(device = device)
    y_tensor <- y_tensor$to(device = device)
  }
  
  return(list(x = x_tensor, y = y_tensor))
}

#' 創建資料載入器
#' @param dataset aqi_dataset 物件
#' @param batch_size 批次大小
#' @param shuffle 是否打亂
#' @param device 設備
#' @return torch資料載入器
create_dataloader <- function(dataset, batch_size = 256, shuffle = TRUE, device = "cpu") {
  # 轉換為torch張量
  torch_data <- dataset_to_torch(dataset, device = device)
  
  # 創建TensorDataset
  tensor_dataset <- torch::tensor_dataset(torch_data$x, torch_data$y)
  
  # 創建DataLoader
  dataloader <- torch::dataloader(
    dataset = tensor_dataset,
    batch_size = batch_size,
    shuffle = shuffle,
    drop_last = FALSE
  )
  
  return(dataloader)
}

# ================================================================================
# 3. 訓練函數
# ================================================================================

#' 訓練LSTM模型
#' @param train_dataset 訓練資料集
#' @param val_dataset 驗證資料集 (可選)
#' @param params LSTM參數列表
#' @param verbose 是否顯示詳細資訊
#' @return 訓練好的模型物件
train_lstm <- function(train_dataset, val_dataset = NULL, params = LSTM_PARAMS, verbose = TRUE) {
  if(!inherits(train_dataset, "aqi_dataset")) {
    stop("train_dataset 必須是 aqi_dataset 物件")
  }
  
  if(verbose) {
    cat("🧠 開始訓練 LSTM 模型...\n")
    cat("  訓練樣本數:", format(train_dataset$n_windows, big.mark = ","), "\n")
    cat("  特徵數量:", train_dataset$n_features, "\n")
    cat("  序列長度:", train_dataset$seq_len, "\n")
  }
  
  start_time <- Sys.time()
  
  # 檢查設備可用性
  device <- params$device
  if(device == "cuda" && !torch::cuda_is_available()) {
    warning("CUDA不可用，切換到CPU")
    device <- "cpu"
  }
  
  if(verbose) {
    cat("  使用設備:", device, "\n")
    if(device == "cuda") {
      cat("  GPU記憶體:", round(torch::cuda_memory_allocated() / 1024^3, 2), "GB\n")
    }
  }
  
  # 創建模型
  model <- create_lstm_model(train_dataset$n_features, params)
  model <- model$to(device = device)
  
  if(verbose) {
    cat("  模型參數數量:", sum(sapply(model$parameters, function(p) p$numel())), "\n")
  }
  
  # 創建資料載入器
  train_loader <- create_dataloader(
    train_dataset, 
    batch_size = params$batch_size, 
    shuffle = TRUE, 
    device = device
  )
  
  val_loader <- NULL
  if(!is.null(val_dataset)) {
    if(!inherits(val_dataset, "aqi_dataset")) {
      stop("val_dataset 必須是 aqi_dataset 物件")
    }
    
    val_loader <- create_dataloader(
      val_dataset, 
      batch_size = params$batch_size, 
      shuffle = FALSE, 
      device = device
    )
    
    if(verbose) {
      cat("  驗證樣本數:", format(val_dataset$n_windows, big.mark = ","), "\n")
    }
  }
  
  # 設定優化器
  if(params$optimizer == "adam") {
    optimizer <- torch::optim_adam(
      model$parameters,
      lr = params$learning_rate,
      betas = c(params$beta1, params$beta2),
      eps = params$eps,
      weight_decay = params$weight_decay
    )
  } else {
    stop("不支援的優化器: ", params$optimizer)
  }
  
  # 設定損失函數
  criterion <- torch::nn_mse_loss()
  
  # 設定學習率調度器
  scheduler <- NULL
  if(params$scheduler == "reduce_on_plateau") {
    scheduler <- torch::lr_reduce_on_plateau(
      optimizer,
      mode = "min",
      factor = params$scheduler_factor,
      patience = params$scheduler_patience,
      min_lr = params$scheduler_min_lr
    )
  }
  
  # 設定混合精度訓練
  scaler <- NULL
  if(params$mixed_precision && device == "cuda") {
    scaler <- torch::cuda_amp_grad_scaler()
    if(verbose) {
      cat("  啟用混合精度訓練\n")
    }
  }
  
  # 訓練歷史記錄
  train_losses <- c()
  val_losses <- c()
  best_val_loss <- Inf
  patience_counter <- 0
  best_model_state <- NULL
  
  if(verbose) {
    cat("  開始訓練...\n")
  }
  
  # 訓練循環
  for(epoch in 1:params$epochs) {
    # 訓練階段
    model$train()
    train_loss <- 0
    n_train_batches <- 0
    
    # 簡化的訓練循環 (避免使用coro)
    train_iter <- torch::dataloader_make_iter(train_loader)
    repeat {
      batch <- torch::dataloader_next(train_iter)
      if(is.null(batch)) break
      
      optimizer$zero_grad()
      
      x_batch <- batch[[1]]
      y_batch <- batch[[2]]$view(c(-1, 1))
      
      if(!is.null(scaler)) {
        # 混合精度訓練
        with(torch::autocast(device_type = "cuda"), {
          outputs <- model(x_batch)
          loss <- criterion(outputs, y_batch)
        })
        
        scaler$scale(loss)$backward()
        scaler$step(optimizer)
        scaler$update()
      } else {
        # 標準訓練
        outputs <- model(x_batch)
        loss <- criterion(outputs, y_batch)
        
        loss$backward()
        optimizer$step()
      }
      
      train_loss <- train_loss + loss$item()
      n_train_batches <- n_train_batches + 1
    }
    
    avg_train_loss <- train_loss / n_train_batches
    train_losses <- c(train_losses, avg_train_loss)
    
    # 驗證階段
    avg_val_loss <- NULL
    if(!is.null(val_loader)) {
      model$eval()
      val_loss <- 0
      n_val_batches <- 0
      
      with(torch::no_grad(), {
        val_iter <- torch::dataloader_make_iter(val_loader)
        repeat {
          batch <- torch::dataloader_next(val_iter)
          if(is.null(batch)) break
          
          x_batch <- batch[[1]]
          y_batch <- batch[[2]]$view(c(-1, 1))
          
          outputs <- model(x_batch)
          loss <- criterion(outputs, y_batch)
          
          val_loss <- val_loss + loss$item()
          n_val_batches <- n_val_batches + 1
        }
      })
      
      avg_val_loss <- val_loss / n_val_batches
      val_losses <- c(val_losses, avg_val_loss)
      
      # Early stopping檢查
      if(avg_val_loss < best_val_loss - params$min_delta) {
        best_val_loss <- avg_val_loss
        patience_counter <- 0
        best_model_state <- model$state_dict()
      } else {
        patience_counter <- patience_counter + 1
      }
      
      # 學習率調度
      if(!is.null(scheduler)) {
        scheduler$step(avg_val_loss)
      }
    }
    
    # 顯示進度
    if(verbose && (epoch %% 10 == 0 || epoch == 1)) {
      cat("  Epoch", epoch, "/", params$epochs, 
          "- 訓練損失:", round(avg_train_loss, 6))
      if(!is.null(avg_val_loss)) {
        cat(", 驗證損失:", round(avg_val_loss, 6))
      }
      cat("\n")
    }
    
    # Early stopping
    if(!is.null(val_loader) && patience_counter >= params$patience) {
      if(verbose) {
        cat("  Early stopping at epoch", epoch, "\n")
      }
      break
    }
    
    # GPU記憶體清理
    if(device == "cuda" && epoch %% 50 == 0) {
      torch::cuda_empty_cache()
    }
  }
  
  # 載入最佳模型
  if(!is.null(best_model_state)) {
    model$load_state_dict(best_model_state)
    if(verbose) {
      cat("  載入最佳模型 (驗證損失:", round(best_val_loss, 6), ")\n")
    }
  }
  
  end_time <- Sys.time()
  training_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if(verbose) {
    cat("✅ 模型訓練完成\n")
    cat("  訓練時間:", round(training_time, 2), "分鐘\n")
    cat("  最終訓練損失:", round(tail(train_losses, 1), 6), "\n")
    if(length(val_losses) > 0) {
      cat("  最終驗證損失:", round(tail(val_losses, 1), 6), "\n")
      cat("  最佳驗證損失:", round(best_val_loss, 6), "\n")
    }
  }
  
  # 創建模型包裝物件
  lstm_model <- list(
    model = model,
    n_features = train_dataset$n_features,
    seq_len = train_dataset$seq_len,
    data_type = train_dataset$data_type,
    params = params,
    device = device,
    training_time = training_time,
    train_losses = train_losses,
    val_losses = val_losses,
    best_val_loss = best_val_loss,
    trained_at = end_time
  )
  
  class(lstm_model) <- c("aqi_lstm_model", "list")
  
  return(lstm_model)
}

#' 打印LSTM模型摘要
#' @param x aqi_lstm_model 物件
print.aqi_lstm_model <- function(x, ...) {
  cat("AQI LSTM 模型\n")
  cat("==============\n")
  cat("資料類型:", x$data_type, "\n")
  cat("特徵數量:", x$n_features, "\n")
  cat("序列長度:", x$seq_len, "\n")
  cat("設備:", x$device, "\n")
  cat("訓練時間:", round(x$training_time, 2), "分鐘\n")
  cat("訓練時間:", format(x$trained_at, "%Y-%m-%d %H:%M:%S"), "\n")
  
  # 顯示主要參數
  cat("\n主要參數:\n")
  cat("  隱藏層大小:", x$params$hidden_size, "\n")
  cat("  LSTM層數:", x$params$num_layers, "\n")
  cat("  Dropout:", x$params$dropout, "\n")
  cat("  學習率:", x$params$learning_rate, "\n")
  cat("  批次大小:", x$params$batch_size, "\n")
  
  # 顯示訓練結果
  cat("\n訓練結果:\n")
  cat("  最終訓練損失:", round(tail(x$train_losses, 1), 6), "\n")
  if(length(x$val_losses) > 0) {
    cat("  最終驗證損失:", round(tail(x$val_losses, 1), 6), "\n")
    cat("  最佳驗證損失:", round(x$best_val_loss, 6), "\n")
  }
}

# ================================================================================
# 4. 預測函數
# ================================================================================

#' 使用LSTM模型進行預測
#' @param model aqi_lstm_model 物件
#' @param test_dataset 測試資料集
#' @param batch_size 預測批次大小
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lstm <- function(model, test_dataset, batch_size = NULL, verbose = TRUE) {
  if(!inherits(model, "aqi_lstm_model")) {
    stop("model 必須是 aqi_lstm_model 物件")
  }
  
  if(!inherits(test_dataset, "aqi_dataset")) {
    stop("test_dataset 必須是 aqi_dataset 物件")
  }
  
  if(verbose) {
    cat("🔮 使用 LSTM 模型進行預測...\n")
    cat("  測試樣本數:", format(test_dataset$n_windows, big.mark = ","), "\n")
  }
  
  # 檢查特徵一致性
  if(test_dataset$n_features != model$n_features) {
    stop("測試資料的特徵數與模型不一致")
  }
  
  if(test_dataset$seq_len != model$seq_len) {
    stop("測試資料的序列長度與模型不一致")
  }
  
  # 設定批次大小
  if(is.null(batch_size)) {
    batch_size <- model$params$batch_size
  }
  
  # 創建資料載入器
  test_loader <- create_dataloader(
    test_dataset, 
    batch_size = batch_size, 
    shuffle = FALSE, 
    device = model$device
  )
  
  # 預測
  model$model$eval()
  predictions <- c()
  
  with(torch::no_grad(), {
    test_iter <- torch::dataloader_make_iter(test_loader)
    repeat {
      batch <- torch::dataloader_next(test_iter)
      if(is.null(batch)) break
      
      x_batch <- batch[[1]]
      
      outputs <- model$model(x_batch)
      batch_predictions <- as.numeric(outputs$cpu())
      
      predictions <- c(predictions, batch_predictions)
    }
  })
  
  if(verbose) {
    cat("✅ 預測完成\n")
    cat("  預測範圍: [", round(min(predictions), 2), ", ", round(max(predictions), 2), "]\n")
    cat("  預測均值:", round(mean(predictions), 2), "\n")
  }
  
  return(predictions)
}

#' 批次預測函數 (適用於大型資料集)
#' @param model aqi_lstm_model 物件
#' @param x_array 輸入陣列 [n_samples, seq_len, n_features]
#' @param batch_size 批次大小
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lstm_batch <- function(model, x_array, batch_size = 1000, verbose = TRUE) {
  if(!inherits(model, "aqi_lstm_model")) {
    stop("model 必須是 aqi_lstm_model 物件")
  }
  
  if(!is.array(x_array) || length(dim(x_array)) != 3) {
    stop("x_array 必須是3維陣列")
  }
  
  n_samples <- dim(x_array)[1]
  
  if(verbose) {
    cat("🔮 執行批次預測...\n")
    cat("  總樣本數:", format(n_samples, big.mark = ","), "\n")
    cat("  批次大小:", format(batch_size, big.mark = ","), "\n")
  }
  
  model$model$eval()
  predictions <- numeric(n_samples)
  n_batches <- ceiling(n_samples / batch_size)
  
  with(torch::no_grad(), {
    for(i in 1:n_batches) {
      start_idx <- (i - 1) * batch_size + 1
      end_idx <- min(i * batch_size, n_samples)
      
      # 提取批次資料
      batch_x <- x_array[start_idx:end_idx, , , drop = FALSE]
      x_tensor <- torch::torch_tensor(batch_x, dtype = torch::torch_float32())
      x_tensor <- x_tensor$to(device = model$device)
      
      # 預測
      outputs <- model$model(x_tensor)
      batch_predictions <- as.numeric(outputs$cpu())
      
      predictions[start_idx:end_idx] <- batch_predictions
      
      if(verbose && i %% 10 == 0) {
        cat("  完成批次:", i, "/", n_batches, "\n")
      }
    }
  })
  
  if(verbose) {
    cat("✅ 批次預測完成\n")
  }
  
  return(predictions)
}

# ================================================================================
# 5. 模型儲存與載入
# ================================================================================

#' 儲存LSTM模型
#' @param model aqi_lstm_model 物件
#' @param file_path 儲存路徑 (不含副檔名)
save_lstm_model <- function(model, file_path) {
  if(!inherits(model, "aqi_lstm_model")) {
    stop("model 必須是 aqi_lstm_model 物件")
  }
  
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  
  # 儲存完整模型物件 (不包含torch模型)
  model_copy <- model
  model_copy$model <- NULL  # 移除torch模型以避免序列化問題
  
  model_path <- paste0(file_path, "_model.rds")
  saveRDS(model_copy, model_path)
  
  # 儲存torch模型狀態
  torch_path <- paste0(file_path, "_torch.pt")
  torch::torch_save(model$model$state_dict(), torch_path)
  
  # 儲存訓練歷史
  history_path <- paste0(file_path, "_history.rds")
  history <- list(
    train_losses = model$train_losses,
    val_losses = model$val_losses,
    best_val_loss = model$best_val_loss
  )
  saveRDS(history, history_path)
  
  cat("✅ LSTM 模型已儲存:\n")
  cat("  模型物件:", model_path, "\n")
  cat("  Torch狀態:", torch_path, "\n")
  cat("  訓練歷史:", history_path, "\n")
}

#' 載入LSTM模型
#' @param file_path 模型路徑 (不含副檔名)
#' @return aqi_lstm_model 物件
load_lstm_model <- function(file_path) {
  model_path <- paste0(file_path, "_model.rds")
  torch_path <- paste0(file_path, "_torch.pt")
  
  if(!file.exists(model_path)) {
    stop("模型檔案不存在: ", model_path)
  }
  
  if(!file.exists(torch_path)) {
    stop("Torch狀態檔案不存在: ", torch_path)
  }
  
  # 載入模型物件
  model <- readRDS(model_path)
  
  if(!inherits(model, "aqi_lstm_model")) {
    stop("載入的物件不是 aqi_lstm_model 類型")
  }
  
  # 重建torch模型
  torch_model <- create_lstm_model(model$n_features, model$params)
  torch_model <- torch_model$to(device = model$device)
  
  # 載入模型狀態
  state_dict <- torch::torch_load(torch_path)
  torch_model$load_state_dict(state_dict)
  
  model$model <- torch_model
  
  cat("✅ LSTM 模型載入完成:", model_path, "\n")
  return(model)
}

# ================================================================================
# 6. 模型診斷函數
# ================================================================================

#' 診斷LSTM模型
#' @param model aqi_lstm_model 物件
#' @param test_dataset 測試資料集
#' @return 診斷結果列表
diagnose_lstm_model <- function(model, test_dataset = NULL) {
  if(!inherits(model, "aqi_lstm_model")) {
    stop("model 必須是 aqi_lstm_model 物件")
  }
  
  diagnosis <- list()
  
  # 基本資訊
  diagnosis$basic_info <- list(
    data_type = model$data_type,
    n_features = model$n_features,
    seq_len = model$seq_len,
    device = model$device,
    training_time = model$training_time
  )
  
  # 模型參數統計
  n_params <- sum(sapply(model$model$parameters, function(p) p$numel()))
  diagnosis$model_stats <- list(
    n_parameters = n_params,
    hidden_size = model$params$hidden_size,
    num_layers = model$params$num_layers,
    dropout = model$params$dropout
  )
  
  # 訓練歷史
  diagnosis$training_history <- list(
    train_losses = model$train_losses,
    val_losses = model$val_losses,
    best_val_loss = model$best_val_loss,
    n_epochs = length(model$train_losses)
  )
  
  # 收斂性分析
  if(length(model$train_losses) > 10) {
    recent_losses <- tail(model$train_losses, 10)
    loss_trend <- lm(recent_losses ~ seq_along(recent_losses))$coefficients[2]
    diagnosis$convergence <- list(
      loss_trend = loss_trend,
      is_converging = loss_trend < 0,
      final_loss = tail(model$train_losses, 1)
    )
  }
  
  # 如果提供測試資料，進行預測診斷
  if(!is.null(test_dataset)) {
    predictions <- predict_lstm(model, test_dataset, verbose = FALSE)
    evaluation <- evaluate_predictions(test_dataset$y, predictions)
    diagnosis$test_performance <- evaluation
  }
  
  class(diagnosis) <- c("aqi_lstm_diagnosis", "list")
  return(diagnosis)
}

#' 打印LSTM診斷結果
#' @param x aqi_lstm_diagnosis 物件
print.aqi_lstm_diagnosis <- function(x, ...) {
  cat("AQI LSTM 模型診斷\n")
  cat("==================\n")
  
  # 基本資訊
  cat("📊 基本資訊:\n")
  cat("  資料類型:", x$basic_info$data_type, "\n")
  cat("  特徵數量:", x$basic_info$n_features, "\n")
  cat("  序列長度:", x$basic_info$seq_len, "\n")
  cat("  設備:", x$basic_info$device, "\n")
  cat("  訓練時間:", round(x$basic_info$training_time, 2), "分鐘\n\n")
  
  # 模型統計
  cat("🧠 模型統計:\n")
  cat("  參數數量:", format(x$model_stats$n_parameters, big.mark = ","), "\n")
  cat("  隱藏層大小:", x$model_stats$hidden_size, "\n")
  cat("  LSTM層數:", x$model_stats$num_layers, "\n")
  cat("  Dropout:", x$model_stats$dropout, "\n\n")
  
  # 訓練歷史
  cat("📈 訓練歷史:\n")
  cat("  訓練輪數:", x$training_history$n_epochs, "\n")
  cat("  最終訓練損失:", round(tail(x$training_history$train_losses, 1), 6), "\n")
  if(length(x$training_history$val_losses) > 0) {
    cat("  最終驗證損失:", round(tail(x$training_history$val_losses, 1), 6), "\n")
    cat("  最佳驗證損失:", round(x$training_history$best_val_loss, 6), "\n")
  }
  
  # 收斂性分析
  if(!is.null(x$convergence)) {
    cat("\n🎯 收斂性分析:\n")
    cat("  損失趨勢:", ifelse(x$convergence$is_converging, "收斂", "發散"), "\n")
    cat("  趨勢斜率:", round(x$convergence$loss_trend, 8), "\n")
  }
  
  # 測試性能
  if(!is.null(x$test_performance)) {
    cat("\n🎯 測試集性能:\n")
    cat("  RMSE:", round(x$test_performance$rmse, 4), "\n")
    cat("  MAE:", round(x$test_performance$mae, 4), "\n")
    cat("  R²:", round(x$test_performance$r2, 4), "\n")
  }
}

# ================================================================================
# 7. 實用工具函數
# ================================================================================

#' 清理GPU記憶體
clear_gpu_memory <- function() {
  if(torch::cuda_is_available()) {
    torch::cuda_empty_cache()
    gc()
    cat("✅ GPU記憶體已清理\n")
  }
}

#' 檢查GPU記憶體使用情況
check_gpu_memory <- function() {
  if(torch::cuda_is_available()) {
    allocated <- torch::cuda_memory_allocated() / 1024^3
    reserved <- torch::cuda_memory_reserved() / 1024^3
    
    cat("GPU記憶體使用情況:\n")
    cat("  已分配:", round(allocated, 2), "GB\n")
    cat("  已保留:", round(reserved, 2), "GB\n")
    
    return(list(allocated = allocated, reserved = reserved))
  } else {
    cat("CUDA不可用\n")
    return(NULL)
  }
}

cat("✅ LSTM 模型模組載入完成\n") 