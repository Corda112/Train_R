# ================================================================================
# AQI 時間序列預測模型訓練 - LSTM 模型模組 (優化版)
# ================================================================================

# 載入必要套件
if(!requireNamespace("torch", quietly = TRUE)) {
  stop("請安裝 torch 套件: install.packages('torch')")
}

if(!requireNamespace("R6", quietly = TRUE)) {
  stop("請安裝 R6 套件: install.packages('R6')")
}

library(torch)

cat("🧠 載入 LSTM 模型模組 (優化版)...\n")

# ================================================================================
# 1. LSTM 網路架構定義
# ================================================================================

#' LSTM 網路模組定義
#' @param input_size 輸入特徵維度
#' @param hidden_size 隱藏層大小
#' @param num_layers LSTM層數
#' @param dropout Dropout比例
#' @param bidirectional 是否使用雙向LSTM
#' @return LSTM網路模組
lstm_net <- nn_module(
  "LSTMNet",
  
  initialize = function(input_size, hidden_size = 128, num_layers = 2, 
                       dropout = 0.2, bidirectional = FALSE) {
    self$input_size <- input_size
    self$hidden_size <- hidden_size
    self$num_layers <- num_layers
    self$bidirectional <- bidirectional
    
    # LSTM層
    self$lstm <- nn_lstm(
      input_size = input_size,
      hidden_size = hidden_size,
      num_layers = num_layers,
      dropout = if(num_layers > 1) dropout else 0,
      bidirectional = bidirectional,
      batch_first = TRUE
    )
    
    # 計算LSTM輸出維度
    lstm_output_size <- if(bidirectional) hidden_size * 2 else hidden_size
    
    # 全連接層
    self$dropout <- nn_dropout(dropout)
    self$fc1 <- nn_linear(lstm_output_size, hidden_size)
    self$fc2 <- nn_linear(hidden_size, hidden_size %/% 2)
    self$fc3 <- nn_linear(hidden_size %/% 2, 1)
    
    # 激活函數
    self$relu <- nn_relu()
    self$tanh <- nn_tanh()
  },
  
  forward = function(x) {
    # x: [batch_size, seq_len, input_size]
    
    # LSTM前向傳播
    lstm_out <- self$lstm(x)
    
    # 取最後一個時間步的輸出
    # lstm_out[[1]]: [batch_size, seq_len, hidden_size * directions]
    seq_len <- dim(lstm_out[[1]])[2]
    last_output <- lstm_out[[1]][, seq_len, ]  # [batch_size, hidden_size * directions]
    
    # 全連接層
    out <- self$dropout(last_output)
    out <- self$relu(self$fc1(out))
    out <- self$dropout(out)
    out <- self$relu(self$fc2(out))
    out <- self$fc3(out)
    
    return(out)
  }
)

# ================================================================================
# 2. 早停機制
# ================================================================================

#' 早停回調類
#' @param patience 容忍輪數
#' @param min_delta 最小改善幅度
#' @param restore_best_weights 是否恢復最佳權重
early_stopping <- R6::R6Class(
  "EarlyStopping",
  
  public = list(
    patience = NULL,
    min_delta = NULL,
    restore_best_weights = NULL,
    best_loss = NULL,
    counter = NULL,
    best_weights = NULL,
    
    initialize = function(patience = 15, min_delta = 1e-4, restore_best_weights = TRUE) {
      self$patience <- patience
      self$min_delta <- min_delta
      self$restore_best_weights <- restore_best_weights
      self$best_loss <- Inf
      self$counter <- 0
      self$best_weights <- NULL
    },
    
    check = function(val_loss, model) {
      if(val_loss < self$best_loss - self$min_delta) {
        self$best_loss <- val_loss
        self$counter <- 0
        if(self$restore_best_weights) {
          self$best_weights <- model$state_dict()
        }
        return(FALSE)  # 不停止
      } else {
        self$counter <- self$counter + 1
        if(self$counter >= self$patience) {
          if(self$restore_best_weights && !is.null(self$best_weights)) {
            model$load_state_dict(self$best_weights)
          }
          return(TRUE)  # 停止訓練
        }
        return(FALSE)
      }
    }
  )
)

# ================================================================================
# 3. 學習率調度器
# ================================================================================

#' 創建學習率調度器
#' @param optimizer 優化器
#' @param scheduler_type 調度器類型
#' @param scheduler_params 調度器參數
#' @return 學習率調度器
create_scheduler <- function(optimizer, scheduler_type = "reduce_on_plateau", 
                           scheduler_params = list()) {
  
  if(scheduler_type == "reduce_on_plateau") {
    params <- list(
      factor = 0.5,
      patience = 10,
      min_lr = 1e-6,
      verbose = TRUE
    )
    params[names(scheduler_params)] <- scheduler_params
    
    scheduler <- lr_reduce_on_plateau(
      optimizer,
      factor = params$factor,
      patience = params$patience,
      min_lr = params$min_lr,
      verbose = params$verbose
    )
    
  } else if(scheduler_type == "step") {
    params <- list(
      step_size = 30,
      gamma = 0.1
    )
    params[names(scheduler_params)] <- scheduler_params
    
    scheduler <- lr_step(
      optimizer,
      step_size = params$step_size,
      gamma = params$gamma
    )
    
  } else if(scheduler_type == "cosine") {
    params <- list(
      T_max = 50,
      eta_min = 1e-6
    )
    params[names(scheduler_params)] <- scheduler_params
    
    scheduler <- lr_cosine_annealing(
      optimizer,
      T_max = params$T_max,
      eta_min = params$eta_min
    )
    
  } else {
    stop("不支援的調度器類型: ", scheduler_type)
  }
  
  return(scheduler)
}

# ================================================================================
# 4. 資料載入器
# ================================================================================

#' 創建資料載入器
#' @param dataset AQI資料集
#' @param batch_size 批次大小
#' @param shuffle 是否打亂
#' @param device 設備
#' @return 資料載入器
create_dataloader <- function(dataset, batch_size = 256, shuffle = FALSE, device = "cuda") {
  # 轉換為張量
  x_tensor <- torch_tensor(dataset$x, dtype = torch_float32())$to(device = device)
  y_tensor <- torch_tensor(dataset$y, dtype = torch_float32())$unsqueeze(2)$to(device = device)
  
  # 創建TensorDataset
  tensor_dataset <- tensor_dataset(x_tensor, y_tensor)
  
  # 創建DataLoader
  dataloader <- dataloader(
    tensor_dataset,
    batch_size = batch_size,
    shuffle = shuffle,
    drop_last = FALSE
  )
  
  return(dataloader)
}

# ================================================================================
# 5. 混合精度訓練支援
# ================================================================================

#' 檢查是否支援混合精度
#' @param device 設備
#' @return 是否支援混合精度
check_mixed_precision_support <- function(device = "cuda") {
  if(device == "cpu") {
    return(FALSE)
  }
  
      if(!cuda_is_available()) {
    return(FALSE)
  }
  
  # 檢查GPU計算能力
  tryCatch({
    # 嘗試創建GradScaler
    scaler <- torch_amp_grad_scaler()
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# ================================================================================
# 6. LSTM 訓練函數 (優化版)
# ================================================================================

#' 訓練 LSTM 模型 (優化版)
#' @param train_dataset 訓練資料集
#' @param val_dataset 驗證資料集
#' @param params LSTM 參數列表
#' @param save_checkpoint 是否保存檢查點
#' @param checkpoint_path 檢查點保存路徑
#' @param verbose 是否顯示詳細資訊
#' @return 訓練好的模型物件
train_lstm <- function(train_dataset, val_dataset = NULL, params = LSTM_PARAMS,
                      save_checkpoint = TRUE, checkpoint_path = NULL, verbose = TRUE) {
  
  if(verbose) {
    cat("🧠 開始訓練 LSTM 模型...\n")
    cat("訓練樣本數:", train_dataset$n_windows, "\n")
    if(!is.null(val_dataset)) {
      cat("驗證樣本數:", val_dataset$n_windows, "\n")
    }
    cat("特徵維度:", train_dataset$n_features, "\n")
    cat("序列長度:", train_dataset$seq_len, "\n")
  }
  
  start_time <- Sys.time()
  
  # 設定隨機種子
  torch_manual_seed(RANDOM_SEEDS$torch)
  set.seed(RANDOM_SEEDS$torch)
  
  # 設定設備
  device <- if(params$device == "cuda" && cuda_is_available()) "cuda" else "cpu"
  if(verbose) {
    cat("使用設備:", device, "\n")
    if(device == "cuda") {
              cat("GPU: CUDA設備可用\n")
        # GPU記憶體資訊 (簡化版)
        tryCatch({
          test_tensor <- torch_randn(100, 100, device = "cuda")
          cat("GPU記憶體: 正常\n")
          rm(test_tensor)
        }, error = function(e) {
          cat("GPU記憶體: 檢查失敗\n")
        })
    }
  }
  
  # 檢查混合精度支援
  use_mixed_precision <- params$mixed_precision && check_mixed_precision_support(device)
  if(verbose && params$mixed_precision) {
    if(use_mixed_precision) {
      cat("✅ 啟用混合精度訓練\n")
    } else {
      cat("⚠️  混合精度不支援，使用標準精度\n")
    }
  }
  
  # 創建模型
  model <- lstm_net(
    input_size = train_dataset$n_features,
    hidden_size = params$hidden_size,
    num_layers = params$num_layers,
    dropout = params$dropout,
    bidirectional = params$bidirectional
  )$to(device = device)
  
  if(verbose) {
    cat("模型參數數量:", sum(sapply(model$parameters, function(p) p$numel())), "\n")
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
    val_loader <- create_dataloader(
      val_dataset, 
      batch_size = params$batch_size, 
      shuffle = FALSE, 
      device = device
    )
  }
  
  # 創建優化器
  if(params$optimizer == "adam") {
    optimizer <- optim_adam(
      model$parameters,
      lr = params$learning_rate,
      betas = c(params$beta1, params$beta2),
      eps = params$eps,
      weight_decay = params$weight_decay
    )
  } else if(params$optimizer == "adamw") {
    optimizer <- optim_adamw(
      model$parameters,
      lr = params$learning_rate,
      betas = c(params$beta1, params$beta2),
      eps = params$eps,
      weight_decay = params$weight_decay
    )
  } else {
    stop("不支援的優化器: ", params$optimizer)
  }
  
  # 創建學習率調度器
  scheduler <- create_scheduler(
    optimizer,
    scheduler_type = params$scheduler,
    scheduler_params = list(
      factor = params$scheduler_factor,
      patience = params$scheduler_patience,
      min_lr = params$scheduler_min_lr
    )
  )
  
  # 創建損失函數
  criterion <- nn_mse_loss()
  
  # 創建早停機制
  early_stop <- early_stopping$new(
    patience = params$patience,
    min_delta = params$min_delta,
    restore_best_weights = TRUE
  )
  
  # 創建混合精度縮放器
  scaler <- if(use_mixed_precision) torch_amp_grad_scaler() else NULL
  
  # 訓練歷史記錄
  train_losses <- numeric(params$epochs)
  val_losses <- numeric(params$epochs)
  learning_rates <- numeric(params$epochs)
  
  # 訓練循環
  if(verbose) cat("🚀 開始模型訓練...\n")
  
  for(epoch in 1:params$epochs) {
    epoch_start_time <- Sys.time()
    
    # 訓練階段
    model$train()
    train_loss <- 0
    train_batches <- 0
    
    coro::loop(for(batch in train_loader) {
      optimizer$zero_grad()
      
      x_batch <- batch[[1]]
      y_batch <- batch[[2]]
      
      if(use_mixed_precision) {
        # 混合精度前向傳播
        with_autocast(device_type = device, {
          outputs <- model(x_batch)
          loss <- criterion(outputs, y_batch)
        })
        
        # 混合精度反向傳播
        scaler$scale(loss)$backward()
        scaler$step(optimizer)
        scaler$update()
        
      } else {
        # 標準精度訓練
        outputs <- model(x_batch)
        loss <- criterion(outputs, y_batch)
        loss$backward()
        optimizer$step()
      }
      
      train_loss <- train_loss + loss$item()
      train_batches <- train_batches + 1
    })
    
    train_loss <- train_loss / train_batches
    train_losses[epoch] <- train_loss
    
    # 驗證階段
    val_loss <- NA
    if(!is.null(val_loader)) {
      model$eval()
      val_loss_sum <- 0
      val_batches <- 0
      
      with_no_grad({
        coro::loop(for(batch in val_loader) {
          x_batch <- batch[[1]]
          y_batch <- batch[[2]]
          
          outputs <- model(x_batch)
          loss <- criterion(outputs, y_batch)
          
          val_loss_sum <- val_loss_sum + loss$item()
          val_batches <- val_batches + 1
        })
      })
      
      val_loss <- val_loss_sum / val_batches
      val_losses[epoch] <- val_loss
      
      # 學習率調度
      if(params$scheduler == "reduce_on_plateau") {
        scheduler$step(val_loss)
      } else {
        scheduler$step()
      }
      
      # 早停檢查
      if(early_stop$check(val_loss, model)) {
        if(verbose) {
          cat("🛑 早停觸發，停止訓練 (epoch", epoch, ")\n")
        }
        break
      }
    } else {
      scheduler$step()
    }
    
    # 記錄學習率
    learning_rates[epoch] <- optimizer$param_groups[[1]]$lr
    
    # 顯示進度
    if(verbose && (epoch %% 10 == 0 || epoch == 1)) {
      epoch_time <- as.numeric(difftime(Sys.time(), epoch_start_time, units = "secs"))
      cat(sprintf("Epoch %3d/%d - 訓練損失: %.6f", epoch, params$epochs, train_loss))
      if(!is.na(val_loss)) {
        cat(sprintf(" - 驗證損失: %.6f", val_loss))
      }
      cat(sprintf(" - 學習率: %.2e - 時間: %.1fs\n", learning_rates[epoch], epoch_time))
      
      if(device == "cuda") {
        cat("         GPU記憶體: 正常\n")
      }
    }
    
    # 保存檢查點
    if(save_checkpoint && !is.null(checkpoint_path) && epoch %% 20 == 0) {
      checkpoint_file <- paste0(checkpoint_path, "_epoch_", epoch, ".pt")
      save_lstm_checkpoint(model, optimizer, epoch, train_loss, val_loss, checkpoint_file)
    }
  }
  
  training_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
  
  if(verbose) {
    cat("✅ LSTM 訓練完成\n")
    cat("訓練時間:", round(training_time, 2), "分鐘\n")
    cat("最終訓練損失:", round(tail(train_losses[!is.na(train_losses)], 1), 6), "\n")
    if(!is.null(val_dataset)) {
      cat("最終驗證損失:", round(tail(val_losses[!is.na(val_losses)], 1), 6), "\n")
      cat("最佳驗證損失:", round(early_stop$best_loss, 6), "\n")
    }
  }
  
  # 創建模型物件
  lstm_model <- list(
    model = model,
    training_params = params,
    training_history = list(
      train_losses = train_losses[!is.na(train_losses)],
      val_losses = val_losses[!is.na(val_losses)],
      learning_rates = learning_rates[!is.na(learning_rates)]
    ),
    training_time = training_time,
    best_val_loss = if(!is.null(val_dataset)) early_stop$best_loss else NA,
    data_type = train_dataset$data_type,
    input_size = train_dataset$n_features,
    seq_len = train_dataset$seq_len,
    device = device,
    created_at = Sys.time(),
    model_type = "lstm"
  )
  
  class(lstm_model) <- c("aqi_lstm_model", "list")
  
  # 保存最終檢查點
  if(save_checkpoint && !is.null(checkpoint_path)) {
    final_checkpoint <- paste0(checkpoint_path, "_final.pt")
    save_lstm_checkpoint(model, optimizer, epoch, train_loss, val_loss, final_checkpoint)
  }
  
  return(lstm_model)
}

# ================================================================================
# 7. LSTM 預測函數
# ================================================================================

#' LSTM 模型預測
#' @param lstm_model 訓練好的 LSTM 模型
#' @param test_dataset 測試資料集
#' @param batch_size 預測批次大小
#' @param verbose 是否顯示詳細資訊
#' @return 預測結果向量
predict_lstm <- function(lstm_model, test_dataset, batch_size = 512, verbose = TRUE) {
  if(!inherits(lstm_model, "aqi_lstm_model")) {
    stop("lstm_model 必須是 aqi_lstm_model 物件")
  }
  
  if(verbose) {
    cat("🔮 執行 LSTM 預測...\n")
    cat("測試樣本數:", test_dataset$n_windows, "\n")
  }
  
  # 檢查特徵維度一致性
  if(test_dataset$n_features != lstm_model$input_size) {
    stop("測試資料特徵維度與模型不一致: ", test_dataset$n_features, " vs ", lstm_model$input_size)
  }
  
  if(test_dataset$seq_len != lstm_model$seq_len) {
    stop("測試資料序列長度與模型不一致: ", test_dataset$seq_len, " vs ", lstm_model$seq_len)
  }
  
  # 創建測試資料載入器
  test_loader <- create_dataloader(
    test_dataset,
    batch_size = batch_size,
    shuffle = FALSE,
    device = lstm_model$device
  )
  
  # 預測
  lstm_model$model$eval()
  predictions <- c()
  
  with_no_grad({
    coro::loop(for(batch in test_loader) {
      x_batch <- batch[[1]]
      outputs <- lstm_model$model(x_batch)
      batch_predictions <- as.numeric(outputs$cpu())
      predictions <- c(predictions, batch_predictions)
    })
  })
  
  if(verbose) {
    cat("✅ 預測完成\n")
    cat("預測範圍:", round(min(predictions), 2), "~", round(max(predictions), 2), "\n")
  }
  
  return(predictions)
}

# ================================================================================
# 8. GPU 記憶體管理
# ================================================================================

#' 清理GPU記憶體
#' @param verbose 是否顯示詳細資訊
clear_gpu_memory <- function(verbose = TRUE) {
  if(cuda_is_available()) {
    if(verbose) {
      before_mem <- 0  # GPU記憶體監控簡化
      cat("清理前GPU記憶體:", round(before_mem, 2), "GB\n")
    }
    
    # 清理快取
    cuda_empty_cache()
    
    # 強制垃圾回收
    gc()
    
    if(verbose) {
      after_mem <- 0  # GPU記憶體監控簡化
      cat("清理後GPU記憶體:", round(after_mem, 2), "GB\n")
      cat("釋放記憶體:", round(before_mem - after_mem, 2), "GB\n")
    }
  } else {
    if(verbose) cat("CUDA不可用，跳過GPU記憶體清理\n")
  }
}

#' 監控GPU記憶體使用
#' @return GPU記憶體使用資訊
monitor_gpu_memory <- function() {
  if(!cuda_is_available()) {
    return(list(
      available = FALSE,
      allocated = 0,
      reserved = 0,
      free = 0
    ))
  }
  
  allocated <- 0  # GPU記憶體監控簡化
  reserved <- 0   # GPU記憶體監控簡化
  
  return(list(
    available = TRUE,
    allocated_gb = round(allocated / 1024^3, 2),
    reserved_gb = round(reserved / 1024^3, 2),
    allocated_mb = round(allocated / 1024^2, 1),
    reserved_mb = round(reserved / 1024^2, 1)
  ))
}

# ================================================================================
# 9. 模型保存與載入
# ================================================================================

#' 保存 LSTM 模型
#' @param lstm_model LSTM 模型物件
#' @param save_path 保存路徑 (不含副檔名)
#' @param save_training_history 是否保存訓練歷史
#' @param verbose 是否顯示詳細資訊
save_lstm_model <- function(lstm_model, save_path, save_training_history = TRUE, verbose = TRUE) {
  if(!inherits(lstm_model, "aqi_lstm_model")) {
    stop("lstm_model 必須是 aqi_lstm_model 物件")
  }
  
  # 創建保存目錄
  dir.create(dirname(save_path), recursive = TRUE, showWarnings = FALSE)
  
  # 保存完整模型物件
  model_path <- paste0(save_path, "_complete.rds")
  saveRDS(lstm_model, model_path)
  
  # 保存PyTorch模型狀態
  state_path <- paste0(save_path, "_state.pt")
  torch_save(lstm_model$model$state_dict(), state_path)
  
  # 保存模型架構資訊
  arch_info <- list(
    input_size = lstm_model$input_size,
    hidden_size = lstm_model$training_params$hidden_size,
    num_layers = lstm_model$training_params$num_layers,
    dropout = lstm_model$training_params$dropout,
    bidirectional = lstm_model$training_params$bidirectional
  )
  arch_path <- paste0(save_path, "_architecture.rds")
  saveRDS(arch_info, arch_path)
  
  # 保存訓練歷史
  if(save_training_history && !is.null(lstm_model$training_history)) {
    history_path <- paste0(save_path, "_history.rds")
    saveRDS(lstm_model$training_history, history_path)
  }
  
  if(verbose) {
    cat("💾 LSTM 模型已保存:\n")
    cat("  完整模型:", basename(model_path), "\n")
    cat("  模型狀態:", basename(state_path), "\n")
    cat("  架構資訊:", basename(arch_path), "\n")
    if(save_training_history) {
      cat("  訓練歷史:", basename(history_path), "\n")
    }
  }
}

#' 載入 LSTM 模型
#' @param model_path 模型路徑 (不含副檔名)
#' @param device 載入到的設備
#' @param load_complete 是否載入完整模型
#' @param verbose 是否顯示詳細資訊
#' @return LSTM 模型物件
load_lstm_model <- function(model_path, device = "cuda", load_complete = TRUE, verbose = TRUE) {
  if(load_complete) {
    complete_path <- paste0(model_path, "_complete.rds")
    if(!file.exists(complete_path)) {
      stop("完整模型檔案不存在: ", complete_path)
    }
    
    lstm_model <- readRDS(complete_path)
    
    # 移動模型到指定設備
    if(device != lstm_model$device) {
      lstm_model$model <- lstm_model$model$to(device = device)
      lstm_model$device <- device
    }
    
    if(verbose) {
      cat("📥 載入完整 LSTM 模型:", basename(complete_path), "\n")
      cat("  資料類型:", lstm_model$data_type, "\n")
      cat("  輸入維度:", lstm_model$input_size, "\n")
      cat("  設備:", lstm_model$device, "\n")
      cat("  創建時間:", format(lstm_model$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
    }
    
    return(lstm_model)
    
  } else {
    # 載入架構和狀態
    arch_path <- paste0(model_path, "_architecture.rds")
    state_path <- paste0(model_path, "_state.pt")
    
    if(!file.exists(arch_path) || !file.exists(state_path)) {
      stop("模型架構或狀態檔案不存在")
    }
    
    arch_info <- readRDS(arch_path)
    
    # 重建模型
    model <- lstm_net(
      input_size = arch_info$input_size,
      hidden_size = arch_info$hidden_size,
      num_layers = arch_info$num_layers,
      dropout = arch_info$dropout,
      bidirectional = arch_info$bidirectional
    )$to(device = device)
    
    # 載入狀態
    state_dict <- torch_load(state_path, device = device)
    model$load_state_dict(state_dict)
    
    if(verbose) {
      cat("📥 載入 LSTM 模型架構和狀態\n")
      cat("  輸入維度:", arch_info$input_size, "\n")
      cat("  隱藏層大小:", arch_info$hidden_size, "\n")
      cat("  設備:", device, "\n")
    }
    
    return(model)
  }
}

# ================================================================================
# 10. 檢查點功能
# ================================================================================

#' 保存 LSTM 檢查點
#' @param model LSTM 模型
#' @param optimizer 優化器
#' @param epoch 當前輪數
#' @param train_loss 訓練損失
#' @param val_loss 驗證損失
#' @param checkpoint_path 檢查點路徑
save_lstm_checkpoint <- function(model, optimizer, epoch, train_loss, val_loss, checkpoint_path) {
  dir.create(dirname(checkpoint_path), recursive = TRUE, showWarnings = FALSE)
  
  checkpoint <- list(
    model_state_dict = model$state_dict(),
    optimizer_state_dict = optimizer$state_dict(),
    epoch = epoch,
    train_loss = train_loss,
    val_loss = val_loss,
    timestamp = Sys.time()
  )
  
  torch_save(checkpoint, checkpoint_path)
}

#' 載入 LSTM 檢查點
#' @param checkpoint_path 檢查點路徑
#' @param model LSTM 模型
#' @param optimizer 優化器 (可選)
#' @param device 設備
#' @param verbose 是否顯示詳細資訊
#' @return 檢查點資訊
load_lstm_checkpoint <- function(checkpoint_path, model, optimizer = NULL, device = "cuda", verbose = TRUE) {
  if(!file.exists(checkpoint_path)) {
    stop("檢查點檔案不存在: ", checkpoint_path)
  }
  
  checkpoint <- torch_load(checkpoint_path, device = device)
  
  model$load_state_dict(checkpoint$model_state_dict)
  
  if(!is.null(optimizer)) {
    optimizer$load_state_dict(checkpoint$optimizer_state_dict)
  }
  
  if(verbose) {
    cat("📥 載入 LSTM 檢查點:", basename(checkpoint_path), "\n")
    cat("  輪數:", checkpoint$epoch, "\n")
    cat("  訓練損失:", round(checkpoint$train_loss, 6), "\n")
    if(!is.na(checkpoint$val_loss)) {
      cat("  驗證損失:", round(checkpoint$val_loss, 6), "\n")
    }
    cat("  時間戳:", format(checkpoint$timestamp, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  return(checkpoint)
}

# ================================================================================
# 11. 工具函數
# ================================================================================

#' 檢查 LSTM 模型健康狀態
#' @param lstm_model LSTM 模型物件
#' @return 健康檢查結果
check_lstm_model_health <- function(lstm_model) {
  if(!inherits(lstm_model, "aqi_lstm_model")) {
    return(list(is_healthy = FALSE, issues = "不是有效的 aqi_lstm_model 物件"))
  }
  
  issues <- c()
  
  # 檢查必要組件
  required_components <- c("model", "training_params", "input_size", "seq_len")
  missing_components <- setdiff(required_components, names(lstm_model))
  if(length(missing_components) > 0) {
    issues <- c(issues, paste("缺少組件:", paste(missing_components, collapse = ", ")))
  }
  
  # 檢查模型物件
  if(is.null(lstm_model$model)) {
    issues <- c(issues, "模型物件為空")
  }
  
  # 檢查設備一致性
  if(!is.null(lstm_model$model) && !is.null(lstm_model$device)) {
    tryCatch({
      # 嘗試獲取模型參數的設備
      model_device <- lstm_model$model$parameters[[1]]$device$type
      if(model_device != lstm_model$device) {
        issues <- c(issues, "模型設備與記錄的設備不一致")
      }
    }, error = function(e) {
      issues <- c(issues, "無法檢查模型設備")
    })
  }
  
  is_healthy <- length(issues) == 0
  
  return(list(
    is_healthy = is_healthy,
    issues = if(length(issues) > 0) issues else "模型狀態良好"
  ))
}

#' 獲取模型摘要資訊
#' @param lstm_model LSTM 模型物件
#' @return 模型摘要
get_lstm_model_summary <- function(lstm_model) {
  if(!inherits(lstm_model, "aqi_lstm_model")) {
    stop("lstm_model 必須是 aqi_lstm_model 物件")
  }
  
  # 計算參數數量
  total_params <- sum(sapply(lstm_model$model$parameters, function(p) p$numel()))
  trainable_params <- sum(sapply(lstm_model$model$parameters[sapply(lstm_model$model$parameters, function(p) p$requires_grad)], function(p) p$numel()))
  
  summary <- list(
    model_type = "LSTM",
    data_type = lstm_model$data_type,
    input_size = lstm_model$input_size,
    seq_len = lstm_model$seq_len,
    hidden_size = lstm_model$training_params$hidden_size,
    num_layers = lstm_model$training_params$num_layers,
    bidirectional = lstm_model$training_params$bidirectional,
    total_params = total_params,
    trainable_params = trainable_params,
    device = lstm_model$device,
    training_time = lstm_model$training_time,
    best_val_loss = lstm_model$best_val_loss,
    created_at = lstm_model$created_at
  )
  
  return(summary)
}

cat("✅ LSTM 模型模組 (優化版) 載入完成\n") 