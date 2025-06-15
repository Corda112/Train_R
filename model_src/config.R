# ================================================================================
# AQI 時間序列預測模型訓練 - 基礎配置文件
# ================================================================================

cat("📋 載入模型訓練配置...\n")

# ================================================================================
# 1. 基礎路徑配置
# ================================================================================

# 專案根目錄
PROJECT_ROOT <- getwd()

# 資料路徑配置
DATA_PATHS <- list(
  # 滑動窗口資料根目錄
  sliding_windows = file.path(PROJECT_ROOT, "generate_sliding_windows", "sliding_windows_production"),
  
  # 四種資料類型路徑
  separate = file.path(PROJECT_ROOT, "generate_sliding_windows", "sliding_windows_production", "Separate"),
  separate_norm = file.path(PROJECT_ROOT, "generate_sliding_windows", "sliding_windows_production", "Separate_Normalization"),
  combine = file.path(PROJECT_ROOT, "generate_sliding_windows", "sliding_windows_production", "Combine"),
  combine_norm = file.path(PROJECT_ROOT, "generate_sliding_windows", "sliding_windows_production", "Combine_Normalization")
)

# 輸出路徑配置
OUTPUT_PATHS <- list(
  models = file.path(PROJECT_ROOT, "model_outputs", "models"),
  logs = file.path(PROJECT_ROOT, "model_outputs", "logs"),
  metrics = file.path(PROJECT_ROOT, "model_outputs", "metrics"),
  checkpoints = file.path(PROJECT_ROOT, "model_outputs", "checkpoints")
)

# 創建輸出目錄（如果不存在）
for(path in OUTPUT_PATHS) {
  if(!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# ================================================================================
# 2. 模型訓練常數
# ================================================================================

# 時間序列參數
TIME_SERIES_CONFIG <- list(
  input_seq_len = 72,      # 輸入序列長度 (72小時)
  output_horizon = 1,      # 預測時間範圍 (1小時)
  stride = 1,              # 滑動步長
  target_col = "AQI_aqi"   # 目標變數名稱
)

# 資料切分比例
SPLIT_CONFIG <- list(
  train_ratio = 0.7,       # 訓練集比例
  val_ratio = 0.1,         # 驗證集比例  
  test_ratio = 0.2,        # 測試集比例
  time_based = TRUE        # 基於時間順序切分
)

# 隨機種子設定
RANDOM_SEEDS <- list(
  global = 42,
  lgbm = 123,
  torch = 456
)

# ================================================================================
# 3. LightGBM 超參數配置
# ================================================================================

LGBM_PARAMS <- list(
  # 基礎參數
  objective = "regression",
  metric = "rmse",
  boosting_type = "gbdt",
  
  # 樹結構參數
  num_leaves = 127,
  max_depth = 8,
  min_data_in_leaf = 100,
  
  # 學習參數
  learning_rate = 0.05,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  
  # 正則化參數
  lambda_l1 = 0.1,
  lambda_l2 = 0.1,
  min_gain_to_split = 0.1,
  
  # 性能參數
  num_threads = max(1, parallel::detectCores() - 2),
  max_bin = 63,           # 減少記憶體使用
  
  # 訓練參數
  num_iterations = 1000,
  early_stopping_rounds = 50,
  verbose = -1,
  
  # 隨機種子
  seed = RANDOM_SEEDS$lgbm
)

# ================================================================================
# 4. LSTM 超參數配置
# ================================================================================

LSTM_PARAMS <- list(
  # 網路架構
  hidden_size = 128,
  num_layers = 2,
  dropout = 0.2,
  bidirectional = FALSE,
  
  # 訓練參數
  epochs = 100,
  batch_size = 256,
  learning_rate = 0.001,
  weight_decay = 1e-5,
  
  # Early stopping
  patience = 15,
  min_delta = 1e-4,
  
  # 設備配置 (更新: CUDA 12.4 + cuDNN 9.1.1.17)
  device = "cuda",        # GPU可用！
  mixed_precision = TRUE, # 支援混合精度訓練
  
  # 優化器參數
  optimizer = "adam",
  beta1 = 0.9,
  beta2 = 0.999,
  eps = 1e-8,
  
  # 學習率調度
  scheduler = "reduce_on_plateau",
  scheduler_factor = 0.5,
  scheduler_patience = 10,
  scheduler_min_lr = 1e-6
)

# ================================================================================
# 5. 評估指標配置
# ================================================================================

EVAL_METRICS <- c("rmse", "mae", "mape", "r2", "smape")

# 評估閾值設定
EVAL_THRESHOLDS <- list(
  rmse_good = 20,      # RMSE < 20 為良好
  mae_good = 15,       # MAE < 15 為良好
  mape_good = 0.15,    # MAPE < 15% 為良好
  r2_good = 0.8        # R² > 0.8 為良好
)

# ================================================================================
# 6. 資料類型配置
# ================================================================================

DATA_TYPES <- list(
  separate = list(
    name = "separate",
    display_name = "分站原始資料",
    path = DATA_PATHS$separate,
    is_large = FALSE,
    normalization = FALSE
  ),
  separate_norm = list(
    name = "separate_norm", 
    display_name = "分站標準化資料",
    path = DATA_PATHS$separate_norm,
    is_large = FALSE,
    normalization = TRUE
  ),
  combine = list(
    name = "combine",
    display_name = "合併原始資料", 
    path = DATA_PATHS$combine,
    is_large = TRUE,
    normalization = FALSE
  ),
  combine_norm = list(
    name = "combine_norm",
    display_name = "合併標準化資料",
    path = DATA_PATHS$combine_norm, 
    is_large = TRUE,
    normalization = TRUE
  )
)

# ================================================================================
# 7. 模型配置
# ================================================================================

MODEL_CONFIGS <- list(
  lgbm = list(
    name = "lightgbm",
    display_name = "LightGBM",
    device = "cpu",
    params = LGBM_PARAMS,
    requires_flatten = TRUE,
    supports_feature_importance = TRUE
  ),
  lstm = list(
    name = "lstm", 
    display_name = "LSTM",
    device = "gpu",  # 更新: GPU完全可用！
    params = LSTM_PARAMS,
    requires_flatten = FALSE,
    supports_feature_importance = FALSE
  )
)

# ================================================================================
# 8. 日誌配置
# ================================================================================

LOG_CONFIG <- list(
  level = "INFO",          # DEBUG, INFO, WARN, ERROR
  console = TRUE,          # 是否輸出到控制台
  file = TRUE,             # 是否輸出到文件
  timestamp = TRUE,        # 是否包含時間戳
  max_file_size = "10MB",  # 日誌文件最大大小
  backup_count = 5         # 保留的備份文件數量
)

# ================================================================================
# 9. 實用函數
# ================================================================================

# 設定隨機種子
set_random_seeds <- function() {
  set.seed(RANDOM_SEEDS$global)
  if(requireNamespace("torch", quietly = TRUE)) {
    tryCatch({
      torch::torch_manual_seed(RANDOM_SEEDS$torch)
      if(torch::cuda_is_available()) {
        torch::cuda_manual_seed_all(RANDOM_SEEDS$torch)
      }
    }, error = function(e) {
      # torch未完全安裝，跳過torch種子設定
    })
  }
}

# 檢查GPU可用性
check_gpu_availability <- function() {
  if(!requireNamespace("torch", quietly = TRUE)) {
    cat("⚠️  torch套件未安裝，GPU功能不可用\n")
    return(FALSE)
  }
  
  # 檢查torch是否正確安裝
  tryCatch({
    # 先嘗試載入torch
    library(torch)
    
    # 檢查基本功能
    test_tensor <- torch_tensor(c(1, 2, 3))
    
    # 檢查CUDA
    gpu_available <- cuda_is_available()
    if(gpu_available) {
      gpu_count <- cuda_device_count()
      cat("🔥 GPU可用:", gpu_count, "張卡\n")
      # 安全地檢查GPU記憶體
      tryCatch({
        gpu_memory <- cuda_memory_allocated(0) / 1024^3  # GB
        cat("💾 GPU記憶體使用:", round(gpu_memory, 2), "GB\n")
      }, error = function(e) {
        cat("💾 GPU記憶體狀態檢查跳過\n")
      })
    } else {
      cat("⚠️  GPU不可用，將使用CPU\n")
    }
    return(gpu_available)
  }, error = function(e) {
    cat("⚠️  torch未完全安裝，GPU功能不可用\n")
    cat("💡 錯誤詳情:", e$message, "\n")
    cat("💡 建議: 使用 CPU 模式進行訓練\n")
    return(FALSE)
  })
}

# 檢查必要套件
check_required_packages <- function(auto_install = FALSE) {
  required_packages <- c(
    "data.table", "lightgbm", "Matrix", "abind", "caret"
  )
  
  # 可選套件（缺少時會警告但不會停止）
  optional_packages <- c(
    "torch", "future.apply", "logger", "jsonlite"
  )
  
  missing_packages <- c()
  missing_optional <- c()
  
  # 檢查必需套件
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  # 檢查可選套件
  for(pkg in optional_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      missing_optional <- c(missing_optional, pkg)
    }
  }
  
  # 處理缺少的必需套件
  if(length(missing_packages) > 0) {
    cat("❌ 缺少必要套件:", paste(missing_packages, collapse = ", "), "\n")
    
    if(auto_install) {
      cat("🔄 正在自動安裝缺少的套件...\n")
      tryCatch({
        install.packages(missing_packages, repos = "https://cran.rstudio.com/", dependencies = TRUE)
        cat("✅ 套件安裝完成\n")
      }, error = function(e) {
        cat("❌ 自動安裝失敗:", e$message, "\n")
        cat("請手動執行: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
        return(FALSE)
      })
    } else {
      cat("請執行: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
      return(FALSE)
    }
  }
  
  # 處理缺少的可選套件
  if(length(missing_optional) > 0) {
    cat("⚠️  缺少可選套件:", paste(missing_optional, collapse = ", "), "\n")
    
    # 特別處理torch
    if("torch" %in% missing_optional) {
      cat("📝 注意: torch套件需要額外安裝步驟\n")
      cat("1. install.packages('torch')\n")
      cat("2. torch::install_torch()\n")
    }
    
    if(auto_install) {
      cat("🔄 正在安裝可選套件...\n")
      tryCatch({
        install.packages(missing_optional, repos = "https://cran.rstudio.com/", dependencies = TRUE)
        cat("✅ 可選套件安裝完成\n")
      }, error = function(e) {
        cat("⚠️  可選套件安裝失敗:", e$message, "\n")
      })
    }
  }
  
  cat("✅ 必要套件檢查完成\n")
  return(TRUE)
}

# ================================================================================
# 9.5 錯誤恢復和Checkpoint機制
# ================================================================================

#' 創建checkpoint檔案
#' @param checkpoint_id 檢查點ID
#' @param data 要保存的資料
#' @param checkpoint_dir 檢查點目錄
#' @param verbose 是否顯示詳細資訊
create_checkpoint <- function(checkpoint_id, data, checkpoint_dir = OUTPUT_PATHS$checkpoints, verbose = TRUE) {
  if(!dir.exists(checkpoint_dir)) {
    dir.create(checkpoint_dir, recursive = TRUE)
  }
  
  checkpoint_file <- file.path(checkpoint_dir, paste0(checkpoint_id, "_checkpoint.rds"))
  
  tryCatch({
    saveRDS(data, checkpoint_file)
    
    if(verbose) {
      cat("📝 Checkpoint已創建:", basename(checkpoint_file), "\n")
    }
    
    return(checkpoint_file)
    
  }, error = function(e) {
    warning("創建Checkpoint失敗: ", e$message)
    return(NULL)
  })
}

#' 載入checkpoint檔案
#' @param checkpoint_id 檢查點ID
#' @param checkpoint_dir 檢查點目錄
#' @param verbose 是否顯示詳細資訊
#' @return checkpoint資料或NULL
load_checkpoint <- function(checkpoint_id, checkpoint_dir = OUTPUT_PATHS$checkpoints, verbose = TRUE) {
  checkpoint_file <- file.path(checkpoint_dir, paste0(checkpoint_id, "_checkpoint.rds"))
  
  if(!file.exists(checkpoint_file)) {
    if(verbose) {
      cat("📝 未找到checkpoint:", basename(checkpoint_file), "\n")
    }
    return(NULL)
  }
  
  tryCatch({
    data <- readRDS(checkpoint_file)
    
    if(verbose) {
      cat("📥 載入checkpoint:", basename(checkpoint_file), "\n")
    }
    
    return(data)
    
  }, error = function(e) {
    warning("載入Checkpoint失敗: ", e$message)
    return(NULL)
  })
}

#' 檢查任務是否已完成
#' @param task_id 任務ID
#' @param output_dir 輸出目錄
#' @return 是否已完成
is_task_completed <- function(task_id, output_dir = OUTPUT_PATHS$models) {
  done_file <- file.path(output_dir, paste0(task_id, ".done"))
  return(file.exists(done_file))
}

#' 標記任務完成
#' @param task_id 任務ID
#' @param output_dir 輸出目錄
#' @param metadata 任務元資料
#' @param verbose 是否顯示詳細資訊
mark_task_completed <- function(task_id, output_dir = OUTPUT_PATHS$models, metadata = list(), verbose = TRUE) {
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  done_file <- file.path(output_dir, paste0(task_id, ".done"))
  
  # 創建完成記錄
  completion_record <- list(
    task_id = task_id,
    completed_at = Sys.time(),
    metadata = metadata,
    version = "1.0"
  )
  
  tryCatch({
    writeLines(jsonlite::toJSON(completion_record, pretty = TRUE), done_file)
    
    if(verbose) {
      cat("✅ 任務已標記完成:", task_id, "\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    warning("標記任務完成失敗: ", e$message)
    return(FALSE)
  })
}

#' 安全執行函數（帶錯誤恢復）
#' @param func 要執行的函數
#' @param args 函數參數
#' @param task_id 任務ID
#' @param max_retries 最大重試次數
#' @param retry_delay 重試延遲（秒）
#' @param verbose 是否顯示詳細資訊
safe_execute <- function(func, args = list(), task_id = NULL, max_retries = 3, retry_delay = 5, verbose = TRUE) {
  # 檢查任務是否已完成
  if(!is.null(task_id) && is_task_completed(task_id)) {
    if(verbose) {
      cat("⏭️  任務已完成，跳過:", task_id, "\n")
    }
    return(list(success = TRUE, result = NULL, skipped = TRUE))
  }
  
  for(attempt in 1:max_retries) {
    if(verbose && attempt > 1) {
      cat("🔄 重試第", attempt, "次...\n")
    }
    
    tryCatch({
      # 執行函數
      result <- do.call(func, args)
      
      # 標記任務完成
      if(!is.null(task_id)) {
        mark_task_completed(task_id, metadata = list(
          attempt = attempt,
          success = TRUE,
          execution_time = Sys.time()
        ))
      }
      
      return(list(success = TRUE, result = result, skipped = FALSE))
      
    }, error = function(e) {
      error_msg <- e$message
      
      if(verbose) {
        cat("❌ 執行失敗 (嘗試", attempt, "/", max_retries, "):", error_msg, "\n")
      }
      
      # 如果不是最後一次嘗試，等待後重試
      if(attempt < max_retries) {
        if(verbose) {
          cat("⏰ 等待", retry_delay, "秒後重試...\n")
        }
        Sys.sleep(retry_delay)
      } else {
        # 最後一次嘗試失敗
        if(verbose) {
          cat("💥 所有重試都失敗了\n")
        }
        return(list(success = FALSE, result = NULL, error = error_msg, skipped = FALSE))
      }
    })
  }
}

#' 清理資源（GPU記憶體等）
#' @param clear_gpu 是否清理GPU記憶體
#' @param run_gc 是否運行垃圾回收
#' @param verbose 是否顯示詳細資訊
cleanup_resources <- function(clear_gpu = TRUE, run_gc = TRUE, verbose = TRUE) {
  if(run_gc) {
    if(verbose) {
      cat("🧹 執行垃圾回收...\n")
    }
    gc()
  }
  
  if(clear_gpu && requireNamespace("torch", quietly = TRUE)) {
    tryCatch({
      if(torch::cuda_is_available()) {
        if(verbose) {
          cat("🔥 清理GPU記憶體...\n")
        }
        torch::cuda_empty_cache()
        
        if(verbose) {
          gpu_memory <- torch::cuda_memory_allocated(0) / 1024^3
          cat("💾 當前GPU記憶體使用:", round(gpu_memory, 2), "GB\n")
        }
      }
    }, error = function(e) {
      if(verbose) {
        cat("⚠️  GPU記憶體清理跳過（torch未完全安裝）\n")
      }
    })
  }
}

# 創建時間戳
create_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

# 格式化檔案大小
format_file_size <- function(bytes) {
  if(bytes < 1024) return(paste(bytes, "B"))
  if(bytes < 1024^2) return(paste(round(bytes/1024, 1), "KB"))
  if(bytes < 1024^3) return(paste(round(bytes/1024^2, 1), "MB"))
  return(paste(round(bytes/1024^3, 1), "GB"))
}

# ================================================================================
# 10. 初始化檢查
# ================================================================================

# 執行初始化檢查
initialize_config <- function() {
  cat("🚀 初始化模型訓練環境...\n")
  
  # 檢查必要套件
  if(!check_required_packages()) {
    stop("請先安裝缺少的套件")
  }
  
  # 設定隨機種子
  set_random_seeds()
  
  # 檢查GPU
  gpu_available <- check_gpu_availability()
  
  # 檢查資料路徑
  for(data_type in names(DATA_TYPES)) {
    path <- DATA_TYPES[[data_type]]$path
    if(!dir.exists(path)) {
      cat("⚠️  資料路徑不存在:", path, "\n")
    } else {
      files <- list.files(path, pattern = "\\.rds$")
      cat("📁", DATA_TYPES[[data_type]]$display_name, ":", length(files), "個檔案\n")
    }
  }
  
  # 更新LSTM配置基於GPU可用性
  if(!gpu_available) {
    LSTM_PARAMS$device <<- "cpu"
    LSTM_PARAMS$mixed_precision <<- FALSE
    cat("⚠️  LSTM將使用CPU訓練\n")
  }
  
  cat("✅ 環境初始化完成\n\n")
  
  return(list(
    gpu_available = gpu_available,
    data_paths = DATA_PATHS,
    output_paths = OUTPUT_PATHS
  ))
}

# ================================================================================
# 11. 配置摘要
# ================================================================================

print_config_summary <- function() {
  cat("📋 模型訓練配置摘要\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  cat("🔹 資料配置:\n")
  cat("  - 輸入序列長度:", TIME_SERIES_CONFIG$input_seq_len, "小時\n")
  cat("  - 預測時間範圍:", TIME_SERIES_CONFIG$output_horizon, "小時\n")
  cat("  - 資料切分比例:", SPLIT_CONFIG$train_ratio, "/", SPLIT_CONFIG$val_ratio, "/", SPLIT_CONFIG$test_ratio, "\n")
  
  cat("\n🔹 模型配置:\n")
  cat("  - LightGBM: CPU,", LGBM_PARAMS$num_iterations, "輪,", LGBM_PARAMS$learning_rate, "學習率\n")
  cat("  - LSTM:", LSTM_PARAMS$device, ",", LSTM_PARAMS$epochs, "輪,", LSTM_PARAMS$batch_size, "批次大小\n")
  
  cat("\n🔹 輸出路徑:\n")
  for(name in names(OUTPUT_PATHS)) {
    cat("  -", name, ":", OUTPUT_PATHS[[name]], "\n")
  }
  
  cat("\n")
}

# 自動執行初始化（當載入此文件時）
if(!exists("CONFIG_INITIALIZED")) {
  CONFIG_INITIALIZED <- TRUE
  env_info <- initialize_config()
  
  # 如果是互動模式，顯示配置摘要
  if(interactive()) {
    print_config_summary()
  }
}

cat("✅ 配置文件載入完成\n") 