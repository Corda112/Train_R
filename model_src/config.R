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
  
  # 設備配置
  device = "cuda",        # 優先使用GPU
  mixed_precision = TRUE, # 混合精度訓練
  
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
    device = "gpu",
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
    torch::torch_manual_seed(RANDOM_SEEDS$torch)
    if(torch::cuda_is_available()) {
      torch::cuda_manual_seed_all(RANDOM_SEEDS$torch)
    }
  }
}

# 檢查GPU可用性
check_gpu_availability <- function() {
  if(!requireNamespace("torch", quietly = TRUE)) {
    return(FALSE)
  }
  
  gpu_available <- torch::cuda_is_available()
  if(gpu_available) {
    gpu_count <- torch::cuda_device_count()
    gpu_memory <- torch::cuda_memory_allocated(0) / 1024^3  # GB
    cat("🔥 GPU可用:", gpu_count, "張卡\n")
    cat("💾 GPU記憶體使用:", round(gpu_memory, 2), "GB\n")
  } else {
    cat("⚠️  GPU不可用，將使用CPU\n")
  }
  
  return(gpu_available)
}

# 檢查必要套件
check_required_packages <- function() {
  required_packages <- c(
    "data.table", "lightgbm", "torch", "caret", 
    "Matrix", "abind", "future.apply", "logger"
  )
  
  missing_packages <- c()
  for(pkg in required_packages) {
    if(!requireNamespace(pkg, quietly = TRUE)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }
  
  if(length(missing_packages) > 0) {
    cat("❌ 缺少必要套件:", paste(missing_packages, collapse = ", "), "\n")
    cat("請執行: install.packages(c(", paste0("'", missing_packages, "'", collapse = ", "), "))\n")
    return(FALSE)
  }
  
  cat("✅ 所有必要套件已安裝\n")
  return(TRUE)
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