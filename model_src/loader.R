# ================================================================================
# AQI 時間序列預測模型訓練 - 資料載入模組
# ================================================================================

# 載入必要套件
if(!requireNamespace("data.table", quietly = TRUE)) {
  stop("請安裝 data.table 套件: install.packages('data.table')")
}

cat("📥 載入資料載入模組...\n")

# ================================================================================
# 1. 統一資料集介面定義
# ================================================================================

#' 創建標準化資料集物件
#' @param x 特徵陣列 [n_windows, seq_len, n_features]
#' @param y 目標向量 [n_windows]
#' @param features 特徵名稱向量
#' @param data_type 資料類型標識
#' @param metadata 額外元資料
#' @return 標準化資料集物件
create_dataset <- function(x, y, features, data_type, metadata = list()) {
  # 驗證輸入維度
  if(!is.array(x) || length(dim(x)) != 3) {
    stop("x 必須是三維陣列 [n_windows, seq_len, n_features]")
  }
  
  if(length(y) != dim(x)[1]) {
    stop("y 的長度必須等於 x 的第一維度")
  }
  
  if(length(features) != dim(x)[3]) {
    stop("features 的長度必須等於 x 的第三維度")
  }
  
  # 創建標準化物件
  dataset <- list(
    x = x,
    y = y,
    features = features,
    data_type = data_type,
    n_windows = dim(x)[1],
    seq_len = dim(x)[2],
    n_features = dim(x)[3],
    metadata = metadata,
    created_at = Sys.time()
  )
  
  class(dataset) <- c("aqi_dataset", "list")
  return(dataset)
}

# ================================================================================
# 1.5 Scaler 持久化功能
# ================================================================================

#' 保存標準化參數
#' @param scaler_data 包含均值和標準差的列表
#' @param file_path 保存路徑
#' @param data_type 資料類型
#' @param verbose 是否顯示詳細資訊
save_scaler <- function(scaler_data, file_path, data_type = "unknown", verbose = TRUE) {
  # 驗證輸入
  required_fields <- c("mean", "sd", "features")
  missing_fields <- setdiff(required_fields, names(scaler_data))
  if(length(missing_fields) > 0) {
    stop("Scaler資料缺少必要欄位: ", paste(missing_fields, collapse = ", "))
  }
  
  # 創建完整的scaler物件
  scaler_obj <- list(
    mean = scaler_data$mean,
    sd = scaler_data$sd,
    features = scaler_data$features,
    data_type = data_type,
    n_features = length(scaler_data$features),
    created_at = Sys.time(),
    version = "1.0"
  )
  
  class(scaler_obj) <- c("aqi_scaler", "list")
  
  # 創建目錄（如果不存在）
  dir_path <- dirname(file_path)
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  # 保存檔案
  tryCatch({
    saveRDS(scaler_obj, file_path)
    
    if(verbose) {
      cat("✅ Scaler已保存:", basename(file_path), "\n")
      cat("  資料類型:", data_type, "\n")
      cat("  特徵數量:", length(scaler_data$features), "\n")
    }
    
    return(TRUE)
    
  }, error = function(e) {
    stop("保存Scaler失敗: ", e$message)
  })
}

#' 載入標準化參數
#' @param file_path 檔案路徑
#' @param verbose 是否顯示詳細資訊
#' @return scaler物件
load_scaler <- function(file_path, verbose = TRUE) {
  if(!file.exists(file_path)) {
    stop("Scaler檔案不存在: ", file_path)
  }
  
  tryCatch({
    scaler_obj <- readRDS(file_path)
    
    # 驗證scaler物件
    if(!inherits(scaler_obj, "aqi_scaler")) {
      warning("載入的檔案不是有效的AQI scaler物件")
    }
    
    # 檢查必要欄位
    required_fields <- c("mean", "sd", "features")
    missing_fields <- setdiff(required_fields, names(scaler_obj))
    if(length(missing_fields) > 0) {
      stop("Scaler物件缺少必要欄位: ", paste(missing_fields, collapse = ", "))
    }
    
    if(verbose) {
      cat("📥 載入Scaler:", basename(file_path), "\n")
      cat("  資料類型:", if("data_type" %in% names(scaler_obj)) scaler_obj$data_type else "未知", "\n")
      cat("  特徵數量:", length(scaler_obj$features), "\n")
      cat("  創建時間:", if("created_at" %in% names(scaler_obj)) format(scaler_obj$created_at, "%Y-%m-%d %H:%M:%S") else "未知", "\n")
    }
    
    return(scaler_obj)
    
  }, error = function(e) {
    stop("載入Scaler失敗 (", basename(file_path), "): ", e$message)
  })
}

#' 應用標準化變換
#' @param data 原始資料陣列 [n_windows, seq_len, n_features]
#' @param scaler scaler物件
#' @param verbose 是否顯示詳細資訊
#' @return 標準化後的資料陣列
apply_scaler <- function(data, scaler, verbose = TRUE) {
  if(!inherits(scaler, "aqi_scaler")) {
    stop("scaler必須是aqi_scaler物件")
  }
  
  if(!is.array(data) || length(dim(data)) != 3) {
    stop("data必須是三維陣列 [n_windows, seq_len, n_features]")
  }
  
  n_features <- dim(data)[3]
  if(n_features != length(scaler$features)) {
    stop("資料特徵數量與scaler不匹配: ", n_features, " vs ", length(scaler$features))
  }
  
  # 應用標準化
  normalized_data <- data
  for(i in 1:n_features) {
    normalized_data[, , i] <- (data[, , i] - scaler$mean[i]) / scaler$sd[i]
  }
  
  if(verbose) {
    cat("✅ 標準化變換完成\n")
    cat("  處理特徵數:", n_features, "\n")
  }
  
  return(normalized_data)
}

#' 反向標準化變換
#' @param normalized_data 標準化資料陣列
#' @param scaler scaler物件
#' @param feature_indices 要反向變換的特徵索引（預設全部）
#' @param verbose 是否顯示詳細資訊
#' @return 反標準化後的資料陣列
inverse_scaler <- function(normalized_data, scaler, feature_indices = NULL, verbose = TRUE) {
  if(!inherits(scaler, "aqi_scaler")) {
    stop("scaler必須是aqi_scaler物件")
  }
  
  if(is.null(feature_indices)) {
    feature_indices <- 1:length(scaler$features)
  }
  
  # 應用反向標準化
  if(is.array(normalized_data) && length(dim(normalized_data)) == 3) {
    # 三維陣列情況
    original_data <- normalized_data
    for(i in feature_indices) {
      original_data[, , i] <- normalized_data[, , i] * scaler$sd[i] + scaler$mean[i]
    }
  } else if(is.vector(normalized_data) || is.matrix(normalized_data)) {
    # 向量或矩陣情況
    original_data <- normalized_data
    for(i in feature_indices) {
      if(is.matrix(normalized_data)) {
        original_data[, i] <- normalized_data[, i] * scaler$sd[i] + scaler$mean[i]
      } else {
        original_data[i] <- normalized_data[i] * scaler$sd[i] + scaler$mean[i]
      }
    }
  } else {
    stop("不支援的資料格式")
  }
  
  if(verbose) {
    cat("✅ 反標準化變換完成\n")
  }
  
  return(original_data)
}

#' 打印scaler摘要
print.aqi_scaler <- function(x, ...) {
  cat("AQI 標準化參數物件\n")
  cat("==================\n")
  cat("資料類型:", if("data_type" %in% names(x)) x$data_type else "未知", "\n")
  cat("特徵數量:", length(x$features), "\n")
  cat("版本:", if("version" %in% names(x)) x$version else "未知", "\n")
  cat("創建時間:", if("created_at" %in% names(x)) format(x$created_at, "%Y-%m-%d %H:%M:%S") else "未知", "\n")
  
  cat("\n統計摘要:\n")
  cat("均值範圍: [", round(min(x$mean, na.rm = TRUE), 3), ", ", round(max(x$mean, na.rm = TRUE), 3), "]\n", sep = "")
  cat("標準差範圍: [", round(min(x$sd, na.rm = TRUE), 3), ", ", round(max(x$sd, na.rm = TRUE), 3), "]\n", sep = "")
  
  if(length(x$features) <= 10) {
    cat("\n特徵名稱:\n")
    for(i in 1:length(x$features)) {
      cat("  ", x$features[i], ": μ=", round(x$mean[i], 3), ", σ=", round(x$sd[i], 3), "\n", sep = "")
    }
  }
}

#' 打印資料集摘要
#' @param x aqi_dataset 物件
print.aqi_dataset <- function(x, ...) {
  cat("AQI 時間序列資料集\n")
  cat("==================\n")
  cat("資料類型:", x$data_type, "\n")
  cat("窗口數量:", format(x$n_windows, big.mark = ","), "\n")
  cat("序列長度:", x$seq_len, "小時\n")
  cat("特徵數量:", x$n_features, "\n")
  cat("目標範圍: [", round(min(x$y, na.rm = TRUE), 2), ", ", 
      round(max(x$y, na.rm = TRUE), 2), "]\n", sep = "")
  cat("NA 比例: X =", round(sum(is.na(x$x)) / length(x$x) * 100, 2), "%, ",
      "y =", round(sum(is.na(x$y)) / length(x$y) * 100, 2), "%\n")
  cat("創建時間:", format(x$created_at, "%Y-%m-%d %H:%M:%S"), "\n")
}

# ================================================================================
# 2. 核心載入函數 (符合規劃要求)
# ================================================================================

#' 載入滑動窗口資料 (統一介面)
#' @param path RDS檔案路徑
#' @param verbose 是否顯示詳細資訊
#' @return 統一格式的資料集物件
load_windows <- function(path, verbose = TRUE) {
  if(!file.exists(path)) {
    stop("檔案不存在: ", path)
  }
  
  if(verbose) {
    file_size <- file.info(path)$size
    cat("📄 載入滑動窗口資料:", basename(path), 
        "(", format_file_size(file_size), ")\n")
  }
  
  # 讀取RDS檔案
  tryCatch({
    w <- readRDS(path)
    
    # 驗證必要欄位
    required_fields <- c("X_raw", "y_raw")
    missing_fields <- setdiff(required_fields, names(w))
    if(length(missing_fields) > 0) {
      stop("檔案缺少必要欄位: ", paste(missing_fields, collapse = ", "))
    }
    
    # 處理 features 欄位（如果缺失則自動生成）
    if(!"features" %in% names(w)) {
      if(verbose) {
        cat("⚠️  檔案缺少 features 欄位，自動生成特徵名稱\n")
      }
      
      # 從 X_raw 維度推斷特徵數量
      if(is.array(w$X_raw) && length(dim(w$X_raw)) == 3) {
        n_features <- dim(w$X_raw)[3]
        w$features <- paste0("feature_", 1:n_features)
        
        if(verbose) {
          cat("  生成", n_features, "個特徵名稱:", paste(head(w$features, 3), collapse = ", "), "...\n")
        }
      } else {
        stop("無法從 X_raw 推斷特徵數量")
      }
    }
    
    # 驗證特徵數量一致性（修復關鍵問題）
    actual_features <- dim(w$X_raw)[3]
    provided_features <- length(w$features)
    if(actual_features != provided_features) {
      if(verbose) {
        cat("⚠️  特徵數量不一致，重新生成特徵名稱\n")
        cat("    資料維度:", actual_features, "個特徵\n")
        cat("    提供的特徵名稱:", provided_features, "個\n")
      }
      # 強制重新生成特徵名稱以匹配實際維度
      w$features <- paste0("feature_", 1:actual_features)
      if(verbose) {
        cat("  重新生成", actual_features, "個特徵名稱\n")
      }
    }
    
    # 提取資料類型
    data_type <- if("data_type" %in% names(w)) {
      w$data_type
    } else {
      # 從檔案路徑推斷
      if(grepl("Separate_Normalization", path)) "separate_norm"
      else if(grepl("Separate", path)) "separate"
      else if(grepl("Combine_Normalization", path)) "combine_norm"
      else if(grepl("Combine", path)) "combine"
      else "unknown"
    }
    
    # 返回統一格式 (符合規劃要求)
    result <- list(
      x = w$X_raw,           # array(n, 72, n_feat)
      y = w$y_raw,           # numeric(n)
      features = w$features, # character(n_feat)
      data_type = data_type  # 來源標籤
    )
    
    # 添加維度資訊
    result$n_windows <- dim(result$x)[1]
    result$seq_len <- dim(result$x)[2]
    result$n_features <- dim(result$x)[3]
    
    # 添加元資料
    result$metadata <- list(
      source_file = basename(path),
      file_size = file.info(path)$size,
      load_time = Sys.time()
    )
    
    class(result) <- c("aqi_dataset", "list")
    
    if(verbose) {
      cat("✅ 載入完成:", format(result$n_windows, big.mark = ","), "個窗口\n")
      cat("  序列長度:", result$seq_len, "小時\n")
      cat("  特徵數量:", result$n_features, "\n")
      cat("  資料類型:", result$data_type, "\n")
    }
    
    return(result)
    
  }, error = function(e) {
    stop("載入檔案失敗 (", basename(path), "): ", e$message)
  })
}

#' 載入索引檔案 (大檔案支援)
#' @param index_rds 索引檔案路徑
#' @param verbose 是否顯示詳細資訊
#' @return 資料集物件列表
load_index <- function(index_rds, verbose = TRUE) {
  if(!file.exists(index_rds)) {
    stop("索引檔案不存在: ", index_rds)
  }
  
  if(verbose) {
    cat("📋 載入索引檔案:", basename(index_rds), "\n")
  }
  
  tryCatch({
    idx <- readRDS(index_rds)
    
    # 驗證索引結構
    if(!"chunk_files" %in% names(idx) || !"output_dir" %in% names(idx)) {
      stop("無效的索引檔案格式")
    }
    
    # 載入所有區塊檔案
    datasets <- lapply(idx$chunk_files, function(cf) {
      chunk_path <- file.path(idx$output_dir, cf)
      load_windows(chunk_path, verbose = FALSE)
    })
    
    if(verbose) {
      total_windows <- sum(sapply(datasets, function(d) d$n_windows))
      cat("✅ 索引載入完成:", length(datasets), "個區塊,", 
          format(total_windows, big.mark = ","), "個窗口\n")
    }
    
    return(datasets)
    
  }, error = function(e) {
    stop("載入索引失敗 (", basename(index_rds), "): ", e$message)
  })
}

# ================================================================================
# 3. 小檔案載入函數 (向後相容)
# ================================================================================

#' 載入單一小檔案 (RDS格式)
#' @param file_path RDS檔案路徑
#' @param verbose 是否顯示詳細資訊
#' @return aqi_dataset 物件
load_small_file <- function(file_path, verbose = TRUE) {
  if(!file.exists(file_path)) {
    stop("檔案不存在: ", file_path)
  }
  
  if(verbose) {
    file_size <- file.info(file_path)$size
    cat("📄 載入小檔案:", basename(file_path), 
        "(", format_file_size(file_size), ")\n")
  }
  
  # 讀取RDS檔案
  tryCatch({
    raw_data <- readRDS(file_path)
    
    # 驗證必要欄位
    required_fields <- c("X_raw", "y_raw", "features")
    missing_fields <- setdiff(required_fields, names(raw_data))
    if(length(missing_fields) > 0) {
      stop("檔案缺少必要欄位: ", paste(missing_fields, collapse = ", "))
    }
    
    # 提取資料類型
    data_type <- if("data_type" %in% names(raw_data)) {
      raw_data$data_type
    } else {
      # 從檔案路徑推斷
      if(grepl("Separate_Normalization", file_path)) "separate_norm"
      else if(grepl("Separate", file_path)) "separate"
      else if(grepl("Combine_Normalization", file_path)) "combine_norm"
      else if(grepl("Combine", file_path)) "combine"
      else "unknown"
    }
    
    # 創建標準化資料集
    dataset <- create_dataset(
      x = raw_data$X_raw,
      y = raw_data$y_raw,
      features = raw_data$features,
      data_type = data_type,
      metadata = list(
        source_file = basename(file_path),
        file_size = file.info(file_path)$size,
        load_time = Sys.time()
      )
    )
    
    if(verbose) {
      cat("✅ 載入完成:", format(dataset$n_windows, big.mark = ","), "個窗口\n")
    }
    
    return(dataset)
    
  }, error = function(e) {
    stop("載入檔案失敗 (", basename(file_path), "): ", e$message)
  })
}

#' 批次載入小檔案
#' @param data_path 資料目錄路徑
#' @param pattern 檔案名稱模式
#' @param max_files 最大載入檔案數 (NULL = 全部)
#' @param verbose 是否顯示詳細資訊
#' @return aqi_dataset 物件列表
load_small_files_batch <- function(data_path, pattern = "_windows\\.rds$", 
                                   max_files = NULL, verbose = TRUE) {
  if(!dir.exists(data_path)) {
    stop("資料目錄不存在: ", data_path)
  }
  
  # 找到所有符合條件的檔案
  files <- list.files(data_path, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    stop("在目錄中找不到符合模式的檔案: ", data_path)
  }
  
  # 限制檔案數量
  if(!is.null(max_files) && length(files) > max_files) {
    files <- files[1:max_files]
    if(verbose) {
      cat("⚠️  限制載入前", max_files, "個檔案\n")
    }
  }
  
  if(verbose) {
    cat("📂 批次載入", length(files), "個小檔案...\n")
  }
  
  # 載入所有檔案
  datasets <- list()
  total_windows <- 0
  
  for(i in seq_along(files)) {
    file_path <- files[i]
    
    tryCatch({
      dataset <- load_small_file(file_path, verbose = FALSE)
      
      # 使用檔案名作為鍵值
      file_key <- tools::file_path_sans_ext(basename(file_path))
      datasets[[file_key]] <- dataset
      
      total_windows <- total_windows + dataset$n_windows
      
      if(verbose && i %% 10 == 0) {
        cat("  進度:", i, "/", length(files), "檔案\n")
      }
      
    }, error = function(e) {
      if(verbose) {
        cat("⚠️  跳過檔案", basename(file_path), ":", e$message, "\n")
      }
    })
  }
  
  if(verbose) {
    cat("✅ 批次載入完成:", length(datasets), "個檔案,", 
        format(total_windows, big.mark = ","), "個窗口\n")
  }
  
  return(datasets)
}

# ================================================================================
# 3. 大檔案載入函數
# ================================================================================

#' 載入大檔案索引
#' @param index_path 索引檔案路徑
#' @param verbose 是否顯示詳細資訊
#' @return 索引資訊物件
load_large_file_index <- function(index_path, verbose = TRUE) {
  if(!file.exists(index_path)) {
    stop("索引檔案不存在: ", index_path)
  }
  
  if(verbose) {
    cat("📋 載入大檔案索引:", basename(index_path), "\n")
  }
  
  tryCatch({
    index <- readRDS(index_path)
    
    # 驗證索引結構
    required_fields <- c("chunk_files", "total_windows", "n_chunks", 
                        "features", "output_dir")
    missing_fields <- setdiff(required_fields, names(index))
    if(length(missing_fields) > 0) {
      stop("索引檔案缺少必要欄位: ", paste(missing_fields, collapse = ", "))
    }
    
    # 檢查區塊檔案是否存在
    existing_chunks <- 0
    for(chunk_file in index$chunk_files) {
      chunk_path <- file.path(index$output_dir, chunk_file)
      if(file.exists(chunk_path)) {
        existing_chunks <- existing_chunks + 1
      }
    }
    
    if(verbose) {
      cat("  總窗口數:", format(index$total_windows, big.mark = ","), "\n")
      cat("  區塊數量:", index$n_chunks, "(", existing_chunks, "個存在)\n")
      cat("  特徵數量:", length(index$features), "\n")
    }
    
    if(existing_chunks < index$n_chunks) {
      warning("部分區塊檔案不存在 (", existing_chunks, "/", index$n_chunks, ")")
    }
    
    return(index)
    
  }, error = function(e) {
    stop("載入索引檔案失敗: ", e$message)
  })
}

#' 載入大檔案的單一區塊
#' @param index 索引物件
#' @param chunk_idx 區塊索引 (1-based)
#' @param verbose 是否顯示詳細資訊
#' @return aqi_dataset 物件
load_large_file_chunk <- function(index, chunk_idx, verbose = TRUE) {
  if(chunk_idx < 1 || chunk_idx > length(index$chunk_files)) {
    stop("區塊索引超出範圍: ", chunk_idx, " (1-", length(index$chunk_files), ")")
  }
  
  chunk_file <- index$chunk_files[chunk_idx]
  chunk_path <- file.path(index$output_dir, chunk_file)
  
  if(!file.exists(chunk_path)) {
    stop("區塊檔案不存在: ", chunk_path)
  }
  
  if(verbose) {
    file_size <- file.info(chunk_path)$size
    cat("📦 載入區塊", chunk_idx, ":", chunk_file, 
        "(", format_file_size(file_size), ")\n")
  }
  
  tryCatch({
    raw_data <- readRDS(chunk_path)
    
    # 推斷資料類型
    data_type <- if("data_type" %in% names(index)) {
      index$data_type
    } else {
      # 從路徑推斷
      if(grepl("Combine_Normalization", index$output_dir)) "combine_norm"
      else if(grepl("Combine", index$output_dir)) "combine"
      else "unknown"
    }
    
    # 創建標準化資料集
    dataset <- create_dataset(
      x = raw_data$X_raw,
      y = raw_data$y_raw,
      features = index$features,  # 使用索引中的特徵列表
      data_type = data_type,
      metadata = list(
        source_chunk = chunk_file,
        chunk_index = chunk_idx,
        total_chunks = index$n_chunks,
        file_size = file.info(chunk_path)$size,
        load_time = Sys.time()
      )
    )
    
    if(verbose) {
      cat("✅ 區塊載入完成:", format(dataset$n_windows, big.mark = ","), "個窗口\n")
    }
    
    return(dataset)
    
  }, error = function(e) {
    stop("載入區塊失敗 (", chunk_file, "): ", e$message)
  })
}

#' 大檔案區塊迭代器
#' @param index 索引物件
#' @param chunk_indices 要載入的區塊索引 (NULL = 全部)
#' @param verbose 是否顯示詳細資訊
#' @return 迭代器函數
create_large_file_iterator <- function(index, chunk_indices = NULL, verbose = TRUE) {
  if(is.null(chunk_indices)) {
    chunk_indices <- seq_along(index$chunk_files)
  }
  
  current_idx <- 1
  
  # 返回迭代器函數
  function() {
    if(current_idx > length(chunk_indices)) {
      return(NULL)  # 迭代結束
    }
    
    chunk_idx <- chunk_indices[current_idx]
    current_idx <<- current_idx + 1
    
    return(load_large_file_chunk(index, chunk_idx, verbose = verbose))
  }
}

# ================================================================================
# 4. 統一載入介面
# ================================================================================

#' 自動檢測並載入資料
#' @param data_path 資料路徑 (目錄或檔案)
#' @param data_type 資料類型 ("separate", "separate_norm", "combine", "combine_norm")
#' @param max_files 最大載入檔案數 (僅對小檔案有效)
#' @param verbose 是否顯示詳細資訊
#' @return 資料集物件或迭代器
load_data_auto <- function(data_path, data_type = NULL, max_files = NULL, verbose = TRUE) {
  if(!file.exists(data_path) && !dir.exists(data_path)) {
    stop("路徑不存在: ", data_path)
  }
  
  # 如果是檔案，直接載入
  if(file.exists(data_path) && !dir.exists(data_path)) {
    if(grepl("_index\\.rds$", data_path)) {
      # 大檔案索引
      index <- load_large_file_index(data_path, verbose = verbose)
      return(create_large_file_iterator(index, verbose = verbose))
    } else {
      # 單一小檔案
      return(load_small_file(data_path, verbose = verbose))
    }
  }
  
  # 如果是目錄，檢測檔案類型
  if(dir.exists(data_path)) {
    # 檢查是否有索引檔案 (大檔案)
    index_files <- list.files(data_path, pattern = "_index\\.rds$", full.names = TRUE)
    
    if(length(index_files) > 0) {
      # 大檔案模式
      if(verbose) {
        cat("🔍 檢測到大檔案模式\n")
      }
      index <- load_large_file_index(index_files[1], verbose = verbose)
      return(create_large_file_iterator(index, verbose = verbose))
      
    } else {
      # 小檔案模式
      if(verbose) {
        cat("🔍 檢測到小檔案模式\n")
      }
      return(load_small_files_batch(data_path, max_files = max_files, verbose = verbose))
    }
  }
}

# ================================================================================
# 5. 資料預處理函數
# ================================================================================

#' 合併多個資料集
#' @param datasets 資料集列表
#' @param verbose 是否顯示詳細資訊
#' @return 合併後的 aqi_dataset 物件
combine_datasets <- function(datasets, verbose = TRUE) {
  if(length(datasets) == 0) {
    stop("資料集列表為空")
  }
  
  if(length(datasets) == 1) {
    return(datasets[[1]])
  }
  
  if(verbose) {
    cat("🔗 合併", length(datasets), "個資料集...\n")
  }
  
  # 檢查特徵一致性
  first_features <- datasets[[1]]$features
  for(i in 2:length(datasets)) {
    if(!identical(datasets[[i]]$features, first_features)) {
      stop("資料集", i, "的特徵與第一個資料集不一致")
    }
  }
  
  # 合併資料
  all_x <- abind::abind(lapply(datasets, function(d) d$x), along = 1)
  all_y <- unlist(lapply(datasets, function(d) d$y))
  
  # 創建合併後的資料集
  combined_dataset <- create_dataset(
    x = all_x,
    y = all_y,
    features = first_features,
    data_type = datasets[[1]]$data_type,
    metadata = list(
      source_count = length(datasets),
      combined_at = Sys.time()
    )
  )
  
  if(verbose) {
    cat("✅ 合併完成:", format(combined_dataset$n_windows, big.mark = ","), "個窗口\n")
  }
  
  return(combined_dataset)
}

#' 資料集統計摘要
#' @param dataset aqi_dataset 物件
#' @return 統計摘要列表
summarize_dataset <- function(dataset) {
  summary_stats <- list(
    # 基本資訊
    data_type = dataset$data_type,
    n_windows = dataset$n_windows,
    seq_len = dataset$seq_len,
    n_features = dataset$n_features,
    
    # 目標變數統計
    y_stats = list(
      min = min(dataset$y, na.rm = TRUE),
      max = max(dataset$y, na.rm = TRUE),
      mean = mean(dataset$y, na.rm = TRUE),
      median = median(dataset$y, na.rm = TRUE),
      sd = sd(dataset$y, na.rm = TRUE),
      na_count = sum(is.na(dataset$y)),
      na_ratio = sum(is.na(dataset$y)) / length(dataset$y)
    ),
    
    # 特徵統計
    x_stats = list(
      total_values = length(dataset$x),
      na_count = sum(is.na(dataset$x)),
      na_ratio = sum(is.na(dataset$x)) / length(dataset$x),
      min = min(dataset$x, na.rm = TRUE),
      max = max(dataset$x, na.rm = TRUE)
    ),
    
    # 特徵名稱
    features = dataset$features,
    
    # 元資料
    metadata = dataset$metadata
  )
  
  return(summary_stats)
}

# ================================================================================
# 6. 實用工具函數
# ================================================================================

#' 檢查資料集完整性
#' @param dataset aqi_dataset 物件
#' @return 檢查結果列表
validate_dataset <- function(dataset) {
  issues <- list()
  
  # 檢查維度一致性
  if(dim(dataset$x)[1] != length(dataset$y)) {
    issues <- c(issues, "X和y的樣本數不一致")
  }
  
  if(dim(dataset$x)[3] != length(dataset$features)) {
    issues <- c(issues, "X的特徵數與features長度不一致")
  }
  
  # 檢查NA值
  if(sum(is.na(dataset$y)) > 0) {
    issues <- c(issues, paste("目標變數包含", sum(is.na(dataset$y)), "個NA值"))
  }
  
  # 檢查數值範圍
  if(any(dataset$y < 0, na.rm = TRUE)) {
    issues <- c(issues, "目標變數包含負值")
  }
  
  if(any(is.infinite(dataset$y))) {
    issues <- c(issues, "目標變數包含無限值")
  }
  
  return(list(
    is_valid = length(issues) == 0,
    issues = issues
  ))
}

#' 獲取資料載入摘要
#' @param data_type 資料類型
#' @return 載入摘要
get_data_loading_summary <- function(data_type) {
  if(!data_type %in% names(DATA_TYPES)) {
    stop("不支援的資料類型: ", data_type)
  }
  
  config <- DATA_TYPES[[data_type]]
  path <- config$path
  
  if(!dir.exists(path)) {
    return(list(
      data_type = data_type,
      status = "路徑不存在",
      file_count = 0,
      total_size = 0
    ))
  }
  
  if(config$is_large) {
    # 大檔案模式
    index_files <- list.files(path, pattern = "_index\\.rds$", full.names = TRUE)
    chunk_files <- list.files(path, pattern = "_chunk\\d+\\.rds$", full.names = TRUE)
    
    total_size <- sum(file.info(c(index_files, chunk_files))$size, na.rm = TRUE)
    
    return(list(
      data_type = data_type,
      status = "大檔案模式",
      index_count = length(index_files),
      chunk_count = length(chunk_files),
      total_size = total_size
    ))
    
  } else {
    # 小檔案模式
    files <- list.files(path, pattern = "_windows\\.rds$", full.names = TRUE)
    total_size <- sum(file.info(files)$size, na.rm = TRUE)
    
    return(list(
      data_type = data_type,
      status = "小檔案模式",
      file_count = length(files),
      total_size = total_size
    ))
  }
}

cat("✅ 資料載入模組載入完成\n") 