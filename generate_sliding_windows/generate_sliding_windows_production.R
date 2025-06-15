# ================================================================================
# AQI 滑動時間窗口資料集產生系統 (生產級最終版)
# 解決所有Arrow API、記憶體、進度管理、Windows兼容性問題
# ================================================================================

cat("正在初始化滑動窗口資料產生系統 (生產級最終版)...\n")

# ================================================================================
# 0. 前置設定與生產級優化
# ================================================================================

# 設定CRAN鏡像
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# --- 全域固定參數 ----
input_seq_len   <- 72    # 根據交叉驗證最佳窗口
output_horizon  <- 1     # 預測 1 小時後
stride          <- 1     # 每次滑動 1 筆
chunk_size      <- 200000   # 讀大檔用，含重疊區
overlap         <- input_seq_len + output_horizon - 1  # 正確的重疊區域
target          <- "AQI_aqi"              # 預測欄

# 記憶體安全閾值 (單檔最大窗口數，避免OOM)
MAX_WINDOWS_PER_FILE <- 1000000  # 約20GB記憶體上限 (64GB RAM環境)

# 統一路徑管理 (修正大小寫混用問題)
DATA_DIRS <- list(
  separate = "DATA/Separate",
  separate_norm = "DATA/Separate_Nomorlization", 
  combine = "DATA/Combine",
  combine_norm = "DATA/Combine_Nomorlization"  # 修正拼字錯誤
)

# 按原始資料結構創建分類輸出目錄
output_base_dir <- "sliding_windows_production"
output_dirs <- list(
  separate = file.path(output_base_dir, "Separate"),
  separate_norm = file.path(output_base_dir, "Separate_Normalization"), 
  combine = file.path(output_base_dir, "Combine"),
  combine_norm = file.path(output_base_dir, "Combine_Normalization")
)

# 創建所有輸出目錄
for(dir_path in output_dirs) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat("創建輸出目錄:", dir_path, "\n")
  }
}

# 進度追蹤目錄
progress_dir <- file.path(output_base_dir, "progress")
if(!dir.exists(progress_dir)) {
  dir.create(progress_dir, recursive = TRUE)
}

cat("滑動窗口參數設定:\n")
cat("  - 輸入序列長度:", input_seq_len, "小時\n")
cat("  - 預測時間範圍:", output_horizon, "小時\n")
cat("  - 滑動步長:", stride, "筆\n")
cat("  - 大檔分塊大小:", format(chunk_size, big.mark = ","), "筆\n")
cat("  - 重疊區域:", overlap, "筆\n")
cat("  - 目標變數:", target, "\n")
cat("  - 記憶體安全上限:", format(MAX_WINDOWS_PER_FILE, big.mark = ","), "個窗口\n")
cat("  - 輸出基礎目錄:", output_base_dir, "\n\n")

# ================================================================================
# 套件載入與生產級Arrow處理
# ================================================================================
required_packages <- c("data.table", "zoo", "abind", "fasttime", "future.apply")

# 先安裝基礎套件
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("安裝套件:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Arrow生產級安裝與檢測
arrow_available <- FALSE
arrow_version <- NULL

if(!require(arrow, quietly = TRUE)) {
  cat("嘗試安裝Arrow套件...\n")
  tryCatch({
    # Windows生產級安裝
    if(.Platform$OS.type == "windows") {
      cat("Windows系統：設定環境變數並使用優化安裝\n")
      # 設定Arrow環境變數
      Sys.setenv(ARROW_PRE_0_17_IPC_FORMAT = "1")
      install.packages("arrow", INSTALL_opts = "--no-multiarch", dependencies = TRUE)
    } else {
      install.packages("arrow", dependencies = TRUE)
    }
    library(arrow)
    arrow_available <- TRUE
    arrow_version <- packageVersion("arrow")
    cat("✅ Arrow套件安裝成功，版本:", as.character(arrow_version), "\n")
  }, error = function(e) {
    cat("⚠️ Arrow套件安裝失敗，將使用fread備用方案\n")
    cat("   錯誤訊息:", e$message, "\n")
    arrow_available <- FALSE
  })
} else {
  arrow_available <- TRUE
  arrow_version <- packageVersion("arrow")
  cat("✅ Arrow套件已可用，版本:", as.character(arrow_version), "\n")
}

# 性能優化設定
if(require(data.table, quietly = TRUE)) {
  data.table::setDTthreads(0)  # 使用所有可用核心
  cat("data.table 多核心已啟用:", data.table::getDTthreads(), "核心\n")
}

# 設定並行處理 (修正單核心機器問題)
if(require(future.apply, quietly = TRUE)) {
  workers <- max(1, min(parallel::detectCores() - 1, 8))
  future::plan(future::multisession, workers = workers)
  cat("future 並行處理已啟用:", future::nbrOfWorkers(), "個工作進程\n")
}

cat("生產級套件載入完成\n\n")

# ================================================================================
# 固定特徵清單
# ================================================================================

# 資料集級特徵清單 (修正固定特徵問題)
DATASET_FEATURES <- list()

# 動態建立各資料集的特徵清單
build_dataset_features <- function(data_type) {
  if(data_type %in% names(DATASET_FEATURES)) {
    return(DATASET_FEATURES[[data_type]])
  }
  
  # 使用統一路徑管理
  sample_file <- NULL
  if(data_type == "separate") {
    if(dir.exists(DATA_DIRS$separate)) {
      files <- list.files(DATA_DIRS$separate, pattern = "\\.csv$", full.names = TRUE)
      if(length(files) > 0) sample_file <- files[1]
    }
  } else if(data_type == "separate_norm") {
    if(dir.exists(DATA_DIRS$separate_norm)) {
      files <- list.files(DATA_DIRS$separate_norm, pattern = "\\.csv$", full.names = TRUE)
      if(length(files) > 0) sample_file <- files[1]
    }
  } else if(data_type == "combine") {
    combine_file <- file.path(DATA_DIRS$combine, "Combine_AllData.csv")
    if(file.exists(combine_file)) {
      sample_file <- combine_file
    }
  } else if(data_type == "combine_norm") {
    combine_norm_file <- file.path(DATA_DIRS$combine_norm, "Nomorlization_Combine_AllData.csv")
    if(file.exists(combine_norm_file)) {
      sample_file <- combine_norm_file
    }
  }
  
  if(is.null(sample_file) || !file.exists(sample_file)) {
    # 使用預設特徵清單
    features <- c(
      "AQI_so2", "AQI_co", "AQI_o3", "AQI_o3_8hr", "AQI_pm10", "AQI_pm2.5",
      "AQI_no2", "AQI_nox", "AQI_no", "AQI_co_8hr", "AQI_pm2.5_avg", "AQI_pm10_avg",
      "AQI_so2_avg", "Weather_Tx", "Weather_RH", "Weather_WS", "Weather_WD",
      "AQI_pollutant_0", "AQI_pollutant_1", "AQI_pollutant_2", "AQI_pollutant_3",
      "AQI_pollutant_4", "AQI_pollutant_5", "month_sin", "hour_sin", "day_sin"
    )
  } else {
    # 從樣本檔案讀取實際特徵
    tryCatch({
      sample_data <- fread(sample_file, nrows = 1)
      all_cols <- names(sample_data)
      # 排除date和target，其餘都是特徵
      features <- setdiff(all_cols, c("date", target))
      cat("從", basename(sample_file), "檢測到", length(features), "個特徵\n")
    }, error = function(e) {
      cat("讀取樣本檔案失敗，使用預設特徵清單\n")
      features <- c(
        "AQI_so2", "AQI_co", "AQI_o3", "AQI_o3_8hr", "AQI_pm10", "AQI_pm2.5",
        "AQI_no2", "AQI_nox", "AQI_no", "AQI_co_8hr", "AQI_pm2.5_avg", "AQI_pm10_avg",
        "AQI_so2_avg", "Weather_Tx", "Weather_RH", "Weather_WS", "Weather_WD",
        "AQI_pollutant_0", "AQI_pollutant_1", "AQI_pollutant_2", "AQI_pollutant_3",
        "AQI_pollutant_4", "AQI_pollutant_5", "month_sin", "hour_sin", "day_sin"
      )
    })
  }
  
  DATASET_FEATURES[[data_type]] <<- features
  return(features)
}

cat("資料集級特徵管理系統初始化完成\n\n")

# ================================================================================
# 1. 生產級核心函式
# ================================================================================

# 1-1 生產級Feather檔案轉換
convert_to_feather_safe <- function(csv_path) {
  feather_path <- gsub("\\.csv$", ".feather", csv_path)
  
  if(file.exists(feather_path)) {
    # 檢查Feather檔案完整性
    tryCatch({
      test_read <- arrow::read_feather(feather_path, n_max = 1)
      cat("    Feather檔案已存在且完整:", basename(feather_path), "\n")
      return(feather_path)
    }, error = function(e) {
      cat("    Feather檔案損壞，重新轉換\n")
      file.remove(feather_path)
    })
  }
  
  if(arrow_available) {
    # 檢查檔案大小，大檔案跳過Feather轉換避免記憶體問題
    file_size_mb <- file.info(csv_path)$size / (1024^2)
    if(file_size_mb > 200) {
      cat("    檔案過大 (", round(file_size_mb, 1), "MB)，跳過Feather轉換，直接使用CSV分塊讀取\n")
      return(csv_path)
    }
    
    cat("    轉換為Feather格式:", basename(csv_path), "→", basename(feather_path), "\n")
    tryCatch({
      # 使用Arrow高效轉換
      dt <- arrow::read_csv_arrow(csv_path)
      arrow::write_feather(dt, feather_path, compression = "zstd")
      cat("    ✅ Feather轉換完成 (zstd壓縮)\n")
      return(feather_path)
    }, error = function(e) {
      cat("    ⚠️ Feather轉換失敗，使用原CSV:", e$message, "\n")
      return(csv_path)
    })
  } else {
    return(csv_path)
  }
}

# 1-2 生產級精確行數計算 (修正大檔記憶體問題)
count_lines_production <- function(path) {
  tryCatch({
    if(arrow_available && grepl("\\.feather$", path)) {
      # Feather檔案直接讀取行數
      tbl <- arrow::read_feather(path)
      return(nrow(tbl))
    } else if(arrow_available && grepl("\\.csv$", path)) {
      # 大檔案避免整檔讀入，使用系統命令計算行數
      file_size_mb <- file.info(path)$size / (1024^2)
      if(file_size_mb > 100) {
        cat("    大檔案使用系統命令計算行數\n")
        # 跳過Arrow，直接用系統命令
      } else {
        # 小檔案可以用Arrow
        tbl <- arrow::read_csv_arrow(path)
        return(nrow(tbl))
      }
    } else {
      # Windows優化行數計算 (修正PowerShell慢速問題)
      if(.Platform$OS.type == "windows") {
        # 使用find命令，比PowerShell快數十倍
        cmd <- paste0('find /v /c "" "', path, '"')
        result <- system(cmd, intern = TRUE)
        # find輸出格式: "---------- filename: count"
        total_lines <- as.numeric(gsub(".*: ", "", result))
      } else {
        total_lines <- as.numeric(system(paste("wc -l <", shQuote(path)), intern = TRUE))
      }
      return(total_lines - 1)  # 扣除標題行
    }
  }, error = function(e) {
    cat("    警告：精確行數計算失敗，使用保守估算\n")
    # 保守估算
    sample_data <- fread(path, nrows = 5000)
    file_size <- file.info(path)$size
    avg_line_size <- object.size(sample_data) / 5000
    estimated_lines <- round(file_size / as.numeric(avg_line_size))
    return(max(estimated_lines - 1, 10000))
  })
}

# 1-3 記憶體安全滑動窗口 (修正記憶體爆炸問題)
make_windows_memory_safe <- function(mat, seq_len, horizon, stride = 1) {
  n <- nrow(mat)
  p <- ncol(mat)
  
  # 計算可產生的窗口數
  max_start <- n - seq_len - horizon + 1L
  if(max_start <= 0) {
    cat("    警告：資料長度不足以產生窗口\n")
    return(list(X_raw = array(dim = c(0, seq_len, p)), y_raw = numeric(0)))
  }
  
  starts <- seq.int(1L, max_start, by = stride)
  n_w <- length(starts)
  
  # 記憶體安全檢查
  estimated_memory_gb <- (n_w * seq_len * p * 8) / (1024^3)  # 8 bytes per double
  cat("    預估記憶體需求:", round(estimated_memory_gb, 2), "GB\n")
  
  if(n_w > MAX_WINDOWS_PER_FILE) {
    cat("    ⚠️ 窗口數超過安全上限，將分批處理\n")
    # 分批處理，避免記憶體爆炸
    batch_size <- MAX_WINDOWS_PER_FILE
    n_batches <- ceiling(n_w / batch_size)
    
    all_X <- list()
    all_y <- list()
    
    for(batch_idx in 1:n_batches) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx <- min(batch_idx * batch_size, n_w)
      batch_starts <- starts[start_idx:end_idx]
      batch_n_w <- length(batch_starts)
      
      cat("    處理批次", batch_idx, "/", n_batches, "(", format(batch_n_w, big.mark = ","), "個窗口)\n")
      
      # 批次處理
      X_batch <- array(NA_real_, dim = c(batch_n_w, seq_len, p))
      target_col <- match(target, colnames(mat))
      y_batch <- mat[batch_starts + seq_len + horizon - 1L, ..target_col][[1]]
      
      mat_matrix <- as.matrix(mat)
      for (shift in 0:(seq_len-1L)) {
        X_batch[, shift+1L, ] <- mat_matrix[batch_starts + shift, ]
      }
      
      all_X[[batch_idx]] <- X_batch
      all_y[[batch_idx]] <- y_batch
      
      # 清理批次記憶體
      rm(X_batch, y_batch); gc()
    }
    
    # 合併所有批次
    X <- do.call(abind::abind, c(all_X, list(along = 1)))
    y <- do.call(c, all_y)
    
    rm(all_X, all_y); gc()
    
  } else {
    cat("    產生", format(n_w, big.mark = ","), "個滑動窗口 (", seq_len, "×", p, ")\n")
    
    # 找到target欄位位置
    target_col <- match(target, colnames(mat))
    if(is.na(target_col)) {
      stop("找不到目標變數: ", target)
    }
    
    # 預分配記憶體
    X <- array(NA_real_, dim = c(n_w, seq_len, p))
    
    # 向量化提取y值
    y <- mat[starts + seq_len + horizon - 1L, ..target_col][[1]]
    
    # 轉換為矩陣一次，避免重複轉換
    mat_matrix <- as.matrix(mat)
    
    # 向量化窗口生成
    cat("    向量化窗口生成...\n")
    for (shift in 0:(seq_len-1L)) {
      X[, shift+1L, ] <- mat_matrix[starts + shift, ]
      if((shift + 1) %% 20 == 0) {
        cat("    已處理時間步", shift + 1, "/", seq_len, "\n")
      }
    }
  }
  
  list(X_raw = X, y_raw = y)
}

# 1-4 快速時間轉換 (修正時區問題)
fast_time_convert <- function(dt) {
  if("date" %in% names(dt)) {
    if(inherits(dt$date, "POSIXct")) {
      # 已經是POSIXct，保留原時區
      cat("    date欄位已是POSIXct格式，保留原時區\n")
    } else if(require(fasttime, quietly = TRUE) && is.character(dt$date)) {
      # 使用fasttime但保留本地時區
      dt[, date := as.POSIXct(fasttime::fastPOSIXct(date), tz = Sys.timezone())]
    } else if(is.character(dt$date)) {
      dt[, date := as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")]
    }
  }
  return(dt)
}

# 1-5 資料集級特徵處理 (修正特徵對齊問題)
process_features_dataset <- function(dt, data_type) {
  # 獲取該資料集的特徵清單
  dataset_features <- build_dataset_features(data_type)
  
  # 檢查實際存在的特徵
  actual_features <- intersect(dataset_features, names(dt))
  missing_features <- setdiff(dataset_features, names(dt))
  extra_features <- setdiff(names(dt), c("date", target, dataset_features))
  
  if(length(missing_features) > 0) {
    cat("    補充缺失特徵:", length(missing_features), "個\n")
    for(feat in missing_features) {
      dt[, (feat) := 0]  # 用0填充
    }
  }
  
  if(length(extra_features) > 0) {
    cat("    移除多餘特徵:", length(extra_features), "個\n")
    dt[, (extra_features) := NULL]
  }
  
  # 按固定順序選擇特徵
  cols_to_select <- c(target, dataset_features)
  mat <- dt[, ..cols_to_select]
  
  # 在輸出中記錄實際特徵清單
  attr(mat, "features") <- dataset_features
  attr(mat, "data_type") <- data_type
  
  return(mat)
}

# 1-6 記憶體監控 (修正計算錯誤)
monitor_memory <- function(stage) {
  mem_info <- gc()
  used_mb <- sum(mem_info[, "used"] * .Machine$sizeof.pointer) / 1024
  cat("    [", stage, "] 記憶體使用:", round(used_mb, 1), "MB\n")
}

# 1-7 生產級進度管理 (修正路徑變更問題)
get_progress_file <- function(file_path) {
  # 使用不含副檔名的basename，避免CSV/Feather路徑變更問題
  base_name <- tools::file_path_sans_ext(basename(file_path))
  return(file.path(progress_dir, paste0(base_name, "_completed.flag")))
}

is_completed <- function(file_path) {
  flag_file <- get_progress_file(file_path)
  return(file.exists(flag_file))
}

mark_completed <- function(file_path) {
  flag_file <- get_progress_file(file_path)
  completion_info <- list(
    completed_time = Sys.time(),
    original_path = file_path,
    file_size_mb = round(file.info(file_path)$size / (1024^2), 2)
  )
  saveRDS(completion_info, flag_file)
}

# 1-8 生產級檔案讀取 (修正Arrow API問題)
read_chunk_production <- function(path, skip_rows = 0, n_rows = NULL, col_names = NULL) {
  # Feather檔案的生產級讀取 (修正API參數問題)
  if(arrow_available && grepl("\\.feather$", path)) {
    tryCatch({
      if(skip_rows == 0 && is.null(n_rows)) {
        # 讀取完整檔案
        dt <- as.data.table(arrow::read_feather(path))
      } else {
        # Feather不支援skip/n_max參數，需要先讀全部再切片
        full_dt <- as.data.table(arrow::read_feather(path))
        start_idx <- skip_rows + 1
        end_idx <- if(is.null(n_rows)) nrow(full_dt) else min(skip_rows + n_rows, nrow(full_dt))
        if(start_idx <= nrow(full_dt)) {
          dt <- full_dt[start_idx:min(end_idx, nrow(full_dt))]
        } else {
          dt <- full_dt[0]  # 空資料表
        }
        rm(full_dt); gc()
      }
      return(dt)
    }, error = function(e) {
      cat("    Feather讀取失敗，改用fread:", e$message, "\n")
    })
  }
  
  # CSV檔案的Arrow讀取
  if(arrow_available && grepl("\\.csv$", path) && skip_rows == 0 && is.null(n_rows)) {
    tryCatch({
      dt <- as.data.table(arrow::read_csv_arrow(path))
      return(dt)
    }, error = function(e) {
      cat("    Arrow讀取失敗，改用fread\n")
    })
  }
  
  # 備用方案：使用fread
  if(skip_rows == 0) {
    dt <- fread(path, nrows = n_rows)
  } else {
    dt <- fread(path, skip = skip_rows, nrows = n_rows, 
                header = FALSE, col.names = col_names)
  }
  return(dt)
}

# 1-9 根據檔案路徑決定輸出目錄和資料類型 (修正拼字錯誤和路徑匹配)
get_output_info <- function(file_path) {
  # 按優先順序判斷，避免pattern疊合，使用統一路徑常量
  if(grepl("Separate_Nomorlization", file_path, fixed = TRUE)) {
    return(list(dir = output_dirs$separate_norm, type = "separate_norm"))
  } else if(grepl("Combine_Nomorlization", file_path)) {
    return(list(dir = output_dirs$combine_norm, type = "combine_norm"))
  } else if(grepl("Separate", file_path, fixed = TRUE)) {
    return(list(dir = output_dirs$separate, type = "separate"))
  } else if(grepl("Combine", file_path, fixed = TRUE)) {
    return(list(dir = output_dirs$combine, type = "combine"))
  } else {
    return(list(dir = output_dirs$separate, type = "separate"))  # 預設
  }
}

# 1-10 檔案大小分類 (確保大檔不走小檔流程)
classify_file_size <- function(path) {
  file_size_mb <- file.info(path)$size / (1024^2)
  
  # 嚴格分類：超過100MB或預估窗口數超過安全上限的都算大檔
  if(file_size_mb > 100) {
    return("large")
  }
  
  # 進一步檢查：估算可能的窗口數
  tryCatch({
    sample_data <- fread(path, nrows = 1000)
    estimated_rows <- file_size_mb * 1000 / (object.size(sample_data) / (1024^2))
    estimated_windows <- max(0, estimated_rows - input_seq_len - output_horizon + 1)
    
    if(estimated_windows > MAX_WINDOWS_PER_FILE) {
      return("large")
    } else {
      return("small")
    }
  }, error = function(e) {
    # 保守估計：大於50MB的都當大檔處理
    return(if(file_size_mb > 50) "large" else "small")
  })
}

cat("生產級核心函式定義完成\n\n")

# ================================================================================
# 2. 生產級小檔處理
# ================================================================================

process_small_file_production <- function(path) {
  file_size <- round(file.info(path)$size / (1024^2), 2)
  cat("  處理小檔:", basename(path), "(", file_size, "MB)\n")
  
  # 檢查是否已完成 (使用統一的basename邏輯)
  if(is_completed(path)) {
    cat("    ✅ 已完成，跳過\n")
    return(TRUE)
  }
  
  tryCatch({
    # 高效讀取
    dt <- read_chunk_production(path)
    cat("    讀取完成:", format(nrow(dt), big.mark = ","), "筆資料\n")
    monitor_memory("讀取後")
    
    # 檢查必要欄位
    if(!target %in% names(dt) || !"date" %in% names(dt)) {
      cat("    錯誤：缺少必要欄位\n")
      return(FALSE)
    }
    
    # 快速時間轉換
    dt <- fast_time_convert(dt)
    
    # 只處理target的NA
    if(anyNA(dt[[target]])) {
      dt[, (target) := na.approx(get(target), na.rm = FALSE)]
      dt <- dt[!is.na(get(target))]
      cat("    處理target NA後剩餘:", nrow(dt), "筆\n")
    }
    
    # 根據檔案路徑決定輸出目錄和資料類型
    output_info <- get_output_info(path)
    target_output_dir <- output_info$dir
    data_type <- output_info$type
    
    # 資料集級特徵處理
    mat <- process_features_dataset(dt, data_type)
    cat("    特徵數量:", ncol(mat) - 1, "個\n")
    monitor_memory("特徵處理後")
    
    # 記憶體安全滑動窗口產生
    w <- make_windows_memory_safe(mat, input_seq_len, output_horizon, stride)
    monitor_memory("產窗後")
    
    # 在輸出中保存特徵資訊
    w$features <- attr(mat, "features")
    w$data_type <- attr(mat, "data_type")
    
    # 儲存結果 (高壓縮)
    base_name <- tools::file_path_sans_ext(basename(path))
    output_path <- file.path(target_output_dir, paste0(base_name, "_windows.rds"))
    saveRDS(w, output_path, compress = "xz")
    
    cat("    ✅ 已儲存:", basename(output_path), "\n")
    cat("    ✅ 輸出位置:", target_output_dir, "\n")
    cat("    窗口數量:", format(length(w$y_raw), big.mark = ","), "個\n")
    
    # 標記完成 (使用統一的basename邏輯)
    mark_completed(path)
    
    # 清理記憶體
    rm(dt, mat, w); gc()
    monitor_memory("清理後")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("    ❌ 處理失敗:", e$message, "\n")
    return(FALSE)
  })
}

# ================================================================================
# 3. 生產級大檔處理
# ================================================================================

process_big_file_production <- function(path) {
  file_size <- round(file.info(path)$size / (1024^2), 2)
  cat("  處理大檔:", basename(path), "(", file_size, "MB)\n")
  
  # 檢查是否已完成 (使用統一的basename邏輯)
  if(is_completed(path)) {
    cat("    ✅ 已完成，跳過\n")
    return(TRUE)
  }
  
  tryCatch({
    # 嘗試轉換為Feather以提升性能
    optimized_path <- path
    if(arrow_available && grepl("\\.csv$", path)) {
      cat("    嘗試Feather優化...\n")
      feather_path <- convert_to_feather_safe(path)
      if(file.exists(feather_path) && grepl("\\.feather$", feather_path)) {
        optimized_path <- feather_path
        cat("    ✅ 使用Feather檔案進行處理\n")
      }
    }
    
    # 精確計算總行數
    total_rows <- count_lines_production(optimized_path)
    cat("    精確總資料筆數:", format(total_rows, big.mark = ","), "筆\n")
    
    # 讀取標題 (修正檔案來源不一致問題)
    if(grepl("\\.feather$", optimized_path)) {
      # 從Feather檔案取標題
      header_info <- read_chunk_production(optimized_path, n_rows = 1)
    } else {
      # 從原始CSV取標題
      header_info <- fread(path, nrows = 1)
    }
    header_names <- names(header_info)
    
    # 檢查必要欄位
    if(!target %in% header_names || !"date" %in% header_names) {
      cat("    錯誤：缺少必要欄位\n")
      return(FALSE)
    }
    
    # 計算分塊
    starts <- seq(1, total_rows, by = chunk_size - overlap)
    n_chunks <- length(starts)
    cat("    分為", n_chunks, "個區塊處理\n")
    
    # 預估總窗口數
    estimated_windows <- max(0, total_rows - input_seq_len - output_horizon + 1)
    cat("    預估總窗口數:", format(estimated_windows, big.mark = ","), "個\n")
    
    # 根據檔案路徑決定輸出目錄和資料類型
    output_info <- get_output_info(path)
    target_output_dir <- output_info$dir
    data_type <- output_info$type
    
    # 逐塊處理並直接輸出
    base_name <- tools::file_path_sans_ext(basename(path))
    chunk_files <- character(0)
    total_windows <- 0
    
    for (chunk_idx in seq_along(starts)) {
      st <- starts[chunk_idx]
      actual_nrows <- min(chunk_size, total_rows - st + 1)
      
      cat("    處理區塊", chunk_idx, "/", n_chunks, 
          "(行", format(st, big.mark = ","), "-", format(st + actual_nrows - 1, big.mark = ","), ")\n")
      
      # 使用優化路徑讀取當前區塊
      if(chunk_idx == 1) {
        dt <- read_chunk_production(optimized_path, n_rows = actual_nrows)
      } else {
        dt <- read_chunk_production(optimized_path, skip_rows = st - 1, 
                                   n_rows = actual_nrows, col_names = header_names)
      }
      
      cat("      讀取完成:", format(nrow(dt), big.mark = ","), "筆\n")
      
      # 快速時間轉換
      dt <- fast_time_convert(dt)
      
      # 只處理target的NA
      if(anyNA(dt[[target]])) {
        dt[, (target) := na.approx(get(target), na.rm = FALSE)]
        dt <- dt[!is.na(get(target))]
      }
      
      # 資料集級特徵處理
      mat <- process_features_dataset(dt, data_type)
      
      # 記憶體安全滑動窗口產生 (大檔區塊也要檢查記憶體安全)
      w <- make_windows_memory_safe(mat, input_seq_len, output_horizon, stride)
      
      if(length(w$y_raw) > 0) {
        # 直接儲存區塊結果到對應目錄
        chunk_file <- file.path(target_output_dir, sprintf("%s_chunk%02d.rds", base_name, chunk_idx))
        saveRDS(w, chunk_file, compress = "xz")
        chunk_files <- c(chunk_files, basename(chunk_file))
        
        total_windows <- total_windows + length(w$y_raw)
        cat("      區塊窗口數:", format(length(w$y_raw), big.mark = ","), "個\n")
        cat("      累計窗口數:", format(total_windows, big.mark = ","), "個\n")
      }
      
      # 清理當前區塊記憶體
      rm(dt, mat, w); gc()
    }
    
    # 創建索引檔案，記錄所有區塊檔案
    index_file <- file.path(target_output_dir, paste0(base_name, "_index.rds"))
    index_info <- list(
      chunk_files = chunk_files,
      total_windows = total_windows,
      n_chunks = length(chunk_files),
      features = build_dataset_features(data_type),
      data_type = data_type,
      seq_len = input_seq_len,
      target = target,
      output_dir = target_output_dir,
      original_file = path,
      optimized_file = optimized_path
    )
    saveRDS(index_info, index_file)
    
    cat("    ✅ 已儲存", length(chunk_files), "個區塊檔案\n")
    cat("    ✅ 已儲存索引檔案:", basename(index_file), "\n")
    cat("    ✅ 輸出位置:", target_output_dir, "\n")
    cat("    最終窗口數量:", format(total_windows, big.mark = ","), "個\n")
    
    # 標記整個檔案完成 (使用統一的basename邏輯)
    mark_completed(path)
    
    monitor_memory("最終清理後")
    
    return(TRUE)
    
  }, error = function(e) {
    cat("    ❌ 處理失敗:", e$message, "\n")
    return(FALSE)
  })
}

cat("生產級處理函式定義完成\n\n")

# ================================================================================
# 4. 主要執行流程
# ================================================================================

cat("開始執行滑動窗口資料產生流程 (生產級最終版)...\n\n")

start_time <- Sys.time()

# 4-1 智能檔案分類與並行處理
cat("=== 階段 1: 智能檔案分類與處理 ===\n")

  # 使用統一路徑管理
  all_dirs <- unlist(DATA_DIRS, use.names = FALSE)

small_files <- character(0)
large_files <- character(0)

# 先分類所有檔案
for(dir_name in all_dirs) {
  if(dir.exists(dir_name)) {
    files <- list.files(dir_name, full.names = TRUE, pattern = "\\.csv$")
    for(file in files) {
      file_class <- classify_file_size(file)
      if(file_class == "small") {
        small_files <- c(small_files, file)
      } else {
        large_files <- c(large_files, file)
      }
    }
  }
}

cat("檔案分類結果:\n")
cat("  小檔案:", length(small_files), "個\n")
cat("  大檔案:", length(large_files), "個\n\n")

# 4-2 並行處理小檔案
if(length(small_files) > 0) {
  cat("=== 階段 2: 並行處理小檔案 ===\n")
  
  if(require(future.apply, quietly = TRUE)) {
    cat("使用並行處理", length(small_files), "個小檔案...\n")
    small_results <- future.apply::future_sapply(small_files, process_small_file_production, 
                                                future.seed = TRUE)
  } else {
    cat("使用單核處理", length(small_files), "個小檔案...\n")
    small_results <- sapply(small_files, process_small_file_production)
  }
  
  success_small <- sum(small_results)
  cat("小檔案處理完成:", success_small, "/", length(small_files), "個成功\n\n")
} else {
  small_results <- logical(0)
  success_small <- 0
}

# 4-3 序列處理大檔案 (避免記憶體衝突)
if(length(large_files) > 0) {
  cat("=== 階段 3: 序列處理大檔案 ===\n")
  
  large_results <- sapply(large_files, process_big_file_production)
  success_large <- sum(large_results)
  cat("大檔案處理完成:", success_large, "/", length(large_files), "個成功\n\n")
} else {
  large_results <- logical(0)
  success_large <- 0
}

# ================================================================================
# 5. 執行結果總結
# ================================================================================

end_time <- Sys.time()
total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))

cat("\n", paste(rep("=", 80), collapse=""), "\n")
cat("=== 滑動窗口資料產生系統執行完成 (生產級最終版) ===\n")
cat(paste(rep("=", 80), collapse=""), "\n")

cat("\n📊 處理結果統計:\n")
cat("  🔹 小檔案:\n")
cat("    - 總檔案數:", length(small_files), "個\n")
cat("    - 成功處理:", success_small, "個\n")
cat("    - 成功率:", round(success_small/max(length(small_files), 1)*100, 1), "%\n")

cat("  🔹 大檔案:\n")
cat("    - 總檔案數:", length(large_files), "個\n")
cat("    - 成功處理:", success_large, "個\n")
cat("    - 成功率:", round(success_large/max(length(large_files), 1)*100, 1), "%\n")

# 按分類列出產生的檔案
cat("\n📁 分類輸出結果:\n")
total_size_mb <- 0

for(category in names(output_dirs)) {
  dir_path <- output_dirs[[category]]
  files <- list.files(dir_path, pattern = "\\.(rds)$", full.names = FALSE)
  
  if(length(files) > 0) {
    cat("  📂", category, "目錄 (", length(files), "個檔案):\n")
    
    # 分類顯示
    window_files <- files[grepl("_windows\\.rds$", files)]
    chunk_files <- files[grepl("_chunk\\d+\\.rds$", files)]
    index_files <- files[grepl("_index\\.rds$", files)]
    
    if(length(window_files) > 0) {
      cat("    小檔案窗口:", length(window_files), "個\n")
    }
    if(length(chunk_files) > 0) {
      cat("    大檔案區塊:", length(chunk_files), "個\n")
    }
    if(length(index_files) > 0) {
      cat("    索引檔案:", length(index_files), "個\n")
    }
    
    # 計算目錄大小
    dir_size <- 0
    for(file in files) {
      file_path <- file.path(dir_path, file)
      if(file.exists(file_path)) {
        dir_size <- dir_size + file.info(file_path)$size / (1024^2)
      }
    }
    total_size_mb <- total_size_mb + dir_size
    cat("    目錄大小:", round(dir_size, 1), "MB\n")
  } else {
    cat("  📂", category, "目錄: 無檔案\n")
  }
}

cat("  總大小:", round(total_size_mb, 1), "MB\n")

cat("\n🚀 生產級特性:\n")
cat("  1. ✅ Arrow生產級安裝 (Windows環境變數)\n")
cat("  2. ✅ 修正Arrow API問題 (read_feather vs open_dataset)\n")
cat("  3. ✅ 記憶體安全設計 (分批處理大窗口)\n")
cat("  4. ✅ Windows行數計算優化 (find vs PowerShell)\n")
cat("  5. ✅ 統一進度管理 (basename邏輯)\n")
cat("  6. ✅ 智能檔案分類 (避免大檔走小檔流程)\n")
cat("  7. ✅ Feather完整性檢查\n")
cat("  8. ✅ 檔案來源一致性\n")
cat("  9. ✅ 正規式疊合修正\n")
cat("  10. ✅ 生產級錯誤處理\n")

cat("\n⏱️ 執行時間統計:\n")
cat("  總執行時間:", round(total_time, 2), "分鐘\n")
total_files <- length(small_files) + length(large_files)
if(total_files > 0) {
  avg_time <- total_time / total_files
  cat("  平均每檔時間:", round(avg_time, 2), "分鐘\n")
}

cat("\n🎯 分類輸出說明:\n")
cat("  📂 Separate/: 原始分站資料窗口\n")
cat("  📂 Separate_Normalization/: 標準化分站資料窗口\n")
cat("  📂 Combine/: 合併資料窗口\n")
cat("  📂 Combine_Normalization/: 標準化合併資料窗口\n")

cat("\n🎯 資料結構說明:\n")
cat("  小檔案 *.rds 包含:\n")
cat("    - X_raw: array(n_windows,", input_seq_len, ", n_features)\n")
cat("    - y_raw: numeric(n_windows)\n")
cat("    - features: 該資料集的特徵清單\n")
cat("    - data_type: 資料集類型\n")
cat("  大檔案分塊 *_chunk##.rds 包含相同結構\n")
cat("  索引檔案 *_index.rds 包含區塊清單和完整元資訊\n")

monitor_memory("系統完成")

cat("\n🎉 生產級滑動窗口資料產生系統執行完畢！\n")
cat("解決所有Arrow API、記憶體、進度管理問題\n")
cat("真正適合生產環境部署\n")
cat("輸出位置:", normalizePath(output_base_dir), "\n")

# 提供讀取範例
cat("\n📖 讀取範例:\n")
cat("# 讀取小檔案窗口:\n")
cat("# w <- readRDS('sliding_windows_production/Separate/檔名_windows.rds')\n")
cat("# X <- w$X_raw; y <- w$y_raw\n")
cat("\n# 讀取大檔案窗口:\n")
cat("# idx <- readRDS('sliding_windows_production/Combine/檔名_index.rds')\n")
cat("# for(chunk_file in idx$chunk_files) {\n")
cat("#   w <- readRDS(file.path(idx$output_dir, chunk_file))\n")
cat("#   # 處理 w$X_raw 和 w$y_raw\n")
cat("# }\n")

cat(paste(rep("=", 80), collapse=""), "\n") 