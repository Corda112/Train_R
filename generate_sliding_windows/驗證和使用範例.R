# ================================================================================
# AQI滑動窗口資料驗證和使用範例腳本
# ================================================================================

cat("=== AQI滑動窗口資料驗證和使用範例 ===\n\n")

# ================================================================================
# 1. 系統完整性驗證
# ================================================================================

verify_system_output <- function() {
  cat("📊 系統輸出完整性驗證\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 檢查輸出目錄結構
  base_dir <- "sliding_windows_production"
  expected_dirs <- c("Separate", "Separate_Normalization", "Combine", "Combine_Normalization", "progress")
  
  for(dir_name in expected_dirs) {
    dir_path <- file.path(base_dir, dir_name)
    exists <- dir.exists(dir_path)
    cat("📁", dir_name, "目錄:", if(exists) "✅ 存在" else "❌ 缺失", "\n")
  }
  cat("\n")
  
  # 檢查小檔案數量
  separate_files <- length(list.files(file.path(base_dir, "Separate"), pattern = "\\.rds$"))
  separate_norm_files <- length(list.files(file.path(base_dir, "Separate_Normalization"), pattern = "\\.rds$"))
  
  cat("🔹 小檔案統計:\n")
  cat("  Separate檔案數:", separate_files, "/75", if(separate_files == 75) "✅" else "⚠️", "\n")
  cat("  Separate_Normalization檔案數:", separate_norm_files, "/75", if(separate_norm_files == 75) "✅" else "⚠️", "\n")
  
  # 檢查大檔案區塊
  combine_chunks <- length(list.files(file.path(base_dir, "Combine"), pattern = "chunk.*\\.rds$"))
  combine_norm_chunks <- length(list.files(file.path(base_dir, "Combine_Normalization"), pattern = "chunk.*\\.rds$"))
  
  cat("🔹 大檔案統計:\n")
  cat("  Combine區塊數:", combine_chunks, if(combine_chunks > 0) "✅" else "❌", "\n")
  cat("  Combine_Normalization區塊數:", combine_norm_chunks, if(combine_norm_chunks > 0) "✅" else "❌", "\n")
  
  # 檢查索引檔案
  combine_index <- file.exists(file.path(base_dir, "Combine", "Combine_AllData_index.rds"))
  combine_norm_index <- file.exists(file.path(base_dir, "Combine_Normalization", "Nomorlization_Combine_AllData_index.rds"))
  
  cat("🔹 索引檔案:\n")
  cat("  Combine索引:", if(combine_index) "✅ 存在" else "❌ 缺失", "\n")
  cat("  Combine_Normalization索引:", if(combine_norm_index) "✅ 存在" else "❌ 缺失", "\n")
  
  # 計算總檔案大小
  all_files <- list.files(base_dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  total_size_mb <- sum(file.info(all_files)$size, na.rm = TRUE) / (1024^2)
  
  cat("\n📈 總體統計:\n")
  cat("  總檔案數:", length(all_files), "\n")
  cat("  總大小:", round(total_size_mb, 1), "MB\n")
  cat("  平均檔案大小:", round(total_size_mb / length(all_files), 2), "MB\n\n")
}

# ================================================================================
# 2. 資料品質檢查
# ================================================================================

check_data_quality <- function() {
  cat("🔍 資料品質檢查\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 檢查小檔案樣本
  separate_files <- list.files("sliding_windows_production/Separate", pattern = "\\.rds$", full.names = TRUE)
  if(length(separate_files) > 0) {
    sample_file <- separate_files[1]
    cat("📋 檢查小檔案樣本:", basename(sample_file), "\n")
    
    tryCatch({
      w <- readRDS(sample_file)
      
      cat("  窗口數量:", format(length(w$y_raw), big.mark = ","), "\n")
      cat("  時間步長:", dim(w$X_raw)[2], "\n")
      cat("  特徵數量:", dim(w$X_raw)[3], "\n")
      cat("  資料類型:", w$data_type, "\n")
      cat("  特徵範例:", paste(w$features[1:min(5, length(w$features))], collapse = ", "), "...\n")
      
      # 檢查NA值
      na_count_X <- sum(is.na(w$X_raw))
      na_count_y <- sum(is.na(w$y_raw))
      cat("  X中NA數量:", na_count_X, "\n")
      cat("  y中NA數量:", na_count_y, "\n")
      
      # 檢查數值範圍
      y_range <- range(w$y_raw, na.rm = TRUE)
      cat("  y值範圍: [", round(y_range[1], 2), ",", round(y_range[2], 2), "]\n")
      
    }, error = function(e) {
      cat("  ❌ 讀取失敗:", e$message, "\n")
    })
  }
  
  # 檢查大檔案索引
  if(file.exists("sliding_windows_production/Combine/Combine_AllData_index.rds")) {
    cat("\n📋 檢查大檔案索引:\n")
    
    tryCatch({
      idx <- readRDS("sliding_windows_production/Combine/Combine_AllData_index.rds")
      
      cat("  總窗口數:", format(idx$total_windows, big.mark = ","), "\n")
      cat("  區塊數量:", idx$n_chunks, "\n")
      cat("  特徵數量:", length(idx$features), "\n")
      cat("  序列長度:", idx$seq_len, "\n")
      cat("  目標變數:", idx$target, "\n")
      
      # 檢查第一個區塊
      if(length(idx$chunk_files) > 0) {
        first_chunk <- file.path(idx$output_dir, idx$chunk_files[1])
        if(file.exists(first_chunk)) {
          w_chunk <- readRDS(first_chunk)
          cat("  第一區塊窗口數:", format(length(w_chunk$y_raw), big.mark = ","), "\n")
          
          # 檢查維度一致性
          expected_features <- length(idx$features)
          actual_features <- dim(w_chunk$X_raw)[3]
          cat("  特徵維度一致性:", if(expected_features == actual_features) "✅" else "❌", 
              "(", expected_features, "vs", actual_features, ")\n")
        }
      }
      
    }, error = function(e) {
      cat("  ❌ 讀取失敗:", e$message, "\n")
    })
  }
  cat("\n")
}

# ================================================================================
# 3. 使用範例演示
# ================================================================================

demo_usage_examples <- function() {
  cat("💡 使用範例演示\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 範例1: 讀取單一小檔案
  cat("📖 範例1: 讀取單一小檔案\n")
  separate_files <- list.files("sliding_windows_production/Separate", pattern = "\\.rds$", full.names = TRUE)
  if(length(separate_files) > 0) {
    sample_file <- separate_files[1]
    cat("檔案:", basename(sample_file), "\n")
    
    tryCatch({
      w <- readRDS(sample_file)
      X <- w$X_raw  # [n_windows, 72, n_features]
      y <- w$y_raw  # [n_windows]
      
      cat("程式碼範例:\n")
      cat("w <- readRDS('", sample_file, "')\n", sep = "")
      cat("X <- w$X_raw  # 維度:", paste(dim(X), collapse = " x "), "\n")
      cat("y <- w$y_raw  # 長度:", length(y), "\n")
      
      # 展示前3個窗口的第一個時間步
      if(length(y) >= 3) {
        cat("前3個窗口的第一個時間步 (前5個特徵):\n")
        for(i in 1:3) {
          values <- X[i, 1, 1:min(5, dim(X)[3])]
          cat("  窗口", i, ":", paste(round(values, 2), collapse = ", "), "-> y =", round(y[i], 2), "\n")
        }
      }
      
    }, error = function(e) {
      cat("❌ 讀取失敗:", e$message, "\n")
    })
  }
  
  cat("\n")
  
  # 範例2: 批次讀取大檔案
  cat("📖 範例2: 批次讀取大檔案\n")
  if(file.exists("sliding_windows_production/Combine/Combine_AllData_index.rds")) {
    cat("程式碼範例:\n")
    cat("idx <- readRDS('sliding_windows_production/Combine/Combine_AllData_index.rds')\n")
    cat("for(chunk_file in idx$chunk_files[1:3]) {  # 只處理前3個區塊\n")
    cat("  chunk_path <- file.path(idx$output_dir, chunk_file)\n")
    cat("  w <- readRDS(chunk_path)\n")
    cat("  # 處理 w$X_raw 和 w$y_raw\n")
    cat("}\n\n")
    
    # 實際執行前3個區塊
    tryCatch({
      idx <- readRDS("sliding_windows_production/Combine/Combine_AllData_index.rds")
      total_windows_demo <- 0
      
      for(i in 1:min(3, length(idx$chunk_files))) {
        chunk_file <- idx$chunk_files[i]
        chunk_path <- file.path(idx$output_dir, chunk_file)
        
        if(file.exists(chunk_path)) {
          w <- readRDS(chunk_path)
          chunk_windows <- length(w$y_raw)
          total_windows_demo <- total_windows_demo + chunk_windows
          
          cat("區塊", i, "(", chunk_file, "):", format(chunk_windows, big.mark = ","), "個窗口\n")
        }
      }
      
      cat("前3個區塊總窗口數:", format(total_windows_demo, big.mark = ","), "\n")
      
    }, error = function(e) {
      cat("❌ 讀取失敗:", e$message, "\n")
    })
  }
  
  cat("\n")
  
  # 範例3: 特徵統計分析
  cat("📖 範例3: 特徵統計分析\n")
  if(length(separate_files) > 0) {
    tryCatch({
      w <- readRDS(separate_files[1])
      
      # 計算第一個特徵的統計量
      feature_1_data <- w$X_raw[, , 1]  # 所有窗口的第一個特徵
      feature_name <- w$features[1]
      
      # 計算時間序列統計
      mean_by_timestep <- apply(feature_1_data, 2, mean, na.rm = TRUE)
      
      cat("特徵分析範例 (", feature_name, "):\n", sep = "")
      cat("  時間步1-5的平均值:", paste(round(mean_by_timestep[1:5], 3), collapse = ", "), "\n")
      cat("  整體平均值:", round(mean(feature_1_data, na.rm = TRUE), 3), "\n")
      cat("  整體標準差:", round(sd(feature_1_data, na.rm = TRUE), 3), "\n")
      
    }, error = function(e) {
      cat("❌ 分析失敗:", e$message, "\n")
    })
  }
  
  cat("\n")
}

# ================================================================================
# 4. 效能基準測試
# ================================================================================

benchmark_performance <- function() {
  cat("⚡ 效能基準測試\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 測試小檔案讀取速度
  separate_files <- list.files("sliding_windows_production/Separate", pattern = "\\.rds$", full.names = TRUE)
  if(length(separate_files) >= 5) {
    cat("📊 小檔案讀取速度測試 (前5個檔案):\n")
    
    start_time <- Sys.time()
    total_windows <- 0
    
    for(i in 1:5) {
      file_start <- Sys.time()
      w <- readRDS(separate_files[i])
      file_time <- as.numeric(Sys.time() - file_start)
      
      total_windows <- total_windows + length(w$y_raw)
      file_size_mb <- file.info(separate_files[i])$size / (1024^2)
      
      cat("  檔案", i, ":", round(file_time, 3), "秒,", 
          round(file_size_mb, 1), "MB,", 
          format(length(w$y_raw), big.mark = ","), "窗口\n")
    }
    
    total_time <- as.numeric(Sys.time() - start_time)
    cat("  總計:", round(total_time, 3), "秒,", format(total_windows, big.mark = ","), "窗口\n")
    cat("  平均速度:", round(total_windows / total_time), "窗口/秒\n")
  }
  
  # 測試大檔案索引讀取
  if(file.exists("sliding_windows_production/Combine/Combine_AllData_index.rds")) {
    cat("\n📊 大檔案索引讀取測試:\n")
    
    start_time <- Sys.time()
    idx <- readRDS("sliding_windows_production/Combine/Combine_AllData_index.rds")
    index_time <- as.numeric(Sys.time() - start_time)
    
    cat("  索引讀取時間:", round(index_time, 3), "秒\n")
    cat("  索引檔案大小:", round(file.info("sliding_windows_production/Combine/Combine_AllData_index.rds")$size / 1024, 1), "KB\n")
    
    # 測試單一區塊讀取
    if(length(idx$chunk_files) > 0) {
      chunk_path <- file.path(idx$output_dir, idx$chunk_files[1])
      if(file.exists(chunk_path)) {
        start_time <- Sys.time()
        w <- readRDS(chunk_path)
        chunk_time <- as.numeric(Sys.time() - start_time)
        
        chunk_size_mb <- file.info(chunk_path)$size / (1024^2)
        cat("  單一區塊讀取:", round(chunk_time, 3), "秒,", 
            round(chunk_size_mb, 1), "MB,", 
            format(length(w$y_raw), big.mark = ","), "窗口\n")
        cat("  區塊讀取速度:", round(length(w$y_raw) / chunk_time), "窗口/秒\n")
      }
    }
  }
  
  cat("\n")
}

# ================================================================================
# 5. 主執行函數
# ================================================================================

main <- function() {
  cat("開始執行AQI滑動窗口資料驗證和使用範例...\n\n")
  
  # 檢查輸出目錄是否存在
  if(!dir.exists("sliding_windows_production")) {
    cat("❌ 錯誤: 找不到 sliding_windows_production 目錄\n")
    cat("請先執行 generate_sliding_windows_production.R\n")
    return()
  }
  
  # 執行各項檢查和演示
  verify_system_output()
  check_data_quality()
  demo_usage_examples()
  benchmark_performance()
  
  cat("🎉 驗證和演示完成！\n")
  cat("📖 詳細說明請參考: AQI滑動窗口資料產生系統說明文件.md\n")
}

# 執行主函數
if(!interactive()) {
  main()
}

# ================================================================================
# 6. 實用工具函數
# ================================================================================

# 快速載入指定類型的所有小檔案
load_all_small_files <- function(data_type = "separate") {
  dir_map <- list(
    separate = "Separate",
    separate_norm = "Separate_Normalization"
  )
  
  if(!data_type %in% names(dir_map)) {
    stop("不支援的資料類型。請使用: ", paste(names(dir_map), collapse = ", "))
  }
  
  dir_path <- file.path("sliding_windows_production", dir_map[[data_type]])
  files <- list.files(dir_path, pattern = "_windows\\.rds$", full.names = TRUE)
  
  if(length(files) == 0) {
    stop("在 ", dir_path, " 中找不到窗口檔案")
  }
  
  cat("載入", length(files), "個", data_type, "檔案...\n")
  
  all_X <- list()
  all_y <- list()
  
  for(file in files) {
    w <- readRDS(file)
    file_name <- tools::file_path_sans_ext(basename(file))
    all_X[[file_name]] <- w$X_raw
    all_y[[file_name]] <- w$y_raw
  }
  
  cat("載入完成！總計", sum(sapply(all_y, length)), "個窗口\n")
  
  return(list(X = all_X, y = all_y, files = files))
}

# 快速統計所有資料集
summarize_all_datasets <- function() {
  datasets <- c("Separate", "Separate_Normalization", "Combine", "Combine_Normalization")
  
  for(dataset in datasets) {
    cat("\n📊", dataset, "資料集統計:\n")
    
    if(dataset %in% c("Separate", "Separate_Normalization")) {
      # 小檔案統計
      files <- list.files(file.path("sliding_windows_production", dataset), 
                         pattern = "\\.rds$", full.names = TRUE)
      
      if(length(files) > 0) {
        total_windows <- 0
        total_size <- 0
        
        for(file in files) {
          w <- readRDS(file)
          total_windows <- total_windows + length(w$y_raw)
          total_size <- total_size + file.info(file)$size
        }
        
        cat("  檔案數量:", length(files), "\n")
        cat("  總窗口數:", format(total_windows, big.mark = ","), "\n")
        cat("  總大小:", round(total_size / (1024^2), 1), "MB\n")
        cat("  平均窗口/檔案:", round(total_windows / length(files)), "\n")
      } else {
        cat("  ❌ 無檔案\n")
      }
      
    } else {
      # 大檔案統計
      index_file <- file.path("sliding_windows_production", dataset, 
                             paste0(ifelse(dataset == "Combine", "Combine_AllData", "Nomorlization_Combine_AllData"), "_index.rds"))
      
      if(file.exists(index_file)) {
        idx <- readRDS(index_file)
        
        # 計算總大小
        total_size <- 0
        for(chunk_file in idx$chunk_files) {
          chunk_path <- file.path(idx$output_dir, chunk_file)
          if(file.exists(chunk_path)) {
            total_size <- total_size + file.info(chunk_path)$size
          }
        }
        
        cat("  區塊數量:", idx$n_chunks, "\n")
        cat("  總窗口數:", format(idx$total_windows, big.mark = ","), "\n")
        cat("  總大小:", round(total_size / (1024^2), 1), "MB\n")
        cat("  平均窗口/區塊:", round(idx$total_windows / idx$n_chunks), "\n")
      } else {
        cat("  ❌ 無索引檔案\n")
      }
    }
  }
}

cat("✅ 驗證和使用範例腳本載入完成\n")
cat("💡 使用方法:\n")
cat("  main()                    # 執行完整驗證\n")
cat("  verify_system_output()    # 僅驗證系統輸出\n")
cat("  check_data_quality()      # 僅檢查資料品質\n")
cat("  demo_usage_examples()     # 僅演示使用範例\n")
cat("  benchmark_performance()   # 僅測試效能\n")
cat("  load_all_small_files()    # 載入所有小檔案\n")
cat("  summarize_all_datasets()  # 統計所有資料集\n\n") 