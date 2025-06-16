# ================================================================================
# AQI 時間序列預測模型訓練 - 主要管線腳本
# ================================================================================

cat("🚀 載入 AQI 模型訓練管線...\n")

# ================================================================================
# 1. 載入所有模組
# ================================================================================

# 載入配置
source("model_src/config.R")

# 載入核心模組
source("model_src/loader.R")
source("model_src/split.R")
source("model_src/evaluate.R")
source("model_src/model_lgbm.R")
source("model_src/model_lstm.R")

cat("✅ 所有模組載入完成\n")

# ================================================================================
# 2. 主要訓練函數
# ================================================================================

#' 訓練單一資料類型的模型
#' @param data_type 資料類型 ("separate", "separate_norm", "combine", "combine_norm")
#' @param models 要訓練的模型列表 (c("lgbm", "lstm"))
#' @param max_files 最大載入檔案數 (僅對小檔案有效)
#' @param verbose 是否顯示詳細資訊
#' @return 訓練結果列表
train_single_data_type <- function(data_type, models = c("lgbm", "lstm"), 
                                  max_files = NULL, verbose = TRUE) {
  
  if(verbose) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("🎯 開始訓練資料類型:", toupper(data_type), "\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
  }
  
  start_time <- Sys.time()
  
  # 驗證資料類型
  if(!data_type %in% names(DATA_TYPES)) {
    stop("不支援的資料類型: ", data_type)
  }
  
  config <- DATA_TYPES[[data_type]]
  
  # 創建輸出目錄
  output_dir <- file.path(OUTPUT_PATHS$models, data_type)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  log_dir <- file.path(OUTPUT_PATHS$logs, data_type)
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  
  metrics_dir <- file.path(OUTPUT_PATHS$metrics, data_type)
  dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)
  
  # 載入資料
  if(verbose) {
    cat("📥 載入資料...\n")
  }
  
  tryCatch({
    if(config$is_large) {
      # 大檔案模式 - 載入第一個區塊進行訓練
      # 注意：這是為了記憶體管理的設計選擇，可能影響模型品質
      data_loader <- load_data_auto(config$path, data_type = data_type, verbose = verbose)
      
      if(verbose) {
        cat("⚠️  大檔案模式：僅使用第一個區塊進行訓練\n")
        cat("💡 提示：如需使用完整資料，請考慮增加系統記憶體或分批訓練\n")
      }
      
      # 獲取第一個區塊
      first_chunk <- data_loader()
      if(is.null(first_chunk)) {
        stop("無法載入第一個資料區塊")
      }
      
      dataset <- first_chunk
      
    } else {
      # 小檔案模式 - 載入並合併所有檔案
      datasets_list <- load_data_auto(config$path, data_type = data_type, 
                                     max_files = max_files, verbose = verbose)
      
      if(length(datasets_list) == 0) {
        stop("沒有載入到任何資料")
      }
      
      # 合併資料集
      if(length(datasets_list) > 1) {
        if(verbose) {
          cat("🔗 合併", length(datasets_list), "個資料集...\n")
        }
        dataset <- combine_datasets(datasets_list, verbose = verbose)
      } else {
        dataset <- datasets_list[[1]]
      }
    }
    
    # 驗證資料集
    validation_result <- validate_dataset(dataset)
    if(!validation_result$is_valid) {
      warning("資料集驗證失敗: ", paste(validation_result$issues, collapse = "; "))
    }
    
    if(verbose) {
      print(dataset)
    }
    
  }, error = function(e) {
    cat("❌ 資料載入失敗:", e$message, "\n")
    return(NULL)
  })
  
  # 資料切分
  if(verbose) {
    cat("\n✂️  執行資料切分...\n")
  }
  
  split_result <- time_cv(
    dataset = dataset,
    test_ratio = SPLIT_CONFIG$test_ratio,
    val_ratio = SPLIT_CONFIG$val_ratio,
    method = "sequential",
    verbose = verbose
  )
  
  # 提取資料集
  datasets <- extract_all_sets(dataset, split_result)
  
  # 評估切分品質
  split_quality <- evaluate_split_quality(dataset, split_result)
  if(verbose) {
    cat("切分品質評分:", round(split_quality$quality_score, 3), "\n")
  }
  
  # 儲存切分結果
  split_path <- file.path(log_dir, "data_split.rds")
  saveRDS(list(split = split_result, quality = split_quality), split_path)
  
  # 訓練模型
  trained_models <- list()
  predictions <- list()
  evaluations <- list()
  
  for(model_type in models) {
    if(verbose) {
      cat("\n🤖 訓練", toupper(model_type), "模型...\n")
             cat(paste(rep("-", 50), collapse = ""), "\n")
    }
    
    model_start_time <- Sys.time()
    
    tryCatch({
      if(model_type == "lgbm") {
        # 設定checkpoint路徑
        checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, data_type)
        if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
        checkpoint_path <- file.path(checkpoint_dir, paste0("lgbm_", data_type, "_checkpoint.rds"))
        
        # 訓練LightGBM模型
        model <- train_lgbm(
          train_dataset = datasets$train,
          val_dataset = datasets$val,
          params = LGBM_PARAMS,
          save_checkpoint = TRUE,
          checkpoint_path = checkpoint_path,
          verbose = verbose
        )
        
        # 預測
        test_predictions <- predict_lgbm(model, datasets$test, verbose = verbose)
        
        # 儲存模型（使用一致的基礎路徑，不含副檔名）
        model_path <- file.path(output_dir, paste0("lgbm_", data_type))
        save_lgbm_model(model, model_path, save_importance = TRUE)
        
      } else if(model_type == "lstm") {
        # 設定checkpoint路徑
        checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, data_type)
        if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
        checkpoint_path <- file.path(checkpoint_dir, paste0("lstm_", data_type, "_checkpoint.rds"))
        
        # 訓練LSTM模型
        model <- train_lstm(
          train_dataset = datasets$train,
          val_dataset = datasets$val,
          params = LSTM_PARAMS,
          checkpoint_path = checkpoint_path,
          verbose = verbose
        )
        
        # 預測
        test_predictions <- predict_lstm(model, datasets$test, verbose = verbose)
        
        # 儲存模型（使用一致的基礎路徑，不含副檔名）
        model_path <- file.path(output_dir, paste0("lstm_", data_type))
        save_lstm_model(model, model_path)
        
        # 清理GPU記憶體
        if(LSTM_PARAMS$device == "cuda") {
          clear_gpu_memory()
        }
        
      } else {
        stop("不支援的模型類型: ", model_type)
      }
      
      # 評估模型
      evaluation <- evaluate_predictions(datasets$test$y, test_predictions)
      
      # 添加 test_rmse 欄位以便 registry 掃描器使用
      evaluation$test_rmse <- evaluation$rmse
      
      # 將評估結果添加到模型物件
      model$evaluation <- evaluation
      
      if(verbose) {
        cat("\n📊 模型評估結果:\n")
        print(evaluation)
      }
      
      # 儲存評估結果
      eval_path <- file.path(metrics_dir, paste0(model_type, "_evaluation.rds"))
      save_evaluation(evaluation, eval_path)
      
      # 記錄結果
      trained_models[[model_type]] <- model
      predictions[[model_type]] <- test_predictions
      evaluations[[model_type]] <- evaluation
      
      model_end_time <- Sys.time()
      model_training_time <- as.numeric(difftime(model_end_time, model_start_time, units = "mins"))
      
      if(verbose) {
        cat("✅", toupper(model_type), "模型訓練完成，耗時:", round(model_training_time, 2), "分鐘\n")
      }
      
    }, error = function(e) {
      cat("❌", toupper(model_type), "模型訓練失敗:", e$message, "\n")
      trained_models[[model_type]] <- NULL
      predictions[[model_type]] <- NULL
      evaluations[[model_type]] <- NULL
    })
  }
  
  # 模型比較
  if(length(evaluations) > 1) {
    if(verbose) {
      cat("\n🏆 模型比較...\n")
    }
    
    comparison <- compare_models(evaluations, names(evaluations))
    
    if(verbose) {
      print(comparison)
    }
    
    # 儲存比較結果
    comparison_path <- file.path(metrics_dir, "model_comparison.csv")
    write.csv(comparison, comparison_path, row.names = FALSE)
  }
  
  end_time <- Sys.time()
  total_time <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if(verbose) {
    cat("\n✅ 資料類型", toupper(data_type), "訓練完成\n")
    cat("總耗時:", round(total_time, 2), "分鐘\n")
    cat("成功訓練模型:", length(trained_models), "/", length(models), "\n")
  }
  
  # 返回結果
  result <- list(
    data_type = data_type,
    dataset = dataset,
    split = split_result,
    datasets = datasets,
    models = trained_models,
    predictions = predictions,
    evaluations = evaluations,
    comparison = if(length(evaluations) > 1) comparison else NULL,
    total_time = total_time,
    completed_at = end_time
  )
  
  class(result) <- c("aqi_training_result", "list")
  
  return(result)
}

#' 打印訓練結果摘要
#' @param x aqi_training_result 物件
print.aqi_training_result <- function(x, ...) {
  cat("AQI 模型訓練結果\n")
  cat("================\n")
  cat("資料類型:", toupper(x$data_type), "\n")
  cat("總耗時:", round(x$total_time, 2), "分鐘\n")
  cat("完成時間:", format(x$completed_at, "%Y-%m-%d %H:%M:%S"), "\n")
  
  cat("\n📊 資料統計:\n")
  cat("  總窗口數:", format(x$dataset$n_windows, big.mark = ","), "\n")
  cat("  特徵數量:", x$dataset$n_features, "\n")
  cat("  序列長度:", x$dataset$seq_len, "\n")
  
  cat("\n🤖 訓練模型:\n")
  for(model_name in names(x$models)) {
    if(!is.null(x$models[[model_name]])) {
      eval_result <- x$evaluations[[model_name]]
      cat("  ", toupper(model_name), "- RMSE:", round(eval_result$rmse, 4), 
          ", R²:", round(eval_result$r2, 4), "\n")
    }
  }
  
  if(!is.null(x$comparison)) {
    best_model <- x$comparison$Model[which.min(x$comparison$Overall_Rank)]
    cat("\n🏆 最佳模型:", best_model, "\n")
  }
}

# ================================================================================
# 3. 批次訓練函數
# ================================================================================

#' 批次訓練所有資料類型
#' @param data_types 要訓練的資料類型列表
#' @param models 要訓練的模型列表
#' @param max_files 最大載入檔案數 (僅對小檔案有效)
#' @param verbose 是否顯示詳細資訊
#' @return 批次訓練結果列表
train_all_data_types <- function(data_types = names(DATA_TYPES), 
                                models = c("lgbm", "lstm"),
                                max_files = NULL,
                                verbose = TRUE) {
  
  if(verbose) {
    cat("🚀 開始批次訓練\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("資料類型:", paste(toupper(data_types), collapse = ", "), "\n")
    cat("模型類型:", paste(toupper(models), collapse = ", "), "\n")
    cat("開始時間:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
  }
  
  batch_start_time <- Sys.time()
  
  # 批次訓練結果
  batch_results <- list()
  success_count <- 0
  total_count <- length(data_types)
  
  for(i in seq_along(data_types)) {
    data_type <- data_types[i]
    
    if(verbose) {
      cat("\n📋 進度: [", i, "/", total_count, "] 處理資料類型:", toupper(data_type), "\n")
    }
    
    tryCatch({
      result <- train_single_data_type(
        data_type = data_type,
        models = models,
        max_files = max_files,
        verbose = verbose
      )
      
      if(!is.null(result)) {
        batch_results[[data_type]] <- result
        success_count <- success_count + 1
        
        if(verbose) {
          cat("✅ 資料類型", toupper(data_type), "處理成功\n")
        }
      }
      
    }, error = function(e) {
      cat("❌ 資料類型", toupper(data_type), "處理失敗:", e$message, "\n")
      batch_results[[data_type]] <- NULL
    })
    
    # 記憶體清理
    gc()
    if(any(models == "lstm") && LSTM_PARAMS$device == "cuda") {
      clear_gpu_memory()
    }
  }
  
  batch_end_time <- Sys.time()
  batch_total_time <- as.numeric(difftime(batch_end_time, batch_start_time, units = "hours"))
  
  if(verbose) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("🎉 批次訓練完成\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    cat("成功處理:", success_count, "/", total_count, "個資料類型\n")
    cat("總耗時:", round(batch_total_time, 2), "小時\n")
    cat("完成時間:", format(batch_end_time, "%Y-%m-%d %H:%M:%S"), "\n")
  }
  
  # 生成批次摘要
  batch_summary <- generate_batch_summary(batch_results, models)
  
  # 儲存批次結果
  batch_output_path <- file.path(OUTPUT_PATHS$logs, "batch_training_results.rds")
  saveRDS(list(
    results = batch_results,
    summary = batch_summary,
    success_count = success_count,
    total_count = total_count,
    total_time = batch_total_time,
    completed_at = batch_end_time
  ), batch_output_path)
  
  if(verbose) {
    cat("📄 批次結果已儲存:", batch_output_path, "\n")
    print(batch_summary)
  }
  
  return(list(
    results = batch_results,
    summary = batch_summary,
    success_count = success_count,
    total_count = total_count,
    total_time = batch_total_time
  ))
}

# ================================================================================
# 4. 結果分析函數
# ================================================================================

#' 生成批次訓練摘要
#' @param batch_results 批次訓練結果
#' @param models 模型列表
#' @return 摘要資料框
generate_batch_summary <- function(batch_results, models) {
  summary_data <- data.frame(
    Data_Type = character(),
    Status = character(),
    N_Windows = integer(),
    N_Features = integer(),
    Training_Time_Min = numeric(),
    stringsAsFactors = FALSE
  )
  
  # 為每個模型添加評估指標欄位
  for(model in models) {
    summary_data[[paste0(toupper(model), "_RMSE")]] <- numeric()
    summary_data[[paste0(toupper(model), "_MAE")]] <- numeric()
    summary_data[[paste0(toupper(model), "_R2")]] <- numeric()
  }
  
  for(data_type in names(batch_results)) {
    result <- batch_results[[data_type]]
    
    if(is.null(result)) {
      # 失敗的情況
      row <- data.frame(
        Data_Type = toupper(data_type),
        Status = "Failed",
        N_Windows = NA,
        N_Features = NA,
        Training_Time_Min = NA,
        stringsAsFactors = FALSE
      )
      
      # 為模型指標填充NA
      for(model in models) {
        row[[paste0(toupper(model), "_RMSE")]] <- NA
        row[[paste0(toupper(model), "_MAE")]] <- NA
        row[[paste0(toupper(model), "_R2")]] <- NA
      }
      
    } else {
      # 成功的情況
      row <- data.frame(
        Data_Type = toupper(data_type),
        Status = "Success",
        N_Windows = result$dataset$n_windows,
        N_Features = result$dataset$n_features,
        Training_Time_Min = round(result$total_time, 2),
        stringsAsFactors = FALSE
      )
      
      # 填充模型評估指標
      for(model in models) {
        if(model %in% names(result$evaluations) && !is.null(result$evaluations[[model]])) {
          eval_result <- result$evaluations[[model]]
          row[[paste0(toupper(model), "_RMSE")]] <- round(eval_result$rmse, 4)
          row[[paste0(toupper(model), "_MAE")]] <- round(eval_result$mae, 4)
          row[[paste0(toupper(model), "_R2")]] <- round(eval_result$r2, 4)
        } else {
          row[[paste0(toupper(model), "_RMSE")]] <- NA
          row[[paste0(toupper(model), "_MAE")]] <- NA
          row[[paste0(toupper(model), "_R2")]] <- NA
        }
      }
    }
    
    summary_data <- rbind(summary_data, row)
  }
  
  class(summary_data) <- c("aqi_batch_summary", "data.frame")
  return(summary_data)
}

#' 打印批次摘要
#' @param x aqi_batch_summary 物件
print.aqi_batch_summary <- function(x, ...) {
  cat("AQI 批次訓練摘要\n")
  cat("================\n")
  
  # 基本統計
  success_count <- sum(x$Status == "Success")
  total_count <- nrow(x)
  
  cat("成功率:", success_count, "/", total_count, 
      "(", round(success_count/total_count*100, 1), "%)\n")
  
  if(success_count > 0) {
    successful_data <- x[x$Status == "Success", ]
    
    cat("總窗口數:", format(sum(successful_data$N_Windows, na.rm = TRUE), big.mark = ","), "\n")
    cat("總訓練時間:", round(sum(successful_data$Training_Time_Min, na.rm = TRUE), 1), "分鐘\n")
    
    # 顯示主要結果
    cat("\n📊 詳細結果:\n")
    print(x)
    
    # 模型性能比較
    model_cols <- grep("_RMSE$", names(x), value = TRUE)
    if(length(model_cols) > 0) {
      cat("\n🏆 模型性能比較 (RMSE):\n")
      for(col in model_cols) {
        model_name <- gsub("_RMSE$", "", col)
        rmse_values <- x[[col]][!is.na(x[[col]])]
        if(length(rmse_values) > 0) {
          cat("  ", model_name, "- 平均:", round(mean(rmse_values), 4), 
              ", 範圍: [", round(min(rmse_values), 4), ", ", round(max(rmse_values), 4), "]\n")
        }
      }
    }
  }
}

# ================================================================================
# 5. 便利函數
# ================================================================================

#' 快速訓練單一模型
#' @param data_type 資料類型
#' @param model_type 模型類型
#' @param max_files 最大檔案數
#' @return 訓練結果
quick_train <- function(data_type, model_type, max_files = 5) {
  cat("🚀 快速訓練:", toupper(model_type), "模型，資料類型:", toupper(data_type), "\n")
  
  result <- train_single_data_type(
    data_type = data_type,
    models = model_type,
    max_files = max_files,
    verbose = TRUE
  )
  
  return(result)
}

#' 檢查訓練環境
check_training_environment <- function() {
  cat("🔍 檢查訓練環境\n")
     cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 檢查R版本
  cat("R版本:", R.version.string, "\n")
  
  # 檢查必要套件
  required_packages <- c("lightgbm", "torch", "Matrix", "data.table")
  
  for(pkg in required_packages) {
    if(requireNamespace(pkg, quietly = TRUE)) {
      cat("✅", pkg, "已安裝\n")
    } else {
      cat("❌", pkg, "未安裝\n")
    }
  }
  
  # 檢查CUDA
  if(requireNamespace("torch", quietly = TRUE)) {
    if(torch::cuda_is_available()) {
      cat("✅ CUDA可用\n")
      cat("  GPU數量:", torch::cuda_device_count(), "\n")
      # GPU記憶體檢查 (簡化版)
      tryCatch({
        test_tensor <- torch::torch_randn(100, 100, device = "cuda")
        cat("  GPU記憶體: 正常\n")
        rm(test_tensor)
      }, error = function(e) {
        cat("  GPU記憶體: 檢查失敗\n")
      })
    } else {
      cat("⚠️  CUDA不可用，將使用CPU\n")
    }
  }
  
  # 檢查資料路徑
  cat("\n📁 資料路徑檢查:\n")
  for(data_type in names(DATA_TYPES)) {
    path <- DATA_TYPES[[data_type]]$path
    if(dir.exists(path)) {
      file_count <- length(list.files(path, pattern = "\\.rds$"))
      cat("✅", toupper(data_type), ":", file_count, "個檔案\n")
    } else {
      cat("❌", toupper(data_type), ": 路徑不存在\n")
    }
  }
  
  # 檢查輸出目錄
  cat("\n📂 輸出目錄檢查:\n")
  for(path_name in names(OUTPUT_PATHS)) {
    path <- OUTPUT_PATHS[[path_name]]
    if(dir.exists(path)) {
      cat("✅", path_name, ": 存在\n")
    } else {
      cat("⚠️ ", path_name, ": 不存在，將自動創建\n")
      dir.create(path, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  cat("\n✅ 環境檢查完成\n")
}

# ================================================================================
# 6. 主要執行函數 (符合規劃要求)
# ================================================================================

#' 執行完整訓練管線 (支援大檔案模式)
#' @param models 要訓練的模型類型向量
#' @param max_files 每種資料類型的最大檔案數量
#' @param verbose 是否顯示詳細資訊
#' @return 完整的管線結果
run_full_pipeline <- function(models = c("lgbm", "lstm"), max_files = NULL, verbose = TRUE) {
  pipeline_start_time <- Sys.time()
  
  if(verbose) {
    cat(" ================================================================================ \n")
    cat("🚀 開始執行完整訓練管線\n")
    cat("📋", length(names(DATA_TYPES)), "資料類型 × ", length(models), " 模型\n")
    cat("================================================================================\n")
  }
  
  # 檢查環境
    check_training_environment()
  
  # 初始化結果容器
  all_results <- list()
  
  # 獲取資料夾配置
  data_folders <- list(
    separate = DATA_PATHS$separate,
    separate_norm = DATA_PATHS$separate_norm,
    combine = DATA_PATHS$combine,
    combine_norm = DATA_PATHS$combine_norm
  )
  
  # 處理每種資料類型
  for(dtype in names(data_folders)) {
    if(verbose) {
      cat("\n", paste(rep("=", 60), collapse = ""), "\n")
      cat("📂 處理資料類型:", toupper(dtype), "\n")
      cat("📁 路徑:", data_folders[[dtype]], "\n")
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    
    # 檢查資料夾是否存在
    if(!dir.exists(data_folders[[dtype]])) {
      if(verbose) {
        cat("⚠️  跳過", dtype, ": 資料夾不存在\n")
      }
      all_results[[dtype]] <- NULL
      next
    }
    
    # 檢查資料類型配置
    config <- DATA_TYPES[[dtype]]
    if(is.null(config)) {
      if(verbose) {
        cat("⚠️  跳過", dtype, ": 未知的資料類型\n")
      }
      all_results[[dtype]] <- NULL
      next
    }
    
    # 根據資料類型選擇處理模式
    if(config$is_large) {
      # 大檔案模式處理
      if(verbose) {
        cat("🔍 檢測到大檔案模式:", config$display_name, "\n")
      }
      
      # 檢查是否有索引檔案和chunk檔案
      index_files <- list.files(data_folders[[dtype]], pattern = "_index\\.rds$", full.names = TRUE)
      chunk_files <- list.files(data_folders[[dtype]], pattern = "_chunk\\d+\\.rds$", full.names = TRUE)
      
      if(length(index_files) == 0 || length(chunk_files) == 0) {
        if(verbose) {
          cat("⚠️  跳過", dtype, ": 大檔案模式但缺少索引或chunk檔案\n")
          cat("    索引檔案:", length(index_files), "個\n")
          cat("    Chunk檔案:", length(chunk_files), "個\n")
        }
        all_results[[dtype]] <- NULL
        next
      }
      
      # 處理大檔案 - 改進策略：使用多個chunk而非僅第一個
      dtype_results <- list()
      
      tryCatch({
        if(verbose) {
          cat("📊 載入大檔案索引...\n")
        }
        
        # 載入索引
        index <- readRDS(index_files[1])
        total_chunks <- length(chunk_files)
        
        # 改進的chunk選擇策略
        chunks_to_use <- if(!is.null(max_files) && max_files < total_chunks) {
          # 如果限制檔案數，均勻選擇chunk
          selected_indices <- round(seq(1, total_chunks, length.out = max_files))
          chunk_files[selected_indices]
        } else {
          # 使用所有chunk，但限制最大數量以避免記憶體問題
          max_chunks <- min(total_chunks, 5)  # 最多使用5個chunk
          chunk_files[1:max_chunks]
        }
        
        if(verbose) {
          cat("📋 選擇", length(chunks_to_use), "/", total_chunks, "個chunk進行訓練\n")
        }
        
        # 處理選中的chunk
        for(i in seq_along(chunks_to_use)) {
          chunk_file <- chunks_to_use[i]
          chunk_name <- paste0("chunk_", sprintf("%02d", i))
          
          if(verbose) {
            cat("\n📄 處理chunk:", basename(chunk_file), "\n")
          }
          
          tryCatch({
            # 載入chunk資料
            chunk_data <- readRDS(chunk_file)
            
            # 檢查chunk資料結構並適配
            if("X_raw" %in% names(chunk_data) && "y_raw" %in% names(chunk_data)) {
              # chunk格式：使用X_raw和y_raw
              x_data <- chunk_data$X_raw
              y_data <- chunk_data$y_raw
              
              # 生成特徵名稱
              n_features <- dim(x_data)[3]
              features <- paste0("feature_", 1:n_features)
              
            } else if("x" %in% names(chunk_data) && "y" %in% names(chunk_data)) {
              # 標準格式
              x_data <- chunk_data$x
              y_data <- chunk_data$y
              features <- if(is.null(chunk_data$features)) paste0("feature_", 1:dim(x_data)[3]) else chunk_data$features
              
            } else {
              stop("無法識別的chunk資料格式")
            }
            
            # 創建資料集物件
            ds <- create_dataset(
              x = x_data,
              y = y_data,
              features = features,
              data_type = dtype,
              metadata = list(
                source_file = basename(chunk_file),
                chunk_index = i,
                total_chunks = length(chunks_to_use),
                is_large_file_chunk = TRUE
              )
            )
            
            if(verbose) {
              cat("✅ Chunk載入完成:", format(ds$n_windows, big.mark = ","), "個窗口\n")
            }
            
            # 時序切分
            sp <- time_cv(ds, test_ratio = SPLIT_CONFIG$test_ratio, 
                         val_ratio = SPLIT_CONFIG$val_ratio, verbose = verbose)
            
            # 提取資料集
            datasets <- extract_all_sets(ds, sp)
            
            # 訓練模型
            file_results <- list()
            
            # 模型循環
            if ("lgbm" %in% models) {
              if(verbose) {
                cat("\n🌳 訓練 LightGBM 模型...\n")
              }
              
              tryCatch({
                # 設定checkpoint路徑
                checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, dtype)
                if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
                checkpoint_path <- file.path(checkpoint_dir, paste0("lgbm_", dtype, "_", chunk_name, "_checkpoint.rds"))
                
                lgbm_model <- train_lgbm(
                  train_dataset = datasets$train,
                  val_dataset = datasets$val,
                  params = LGBM_PARAMS,
                  save_checkpoint = TRUE,
                  checkpoint_path = checkpoint_path,
                  verbose = verbose
                )
                
                            # 預測和評估
            lgbm_pred <- predict_lgbm(lgbm_model, datasets$test, verbose = verbose)
            lgbm_eval <- evaluate_predictions(datasets$test$y, lgbm_pred)
            
            # 添加 test_rmse 欄位以便 registry 掃描器使用
            lgbm_eval$test_rmse <- lgbm_eval$rmse
            
            # 將評估結果添加到模型物件
            lgbm_model$evaluation <- lgbm_eval
            
            # 儲存模型（移除副檔名，使用一致的基礎路徑）
            model_name <- paste0("lgbm_", dtype, "_", chunk_name)
            model_path <- file.path(OUTPUT_PATHS$models, model_name)
            save_lgbm_model(lgbm_model, model_path, save_importance = TRUE)
                
                file_results$lgbm <- list(
                  model = lgbm_model,
                  predictions = lgbm_pred,
                  evaluation = lgbm_eval,
                  model_path = model_path
                )
                
                if(verbose) {
                  cat("✅ LightGBM 完成 - RMSE:", round(lgbm_eval$rmse, 4), "\n")
                }
                
              }, error = function(e) {
                if(verbose) {
                  cat("❌ LightGBM 失敗:", e$message, "\n")
                }
                file_results$lgbm <- NULL
              })
            }
            
            if ("lstm" %in% models) {
              if(verbose) {
                cat("\n🧠 訓練 LSTM 模型...\n")
              }
              
              tryCatch({
                # 設定checkpoint路徑
                checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, dtype)
                if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
                checkpoint_path <- file.path(checkpoint_dir, paste0("lstm_", dtype, "_", chunk_name, "_checkpoint.rds"))
                
                lstm_model <- train_lstm(
                  train_dataset = datasets$train,
                  val_dataset = datasets$val,
                  params = LSTM_PARAMS,
                  checkpoint_path = checkpoint_path,
                  verbose = verbose
                )
                
                            # 預測和評估
            lstm_pred <- predict_lstm(lstm_model, datasets$test, verbose = verbose)
            lstm_eval <- evaluate_predictions(datasets$test$y, lstm_pred)
            
            # 添加 test_rmse 欄位以便 registry 掃描器使用
            lstm_eval$test_rmse <- lstm_eval$rmse
            
            # 將評估結果添加到模型物件
            lstm_model$evaluation <- lstm_eval
            
            # 儲存模型（移除副檔名，使用一致的基礎路徑）
            model_name <- paste0("lstm_", dtype, "_", chunk_name)
            model_path <- file.path(OUTPUT_PATHS$models, model_name)
            save_lstm_model(lstm_model, model_path)
                
                file_results$lstm <- list(
                  model = lstm_model,
                  predictions = lstm_pred,
                  evaluation = lstm_eval,
                  model_path = model_path
                )
                
                if(verbose) {
                  cat("✅ LSTM 完成 - RMSE:", round(lstm_eval$rmse, 4), "\n")
                }
                
                # 清理GPU記憶體
                if(LSTM_PARAMS$device == "cuda") {
                  clear_gpu_memory()
                }
                
              }, error = function(e) {
                if(verbose) {
                  cat("❌ LSTM 失敗:", e$message, "\n")
                }
                file_results$lstm <- NULL
              })
            }
            
            # 儲存chunk結果
            dtype_results[[chunk_name]] <- list(
              dataset = ds,
              split = sp,
              models = file_results,
              file_path = chunk_file
            )
            
          }, error = function(e) {
            if(verbose) {
              cat("❌ Chunk處理失敗:", e$message, "\n")
            }
          })
        }
        
      }, error = function(e) {
        if(verbose) {
          cat("❌ 大檔案處理失敗:", e$message, "\n")
        }
        dtype_results <- list()
      })
      
    } else {
      # 小檔案模式處理
      if(verbose) {
        cat("🔍 檢測到小檔案模式:", config$display_name, "\n")
      }
      
      # 找到所有windows檔案
    files <- list.files(data_folders[[dtype]], pattern = "_windows\\.rds$", full.names = TRUE)
    
    if(length(files) == 0) {
      if(verbose) {
          cat("⚠️  跳過", dtype, ": 沒有找到windows檔案\n")
      }
      all_results[[dtype]] <- NULL
      next
    }
    
    # 限制檔案數量（如果指定）
    if(!is.null(max_files) && length(files) > max_files) {
      files <- files[1:max_files]
      if(verbose) {
        cat("📋 限制處理前", max_files, "個檔案\n")
      }
    }
    
    # 處理每個檔案
    dtype_results <- list()
    
    for (fp in files) {
      if(verbose) {
        cat("\n📄 處理檔案:", basename(fp), "\n")
      }
      
      tryCatch({
        # 載入資料 (使用統一介面)
        ds <- load_windows(fp, verbose = verbose)
        
        # 額外驗證：確保特徵數量與資料維度匹配（修復關鍵問題）
        actual_n_features <- dim(ds$x)[3]
        provided_n_features <- length(ds$features)
        if(actual_n_features != provided_n_features) {
          if(verbose) {
            cat("⚠️  檔案特徵不匹配，自動修正\n")
            cat("    資料維度:", actual_n_features, "個特徵\n")
            cat("    特徵名稱:", provided_n_features, "個\n")
          }
          # 重新生成匹配的特徵名稱
          ds$features <- paste0("feature_", 1:actual_n_features)
          ds$n_features <- actual_n_features
          if(verbose) {
            cat("  ✅ 已修正為", actual_n_features, "個特徵名稱\n")
          }
        }
        
        # 時序切分
        sp <- time_cv(ds, test_ratio = SPLIT_CONFIG$test_ratio, 
                     val_ratio = SPLIT_CONFIG$val_ratio, verbose = verbose)
        
        # 提取資料集
        datasets <- extract_all_sets(ds, sp)
        
        # 訓練模型
        file_results <- list()
        
        # 模型循環
        if ("lgbm" %in% models) {
          if(verbose) {
            cat("\n🌳 訓練 LightGBM 模型...\n")
          }
          
          tryCatch({
            # 設定checkpoint路徑
            file_key <- tools::file_path_sans_ext(basename(fp))
            checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, dtype)
            if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
            checkpoint_path <- file.path(checkpoint_dir, paste0("lgbm_", dtype, "_", file_key, "_checkpoint.rds"))
            
            lgbm_model <- train_lgbm(
              train_dataset = datasets$train,
              val_dataset = datasets$val,
              params = LGBM_PARAMS,
              save_checkpoint = TRUE,
              checkpoint_path = checkpoint_path,
              verbose = verbose
            )
            
            # 預測和評估
            lgbm_pred <- predict_lgbm(lgbm_model, datasets$test, verbose = verbose)
            lgbm_eval <- evaluate_predictions(datasets$test$y, lgbm_pred)
            
            # 添加 test_rmse 欄位以便 registry 掃描器使用
            lgbm_eval$test_rmse <- lgbm_eval$rmse
            
            # 將評估結果添加到模型物件
            lgbm_model$evaluation <- lgbm_eval
            
            # 儲存模型（移除副檔名，使用一致的基礎路徑）
            model_name <- paste0("lgbm_", dtype, "_", tools::file_path_sans_ext(basename(fp)))
            model_path <- file.path(OUTPUT_PATHS$models, model_name)
            save_lgbm_model(lgbm_model, model_path, save_importance = TRUE)
            
            file_results$lgbm <- list(
              model = lgbm_model,
              predictions = lgbm_pred,
              evaluation = lgbm_eval,
              model_path = model_path
            )
            
            if(verbose) {
              cat("✅ LightGBM 完成 - RMSE:", round(lgbm_eval$rmse, 4), "\n")
            }
            
          }, error = function(e) {
            if(verbose) {
              cat("❌ LightGBM 失敗:", e$message, "\n")
            }
            file_results$lgbm <- NULL
          })
        }
        
        if ("lstm" %in% models) {
          if(verbose) {
            cat("\n🧠 訓練 LSTM 模型...\n")
          }
          
          tryCatch({
            # 設定checkpoint路徑
            file_key <- tools::file_path_sans_ext(basename(fp))
            checkpoint_dir <- file.path(OUTPUT_PATHS$checkpoints, dtype)
            if(!dir.exists(checkpoint_dir)) dir.create(checkpoint_dir, recursive = TRUE, showWarnings = FALSE)
            checkpoint_path <- file.path(checkpoint_dir, paste0("lstm_", dtype, "_", file_key, "_checkpoint.rds"))
            
            lstm_model <- train_lstm(
              train_dataset = datasets$train,
              val_dataset = datasets$val,
              params = LSTM_PARAMS,
              checkpoint_path = checkpoint_path,
              verbose = verbose
            )
            
                        # 預測和評估
            lstm_pred <- predict_lstm(lstm_model, datasets$test, verbose = verbose)
            lstm_eval <- evaluate_predictions(datasets$test$y, lstm_pred)
            
            # 添加 test_rmse 欄位以便 registry 掃描器使用
            lstm_eval$test_rmse <- lstm_eval$rmse
            
            # 將評估結果添加到模型物件
            lstm_model$evaluation <- lstm_eval
            
            # 儲存模型（移除副檔名，使用一致的基礎路徑）
            model_name <- paste0("lstm_", dtype, "_", tools::file_path_sans_ext(basename(fp)))
            model_path <- file.path(OUTPUT_PATHS$models, model_name)
            save_lstm_model(lstm_model, model_path)
            
            file_results$lstm <- list(
              model = lstm_model,
              predictions = lstm_pred,
              evaluation = lstm_eval,
              model_path = model_path
            )
            
            if(verbose) {
              cat("✅ LSTM 完成 - RMSE:", round(lstm_eval$rmse, 4), "\n")
            }
            
            # 清理GPU記憶體
            if(LSTM_PARAMS$device == "cuda") {
              clear_gpu_memory()
            }
            
          }, error = function(e) {
            if(verbose) {
              cat("❌ LSTM 失敗:", e$message, "\n")
            }
            file_results$lstm <- NULL
          })
        }
        
        # 儲存檔案結果
        file_key <- tools::file_path_sans_ext(basename(fp))
        dtype_results[[file_key]] <- list(
          dataset = ds,
          split = sp,
          models = file_results,
          file_path = fp
        )
        
      }, error = function(e) {
        if(verbose) {
          cat("❌ 檔案處理失敗:", e$message, "\n")
        }
      })
      }
    }
    
    # 儲存資料類型結果
    all_results[[dtype]] <- dtype_results
  }
  
  # 計算總時間
  pipeline_end_time <- Sys.time()
  total_time <- as.numeric(difftime(pipeline_end_time, pipeline_start_time, units = "mins"))
  
  # 生成摘要
  if(verbose) {
    cat("\n", paste(rep("=", 80), collapse = ""), "\n")
    cat("🎉 管線執行完成！\n")
    cat("⏱️  總執行時間:", round(total_time, 2), "分鐘\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    
    # 統計摘要
    total_files <- 0
    successful_files <- 0
    
    for(dtype in names(all_results)) {
      if(!is.null(all_results[[dtype]])) {
        dtype_files <- length(all_results[[dtype]])
        total_files <- total_files + dtype_files
        
        for(file_result in all_results[[dtype]]) {
          if(!is.null(file_result$models) && length(file_result$models) > 0) {
            successful_files <- successful_files + 1
          }
        }
      }
    }
    
    cat("📊 處理統計:\n")
    cat("  總檔案數:", total_files, "\n")
    cat("  成功檔案數:", successful_files, "\n")
    cat("  成功率:", round(successful_files/total_files*100, 1), "%\n")
  }
  
  # 返回完整結果
  result <- list(
    results = all_results,
    models = models,
    data_types = names(data_folders),
    total_time = total_time,
    start_time = pipeline_start_time,
    end_time = pipeline_end_time,
    config = list(
      max_files = max_files,
      lgbm_params = LGBM_PARAMS,
      lstm_params = LSTM_PARAMS,
      split_config = SPLIT_CONFIG
    )
  )
  
  class(result) <- c("aqi_pipeline_result", "list")
  
  # 儲存完整結果
  result_path <- file.path(OUTPUT_PATHS$logs, paste0("pipeline_result_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  saveRDS(result, result_path)
  
  if(verbose) {
    cat("💾 結果已儲存:", result_path, "\n")
  }
  
  return(result)
}

cat("✅ 訓練管線載入完成\n") 