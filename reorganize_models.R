#!/usr/bin/env Rscript
# ================================================================================
# 模型目錄重新組織腳本
# 目的：將混亂的模型檔案重新組織成清晰的目錄結構
# ================================================================================

library(data.table)

#' 重新組織模型目錄結構
#' @param source_dir 原始模型目錄
#' @param target_dir 目標重組目錄
reorganize_model_directory <- function(source_dir = "model_outputs/models/", 
                                     target_dir = "model_outputs/models_organized/") {
  
  cat("🔄 ================================================================================\n")
  cat("🚀 開始重新組織模型目錄結構\n")
  cat("================================================================================\n")
  
  # 掃描原始目錄
  cat("📂 掃描原始目錄:", source_dir, "\n")
  all_files <- list.files(source_dir, full.names = TRUE)
  
  if(length(all_files) == 0) {
    cat("❌ 原始目錄為空，無法重組\n")
    return(FALSE)
  }
  
  cat("✅ 找到", length(all_files), "個檔案\n")
  
  # 創建目標目錄結構
  if(!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # 分析檔案模式並重組
  file_groups <- group_files_by_model(all_files)
  
  cat("📊 識別出", length(file_groups), "個模型組\n")
  
  # 為每個模型組創建目錄並移動檔案
  success_count <- 0
  
  for(model_id in names(file_groups)) {
    tryCatch({
      result <- organize_model_group(file_groups[[model_id]], target_dir, model_id)
      if(result) success_count <- success_count + 1
    }, error = function(e) {
      cat("❌ 處理模型組失敗:", model_id, "-", e$message, "\n")
    })
  }
  
  cat("\n🎉 ================================================================================\n")
  cat("✅ 目錄重組完成！\n")
  cat("📊 成功處理:", success_count, "/", length(file_groups), "個模型組\n")
  cat("📁 新目錄結構:", target_dir, "\n")
  cat("================================================================================\n")
  
  return(TRUE)
}

#' 將檔案按模型分組
#' @param file_paths 檔案路徑向量
#' @return 按模型分組的檔案列表
group_files_by_model <- function(file_paths) {
  
  file_groups <- list()
  
  for(file_path in file_paths) {
    file_name <- basename(file_path)
    
    # 提取模型ID（移除檔案類型後綴）
    model_id <- extract_model_id(file_name)
    
    if(!is.null(model_id)) {
      if(is.null(file_groups[[model_id]])) {
        file_groups[[model_id]] <- list()
      }
      
      # 識別檔案類型
      file_type <- identify_file_type(file_name)
      file_groups[[model_id]][[file_type]] <- file_path
    }
  }
  
  return(file_groups)
}

#' 提取模型ID
#' @param file_name 檔案名稱
#' @return 模型ID
extract_model_id <- function(file_name) {
  
  # 移除常見的檔案後綴
  suffixes <- c("_complete.rds", "_importance.csv", "_original_importance.csv", "_native.txt")
  
  model_id <- file_name
  for(suffix in suffixes) {
    model_id <- gsub(paste0(suffix, "$"), "", model_id)
  }
  
  # 如果還有.rds後綴，也移除
  model_id <- gsub("\\.rds$", "", model_id)
  
  return(model_id)
}

#' 識別檔案類型
#' @param file_name 檔案名稱
#' @return 檔案類型
identify_file_type <- function(file_name) {
  
  if(grepl("_complete\\.rds$", file_name)) {
    return("model")
  } else if(grepl("_original_importance\\.csv$", file_name)) {
    return("original_importance")
  } else if(grepl("_importance\\.csv$", file_name)) {
    return("importance")
  } else if(grepl("_native\\.txt$", file_name)) {
    return("native")
  } else {
    return("other")
  }
}

#' 為單個模型組創建目錄並移動檔案
#' @param file_group 檔案組
#' @param target_dir 目標目錄
#' @param model_id 模型ID
#' @return 是否成功
organize_model_group <- function(file_group, target_dir, model_id) {
  
  # 解析模型資訊
  model_info <- parse_model_id(model_id)
  
  if(is.null(model_info)) {
    cat("⚠️ 無法解析模型ID:", model_id, "\n")
    return(FALSE)
  }
  
  # 創建目標目錄路徑
  model_target_dir <- file.path(
    target_dir,
    model_info$model_type,
    model_info$dataset_type,
    model_info$specific_name
  )
  
  if(!dir.exists(model_target_dir)) {
    dir.create(model_target_dir, recursive = TRUE)
  }
  
  # 移動檔案並重新命名
  file_mapping <- list(
    model = "model.rds",
    importance = "importance.csv",
    original_importance = "original_importance.csv",
    native = "native.txt"
  )
  
  moved_files <- 0
  
  for(file_type in names(file_group)) {
    if(!is.null(file_group[[file_type]]) && file.exists(file_group[[file_type]])) {
      
      target_name <- file_mapping[[file_type]]
      if(is.null(target_name)) target_name <- paste0(file_type, ".txt")
      
      target_path <- file.path(model_target_dir, target_name)
      
      tryCatch({
        file.copy(file_group[[file_type]], target_path, overwrite = TRUE)
        moved_files <- moved_files + 1
      }, error = function(e) {
        cat("  ⚠️ 複製檔案失敗:", basename(file_group[[file_type]]), "\n")
      })
    }
  }
  
  if(moved_files > 0) {
    cat("✅", model_id, "->", model_target_dir, "(", moved_files, "個檔案)\n")
    return(TRUE)
  } else {
    cat("❌", model_id, "-> 無檔案移動\n")
    return(FALSE)
  }
}

#' 解析模型ID獲取結構化資訊
#' @param model_id 模型ID
#' @return 模型資訊列表
parse_model_id <- function(model_id) {
  
  # LGBM模型模式
  if(grepl("^lgbm_", model_id)) {
    parts <- strsplit(model_id, "_")[[1]]
    
    if(length(parts) >= 3) {
      model_type <- "lgbm"
      dataset_type <- parts[2]  # combine, separate
      
      # 處理剩餘部分作為具體名稱
      if(length(parts) > 3) {
        specific_name <- paste(parts[3:length(parts)], collapse = "_")
      } else {
        specific_name <- parts[3]
      }
      
      return(list(
        model_type = model_type,
        dataset_type = dataset_type,
        specific_name = specific_name
      ))
    }
  }
  
  # LSTM模型模式
  if(grepl("^lstm_", model_id)) {
    parts <- strsplit(model_id, "_")[[1]]
    
    if(length(parts) >= 3) {
      model_type <- "lstm"
      dataset_type <- parts[2]
      
      if(length(parts) > 3) {
        specific_name <- paste(parts[3:length(parts)], collapse = "_")
      } else {
        specific_name <- parts[3]
      }
      
      return(list(
        model_type = model_type,
        dataset_type = dataset_type,
        specific_name = specific_name
      ))
    }
  }
  
  # 如果無法解析，創建通用結構
  return(list(
    model_type = "unknown",
    dataset_type = "general",
    specific_name = model_id
  ))
}

#' 生成新的掃描函數（適配新目錄結構）
#' @param models_dir 重組後的模型目錄
#' @return 模型資訊表
scan_organized_models <- function(models_dir = "model_outputs/models_organized/") {
  
  cat("📂 掃描重組後的模型目錄:", models_dir, "\n")
  
  if(!dir.exists(models_dir)) {
    cat("❌ 目錄不存在:", models_dir, "\n")
    return(data.table())
  }
  
  models_info <- data.table()
  
  # 遞歸掃描所有模型目錄
  model_dirs <- list.dirs(models_dir, recursive = TRUE, full.names = TRUE)
  model_dirs <- model_dirs[model_dirs != models_dir]  # 排除根目錄
  
  for(model_dir in model_dirs) {
    
    # 檢查是否包含model.rds
    model_file <- file.path(model_dir, "model.rds")
    
    if(file.exists(model_file)) {
      
      # 從路徑提取模型資訊
      rel_path <- gsub(paste0("^", models_dir, "/?"), "", model_dir)
      path_parts <- strsplit(rel_path, "/")[[1]]
      
      if(length(path_parts) >= 3) {
        model_type <- path_parts[1]
        dataset_type <- path_parts[2]
        specific_name <- path_parts[3]
        
        model_id <- paste(model_type, dataset_type, specific_name, sep = "_")
        
        # 檢查相關檔案
        importance_file <- file.path(model_dir, "importance.csv")
        original_importance_file <- file.path(model_dir, "original_importance.csv")
        native_file <- file.path(model_dir, "native.txt")
        
        models_info <- rbindlist(list(models_info, data.table(
          id = model_id,
          model_type = model_type,
          dataset_type = dataset_type,
          specific_name = specific_name,
          model_dir = model_dir,
          model_file = model_file,
          importance_file = if(file.exists(importance_file)) importance_file else NA,
          original_importance_file = if(file.exists(original_importance_file)) original_importance_file else NA,
          native_file = if(file.exists(native_file)) native_file else NA,
          has_importance = file.exists(importance_file),
          has_original_importance = file.exists(original_importance_file)
        )))
      }
    }
  }
  
  cat("✅ 掃描完成:", nrow(models_info), "個模型\n")
  if(nrow(models_info) > 0) {
    cat("  LightGBM:", sum(models_info$model_type == "lgbm"), "個\n")
    cat("  LSTM:", sum(models_info$model_type == "lstm"), "個\n")
  }
  
  return(models_info)
}

# ================================================================================
# 主執行部分
# ================================================================================

if(!interactive()) {
  cat("🔄 執行模型目錄重組...\n")
  
  # 執行重組
  success <- reorganize_model_directory()
  
  if(success) {
    cat("\n📊 測試新目錄掃描功能...\n")
    organized_models <- scan_organized_models()
    
    if(nrow(organized_models) > 0) {
      cat("\n📋 前5個模型預覽:\n")
      print(head(organized_models[, .(id, model_type, dataset_type, has_original_importance)], 5))
    }
  }
} 