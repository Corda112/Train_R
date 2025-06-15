# ============================================================================
# AQI 模型解釋流程模組 (初版)
# ==========================================================================

# 此腳本提供輔助函式以解析已訓練模型，生成特徵重要度與全域/區域解釋資料。
# 依賴 config.R 中的 OUTPUT_PATHS 設定。

if(!exists("OUTPUT_PATHS")) {
  source(file.path(dirname(__file__), "config.R"))
}

library(data.table)

# ----------------------------------------------------------------------------
# 1. 模型註冊表掃描
# ----------------------------------------------------------------------------

#' 掃描模型輸出目錄並建立註冊表
#' @param models_dir 模型輸出目錄
#' @return data.table 含 id, dataset_type, model_type, path_prefix 等欄位
scan_model_registry <- function(models_dir = OUTPUT_PATHS$models) {
  files <- list.files(models_dir, pattern = "_complete\.rds$", full.names = TRUE)
  tbl <- lapply(files, function(fp) {
    name <- basename(fp)
    # 例: lgbm_separate_chunk_01.rds_complete
    parts <- strsplit(name, "_")[[1]]
    model_type <- parts[1]
    dataset_type <- parts[2]
    id <- sub("_complete.rds$", "", name)
    list(id = id,
         dataset_type = dataset_type,
         model_type = model_type,
         path_prefix = file.path(models_dir, sub("_complete.rds$", "", name)))
  })
  registry <- rbindlist(tbl)
  return(registry)
}

# ----------------------------------------------------------------------------
# 2. LightGBM 重要度解析
# ----------------------------------------------------------------------------

#' 讀取 LightGBM 的原始特徵重要度表
#' @param prefix 模型檔案前綴 (不含副檔名)
#' @return data.table
load_lgbm_importance <- function(prefix) {
  imp_path <- paste0(prefix, "_original_importance.csv")
  if(!file.exists(imp_path)) {
    stop("找不到重要度檔案: ", imp_path)
  }
  fread(imp_path)
}

# ----------------------------------------------------------------------------
# 3. LSTM 訓練歷史解析
# ----------------------------------------------------------------------------

#' 讀取 LSTM 訓練歷史
#' @param prefix 模型檔案前綴 (不含副檔名)
#' @return list
load_lstm_history <- function(prefix) {
  hist_path <- paste0(prefix, "_history.rds")
  if(!file.exists(hist_path)) {
    stop("找不到歷史檔案: ", hist_path)
  }
  readRDS(hist_path)
}

# ----------------------------------------------------------------------------
# 4. 簡易報告產生
# ----------------------------------------------------------------------------

#' 產生全域特徵重要度報告
#' @param registry 模型註冊表
#' @param out_file 輸出路徑 (CSV)
#' @return 無
generate_global_importance <- function(registry, out_file) {
  all_imp <- list()
  for(i in seq_len(nrow(registry))) {
    row <- registry[i]
    if(row$model_type == "lgbm") {
      imp <- load_lgbm_importance(row$path_prefix)
      imp$model_id <- row$id
      all_imp[[length(all_imp)+1]] <- imp
    }
  }
  if(length(all_imp) > 0) {
    out <- rbindlist(all_imp)
    fwrite(out, out_file)
  }
}

#' 載入並整理 LSTM 訓練損失曲線
#' @param registry 模型註冊表
#' @param out_file 輸出路徑 (CSV)
collect_lstm_history <- function(registry, out_file) {
  lst <- list()
  for(i in seq_len(nrow(registry))) {
    row <- registry[i]
    if(row$model_type == "lstm") {
      hist <- load_lstm_history(row$path_prefix)
      df <- data.frame(epoch = seq_along(hist$train_loss),
                       train_loss = hist$train_loss,
                       val_loss = hist$val_loss,
                       model_id = row$id)
      lst[[length(lst)+1]] <- df
    }
  }
  if(length(lst) > 0) {
    out <- rbindlist(lst)
    fwrite(out, out_file)
  }
}

# ----------------------------------------------------------------------------
# Example usage (需手動呼叫)
# ----------------------------------------------------------------------------
# reg <- scan_model_registry()
# generate_global_importance(reg, file.path(OUTPUT_PATHS$metrics, "global_importance.csv"))
# collect_lstm_history(reg, file.path(OUTPUT_PATHS$metrics, "lstm_history.csv"))

