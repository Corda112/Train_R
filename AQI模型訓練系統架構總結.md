# 🎯 AQI 時間序列預測模型訓練系統 - 架構總結報告

## 📋 專案概述

本專案實現了一套 **「純 R、易維護、可擴充」** 的 **LightGBM (CPU) + LSTM (GPU)** 雙模型訓練框架，支援四種資料型別的統一處理和兩條訓練路徑的並行執行。

### 🎯 核心目標
- ✅ **一次支援 4 種資料型別**（Separate / Separate_Normalization / Combine / Combine_Normalization）
- ✅ **兩條訓練路徑共用同一資料載入與評估介面**
- ✅ **隨時可以把路徑、參數抽成 YAML / JSON 而不動到核心邏輯**
- ✅ **完整的錯誤處理、檢查點、資源管理機制**

---

## 🏗️ 專案目錄結構

```
TRAIN_R/
├─ generate_sliding_windows/
│   └─ sliding_windows_production/      # 滑動窗口資料
│       ├─ Separate/                    # 分站原始資料 (70個檔案)
│       ├─ Separate_Normalization/     # 分站標準化資料
│       ├─ Combine/                     # 合併原始資料
│       └─ Combine_Normalization/      # 合併標準化資料
├─ model_src/                           # 核心模組
│   ├─ config.R                         # 基礎路徑、常數、超參數配置
│   ├─ loader.R                         # 統一資料載入與Scaler持久化
│   ├─ split.R                          # 時序切分工具 (train/val/test)
│   ├─ model_lgbm.R                     # LightGBM 訓練/推論/重要度 (優化版)
│   ├─ model_lstm.R                     # LSTM 建模/訓練/推論 (優化版)
│   ├─ evaluate.R                       # RMSE、MAE、MAPE、R²統一介面
│   └─ pipeline.R                       # 主腳本：四資料型 × 二模型循環
├─ model_outputs/                       # 輸出目錄
│   ├─ models/                          # 訓練好的模型
│   ├─ logs/                            # 訓練日誌
│   ├─ metrics/                         # 評估指標
│   └─ checkpoints/                     # 檢查點檔案
├─ test_training_system.R               # 完整系統測試腳本
└─ AQI模型訓練系統架構總結.md           # 本文件
```

---

## ✨ 核心功能與優化

### 🔧 **已完成的優化功能**

#### 1. **統一 Dataset 介面**
```r
# 標準化資料集物件結構
dataset <- list(
  x = array(n_windows, seq_len=72, n_features),  # 72小時特徵序列
  y = numeric(n_windows),                        # 目標AQI值
  features = character(n_features),              # 特徵名稱
  data_type = "separate|separate_norm|combine|combine_norm",
  metadata = list(...)                           # 額外元資料
)
```

#### 2. **LightGBM 模組優化** (`model_lgbm.R`)
- ✅ **展平矩陣 + 可回溯欄位名稱**：`feature_lag71`, `feature_lag70`, ..., `feature_lag0`
- ✅ **稀疏矩陣支援**：自動檢測稀疏度，節省記憶體
- ✅ **特徵重要度分析**：原始特徵聚合、時間窗口分析
- ✅ **檢查點機制**：訓練中斷可恢復
- ✅ **模型健康檢查**：自動驗證模型完整性

#### 3. **LSTM 模組優化** (`model_lstm.R`)
- ✅ **混合精度訓練**：自動檢測GPU支援，節省顯存
- ✅ **早停機制**：R6類實現，自動恢復最佳權重
- ✅ **學習率調度**：支援 ReduceOnPlateau、StepLR、CosineAnnealing
- ✅ **GPU記憶體管理**：自動清理、監控使用情況
- ✅ **檢查點功能**：每N輪自動保存，支援斷點續訓

#### 4. **資料切分優化** (`split.R`)
- ✅ **三分切分**：train (70%) / val (10%) / test (20%)
- ✅ **時序保持**：確保時間順序不被打亂
- ✅ **分層切分**：基於目標變數分佈的可選策略
- ✅ **切分品質評估**：統計各集合的分佈特性

#### 5. **Scaler 持久化** (`loader.R`)
- ✅ **標準化參數保存**：mean、sd、features 完整記錄
- ✅ **版本控制**：創建時間、資料類型標記
- ✅ **自動應用**：訓練時保存，推論時載入
- ✅ **反向變換**：支援預測結果反標準化

---

## 🚀 使用指南

### 1. **環境準備**

#### 安裝R和必要套件
```bash
# 安裝R (如果尚未安裝)
conda install r-base r-essentials -c conda-forge

# 安裝必要的R套件
R -e "install.packages(c('lightgbm', 'torch', 'data.table', 'Matrix', 'R6'))"
```

#### 檢查CUDA環境
```r
# 檢查CUDA可用性
torch::cuda_is_available()
torch::cuda_get_device_name()
```

### 2. **快速開始**

#### 系統測試
```r
# 運行完整系統測試
source("test_training_system.R")
```

#### 單一模型訓練
```r
# 載入所有模組
source("model_src/config.R")
source("model_src/loader.R")
source("model_src/split.R")
source("model_src/evaluate.R")
source("model_src/model_lgbm.R")
source("model_src/model_lstm.R")

# 載入資料
dataset <- load_windows("path/to/data_windows.rds")

# 資料切分
split_result <- time_split(dataset, train_ratio=0.7, val_ratio=0.1, test_ratio=0.2)
datasets <- extract_all_sets(dataset, split_result)

# 訓練LightGBM
lgbm_model <- train_lgbm(datasets$train, datasets$val, LGBM_PARAMS)
lgbm_pred <- predict_lgbm(lgbm_model, datasets$test)

# 訓練LSTM
lstm_model <- train_lstm(datasets$train, datasets$val, LSTM_PARAMS)
lstm_pred <- predict_lstm(lstm_model, datasets$test)

# 評估模型
lgbm_metrics <- evaluate_predictions(datasets$test$y, lgbm_pred)
lstm_metrics <- evaluate_predictions(datasets$test$y, lstm_pred)
```

#### 批次訓練所有資料類型
```r
# 載入主管線
source("model_src/pipeline.R")

# 訓練所有資料類型的所有模型
results <- train_all_data_types(models = c("lgbm", "lstm"))

# 僅訓練LightGBM
results <- train_all_data_types(models = "lgbm")
```

### 3. **進階配置**

#### 自定義超參數
```r
# 修改LightGBM參數
custom_lgbm_params <- LGBM_PARAMS
custom_lgbm_params$learning_rate <- 0.01
custom_lgbm_params$num_leaves <- 255

# 修改LSTM參數
custom_lstm_params <- LSTM_PARAMS
custom_lstm_params$hidden_size <- 256
custom_lstm_params$epochs <- 200
custom_lstm_params$mixed_precision <- TRUE
```

#### 檢查點和恢復
```r
# 啟用檢查點保存
checkpoint_path <- "model_outputs/checkpoints/my_model"

# LightGBM檢查點
lgbm_model <- train_lgbm(..., save_checkpoint=TRUE, checkpoint_path=checkpoint_path)

# LSTM檢查點
lstm_model <- train_lstm(..., save_checkpoint=TRUE, checkpoint_path=checkpoint_path)

# 載入檢查點
checkpoint <- load_lstm_checkpoint(paste0(checkpoint_path, "_final.pt"), model, optimizer)
```

---

## 📊 技術特色

### 🔥 **LightGBM (CPU) 特色**
| 特色 | 說明 | 優勢 |
|------|------|------|
| **CPU優化** | 避免GPU編譯問題，穩定可靠 | Windows/R環境友好 |
| **展平處理** | 72×特徵 → 可回溯欄位名 | 特徵重要度可解釋 |
| **稀疏矩陣** | 自動檢測，節省記憶體 | 處理大維度資料 |
| **特徵分析** | 原始特徵+時間窗口重要度 | 深度業務洞察 |

### 🧠 **LSTM (GPU) 特色**
| 特色 | 說明 | 優勢 |
|------|------|------|
| **混合精度** | 自動檢測GPU能力 | 節省50%顯存+加速 |
| **早停機制** | R6類實現，智能停止 | 防止過擬合 |
| **記憶體管理** | 自動清理GPU快取 | 長時間訓練穩定 |
| **檢查點** | 每N輪自動保存 | 訓練中斷可恢復 |

---

## 🎯 性能基準

### 📈 **預期性能指標**
| 資料類型 | 樣本數 | LightGBM RMSE | LSTM RMSE | 訓練時間 |
|----------|--------|---------------|-----------|----------|
| Separate | ~2K/站 | 15-25 | 12-20 | 2-5分鐘 |
| Separate_Norm | ~2K/站 | 12-20 | 10-18 | 2-5分鐘 |
| Combine | ~150K | 20-30 | 15-25 | 10-30分鐘 |
| Combine_Norm | ~150K | 15-25 | 12-22 | 10-30分鐘 |

### ⚡ **資源使用**
- **CPU**: LightGBM 使用 `detectCores()-2` 執行緒
- **GPU**: LSTM 自動檢測CUDA，混合精度節省50%顯存
- **記憶體**: 稀疏矩陣+批次處理，支援大資料集
- **儲存**: 模型+重要度+歷史+檢查點完整保存

---

## 🔧 故障排除

### ❌ **常見問題與解決方案**

#### 1. **套件安裝問題**
```r
# 問題：lightgbm安裝失敗
# 解決：
install.packages("lightgbm", repos="https://cran.r-project.org")

# 問題：torch下載LibTorch失敗
# 解決：
torch::install_torch(reinstall=TRUE)
```

#### 2. **CUDA相關問題**
```r
# 問題：CUDA不可用
# 檢查：
torch::cuda_is_available()

# 問題：GPU記憶體不足
# 解決：
clear_gpu_memory()
# 或降低batch_size
LSTM_PARAMS$batch_size <- 128
```

#### 3. **資料載入問題**
```r
# 問題：資料集驗證失敗
# 檢查：
validation_result <- validate_dataset(dataset)
print(validation_result$issues)

# 問題：特徵維度不一致
# 解決：確保所有檔案使用相同的特徵集
```

#### 4. **記憶體問題**
```r
# 問題：LightGBM記憶體爆炸
# 解決：
LGBM_PARAMS$max_bin <- 63
LGBM_PARAMS$feature_fraction <- 0.6

# 問題：R記憶體不足
# 解決：
gc()  # 強制垃圾回收
```

---

## 🚀 下一步發展

### 📋 **待實現功能**
1. **配置外部化**：YAML/JSON配置檔案
2. **多GPU支援**：分散式LSTM訓練
3. **自動調參**：貝葉斯優化超參數
4. **模型融合**：Ensemble方法
5. **即時監控**：TensorBoard整合
6. **API服務**：Plumber REST API
7. **容器化**：Docker部署

### 🎯 **優化方向**
1. **效能優化**：更高效的資料載入
2. **可解釋性**：SHAP值分析
3. **魯棒性**：異常檢測與處理
4. **擴展性**：支援更多模型類型
5. **自動化**：CI/CD管線整合

---

## 📞 聯絡資訊

### 🛠️ **技術支援**
- **架構問題**：檢查模組載入順序
- **效能問題**：調整批次大小和參數
- **記憶體問題**：啟用稀疏矩陣和記憶體清理
- **CUDA問題**：檢查驅動版本和torch安裝

### 📚 **參考文件**
- [LightGBM官方文件](https://lightgbm.readthedocs.io/)
- [Torch for R文件](https://torch.mlverse.org/)
- [AQI資料說明](generate_sliding_windows/AQI滑動窗口資料產生系統說明文件.md)

---

## 🎉 總結

本系統成功實現了：
- ✅ **統一的四資料類型處理框架**
- ✅ **LightGBM + LSTM 雙模型並行訓練**
- ✅ **完整的錯誤處理和恢復機制**
- ✅ **可擴展的模組化架構**
- ✅ **生產級的模型管理功能**

系統已準備好進行大規模AQI時間序列預測模型訓練，具備良好的維護性和擴展性。🚀 