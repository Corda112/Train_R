# AQI 模型訓練系統

## 快速開始

### 1. 執行完整訓練
```bash
Rscript run_aqi_training.R
```

### 2. 只訓練特定模型
```bash
# 只訓練 LightGBM
Rscript run_aqi_training.R --models lgbm

# 只訓練 LSTM
Rscript run_aqi_training.R --models lstm
```

### 3. 限制檔案數量（測試用）
```bash
# 每種資料類型最多處理5個檔案
Rscript run_aqi_training.R --max-files 5
```

## 系統架構

### 核心模組 (`model_src/`)
- `config.R` - 系統配置
- `loader.R` - 資料載入
- `split.R` - 資料切分
- `evaluate.R` - 模型評估
- `model_lgbm.R` - LightGBM模型
- `model_lstm.R` - LSTM模型
- `pipeline.R` - 訓練管線

### 資料目錄
- `DATA/` - 原始資料
- `generate_sliding_windows/` - 滑動視窗資料
- `model_outputs/` - 訓練結果

## 系統狀態

✅ **已配置完成**
- CUDA 11.7 已安裝
- 所有R套件已安裝
- 系統符合性評分: 100%

### 可用功能
- ✅ LightGBM 模型（完全可用）
- ✅ LSTM 模型（CPU模式）
- ✅ 四種資料類型支援
- ✅ 自動資料切分
- ✅ 模型評估

### 注意事項
- LSTM 使用 CPU 訓練（GPU 加速暫不可用）
- 建議優先使用 LightGBM（性能優秀且速度快）

## 環境需求

- R 4.4.3+
- conda 環境: `r_datascience`
- 必要套件: data.table, lightgbm, torch, Matrix, abind, caret, jsonlite

---
*系統就緒，可開始 AQI 模型訓練* 