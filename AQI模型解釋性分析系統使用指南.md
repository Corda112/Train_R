# 🎯 AQI模型解釋性分析系統 - 使用指南

## 📋 系統概述

本系統提供兩個版本的模型分析工具：
- **簡化版** (`run_simple_analysis.R`): 快速、穩定、易用
- **完整版** (`run_complete_analysis.R`): 功能豐富、包含模型比較

### 🎯 系統功能
- **LightGBM分析**: 特徵重要度、SHAP值分析
- **LSTM分析**: 訓練歷史、性能指標
- **模型比較**: LSTM vs LightGBM 性能對比
- **報告生成**: HTML報告、PNG圖表、CSV數據

### 📊 支援模型
- **LightGBM**: 123個模型
- **LSTM**: 124個模型
- **總計**: 247個訓練完成的模型

---

## ⚡ 快速開始 (推薦)

### 使用簡化版系統

```bash
# 快速分析 (3個模型)
Rscript run_simple_analysis.R quick

# 標準分析 (5個模型)
Rscript run_simple_analysis.R

# 完整分析 (10個模型)
Rscript run_simple_analysis.R full
```

### 在R中使用

```r
# 載入系統
source("run_simple_analysis.R")

# 快速分析
result <- run_simple_analysis(max_models = 3)

# 標準分析
result <- run_simple_analysis(max_models = 5)

# 完整分析
result <- run_simple_analysis(max_models = 10)

# 自定義輸出目錄
result <- run_simple_analysis(
  output_dir = "my_analysis/",
  max_models = 8
)
```

---

## 🔍 詳細功能說明

### 1. 簡化版系統 (推薦使用)

#### 特點
- ✅ 穩定可靠，不依賴複雜套件
- ✅ 快速執行，適合日常使用
- ✅ 自動檢測檔案結構
- ✅ 生成高質量圖表和報告

#### 分析內容
- **LightGBM**: 特徵重要度分析和可視化
- **LSTM**: 訓練歷史和性能指標
- **輸出**: PNG圖表、CSV數據、HTML報告

#### 使用方法
```r
# 基本使用
source("run_simple_analysis.R")
result <- run_simple_analysis()

# 檢查結果
str(result)
```

### 2. 完整版系統 (進階功能)

#### 特點
- 🔬 功能豐富，包含SHAP分析
- 📊 模型比較分析
- 📈 增強版HTML報告
- ⚠️ 需要額外套件支援

#### 分析內容
- **LightGBM**: 特徵重要度 + SHAP分析
- **LSTM**: 梯度重要度分析
- **模型比較**: 性能對比和特徵分析
- **輸出**: 增強版報告、比較圖表

#### 使用方法
```r
# 載入完整版系統
source("run_complete_analysis.R")

# 快速分析
result <- quick_analysis()

# 深度分析
result <- deep_analysis()

# 自定義分析
result <- run_complete_analysis(
  output_dir = "deep_analysis/",
  max_models = 15,
  include_comparison = TRUE
)
```

---

## 📊 輸出結果說明

### 1. 圖表檔案 (PNG, 300 DPI)

#### LightGBM分析圖表
- `importance_[model_id].png` - 特徵重要度圖
- `shap_[model_id].png` - SHAP重要度圖 (完整版)

#### LSTM分析圖表
- `lstm_history_[model_id].png` - 訓練歷史曲線
- `lstm_gradient_[model_id].png` - 梯度重要度 (完整版)

#### 比較分析圖表 (完整版)
- `lstm_performance_comparison.png` - LSTM性能比較
- `lstm_training_time.png` - 訓練時間比較
- `lgbm_feature_importance_summary.png` - LightGBM特徵總結

### 2. 數據檔案 (CSV)
- `importance_data_[model_id].csv` - 特徵重要度數據
- `model_comparison_summary.csv` - 模型比較總結 (完整版)
- `lstm_performance_details.csv` - LSTM性能詳情 (完整版)

### 3. HTML報告
- **簡化版**: `analysis_report.html` - 基本分析報告
- **完整版**: `model_analysis_enhanced_report.html` - 增強版報告

---

## 🎛️ 分析模式選擇

### 1. 快速檢查模式 ⚡
```bash
Rscript run_simple_analysis.R quick
```
- **適用**: 快速檢查、初步分析
- **模型數**: 3個
- **時間**: 1-2分鐘
- **輸出**: 基本圖表和報告

### 2. 日常分析模式 📊
```bash
Rscript run_simple_analysis.R
```
- **適用**: 日常分析、報告生成
- **模型數**: 5個
- **時間**: 3-5分鐘
- **輸出**: 完整圖表和數據

### 3. 深度研究模式 🔬
```bash
Rscript run_simple_analysis.R full
```
- **適用**: 深入研究、論文撰寫
- **模型數**: 10個
- **時間**: 8-15分鐘
- **輸出**: 詳細分析結果

### 4. 完整比較模式 🔍
```bash
Rscript run_complete_analysis.R deep
```
- **適用**: 模型比較、完整研究
- **模型數**: 20個
- **時間**: 30-60分鐘
- **輸出**: 包含模型比較的完整分析

---

## 🚀 實際使用範例

### 範例1: 快速檢查模型性能
```bash
# 快速分析前3個模型
Rscript run_simple_analysis.R quick

# 查看結果
ls analysis_outputs/
```

### 範例2: 生成報告給主管
```r
# 載入系統
source("run_simple_analysis.R")

# 執行標準分析
result <- run_simple_analysis(
  output_dir = "monthly_report/",
  max_models = 8
)

# 報告位置: monthly_report/analysis_report.html
```

### 範例3: 深度模型比較研究
```r
# 載入完整版系統
source("run_complete_analysis.R")

# 執行深度分析
result <- deep_analysis()

# 檢查比較結果
print(result$comparison_results$summary)
```

### 範例4: 自定義分析流程
```r
# 載入簡化版
source("run_simple_analysis.R")

# 掃描模型
models <- scan_models_simple()

# 分析特定模型
for(i in 1:3) {
  importance_file <- models$importance_files[i]
  model_id <- gsub(".*/(.*?)_importance\\.csv$", "\\1", importance_file)
  
  result <- analyze_lgbm_importance_simple(
    importance_file, 
    model_id, 
    "custom_output/"
  )
}
```

---

## 🔧 故障排除

### 常見問題

#### 1. 套件載入失敗
```r
# 檢查套件
packageVersion("data.table")
packageVersion("ggplot2")

# 安裝缺失套件
install.packages(c("data.table", "ggplot2"))
```

#### 2. 找不到模型檔案
```bash
# 檢查模型目錄
ls model_outputs/models/ | head -10

# 確認檔案存在
ls model_outputs/models/*_complete.rds | head -5
ls model_outputs/models/*_importance.csv | head -5
```

#### 3. 記憶體不足
```r
# 使用更少模型
result <- run_simple_analysis(max_models = 2)

# 清理記憶體
gc()
```

#### 4. 圖表生成失敗
```r
# 檢查ggplot2版本
packageVersion("ggplot2")

# 更新套件
update.packages("ggplot2")
```

### 系統需求
- **R版本**: >= 4.0.0
- **必要套件**: data.table, ggplot2
- **可選套件**: lightgbm, torch (完整版)
- **記憶體**: >= 8GB (推薦16GB)
- **磁碟空間**: >= 2GB

---

## 📈 性能優化建議

### 1. 記憶體優化
```r
# 分批處理
for(batch in 1:3) {
  start_idx <- (batch-1)*3 + 1
  end_idx <- min(batch*3, total_models)
  
  # 處理這批模型
  # ...
  
  # 清理記憶體
  gc()
}
```

### 2. 並行處理 (進階)
```r
library(parallel)

# 設定核心數
options(mc.cores = detectCores() - 1)

# 並行分析 (需要額外配置)
```

### 3. 輸出管理
```r
# 按日期組織輸出
date_dir <- paste0("analysis_", Sys.Date())
result <- run_simple_analysis(output_dir = date_dir)

# 清理舊檔案
old_dirs <- list.dirs(pattern = "analysis_\\d{4}-\\d{2}-\\d{2}")
# 手動刪除不需要的目錄
```

---

## 📞 技術支援

### 版本資訊
- **簡化版**: v1.0 (穩定版)
- **完整版**: v3.0 (功能版)
- **更新日期**: 2024-12-19

### 功能對比

| 功能 | 簡化版 | 完整版 |
|------|--------|--------|
| LightGBM特徵重要度 | ✅ | ✅ |
| LSTM訓練歷史 | ✅ | ✅ |
| SHAP分析 | ❌ | ✅ |
| 模型比較 | ❌ | ✅ |
| 穩定性 | 高 | 中 |
| 執行速度 | 快 | 中 |
| 套件依賴 | 少 | 多 |

### 推薦使用策略
- **日常使用**: 簡化版
- **研究分析**: 完整版
- **報告生成**: 簡化版
- **模型比較**: 完整版

---

## 🎉 總結

### 立即開始使用

1. **快速體驗**:
   ```bash
   Rscript run_simple_analysis.R quick
   ```

2. **日常分析**:
   ```bash
   Rscript run_simple_analysis.R
   ```

3. **深度研究**:
   ```bash
   Rscript run_complete_analysis.R deep
   ```

### 系統優勢
✅ **自動化**: 一鍵執行完整分析流程  
✅ **穩定性**: 簡化版系統高度穩定  
✅ **靈活性**: 多種分析模式可選  
✅ **可視化**: 高質量圖表和報告  
✅ **擴展性**: 支援自定義分析流程  

�� **開始您的AQI模型分析之旅！** 