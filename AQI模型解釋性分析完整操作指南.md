# 🎯 AQI模型解釋性分析系統 - 完整操作指南

## 📋 目錄
1. [系統概述](#系統概述)
2. [快速開始](#快速開始)
3. [詳細功能說明](#詳細功能說明)
4. [分析模式選擇](#分析模式選擇)
5. [輸出結果說明](#輸出結果說明)
6. [進階使用技巧](#進階使用技巧)
7. [故障排除](#故障排除)

---

## 🎯 系統概述

### 系統功能
- **模型解釋分析**: LightGBM特徵重要度、SHAP值分析
- **LSTM模型分析**: 梯度重要度、時間序列貢獻分析
- **模型比較**: LSTM vs LightGBM 性能與特徵比較
- **報告生成**: HTML、Markdown、CSV多格式輸出
- **可視化**: 高質量PNG圖表，白色背景，適合報告

### 支援模型類型
- **LightGBM**: 123個模型，支援完整解釋性分析
- **LSTM**: 124個模型，支援架構和性能分析
- **總計**: 247個訓練完成的模型

---

## ⚡ 快速開始

### 1. 基本執行
```r
# 載入系統
source("run_complete_analysis.R")

# 執行完整分析（推薦）
result <- run_complete_analysis()
```

### 2. 命令行執行
```bash
# Windows PowerShell
Rscript run_complete_analysis.R

# 快速分析模式
Rscript run_complete_analysis.R quick

# 深度分析模式
Rscript run_complete_analysis.R deep
```

### 3. 查看結果
分析完成後，檢查 `analysis_outputs/` 目錄：
- `model_analysis_enhanced_report.html` - 主要報告
- `*.png` - 分析圖表
- `*.csv` - 數據結果

---

## 🔍 詳細功能說明

### 1. 模型掃描與註冊
```r
# 自動掃描所有模型
registry <- scan_and_register_models()

# 查看模型統計
print(registry)
```

**功能說明**:
- 自動掃描 `lgbm_models/` 和 `lstm_models/` 目錄
- 檢測模型檔案和重要度檔案
- 生成完整的模型註冊表

### 2. LightGBM模型分析

#### 特徵重要度分析
```r
# 分析單個模型
importance_result <- analyze_feature_importance(
  importance_file = "lgbm_models/lgbm_model_1_importance.csv",
  model_id = "lgbm_model_1",
  output_dir = "analysis_outputs/"
)
```

**輸出內容**:
- 特徵重要度排序圖
- Gain/Cover/Frequency 三種重要度指標
- 時間滯後特徵分析
- CSV數據檔案

#### SHAP值分析
```r
# SHAP解釋性分析
shap_result <- analyze_shap_values(
  model_file = "lgbm_models/lgbm_model_1.rds",
  model_id = "lgbm_model_1",
  output_dir = "analysis_outputs/"
)
```

**輸出內容**:
- SHAP重要度圖
- 特徵貢獻分析
- 交互作用檢測

### 3. LSTM模型分析

#### 模型架構分析
```r
# 分析LSTM模型
lstm_result <- analyze_lstm_model(
  model_file = "lstm_models/lstm_model_1.rds",
  model_id = "lstm_model_1",
  output_dir = "analysis_outputs/"
)
```

**輸出內容**:
- 模型架構圖
- 訓練歷史曲線
- 梯度重要度分析
- 性能指標統計

### 4. 模型比較分析

#### 執行比較分析
```r
# 完整模型比較
comparison_results <- analyze_model_comparison(
  registry = registry,
  output_dir = "analysis_outputs/"
)
```

**比較內容**:
- **性能比較**: 驗證損失 vs 預測準確度
- **特徵處理**: 手工工程 vs 自動學習
- **訓練效率**: 時間成本分析
- **解釋性**: SHAP vs 梯度分析
- **時間序列能力**: 滯後特徵 vs 原生序列

---

## 🎛️ 分析模式選擇

### 1. 快速分析模式 ⚡
```r
result <- quick_analysis()
```
- **適用場景**: 快速檢查、初步分析
- **分析數量**: 前3個模型
- **執行時間**: 2-5分鐘
- **輸出目錄**: `quick_analysis/`

### 2. 標準分析模式 📊
```r
result <- run_complete_analysis()
```
- **適用場景**: 日常分析、報告生成
- **分析數量**: 前10個模型
- **執行時間**: 10-20分鐘
- **輸出目錄**: `analysis_outputs/`
- **包含功能**: 模型解釋 + 比較分析

### 3. 深度分析模式 🔬
```r
result <- deep_analysis()
```
- **適用場景**: 完整研究、論文撰寫
- **分析數量**: 前20個模型
- **執行時間**: 30-60分鐘
- **輸出目錄**: `deep_analysis/`
- **包含功能**: 全部功能 + 詳細比較

### 4. 自定義分析模式 🛠️
```r
result <- run_complete_analysis(
  output_dir = "custom_analysis/",
  max_models = 15,
  include_comparison = TRUE
)
```

---

## 📊 輸出結果說明

### 1. HTML報告
- **基本報告**: `model_analysis_report.html`
- **增強報告**: `model_analysis_enhanced_report.html` (包含比較)

**報告內容**:
- 📊 模型統計概覽
- 🔍 LSTM vs LightGBM 比較
- 📋 完整模型註冊表
- 🎯 分析功能說明

### 2. 圖表檔案 (PNG, 300 DPI)

#### LightGBM分析圖表
- `feature_importance_[model_id].png` - 特徵重要度
- `shap_importance_[model_id].png` - SHAP重要度
- `feature_interaction_[model_id].png` - 特徵交互作用

#### LSTM分析圖表
- `lstm_architecture_[model_id].png` - 模型架構
- `lstm_training_history_[model_id].png` - 訓練歷史
- `lstm_gradient_importance_[model_id].png` - 梯度重要度

#### 比較分析圖表
- `lstm_performance_comparison.png` - LSTM性能比較
- `lstm_training_time.png` - 訓練時間比較
- `lgbm_feature_importance_summary.png` - LightGBM特徵總結

### 3. 數據檔案 (CSV)
- `model_comparison_summary.csv` - 比較總結
- `lstm_performance_details.csv` - LSTM性能詳情
- `lgbm_performance_details.csv` - LightGBM性能詳情
- `feature_importance_[model_id].csv` - 特徵重要度數據

### 4. Markdown報告
- `model_analysis_report.md` - 文字版報告

---

## 🚀 進階使用技巧

### 1. 批量分析特定模型
```r
# 只分析特定數據集的模型
registry <- scan_and_register_models()
specific_models <- registry[dataset_type == "normalized"]

# 自定義分析
for(i in 1:nrow(specific_models)) {
  model_info <- specific_models[i]
  if(model_info$model_type == "lgbm") {
    analyze_feature_importance(model_info$importance_file, model_info$id)
  }
}
```

### 2. 自定義輸出目錄結構
```r
# 按日期組織輸出
date_dir <- paste0("analysis_", Sys.Date())
result <- run_complete_analysis(output_dir = date_dir)

# 按模型類型分類
lgbm_result <- run_complete_analysis(output_dir = "lgbm_analysis/")
lstm_result <- run_complete_analysis(output_dir = "lstm_analysis/")
```

### 3. 整合到自動化流程
```r
# 定期分析腳本
run_daily_analysis <- function() {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_dir <- paste0("daily_analysis_", timestamp, "/")
  
  result <- run_complete_analysis(
    output_dir = output_dir,
    max_models = 5,
    include_comparison = TRUE
  )
  
  # 發送報告通知
  cat("📧 分析完成，報告位於:", output_dir, "\n")
  return(result)
}
```

### 4. 性能優化設定
```r
# 大量模型分析時的優化設定
library(parallel)

# 設定並行處理
options(mc.cores = detectCores() - 1)

# 記憶體優化
gc()  # 垃圾回收

# 執行分析
result <- deep_analysis()
```

---

## 🔧 故障排除

### 常見問題與解決方案

#### 1. 找不到模型檔案
**錯誤**: `未找到任何模型，請檢查模型目錄`

**解決方案**:
```r
# 檢查目錄結構
list.files("lgbm_models/", pattern = "*.rds")
list.files("lstm_models/", pattern = "*.rds")

# 確認檔案路徑
file.exists("lgbm_models/lgbm_model_1.rds")
```

#### 2. 記憶體不足
**錯誤**: `Error: cannot allocate vector of size`

**解決方案**:
```r
# 減少分析模型數量
result <- run_complete_analysis(max_models = 3)

# 清理記憶體
gc()
rm(list = ls())

# 使用快速模式
result <- quick_analysis()
```

#### 3. 套件載入失敗
**錯誤**: `there is no package called 'xxx'`

**解決方案**:
```r
# 安裝缺失套件
install.packages(c("data.table", "ggplot2", "lightgbm"))

# 檢查torch安裝
torch::torch_is_installed()

# 重新安裝torch (如需要)
torch::install_torch()
```

#### 4. 圖表生成失敗
**錯誤**: PNG背景問題或圖表空白

**解決方案**:
```r
# 檢查ggplot2版本
packageVersion("ggplot2")

# 更新到最新版本
update.packages("ggplot2")

# 手動設定圖表主題
theme_set(theme_classic())
```

#### 5. SHAP分析失敗
**錯誤**: SHAP計算錯誤

**解決方案**:
```r
# 檢查模型檔案完整性
model <- readRDS("lgbm_models/lgbm_model_1.rds")
str(model)

# 跳過SHAP分析
result <- run_complete_analysis(include_shap = FALSE)
```

### 系統需求檢查
```r
# 檢查R版本
R.version.string

# 檢查套件版本
packageVersion("data.table")
packageVersion("ggplot2")
packageVersion("lightgbm")
packageVersion("torch")

# 檢查可用記憶體
memory.limit()  # Windows
```

---

## 📞 技術支援

### 日誌檔案位置
- 分析日誌: 控制台輸出
- 錯誤日誌: R的警告和錯誤訊息
- 輸出檔案: 各分析目錄中的檔案

### 效能監控
```r
# 監控分析進度
system.time({
  result <- run_complete_analysis()
})

# 檢查輸出檔案大小
file.info(list.files("analysis_outputs/", full.names = TRUE))$size
```

### 版本資訊
- **系統版本**: v3.0
- **更新日期**: 2024-12-19
- **支援模型**: LightGBM + LSTM
- **R版本需求**: >= 4.0.0

---

## 🎉 總結

這個AQI模型解釋性分析系統提供了完整的機器學習模型分析解決方案，包括：

✅ **自動化流程**: 一鍵執行完整分析  
✅ **多模型支援**: LightGBM + LSTM  
✅ **豐富輸出**: HTML報告 + 高質量圖表  
✅ **靈活配置**: 多種分析模式  
✅ **比較分析**: 深入的模型對比  

立即開始使用：
```r
source("run_complete_analysis.R")
result <- run_complete_analysis()
```

🎯 **開始您的模型解釋之旅！** 