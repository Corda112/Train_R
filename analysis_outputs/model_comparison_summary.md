# Model Comparison Analysis Report

## 🏆 整體最佳模型 (Top 10)

|model_id                                                      |model_type |dataset_type | test_rmse| model_size_mb|
|:-------------------------------------------------------------|:----------|:------------|---------:|-------------:|
|lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |lstm       |separate     |    0.0611|          0.07|
|lstm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |lstm       |separate     |    0.0614|          0.07|
|lstm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |lstm       |separate     |    0.0653|          0.07|
|lstm_separate_norm_Nomorlization_南投_C0I460_combined_windows |lstm       |separate     |    0.0658|          0.07|
|lstm_separate_norm_Nomorlization_仁武_C0V680_combined_windows |lstm       |separate     |    0.0723|          0.07|
|lstm_separate_norm_Nomorlization_美濃_72V140_combined_windows |lstm       |separate     |    0.0731|          0.07|
|lstm_separate_norm_Nomorlization_朴子_C0M650_combined_windows |lstm       |separate     |    0.0734|          0.07|
|lstm_separate_norm_Nomorlization_安南_467420_combined_windows |lstm       |separate     |    0.0740|          0.07|
|lstm_separate_norm_Nomorlization_屏東_C0R570_combined_windows |lstm       |separate     |    0.0745|          0.07|
|lstm_separate_norm_Nomorlization_大里_C0F9N0_combined_windows |lstm       |separate     |    0.0767|          0.07|

## 📊 模型類型 vs 資料類型比較

|dataset_type |model_type | best_rmse| worst_rmse| avg_rmse| model_count|best_model                                                    | avg_size_mb|
|:------------|:----------|---------:|----------:|--------:|-----------:|:-------------------------------------------------------------|-----------:|
|separate     |lstm       |    0.0611|     0.1168|   0.0851|          27|lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |        0.07|
|combine      |lstm       |    0.1031|     0.1203|   0.1120|           3|lstm_combine_norm_chunk_02                                    |        0.07|

## 🎯 各資料類型最佳表現

|dataset_type | best_rmse| model_count|best_model_type |best_model_id                                                 |
|:------------|---------:|-----------:|:---------------|:-------------------------------------------------------------|
|separate     |    0.0611|          27|lstm            |lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |
|combine      |    0.1031|           3|lstm            |lstm_combine_norm_chunk_02                                    |

## 📈 關鍵洞察

- **最佳整體模型**: lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows (RMSE: 0.0611)
- **模型類型統計**:
  - LightGBM 模型數量: 0
  - LSTM 模型數量: 30
- **資料類型統計**:
  - Combine 資料類型最佳 RMSE: 0.1031
  - Separate 資料類型最佳 RMSE: 0.0611

**分析完成時間:** 2025-06-17 01:09:23.267673
