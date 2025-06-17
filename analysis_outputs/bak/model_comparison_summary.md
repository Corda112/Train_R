# Model Comparison Analysis Report

## 🏆 整體最佳模型 (Top 10)

|model_id                                                      |model_type |dataset_type | test_rmse| model_size_mb|
|:-------------------------------------------------------------|:----------|:------------|---------:|-------------:|
|lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |lgbm       |separate     |    0.0538|         32.88|
|lgbm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |lgbm       |separate     |    0.0604|         35.49|
|lgbm_separate_norm_Nomorlization_安南_467420_combined_windows |lgbm       |separate     |    0.0606|         34.28|
|lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |lstm       |separate     |    0.0611|          0.07|
|lgbm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |lgbm       |separate     |    0.0613|         32.39|
|lstm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |lstm       |separate     |    0.0614|          0.07|
|lgbm_separate_norm_Nomorlization_南投_C0I460_combined_windows |lgbm       |separate     |    0.0636|         35.28|
|lstm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |lstm       |separate     |    0.0653|          0.07|
|lstm_separate_norm_Nomorlization_南投_C0I460_combined_windows |lstm       |separate     |    0.0658|          0.07|
|lgbm_separate_norm_Nomorlization_屏東_C0R570_combined_windows |lgbm       |separate     |    0.0658|         34.93|

## 📊 模型類型 vs 資料類型比較

|dataset_type |model_type | best_rmse| worst_rmse| avg_rmse| model_count|best_model                                                    | avg_size_mb|
|:------------|:----------|---------:|----------:|--------:|-----------:|:-------------------------------------------------------------|-----------:|
|separate     |lgbm       |    0.0538|     3.7511|   1.7879|         121|lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |     35.1252|
|separate     |lstm       |    0.0611|     3.8748|   1.7912|         122|lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |      0.0701|
|combine      |lgbm       |    0.0759|     2.8958|   1.4125|           6|lgbm_combine_norm_chunk_02                                    |    126.3433|
|combine      |lstm       |    0.1031|    16.4426|   7.3040|           6|lstm_combine_norm_chunk_02                                    |      0.0700|

## 🎯 各資料類型最佳表現

|dataset_type | best_rmse| model_count|best_model_type |best_model_id                                                 |
|:------------|---------:|-----------:|:---------------|:-------------------------------------------------------------|
|separate     |    0.0538|         243|lgbm            |lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |
|combine      |    0.0759|          12|lgbm            |lgbm_combine_norm_chunk_02                                    |

## 📈 關鍵洞察

- **最佳整體模型**: lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows (RMSE: 0.0538)
- **模型類型統計**:
  - LightGBM 模型數量: 127
  - LSTM 模型數量: 128
- **資料類型統計**:
  - Combine 資料類型最佳 RMSE: 0.0759
  - Separate 資料類型最佳 RMSE: 0.0538

**分析完成時間:** 2025-06-16 20:59:17.604681
