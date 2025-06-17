# 全量評估摘要報告

**評估時間**: 2025-06-17 00:40:10.563951
**總模型數**: 255 個
**總測站數**: 128 個
**LSTM 模型數**: 128 個
**LGBM 模型數**: 127 個

## 🏆 整體表現統計

- **全域最佳模型**: lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows (RMSE: 0.0538)
- **平均 RMSE**: 1.9105
- **RMSE 標準差**: 1.9563
- **最佳/最差 RMSE 比**: 305.44x

## 📊 模型類型 × 資料類型交叉分析

|model_type |dataset_type | model_count| best_rmse| worst_rmse| avg_rmse| rmse_std|best_station                                    |worst_station                |
|:----------|:------------|-----------:|---------:|----------:|--------:|--------:|:-----------------------------------------------|:----------------------------|
|lgbm       |combine      |           6|    0.0759|     2.8958|   1.4125|   1.4514|norm_chunk_02                                   |chunk_03                     |
|lgbm       |separate     |         121|    0.0538|     3.7511|   1.7879|   1.3577|norm_Nomorlization_竹山_U2HA50_combined_windows |崙背_C0K250_combined_windows |
|lstm       |combine      |           6|    0.1031|    16.4426|   7.3040|   8.1504|norm_chunk_02                                   |chunk_03                     |
|lstm       |separate     |         122|    0.0611|     3.8748|   1.7912|   1.3746|norm_Nomorlization_竹山_U2HA50_combined_windows |崙背_C0K250_combined_windows |

## 🎯 各測站最佳模型概覽

|station                                         | best_rmse|best_model_type |best_model_id                                                 |best_dataset_type | lgbm_count| lstm_count| total_models| avg_rmse| rmse_std|
|:-----------------------------------------------|---------:|:---------------|:-------------------------------------------------------------|:-----------------|----------:|----------:|------------:|--------:|--------:|
|norm_Nomorlization_竹山_U2HA50_combined_windows |    0.0538|lgbm            |lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |separate          |          1|          1|            2|   0.0574|   0.0051|
|norm_Nomorlization_斗六_C0K400_combined_windows |    0.0604|lgbm            |lgbm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |separate          |          1|          1|            2|   0.0609|   0.0007|
|norm_Nomorlization_安南_467420_combined_windows |    0.0606|lgbm            |lgbm_separate_norm_Nomorlization_安南_467420_combined_windows |separate          |          1|          1|            2|   0.0673|   0.0095|
|norm_Nomorlization_埔里_C0H890_combined_windows |    0.0613|lgbm            |lgbm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |separate          |          1|          1|            2|   0.0633|   0.0029|
|norm_Nomorlization_南投_C0I460_combined_windows |    0.0636|lgbm            |lgbm_separate_norm_Nomorlization_南投_C0I460_combined_windows |separate          |          1|          1|            2|   0.0647|   0.0016|
|norm_Nomorlization_屏東_C0R570_combined_windows |    0.0658|lgbm            |lgbm_separate_norm_Nomorlization_屏東_C0R570_combined_windows |separate          |          1|          1|            2|   0.0702|   0.0062|
|norm_Nomorlization_左營_C0V810_combined_windows |    0.0662|lgbm            |lgbm_separate_norm_Nomorlization_左營_C0V810_combined_windows |separate          |          1|          1|            2|   0.0720|   0.0082|
|norm_Nomorlization_仁武_C0V680_combined_windows |    0.0665|lgbm            |lgbm_separate_norm_Nomorlization_仁武_C0V680_combined_windows |separate          |          1|          1|            2|   0.0694|   0.0041|
|norm_Nomorlization_美濃_72V140_combined_windows |    0.0686|lgbm            |lgbm_separate_norm_Nomorlization_美濃_72V140_combined_windows |separate          |          1|          1|            2|   0.0708|   0.0032|
|norm_Nomorlization_前鎮_C0V490_combined_windows |    0.0703|lgbm            |lgbm_separate_norm_Nomorlization_前鎮_C0V490_combined_windows |separate          |          1|          1|            2|   0.0766|   0.0089|

## 🔥 LSTM 模型測站排名 (Top 10)

|station                                         | lstm_best_rmse| lstm_model_count|lstm_best_model                                               |lstm_dataset_type | lstm_rank|
|:-----------------------------------------------|--------------:|----------------:|:-------------------------------------------------------------|:-----------------|---------:|
|norm_Nomorlization_竹山_U2HA50_combined_windows |         0.0611|                1|lstm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |separate          |         1|
|norm_Nomorlization_斗六_C0K400_combined_windows |         0.0614|                1|lstm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |separate          |         2|
|norm_Nomorlization_埔里_C0H890_combined_windows |         0.0653|                1|lstm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |separate          |         3|
|norm_Nomorlization_南投_C0I460_combined_windows |         0.0658|                1|lstm_separate_norm_Nomorlization_南投_C0I460_combined_windows |separate          |         4|
|norm_Nomorlization_仁武_C0V680_combined_windows |         0.0723|                1|lstm_separate_norm_Nomorlization_仁武_C0V680_combined_windows |separate          |         5|
|norm_Nomorlization_美濃_72V140_combined_windows |         0.0731|                1|lstm_separate_norm_Nomorlization_美濃_72V140_combined_windows |separate          |         6|
|norm_Nomorlization_朴子_C0M650_combined_windows |         0.0734|                1|lstm_separate_norm_Nomorlization_朴子_C0M650_combined_windows |separate          |         7|
|norm_Nomorlization_安南_467420_combined_windows |         0.0740|                1|lstm_separate_norm_Nomorlization_安南_467420_combined_windows |separate          |         8|
|norm_Nomorlization_屏東_C0R570_combined_windows |         0.0745|                1|lstm_separate_norm_Nomorlization_屏東_C0R570_combined_windows |separate          |         9|
|norm_Nomorlization_大里_C0F9N0_combined_windows |         0.0767|                1|lstm_separate_norm_Nomorlization_大里_C0F9N0_combined_windows |separate          |        10|

## 🌳 LGBM 模型測站排名 (Top 10)

|station                                         | lgbm_best_rmse| lgbm_model_count|lgbm_best_model                                               |lgbm_dataset_type | lgbm_rank|
|:-----------------------------------------------|--------------:|----------------:|:-------------------------------------------------------------|:-----------------|---------:|
|norm_Nomorlization_竹山_U2HA50_combined_windows |         0.0538|                1|lgbm_separate_norm_Nomorlization_竹山_U2HA50_combined_windows |separate          |         1|
|norm_Nomorlization_斗六_C0K400_combined_windows |         0.0604|                1|lgbm_separate_norm_Nomorlization_斗六_C0K400_combined_windows |separate          |         2|
|norm_Nomorlization_安南_467420_combined_windows |         0.0606|                1|lgbm_separate_norm_Nomorlization_安南_467420_combined_windows |separate          |         3|
|norm_Nomorlization_埔里_C0H890_combined_windows |         0.0613|                1|lgbm_separate_norm_Nomorlization_埔里_C0H890_combined_windows |separate          |         4|
|norm_Nomorlization_南投_C0I460_combined_windows |         0.0636|                1|lgbm_separate_norm_Nomorlization_南投_C0I460_combined_windows |separate          |         5|
|norm_Nomorlization_屏東_C0R570_combined_windows |         0.0658|                1|lgbm_separate_norm_Nomorlization_屏東_C0R570_combined_windows |separate          |         6|
|norm_Nomorlization_左營_C0V810_combined_windows |         0.0662|                1|lgbm_separate_norm_Nomorlization_左營_C0V810_combined_windows |separate          |         7|
|norm_Nomorlization_仁武_C0V680_combined_windows |         0.0665|                1|lgbm_separate_norm_Nomorlization_仁武_C0V680_combined_windows |separate          |         8|
|norm_Nomorlization_美濃_72V140_combined_windows |         0.0686|                1|lgbm_separate_norm_Nomorlization_美濃_72V140_combined_windows |separate          |         9|
|norm_Nomorlization_前鎮_C0V490_combined_windows |         0.0703|                1|lgbm_separate_norm_Nomorlization_前鎮_C0V490_combined_windows |separate          |        10|

## 📈 全量評估關鍵洞察

1. **模型分布**: LSTM (128) vs LGBM (127)
2. **資料類型優勢**: Separate 資料表現更佳
3. **測站表現差異**: 69.68x (最佳/最差測站 RMSE 比)

---

**檔案輸出**:
- `full_evaluation_station_summary.csv` - 各測站最佳模型統計
- `full_evaluation_cross_analysis.csv` - 模型類型交叉分析
- `full_evaluation_lstm_ranking.csv` - LSTM 測站排名
- `full_evaluation_lgbm_ranking.csv` - LGBM 測站排名
- `full_evaluation_combined_ranking.csv` - 綜合測站排名
