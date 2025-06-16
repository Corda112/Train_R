# 修復模型比較分析中的問題
# 讀取原始檔案
content <- readLines("model_src/explainer_advanced.R", encoding = "UTF-8")

# 修復model_file問題
for(i in 1:length(content)) {
  if(grepl("model_file <- model_info\\$model_file", content[i])) {
    content[i] <- gsub(
      "model_file <- model_info\\$model_file",
      "model_file <- model_info$complete_file  # 修復：使用正確的欄位名稱",
      content[i]
    )
    cat("修復第", i, "行: model_file問題\n")
  }
  
  # 修復檔案存在檢查
  if(grepl("if\\(file\\.exists\\(model_file\\)\\)", content[i])) {
    content[i] <- gsub(
      "if\\(file\\.exists\\(model_file\\)\\)",
      "if(!is.na(model_file) && file.exists(model_file))",
      content[i]
    )
    cat("修復第", i, "行: 檔案存在檢查\n")
  }
  
  # 修復importance_file檢查
  if(grepl("file\\.exists\\(model_info\\$importance_file\\)", content[i])) {
    content[i] <- gsub(
      "file\\.exists\\(model_info\\$importance_file\\)",
      "!is.na(model_info$importance_file) && file.exists(model_info$importance_file)",
      content[i]
    )
    cat("修復第", i, "行: importance_file檢查\n")
  }
}

# 寫回檔案
writeLines(content, "model_src/explainer_advanced.R", useBytes = TRUE)
cat("✅ 模型比較分析錯誤修復完成\n") 