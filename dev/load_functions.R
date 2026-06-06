# 自动加载包目录下 R/ 文件夹里所有 .R 和 .r 文件
r_files <- list.files(path = "R", pattern = "\\.(R|r)$", full.names = TRUE)
l <- lapply(r_files, source)

# 提示加载完成
cat("✅ 成功加载所有 R 脚本文件：", length(r_files), "个\n")
