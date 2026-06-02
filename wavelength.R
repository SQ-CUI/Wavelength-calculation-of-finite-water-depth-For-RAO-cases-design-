library(readxl)
library(writexl)

g <- 9.81       # 重力加速度 (m/s²)
d <- 0.75       # 固定水深 (m)，你指定的数值

file_path <- "wl.xls"

wave_data <- read_excel(file_path, col_names = FALSE)

colnames(wave_data) <- c("波高_H_m", "周期_T_s")

calc_wave_length <- function(T, g = 9.81, d = 0.75) {
  # 过滤无效周期（T<=0时返回NA）
  if(T <= 0) return(NA)
  
  omega <- 2 * pi / T        # 角频率
  # 目标方程 f(k)=0
  f <- function(k) { omega^2 - g * k * tanh(k * d) }
  # 目标方程导数
  df <- function(k) { -g * tanh(k*d) - g * k * d * (1 - tanh(k*d)^2) }
  
  # 牛顿迭代求解
  k <- 1.0                   # 迭代初始值
  max_iter <- 50             # 最大迭代次数
  tol <- 1e-8                # 收敛精度
  for (i in 1:max_iter) {
    k_new <- k - f(k)/df(k)
    if(abs(k_new - k) < tol) break
    k <- k_new
  }
  
  # 计算波长
  lambda <- 2 * pi / k
  return(round(lambda, 4))  # 保留4位小数，和之前结果一致
}


wave_data$波长_lambda_m <- sapply(wave_data$周期_T_s, calc_wave_length, g = g, d = d)
wave_data$波高比波长_H_div_lambda <- round(wave_data$波高_H_m / wave_data$波长_lambda_m, 4)

print("===== 波浪波长计算结果 =====")
print(wave_data)

# 可选：将结果写回新的XLSX文件，保存到和原文件同目录
output_path <- paste0(dirname(file_path), "/波浪波长计算结果.xlsx")
write_xlsx(wave_data, output_path)
print(paste0("结果已保存到：", output_path))