library(readxl)
library(writexl)

# 参数设置
g <- 9.81
d <- 0.75   # 水深固定0.75m

file_path <- "hl.xls"

# 读入：第一列H/λ，第二列T
dat <- read_excel(file_path,col_names=F)
colnames(dat) <- c("H_over_lambda","T_s")

# 牛顿迭代求波长函数
cal_lambda <- function(T,g=9.81,d=0.75){
  if(T<=0) return(NA)
  omega <- 2*pi/T
  f <- function(k) omega^2 - g*k*tanh(k*d)
  df<-function(k) -g*tanh(k*d)-g*k*d*(1-tanh(k*d)^2)
  k=1
  for(i in 1:50){
    kk <- k - f(k)/df(k)
    if(abs(kk-k)<1e-8) break
    k <- kk
  }
  lam <- 2*pi/k
  return(round(lam,4))
}

# 批量算波长
dat$lambda_m <- sapply(dat$T_s,cal_lambda)

# 由 H = (H/λ)*λ 反算波高
dat$H_m <- round(dat$H_over_lambda * dat$lambda_m,4)

# 输出结果
print(dat)

# 保存结果excel
outname <- "waveheight.xlsx"
write_xlsx(dat,outname)
cat("结果已保存：",outname)