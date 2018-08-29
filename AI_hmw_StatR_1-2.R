### 配合課程影片 hmwu_StatR-01.2
### Exercise 1-2
# 23/24
library("corpcor")
library("MASS")
n <- 6 
p <- 10 
set.seed(123456)

# 請生成 p*p 共變異數矩陣 sigma，每個元素值來自於常態分佈的隨機抽樣：
sigma <- matrix(rnorm(p * p), ncol = p)
sigma <- crossprod(sigma) + diag(rep(0.1, p)) 

# 請模擬生成維度 n 的多元常態分佈隨機樣本資料 x，共變異數矩陣令為sigma：
x <- mvrnorm(n, mu = rep(0, p), Sigma = sigma)

# 請估計隨機樣本資料 x 的共變異數矩陣 s1：
s1 <- cov(x)
s2 <- cov.shrink(x)

par(mfrow = c(1, 3))
image(t(sigma)[, p:1], main = "true cov", xaxt = "n", yaxt = "n")
image(t(s1)[, p:1], main = "empirical cov", xaxt = "n", yaxt = "n")
image(t(s2)[, p:1], main = "shrinkage cov", xaxt = "n", yaxt = "n")

sum((s1 - sigma) ^ 2)
sum((s2 - sigma) ^ 2)


#==================================================================
# 24/24
is.positive.definite(sigma)
is.positive.definite(s1)
is.positive.definite(s2)

# 請把 data.frame rc 的列名（rownames）改為true, empirical, shrinkage：
rc <- rbind(
  data.frame(rank.condition(sigma)),
  data.frame(rank.condition(s1)),
  data.frame(rank.condition(s2)))

rownames(rc) <- c("true", "empirical", "shrinkage")
rc

# 請查看共變異數矩陣 sigma, s1, s2 的特徵值e0, e1, e2（eigenvalues）整理成 data.frame，
# 並將此data.frame畫出來, y軸命名為eigenvalues：
e0 <- eigen(sigma, symmetric = TRUE)$values
e1 <- eigen(s1, symmetric = TRUE)$values
e2 <- eigen(s2, symmetric = TRUE)$values

matplot(data.frame(e0, e1, e2), type = "l", ylab="eigenvalues", lwd=2)
legend("top", legend=c("true", "empirical", "shrinkage"), lwd=2, lty=1:3, col=1:3)

