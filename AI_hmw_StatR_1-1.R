### 配合課程影片 hmwu_StatR-01.1
### Exercise 1-1
# 請讀取score2015.txt ，文件編碼為big5，請以utf-8讀取：
# Hint: ".../R_part1/R_data/score2015.txt"

score2015.orig <- read.table(file = "../../StatR_part1/R_data/score2015.txt", header = T, sep = "\t", fileEncoding = "big5", encoding = "UTF-8")
#註：在windows系統上若讀入的結果為亂碼則嘗試不指定編碼方式即可：read.table(file = "../../StatR_part1/R_data/score2015.txt", header = T, sep = "\t")
#此不一致是因windows系統下預設編碼與mac不同導致

# 請檢查score2015.orig的維度：80 12
dim(score2015.orig)

# 請查看score2015.orig的前10列（rows）：
head(score2015.orig, n = 10)

# 請查看score2015.orig中，第三行至最後一行（columns）的敘述統計：
summary(score2015.orig[, 3:ncol(score2015.orig)])

# 請查看score2015.orig中，每一種出席次數的次數列表：
table(score2015.orig["出席次數"])


#==================================================================
score2015 <- score2015.orig

# 請找出score2015中NA所在位置，並以0取代：
score2015[is.na(score2015)] <- 0

# 請計算score2015中這7行各自的行平均（column mean）：小考1 ～ 4，助教，期中考，期末考
colMeans(score2015[, 5:11])

# 請計算score2015中，第5行至第11行的每一列平均（row mean）：
apply(score2015[, 5:11], 1, mean)

# 請計算score2015中，第5行至第11行的每一行標準差（column standard deviation）：
apply(score2015[, 5:11], 2, sd)


x <- score2015[, "小考1"]

min(x)
max(x)
sum(x)

# 請計算截去x首尾各自10％資料後的截尾平均數：
mean(x)
mean(x, trim = 0.1)

median(x)

Mode <- function(x, na.rm = FALSE) {
  if(na.rm) x = x[!is.na(x)]  
  ux <- unique(x)
  ifelse(length(x) == length(ux), 
         "no mode",   
         ux[which.max(tabulate(match(x, ux)))])
}

Mode(x)

# 請計算x的十分位數：
quantile(x)
quantile(x, prob= seq(0, 1, 0.1))

range(x)
sd(x)
var(x)
