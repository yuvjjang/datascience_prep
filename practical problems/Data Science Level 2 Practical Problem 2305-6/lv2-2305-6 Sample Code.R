library(data.table)

dt = fread('lv2-2305-6.csv')

# Question 1
dt1 = copy(dt)

# Step 1-1
library(dplyr)
library(caret)

sc <- preProcess(dt1[,c('SURVEY_RES_01')], method = c("range"), rangeBounds = c(2.2,5.7))
res = predict(sc, dt1[,c('SURVEY_RES_01')])
A  = sd(res$SURVEY_RES_01)

#Step 1-2
m <- 7.9
std <- 1.399
dt1$SURVEY_RES_02 <- (dt1$SURVEY_RES_02*std + m)

B <- median(dt1$SURVEY_RES_02)

ans21 <- A+B

print(paste('Q1 Result: ', ans21))
print(paste('Q1 Answer: ', round(ans21, digits=2)))

# Question 2
library(car)
library(dplyr)
dt2 = copy(dt)

# Step 2-1
dt2_1 <- dt2[, 2:(ncol(dt2) - 1)]

var_list = colnames(dt2_1)
vif_list = data.frame(features=var_list)
vif_list$values = 0  
for(i in 1:length(colnames(dt2_1))){
  
  R_2 =  summary(lm(as.formula(paste(var_list[i],'~1+.')), dt2_1))$r.squared
  
  vif_list[i,2] = 1 / (1 - R_2)

}
if(max(vif_list$values)>= 5){
  col_to_remove = vif_list[vif_list$values == max(vif_list$values), 'features'][1]
  dt2_1 = select(dt2_1, -c(col_to_remove))
}

# Step 2-2
dt2_1$CUST_ID = dt2$CUST_ID
dt2_1$SATISFACTION = dt2$SATISFACTION
train = dt2_1[dt2_1$CUST_ID <= 1000]
test = dt2_1[dt2_1$CUST_ID > 1000,]
train = train[,-c('CUST_ID')]
test = test[,-c('CUST_ID')]
set.seed(1234)
lr = glm(SATISFACTION~., family = binomial, train)

test$prd = predict(lr, test, type = 'response')
test$prd = (test$prd >= 0.5) * 1

C = sum(test$prd == test$SATISFACTION)/nrow(test)

# Step 2-3
res <- c()
tmp <- 0
for (i in 1:6) {
  lb <- tmp
  ub <- tmp + 200
  test <- dt2_1[(dt2_1$CUST_ID > lb) & (dt2_1$CUST_ID <= ub),]
  train <- dt2_1[((dt2_1$CUST_ID > 0) & (dt2_1$CUST_ID <= lb)) | ((dt2_1$CUST_ID > ub) & (dt2_1$CUST_ID <= 1200)),]
  train = train[,-c('CUST_ID')]
  test = test[,-c('CUST_ID')]
  set.seed(1234)
  lr = glm(SATISFACTION~., family = binomial, train)
  test$prd = predict(lr, test, type = 'response')
  test$prd = (test$prd >= 0.5) * 1
  
  
  res[i] <- sum(test$prd == test$SATISFACTION)/nrow(test)
  tmp <- tmp + 200
  
}
D = sum(res)/6

ans22 = abs(C - D)
print(paste('Q2 Result: ', ans22))
print(paste('Q2 Answer: ', format(ans22,digits = 2)))

#Question 3
dt3 = copy(dt)

# Step 3-1
sc <- preProcess(dt3[, 2:ncol(dt3)], method = "range")

dt3[, 2:ncol(dt3)] <- predict(sc, dt3[, 2:ncol(dt3)])


# Step 3-3
k_list <- c(3, 5, 7)
dt3_1 <- dt3[, -1]

dt3_1 = dt3_1[, -c('SATISFACTION')]

max_num <- -999
res_list <- list()
i <- 1
library(cluster)

#Step 3-4
for (k in k_list) {
  set.seed(1234)
  md <- kmeans(dt3_1, centers = k, nstart = 50, iter.max = 300)
  res <- md$cluster
  res_list[[i]] <- res
  score <- mean(silhouette(res, dist(dt3_1))[,3])
  if (score > max_num) {
    max_num <- score
    max_k <- k
    max_i <- i
  }
  i <- i + 1
}

tot <- 0
for (num in 1:max_k) {
  num_c <- sum(res_list[[max_i]] == num)
  if (tot < num_c) {
    tot <- num_c
  }
}

ans23 <- tot

print(paste('Q3 Answer:', ans23))


