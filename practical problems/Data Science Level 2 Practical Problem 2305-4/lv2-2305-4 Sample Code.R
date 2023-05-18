################################################################################
##### Data Science Level 2 Practical Problem 2305-4
################################################################################

library(dplyr)
library(data.table)


sensor = fread('lv2-2305-4.csv')
dim(sensor)
head(sensor)

##### Question 1 ##############################################################
# 1-1
dt4 = copy(sensor)
sk_fn = function(x) { mean((x-mean(x))^3)/(mean((x-mean(x))^2))^(3/2) }
sks = dt4[, -c('SEQ','LABEL')][, sapply(.SD, sk_fn)]
sks

# 1-2
abs(max(sks))
round(abs(max(sks)), 4)

# Answer 1
ans1 = round(abs(max(sks)), 2)
ans1

##### Question 2 ##############################################################
# 2-1
dt5 = copy(sensor)

library(caret)
varidx = which(!colnames(dt5)%in%c('SEQ','LABEL'))
scale5 = preProcess(dt5[, ..varidx], method=c('center','scale'))
scaled5 = predict(scale5, dt5)
colnames(scaled5)[varidx] = paste0(colnames(scaled5)[varidx], '_S')
colnames(scaled5)

# 2-2
train5 = scaled5[SEQ%%4!=0]
test5  = scaled5[SEQ%%4==0]
dim(train5)
dim(test5)

# 2-3, 2-4
library(FNN)
acc = c()
for(i in 2:4){
  pred = knn(train5[,..varidx], test5[,..varidx], cl=train5$LABEL, k=i)
  acc[i] = mean(pred==test5$LABEL)
}
acc
max(acc, na.rm=T) + which.max(acc)

# Answer 2
ans2 = round(max(acc, na.rm=T) + which.max(acc), 3)
ans2

##### Question 3 ##############################################################
# 3-1
dt6 = copy(sensor)
scol = colnames(dt6[,-c('SEQ','LABEL')])
remover = function(x) {
  lb = quantile(x, .5) - 2.3*(quantile(x, .75) - quantile(x, .25))
  ub = quantile(x, .5) + 2.3*(quantile(x, .75) - quantile(x, .25))
  ifelse(x<lb|x>ub, NA, x)
}
dt6 = cbind(dt6[,-..scol], dt6[,..scol][, sapply(.SD, remover)])
dt6 = na.omit(dt6)
dim(sensor)
dim(dt6)

# 3-2
train6 = dt6[SEQ%%3!=0,-c('SEQ')]
test6 = dt6[SEQ%%3==0,-c('SEQ')]

# 3-3
library(rpart)
train6$LABEL = as.factor(train6$LABEL)
set.seed(1234)
mdl6 = rpart(LABEL~., train6, control=rpart.control(minsplit=2,cp=0,maxdepth=5))

# 3-4
pred6 = predict(mdl6, test6, type='class')
C = mean(pred6==test6$LABEL)*100

# 3-5
sort(mdl6$variable.importance,decreasing = T)
var6 = names(sort(mdl6$variable.importance, decreasing = T)[1:6])
var6
fm6 = paste0('LABEL~',paste(var6, collapse='+'))

set.seed(1234)
mdl66 = rpart(fm6, train6, control=rpart.control(minsplit=2,cp=0,maxdepth=5))
pred66 = predict(mdl66, test6, type='class')
D = mean(pred66==test6$LABEL)*100

C; D; abs(C-D)

# Answer 3
ans3 = round(abs(C-D), 3)
ans3



