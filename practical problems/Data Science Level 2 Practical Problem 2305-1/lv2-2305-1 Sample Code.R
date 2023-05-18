library(data.table)
# Level 2 Practical Problem 2305-1
dt = fread('lv2-2305-1.csv')

# Q 1
dt4 = copy(dt)

# Step 1-1
library(caret)
sc = preProcess(dt4[,c('NUM_LOGIN','NUM_CONTENTS','NUM_ACTION','TIME_LOGIN')] ,method=c('center','scale'))
dt4_1 = predict(sc, dt4[,c('NUM_LOGIN','NUM_CONTENTS','NUM_ACTION','TIME_LOGIN')] ,method=c('center','scale'))

# Step 1-2
dt4_1[,INVOLVEMENT:= NUM_LOGIN+NUM_CONTENTS+NUM_ACTION+TIME_LOGIN]

# Step 1-3 
res = cor(dt4_1$INVOLVEMENT, dt4$SCORE)
print(paste('Q1 Result:', res))
print(paste('Q1 Answer:', round(res, digits=3)))

# Q 2
dt5 = copy(dt)

# Step 2-1
q1 = dt5[, quantile(SCORE, .25)]
q3 = dt5[, quantile(SCORE, .75)]
iqr = q3 - q1
dt5_1 = dt5[(SCORE >= q1 - 1.5*iqr) &  (SCORE <= q3 + 1.5*iqr)]

#Step 2-2
dt5_1$EMP_ID = substr(dt5_1$EMP_ID, 5,5)
dt5_1$EMP_ID = as.numeric(dt5_1$EMP_ID)
train = dt5_1[EMP_ID %%4 != 0, -c("EMP_ID")]
test = dt5_1[EMP_ID %%4 == 0, -c("EMP_ID")]

# Step 2-3
library(dplyr)
max_num = -999
max_var = 'aa'
f_list= c('NUM_LOGIN', 'NUM_CONTENTS', 'NUM_ACTION', 'TIME_LOGIN', 'TIME_TEST', 'NUM_REVISION')
for (f in f_list){
  variables_I_want <- c(f, "SCORE")
  tmp = select(train, all_of(variables_I_want))
  lr = lm(SCORE~., tmp)
  r_s = summary(lr)$adj.r.squared
  if(r_s > max_num){
    max_num = r_s
    max_var = f
  }
}
print(max_var)
print(max_num)

max_num = -999
max_var = 'aa'
f_list= c('NUM_LOGIN', 'NUM_CONTENTS', 'NUM_ACTION', 'TIME_TEST', 'NUM_REVISION')
for (f in f_list){
  variables_I_want <- c('TIME_LOGIN', f, "SCORE")
  tmp = select(train, all_of(variables_I_want))
  lr = lm(SCORE~., tmp)
  r_s = summary(lr)$adj.r.squared
  if(r_s > max_num){
    max_num = r_s
    max_var = f
  }
}
print(max_var)
print(max_num)

max_num = -999
max_var = 'aa'
f_list= c('NUM_LOGIN', 'NUM_ACTION', 'TIME_TEST', 'NUM_REVISION')
for (f in f_list){
  variables_I_want <- c('TIME_LOGIN','NUM_CONTENTS', f, "SCORE")
  tmp = select(train, all_of(variables_I_want))
  lr = lm(SCORE~., tmp)
  r_s = summary(lr)$adj.r.squared
  if(r_s > max_num){
    max_num = r_s
    max_var = f
  }
}
print(max_var)
print(max_num)

#Step 2-4


train = dt5_1[EMP_ID %%4 != 0, -c("EMP_ID")]
test = dt5_1[EMP_ID %%4 == 0, -c("EMP_ID")]

# Step 2-4
md = lm(SCORE~TIME_LOGIN + NUM_CONTENTS + NUM_REVISION, train )
pred_val = predict(md, test)

res = sqrt(mean((test$SCORE - pred_val)^2))

print(paste('Q2 Result:', res))
print(paste('Q2 Answer:', round(res, digits=2)))

# Question 3
dt6 = copy(dt)

# Step 3-1
a = dt6[SCORE <= 5]
b = dt6[(SCORE > 5) & (SCORE <=7) ]
c = dt6[(SCORE > 7) & (SCORE <=10) ]

# Step 3-2
train_a = a[,c('NUM_LOGIN','NUM_CONTENTS','NUM_ACTION','TIME_LOGIN','TIME_TEST','NUM_REVISION','SCORE')] 
train_b = b[,c('NUM_LOGIN','NUM_CONTENTS','NUM_ACTION','TIME_LOGIN','TIME_TEST','NUM_REVISION','SCORE')]
train_c = c[,c('NUM_LOGIN','NUM_CONTENTS','NUM_ACTION','TIME_LOGIN','TIME_TEST','NUM_REVISION','SCORE')]


# Step 3-3
library(randomForest)

set.seed(1234)
rf_a = randomForest(SCORE~., train_a, importance=TRUE, ntree=10, nodesize = 10)
rf_a_imp = as.data.frame(importance(rf_a,type=2))
rf_a_imp$feat_imp = rf_a_imp$IncNodePurity / sum(rf_a_imp$IncNodePurity)

set.seed(1234)
rf_b = randomForest(SCORE~., train_b, importance=TRUE, ntree=10, nodesize=10)
rf_b_imp = as.data.frame(importance(rf_b,type=2))
rf_b_imp$feat_imp = rf_b_imp$IncNodePurity / sum(rf_b_imp$IncNodePurity)

set.seed(1234)
rf_c = randomForest(SCORE~., train_c, importance=TRUE, ntree=10, nodesize=10)
importance(rf_c)
rf_c_imp = as.data.frame(importance(rf_c,type=2))
rf_c_imp$feat_imp = rf_c_imp$IncNodePurity / sum(rf_c_imp$IncNodePurity)

rf_a_imp
rf_b_imp
rf_c_imp

res = max(rf_a_imp$feat_imp) + max(rf_b_imp$feat_imp) + max(rf_c_imp$feat_imp)

print(paste('Q3 Result:', res))
print(paste('Q3 Answer:', round(res, digits=2)))

