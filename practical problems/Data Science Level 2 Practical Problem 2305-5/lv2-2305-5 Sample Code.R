################################################################################
##### Data Science Level 2 Practical Problem 2305-5
################################################################################

##### Settings #################################################################
library(data.table)

purchase = fread('lv2-2305-5.csv')
dim(purchase)
str(purchase)

### Question 1 ################################################################
# Step 1-1
df1 = purchase[, .(CAT1_AMT = sum(ifelse(PRD_CAT==1, 1*PUR_AMT,0)),
                   CAT2_AMT = sum(ifelse(PRD_CAT==2, 1*PUR_AMT,0)),
                   CAT3_AMT = sum(ifelse(PRD_CAT==3, 1*PUR_AMT,0))), 
               .(USER_ID, MARITAL_YN)]
dim(df1)

# Step 1-2, 1-3
library(reshape2)
cor0 = data.table(melt(cor(df1[MARITAL_YN==0, -c("USER_ID", "MARITAL_YN")])))
cor0
A = max(cor0[Var1!=Var2, value])

cor1 = data.table(melt(cor(df1[MARITAL_YN==1, -c("USER_ID", "MARITAL_YN")])))
cor1
B = min(cor1[Var1!=Var2, value])

C = abs(A-B)
A; B; C

# Answer 1
ans21 = round(C, 3)
ans21

### Question 2 ################################################################
# Step 2-1
df2 = purchase[,.(AGE1_AMT = sum(ifelse(AGE_GRP=='18-25', 1*PUR_AMT,0)),
                  AGE2_AMT = sum(ifelse(AGE_GRP=='26-35', 1*PUR_AMT,0)),
                  AGE3_AMT = sum(ifelse(AGE_GRP=='36-45', 1*PUR_AMT,0)),
                  AGE4_AMT = sum(ifelse(AGE_GRP=='46-50', 1*PUR_AMT,0)),
                  AGE5_AMT = sum(ifelse(AGE_GRP=='51-55', 1*PUR_AMT,0))),PRD_ID]
dim(df2)

# Step 2-2
library(caret)
zscoring = preProcess(df2[, -c('PRD_ID')], method=c("scale","center"))
zscored = predict(zscoring, df2[,-c('PRD_ID')])
colnames(zscored) = paste0(colnames(zscored), '_Z')

# Step 2-3
set.seed(1234)
km = kmeans(zscored, centers=3, nstart=50)
df2$cluster = km$cluster

# Step 2-4
df2_agg = df2[, mean(AGE3_AMT), cluster]
df2_agg
min(df2_agg$V1)

# Answer 2
ans22 = round(min(df2_agg$V1))
ans22


### Question 3 ################################################################
# Step 3-1
df3 = purchase[, .(TOT_AMT = sum(PUR_AMT),
                   CAT1_AMT = sum(ifelse(PRD_CAT==1, 1*PUR_AMT, 0)),
                   CAT2_AMT = sum(ifelse(PRD_CAT==2, 1*PUR_AMT, 0)),
                   CAT3_AMT = sum(ifelse(PRD_CAT==3, 1*PUR_AMT, 0))),
               .(USER_ID, GENDER, AGE_GRP, MARITAL_YN)]
df3[, MAX_AMT := pmax(CAT1_AMT, CAT2_AMT, CAT3_AMT)]
df3[, PRF_CAT := ifelse(CAT1_AMT==MAX_AMT, 1,ifelse(CAT2_AMT==MAX_AMT, 2, 3))]
df3 = df3[, -c("CAT1_AMT","CAT2_AMT","CAT3_AMT","MAX_AMT")]
dim(df3)

# Step 3-2
q2 = quantile(df3$TOT_AMT, .5)
q3 = quantile(df3$TOT_AMT, .75)
df3 = df3[TOT_AMT>=q2 & TOT_AMT<=q3]
dim(df3)

# Step 3-3
df3$GENDER = as.factor(df3$GENDER)
df3$AGE_GRP = as.factor(df3$AGE_GRP)
df3$MARITAL_YN = as.factor(df3$MARITAL_YN)
df3$PRF_CAT = as.factor(df3$PRF_CAT)

library(caret)
encoder = dummyVars(~., df3[, -c("USER_ID", "TOT_AMT")])
encoded = cbind(df3[,.(USER_ID, TOT_AMT)], predict(encoder, df3))
dim(predict(encoder, df3))

# Step 3-4
encoded = encoded[order(USER_ID)]
train3 = encoded[USER_ID%%4!=0, -c("USER_ID")]
test3  = encoded[USER_ID%%4==0, -c("USER_ID")]
dim(train3); dim(test3)

# Step 3-5
library(rpart)
set.seed(1234)
mdl3 = rpart(TOT_AMT~., train3, 
             control=rpart.control(minsplit=2, cp=0, maxdepth=6))

# Step 3-6
test3$PRED = predict(mdl3, test3)
mean(test3[, abs((PRED-TOT_AMT)/TOT_AMT)])*100

# Answer 3
ans23 = round(mean(test3[, abs((PRED-TOT_AMT)/TOT_AMT)])*100, 2)
ans23

################################################################################

