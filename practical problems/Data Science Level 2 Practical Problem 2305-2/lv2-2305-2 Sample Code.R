################################################################################
##### Data Science Level 2 Practical Problem 2305-2
################################################################################
if(!require(dplyr)){
  install.packages('dplyr')
}

library(dplyr)
library(data.table)

dt1 = read.csv('lv2-2305-2.csv', encoding = "UTF-8")
dim(dt1) #400, 9

head(dt1)

#####################################################
## Q1
#####################################################

df1 <- dt1
# Step 1-1.
df1_admit = df1[df1$ADMIT=='Admitted',]
dim(df1_admit)

df1_deny = df1[df1$ADMIT=='Denied',]
dim(df1_deny)

# Step 1-2.
df1_admit$GRE_R <- rank(-df1_admit$GRE, ties.method=c("min"))
df1_admit$TOEFL_R <- rank(-df1_admit$TOEFL, ties.method=c("min"))
df1_admit$CGPA_R <- rank(-df1_admit$CGPA, ties.method=c("min"))

df1_deny$GRE_R <- rank(-df1_deny$GRE, ties.method=c("min"))
df1_deny$TOEFL_R <- rank(-df1_deny$TOEFL, ties.method=c("min"))
df1_deny$CGPA_R <- rank(-df1_deny$CGPA, ties.method=c("min"))


# Step 1-3.
var1 = c('GRE_R','TOEFL_R','CGPA_R')
cor(df1_admit[,var1], method="spearman")[,3]
A = 0.7770913
A_VAR = 'GRE'

# Step 1-4.
cor(df1_deny[,var1], method="spearman")[,3]
B = 0.6167243
B_VAR = 'TOEFL'
cat("A_VAR: ", A_VAR, " A: ", A)
cat("B_VAR: ", B_VAR, " B: ", B)
A1=abs(A-B)
cat("Q1 Answer: ", A1)

#####################################################
## Q2
#####################################################

df2 <- dt1
dim(df2)
# Step 2-1.
q1 = quantile(df2$TOEFL, 0.25)
q3 = quantile(df2$TOEFL, 0.75)
iqr = q3 - q1
df2$outlier = ifelse((df2$TOEFL<(q1-1.5*iqr))|(df2$TOEFL>(q3+1.5*iqr)), 1, 0)
df2 <- df2[df2$outlier==0,]
dim(df2)

# 2-2.
var2=c('GRE','TOEFL','SOP','LOR','CGPA')
df2_2<-df2[,var2]
df2_2$ADMIT <- ifelse(df2$ADMIT=='Admitted',1,0)

set.seed(1234)
lr2_2 <-glm(ADMIT ~ ., family=binomial, data=df2_2)

# 2-3.
predict2 <- predict(lr2_2, df2_2[,var2], type='response')
df2_2$predict2 <- ifelse(predict2>=0.5, 1, 0)

accuracy2 = sum(df2_2$ADMIT==df2_2$predict2)/length(df2_2$ADMIT)*100
A2 = accuracy2
cat("Q2 Answer: ", A2)

#####################################################
## Q3.
####################################################
df3 <- copy(dt1)
head(df3)
# Step 3-1.
df3<-df3[df3$ADMIT=='Denied',]
dim(df3)

# Step 3-2.
dummies3 <- fastDummies::dummy_cols(df3[,'UNIV_RATING'], remove_first=T)[,2:5]
head(dummies3)

# Step 3-3.
var3=c('GRE','TOEFL','SOP','LOR','CGPA')
library(caret)
scale3 <-caret::preProcess(df3[,var3], method=c("range"))
df3_s <- predict(scale3, df3)
head(df3_s)

# Step 3-4.
var3_4 = c('GRE','TOEFL','SOP','LOR','CGPA','RESEARCH')
train3 = cbind(df3_s[,var3_4], dummies3)
head(train3)
dim(train3)

set.seed(1234)
predict3_4 <-kmeans(train3, 4, iter.max=300, nstart=50)

set.seed(1234)
predict3_5 <-kmeans(train3, 5, iter.max=300, nstart=50)

set.seed(1234)
predict3_6 <-kmeans(train3, 6, iter.max=300, nstart=50)

# step 3-5.
library(cluster)
s3_4 = mean(silhouette(predict3_4$cluster, dist(train3))[,"sil_width"])
print(s3_4)
# 0.4910509
s3_5 = mean(silhouette(predict3_5$cluster, dist(train3))[,"sil_width"]) 
print(s3_5)
# 0.5090779
s3_6 = mean(silhouette(predict3_6$cluster, dist(train3))[,"sil_width"]) 
print(s3_6)
# 0.5556813

# 3-6
unbal3_5 = max(table(predict3_5$cluster))/min(table(predict3_5$cluster))
print(unbal3_5)
#3.041667
unbal3_6 = max(table(predict3_6$cluster))/min(table(predict3_6$cluster))
print(unbal3_6)
#5.615385
# selected 5
C=5
D=s3_5

cat('C: ', C)
cat('D: ', D)
A3 = C+D
cat("Q3 Answer: ", A3)