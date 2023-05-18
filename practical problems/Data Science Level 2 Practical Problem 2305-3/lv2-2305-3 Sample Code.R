################################################################################
##### Data Science Level 2 Practical Problem 2305-3
################################################################################

### Settings ###################################################################
library(data.table)
library(dplyr)
options(digits=16)


##### Preprocess ###############################################################
radiation = fread("lv2-2305-3.csv")
dim(radiation)
str(radiation)

##### Question 1 ##############################################################
# Step 1-1
dt4 = copy(radiation)
dt4[, TIME := substr(OBS_DT, 12, 19)]
dt4 = dt4[(TIME_SUNRISE<TIME)&(TIME<TIME_SUNSET)]

# Step 1-2
cor4 = cor(dt4[,.(PRESSURE, HUMIDITY, WIND_SPEED, RADIATION)])[4,-4]
cor4
ans24 = max(abs(cor4))
ans24

# Answer
ans24 = round(ans24, 3)
ans24

##### Question 2 ##############################################################
# Step 2-1
dt5 = radiation[, max(TEMPERATURE), substring(OBS_DT, 1, 10)]
colnames(dt5) = c('DATE', 'MAX_TEMPERATURE')
dim(dt5)

# Step 2-2
t5 = data.table(c('2022-09-30','2022-11-30','2022-12-06','2022-12-07'),rep(NA,4))
colnames(t5) = colnames(dt5)
dt5 = rbind(dt5, t5)[order(DATE)]
dt5[(DATE>='2022-09-28')&(DATE<='2022-10-01')]

# Step 2-3
dt5[, PRED1 := 0.7*shift(MAX_TEMPERATURE,1)+0.3*shift(MAX_TEMPERATURE,2)]

# Step 2-4
dt5[, PRED2 := 0.7*shift(MAX_TEMPERATURE,2)+0.3*shift(MAX_TEMPERATURE,3)]
PRED = c(dt5[DATE%in%c('2022-09-30','2022-11-30','2022-12-06'), PRED1],
         dt5[DATE%in%c('2022-12-07'), PRED2])
ans25 = mean(PRED)
ans25

# Answer
ans25 = round(ans25, 1)
ans25

##### Question 3 ##############################################################
# Step 3-1
dt6 = copy(radiation)
dt6[, DATE := substr(OBS_DT, 1, 10)]
dt6[, SUN_DURATION := 3600*(as.numeric(substr(TIME_SUNSET,  1, 2)) -
                            as.numeric(substr(TIME_SUNRISE, 1, 2))) +
                      60*(as.numeric(substr(TIME_SUNSET,  4, 5)) -
                          as.numeric(substr(TIME_SUNRISE, 4, 5))) +
                      (as.numeric(substr(TIME_SUNSET,  7, 8)) -
                       as.numeric(substr(TIME_SUNRISE, 7, 8)))]

# Step 3-2
col6 = c('DATE', 'SUN_DURATION', 'TEMPERATURE', 'PRESSURE', 'HUMIDITY', 
         'WIND_DIRECTION', 'WIND_SPEED', 'RADIATION')
dt6 = dt6[,..col6][, lapply(.SD, FUN=mean), .(DATE, SUN_DURATION)]

# Step 3-3
train6 = dt6[!substr(DATE, 9,10)%in%c('01','15'), -c('DATE')]
test6  = dt6[substr(DATE, 9,10)%in%c('01','15'), -c('DATE')]
dim(train6); dim(test6)

# Step 3-4
lm = lm(RADIATION~., train6)
test6$PRED = predict(lm, test6)
ans26 = mean(test6[, (PRED-RADIATION)^2])
ans26

# Answer 3
ans26 = round(ans26, 2)
ans26

##### Summary ##################################################################
for(i in 24:26){
  eval(parse(text=paste0('print(ans',i,')')))
}

################################################################################