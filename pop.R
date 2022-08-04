library(MASS)
data <- read.csv('C:\\Users\\INHA\\Documents\\Python Scripts\\population_do\\preprocessing_year.csv')
head(data)

# 스크리닝 모델

data$tem2017 <- data$거리*(data$X2017)/data$전입지.면적
model17 <- lm(log(X2017_x) ~ log(X2017_y) + log(X2017) + log(거리) + tem2017, data = data)

data$tem2018 <- data$거리*(data$X2018)/data$전입지.면적
model18 <- lm(log(X2018_x) ~ log(X2018_y) + log(X2018) + log(거리) + tem2018, data = data)

data$tem2019 <- data$거리*(data$X2019)/data$전입지.면적
model19 <- lm(log(X2019_x) ~ log(X2019_y) + log(X2019) + log(거리) + tem2019, data = data)

data$tem2020 <- data$거리*(data$X2020)/data$전입지.면적
model20 <- lm(log(X2020_x) ~ log(X2020_y) + log(X2020) + log(거리) + tem2020, data = data)

data$tem2021 <- data$거리*(data$X2021)/data$전입지.면적
model21 <- lm(log(X2021_x) ~ log(X2021_y) + log(X2021) + log(거리) + tem2021, data = data)

coeff <- cbind(model17$coefficients,model18$coefficients,model19$coefficients,model20$coefficients,model21$coefficients)
write.csv(coeff,file="coeff.csv")

result <- data[,2:3]
head(result)
result$r2017 <- data$X2017_x
result$r2018 <- data$X2018_x
result$r2019 <- data$X2019_x
result$r2020 <- data$X2020_x
result$r2021 <- data$X2021_x

result$h2017 <- exp(model17$fitted.values)
result$h2018 <- exp(model18$fitted.values)
result$h2019 <- exp(model19$fitted.values)
result$h2020 <- exp(model20$fitted.values)
result$h2021 <- exp(model21$fitted.values)

write.csv(result,file="result.csv")

plot(data$거리, stdres(model17))
plot(data$X2017, stdres(model17))
plot(data$X2017_y, stdres(model17))
plot(data$tem2017, stdres(model17))
plot(data$tem2018, stdres(model18))

plot(data$거리, stdres(model17), xlab='거리', ylab='잔차',main='표준화잔차 vs.거리')


data[data$거리>4000,]

# 중력 모델

model17 <- lm(log(X2017_x) ~ log(X2017_y) + log(X2017) + log(거리), data = data)

model18 <- lm(log(X2018_x) ~ log(X2018_y) + log(X2018) + log(거리), data = data)

model19 <- lm(log(X2019_x) ~ log(X2019_y) + log(X2019) + log(거리), data = data)

model20 <- lm(log(X2020_x) ~ log(X2020_y) + log(X2020) + log(거리), data = data)

model21 <- lm(log(X2021_x) ~ log(X2021_y) + log(X2021) + log(거리), data = data)

coeff <- cbind(model17$coefficients,model18$coefficients,model19$coefficients,model20$coefficients,model21$coefficients)
write.csv(coeff,file="coeff_grav.csv")

result <- data[,2:3]
head(result)
result$r2017 <- data$X2017_x
result$r2018 <- data$X2018_x
result$r2019 <- data$X2019_x
result$r2020 <- data$X2020_x
result$r2021 <- data$X2021_x

result$h2017 <- exp(model17$fitted.values)
result$h2018 <- exp(model18$fitted.values)
result$h2019 <- exp(model19$fitted.values)
result$h2020 <- exp(model20$fitted.values)
result$h2021 <- exp(model21$fitted.values)

write.csv(result,file="result_grav.csv")

plot(data$거리, stdres(model17))
plot(data$X2017, stdres(model17))
plot(data$X2017_y, stdres(model17))
plot(data$tem2017, stdres(model17))
plot(data$tem2018, stdres(model18))

plot(data$거리, stdres(model17), xlab='거리', ylab='잔차',main='표준화잔차 vs.거리')


data[data$거리>4000,]