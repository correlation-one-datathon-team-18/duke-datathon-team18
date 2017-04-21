set.seed(10)
require(lubridate)



jobs<-read.csv('jobs.csv',header= TRUE)
head(jobs)

categories <- unique(jobs$state)
length(categories)
education <- read.csv("education.csv", header = TRUE)
edu_state <- unique(education$state)
jobs$yr <- strftime(jobs$created_date, "%Y")
tab <- table(jobs$state, jobs$yr)


head(education[, 2:ncol(education)])
education_state <- aggregate(. ~ state, education[, 2:ncol(education)], sum)
a <- vector()
for(i in 1:nrow(education_state)){
  a[i] <- (1 - sum(education[i,3:16]) / sum(education[i,3:ncol(education)]))
}
education$city <- factor(education$city)
levels(droplevels(education$state))


tab<-(table(jobs$category_name,jobs$yr))

for (i in 2:148)
{
  plot(tab[i,],ylab = rownames(tab)[i+1])
  lines(tab[i,])
}

econ_state <- read.csv("/Users/Warrior/Desktop/duke-datathon-team18/econ_state.csv", header = TRUE)
econ_state <- econ_state[-c(52:53), ]

state_gdp <- econ_state[, 2:13]
state_per_capita <- econ_state[, 14:25]
unemployment_rate <- econ_state[, 26:37]
econ_state <- cbind(econ_state,a)

for(i in 1:nrow(econ_state)){
  lm1 <- lm(as.matrix(econ_state[, 2:13]) ~ as.matrix(econ_state[, 14:25]) + as.matrix(econ_state[, 26:37] + as.matrix(econ_state[, 38])), data = econ_state)
  plot(lm1)
}
model_data1 <- c()
for(i in 1:ncol(state_gdp)){
  model_data1 <- c(model_data1, state_gdp[,i])
}

model_data2 <- c()
for(i in 1:ncol(state_gdp)){
  model_data2 <- c(model_data2, state_per_capita[,i])
}

model_data3 <- c()
for(i in 1:ncol(state_gdp)){
  model_data3 <- c(model_data3, unemployment_rate[,i])
}

model_data4 <- rep(a, 12)

model_data5 <- c()
for(i in 5:9){
  model_data5 <- c(model_data5, tab[, i])
}

model_data <- data.frame(cbind(model_data1, model_data2, model_data3, model_data4))

train_data <- cbind(model_data[358:510, ], model_data5[1:153])
test_data <- cbind(model_data[511:612], model_data5[154:250])


nrow(train_data)
nrow(test_data)


linear_model <- lm(model_data1 ~ ., data = train_data)
plot(linear_model)
summary(linear_model)

predicted_values <- predict(linear_model, test_data, interval = "predict")

plot(test_data[, 1])
plot(predicted_values[, 1])

error <- sum(((predicted_values[,1] - test_data[,1]) ^ 2)) / 144


lm1 <- lm(as.matrix(econ_state[, 2:13][1]) ~ as.matrix(econ_state[, 14:25][1]) + as.matrix(econ_state[, 26:37][1] + as.matrix(econ_state[, 38][1])), data = econ_state)
plot(lm1$fitted.values)
plot(lm1$residuals)
lm1$residuals
plot(as.matrix(econ_state[,2:13][1]))
abline(lm1)