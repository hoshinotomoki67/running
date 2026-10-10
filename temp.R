library(mgcv)
data <- read.csv("c:/users/hoshi/run/running_temp.csv")
run <- subset(data,km>=2)
model_gam <- gam(time_second~s(bpm)+temp, data=run)
summary(model_gam)
plot(model_gam, select = 1)

model_gam2 <-  gam(time_second~s(bpm)+temp+s(km), data=run)
summary(model_gam2)
#kmはなくてよさそう