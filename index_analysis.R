library(car)
library(lm.beta)
data <- read.csv("c:/users/tomokihoshino0607/Documents/index_analysis.csv")

data$gap <- data$max_bpm - data$ave_bpm

data$hill <- data$up / data$dist
data$hill_2 <- (data$up + data$down) / data$dist

hmmss_to_sec <- function(x){
  x <- as.character(x)
  sapply(x, function(t){
    n <- nchar(t)
    if(n <= 4){
      m <- substr(t, 1, n-2)
      s <- substr(t, n-1, n)
      as.numeric(m) * 60 +
        as.numeric(s)
      
    }else{
      h <- substr(t, 1, 1)
      m <- substr(t, 2, 3)
      s <- substr(t, 4, 5)
      
      as.numeric(h) * 3600 +
        as.numeric(m) * 60 +
        as.numeric(s)
    }
  })
}

time_cols <- c("time", "ave1k_speed", "best1k_speed")
for(col in time_cols){
  data[[paste0(col, "_sec")]] <- hmmss_to_sec(data[[col]])
}

data$speed_gap <- data$ave1k_speed_sec - data$best1k_speed_sec

data$date <- as.Date(data$date)
data <- data[order(data$date), ]

make_dist_n <- function(data, n){
  sapply(data$date, function(d){
    sum(data$dist[data$date < d & data$date >= d - n], na.rm = TRUE)
  })
}

make_high_n <- function(data, n){
  sapply(data$date, function(d){
    sum(data$type %in% c("T", "H") & data$date < d & data$date >= d - n, na.rm = TRUE)
  })
}

#choose date in separate
n_dist_values <- 1:80
n_high_values <- 1:80

r2_matrix <- matrix(NA, nrow = length(n_dist_values), ncol = length(n_high_values),
                    dimnames = list(dist_days = n_dist_values, high_days = n_high_values))

for (d in seq_along(n_dist_values)) {
  for (h in seq_along(n_high_values)) {
    
    nd <- n_dist_values[d]
    nh <- n_high_values[h]
    
    temp_data <- data
    temp_data$dist_n <- make_dist_n(data, nd)
    temp_data$high_n <- make_high_n(data, nh)
    
    model_n <- lm(index ~ dist + ave1k_speed_sec + 
                    speed_gap + ave_bpm + gap + 
                    hill + type + dist_n + high_n, data = temp_data)
    
    r2_matrix[d, h] <- summary(model_n)$adj.r.squared
  }
}
max_coord <- which(r2_matrix == max(r2_matrix), arr.ind = TRUE)
print(max_coord)

AIC_matrix <- matrix(NA, nrow = length(n_dist_values), ncol = length(n_high_values),
                     dimnames = list(dist_days = n_dist_values, high_days = n_high_values))

for (d in seq_along(n_dist_values)) {
  for (h in seq_along(n_high_values)) {
    
    nd <- n_dist_values[d]
    nh <- n_high_values[h]
    
    temp_data <- data
    temp_data$dist_n <- make_dist_n(data, nd)
    temp_data$high_n <- make_high_n(data, nh)
    
    model_n <- lm(index ~ dist + ave1k_speed_sec + 
                    speed_gap + ave_bpm + gap + 
                    hill + type + dist_n + high_n, data = temp_data)
    
    AIC_matrix[d, h] <- AIC(model_n)
  }
}
min_coord <- which(AIC_matrix == min(AIC_matrix), arr.ind = TRUE)
print(min_coord)

data$dist_best  <- make_dist_n(data, 46)
data$high_best  <- make_high_n(data, 17)

model <- lm(index ~ dist + ave1k_speed_sec + 
              speed_gap + ave_bpm + gap + 
              hill + type + dist_best + high_best, data = data)
summary(model)
vif(model)
null_model <- lm(index ~ 1, data = data)

#choose
backward_model <- step(model, direction = "backward")

forward_model <- step(
  null_model,
  scope = list(upper = model),
  direction = "forward"
)

both_model <- step(
  null_model,
  scope = list(upper = model),
  direction = "both"
)

choose_model_best <- lm(index ~ dist + ave1k_speed_sec + ave_bpm + dist_best + high_best + gap, data = data)
summary(choose_model_best)
vif(choose_model_best)
summary(lm.beta(choose_model_best))

pred_best <- predict(choose_model_best)
plot(data$index, pred_best,pch = 19,cex = 1,
     xlab = "観測された Running Index",ylab = "予測した Running Index",
     sub = "予測精度")

abline(0, 1, col = "red", lwd = 2)

plot(data$index, residuals(choose_model_best))
abline(h=0,lty=2)

#iranai
data$dist7  <- make_dist_n(data, 7)
data$high7  <- make_high_n(data, 7)

model <- lm(index ~ dist + ave1k_speed_sec + 
              speed_gap + ave_bpm + gap + 
              hill + type + dist7 + high7, data = data)
summary(model)
vif(model)
null_model <- lm(index ~ 1, data = data)

#choose
backward_model <- step(model, direction = "backward")

forward_model <- step(
  null_model,
  scope = list(upper = model),
  direction = "forward"
)

both_model <- step(
  null_model,
  scope = list(upper = model),
  direction = "both"
)

choose_model <- lm(index ~ dist + ave1k_speed_sec + ave_bpm + dist7 + high7 + gap, data = data)
summary(choose_model)
vif(choose_model)

summary(lm.beta(choose_model))

pred <- predict(choose_model)
plot(data$index, pred,
     xlab = "Observed Running Index",ylab = "Predicted Running Index")

abline(0, 1, col = "red", lwd = 2)

plot(data$index, residuals(model))
abline(h=0,lty=2)

#choose at same date
n_values <- 3:70
r2_results <- numeric(length(n_values))

for (i in seq_along(n_values)) {
  n <- n_values[i] # 今回試す日数 n
  
  temp_data <- data
  temp_data$dist_n <- make_dist_n(data, n)
  temp_data$high_n <- make_high_n(data, n)
  
  model_n <- lm(index ~ dist + ave1k_speed_sec + ave_bpm + dist_n + high_n + gap, data = temp_data)
  
  r2_results[i] <- summary(model_n)$adj.r.squared
}

result_df <- data.frame(days = n_values, adj_R2 = r2_results)
print(result_df)
plot(result_df$days, result_df$adj_R2, 
     type = "b", pch = 19, col = "blue",
     xlab = "過去 n 日間", 
     ylab = "自由度調整済み決定係数 (Adjusted R2)",
     main = "過去の日数 n の違いによるモデル精度の変化")

r2_diff <- diff(result_df$adj_R2)
days_diff <- result_df$days[-1]
plot(days_diff, r2_diff, type = "b", pch = 19, col = "red",
     xlab = "過去 n 日間", 
     ylab = "精度の上昇幅 (微分値)",
     main = "日数増加にともなう精度向上率の推移（微分）")
abline(h = 0, lty = 2, col = "gray") # ゼロのライン

data$dist45  <- make_dist_n(data, 45)
data$high45  <- make_high_n(data, 45)

choose_model_45 <- lm(index ~ dist + ave1k_speed_sec + ave_bpm + dist45 + high45 + gap, data = data)
summary(choose_model_45)
vif(choose_model_45)
summary(lm.beta(choose_model_45))