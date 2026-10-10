library(car)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

files <- list.files(
  path = "C:/users/hoshi/run/rundata",
  pattern = "\\.csv$",
  full.names = TRUE
)

data_list <- lapply(files, function(f){
  dat <- read.csv(f)
  if(!("temp" %in% names(dat))){
    dat$temp <- NA
  }
  dat$month <- tools::file_path_sans_ext(basename(f))
  dat
})

run <- do.call(rbind, data_list)
run$kmh <- 3600/run$time_second
run <- subset(run, km>=2)

run$bpm_c <- run$bpm - mean(run$bpm)

run$month <- factor(
  run$month,
  levels = c("26SEP", "25DEC", "26JAN", "26FEB", "26MAR",
             "26APR", "26MAY","26JUN","26JUL","26AUG")
)

model <- lm(time_second ~ bpm + I(bpm^2) + month + bpm:month + I(bpm^2):month, data=run)
summary(model)
car::Anova(model, type = 3)

bpm_list <- c(150, 160, 170, 180)

result <- sapply(bpm_list, function(b) {
  predict(
    model,
    data.frame(
      bpm = b,
      month = c("25DEC", "26JAN", "26FEB","26MAR","26APR","26MAY",
                "26JUN","26JUL","26AUG","26SEP")
    )
  )
})

rownames(result) <- c("25DEC", "26JAN", "26FEB","26MAR","26APR",
                      "26MAY","26JUN","26JUL","26AUG","26SEP")
colnames(result) <- paste0("bpm", bpm_list)

result

cols <- c(
  "25DEC" = "#1F78B4",  
  "26JAN" = "#A6CEE3",
  "26FEB" = "#33A02C",  
  "26MAR" = "#B2DF8A",  
  "26APR" = "#FFD92F",  
  "26MAY" = "#FF7F00",  
  "26JUN" = "#E31A1C", 
  "26JUL" = "#6A3D9A"
)

run$month <- factor(run$month, levels = names(cols))

png("rundata_plot_month.png")
plot(run$bpm, run$kmh,
     col=cols[run$month],
     pch=19,
     xlab="bpm",
     ylab="km/h")

legend(
  "topleft",
  legend=names(cols),
  col=cols,
  pch=19
)
dev.off()

run$period <- case_when(
  run$month %in% c("25DEC", "26JAN", "26FEB") ~ "25Winter",
  run$month %in% c("26MAR", "26APR", "26MAY") ~ "26Spring",
  run$month %in% c("26JUN","26JUL","26AUG") ~ "26Summer",
  run$month %in% c("26SEP") ~ "26AUTUMN",
  TRUE ~ NA_character_
)

run$period <- factor(
  run$period,
  levels = c("26AUTUMN", "25Winter", "26Spring","26Summer")
)

model_2 <- lm(
  time_second ~ bpm + I(bpm^2) +
    period +
    bpm:period +
    I(bpm^2):period,
  data = run
)
summary(model_2)
car::Anova(model_2, type = 3)

result_2 <- sapply(bpm_list, function(b) {
  predict(
    model_2,
    data.frame(
      bpm = b,
      period = c("25Winter", "26Spring", "26Summer","26AUTUMN")
    )
  )
})

rownames(result_2) <- c("25Winter", "26Spring", "26Summer","26AUTUMN")
colnames(result_2) <- paste0("bpm", bpm_list)

result_2

run$kyori <- case_when(
  run$km %in% c("2", "3", "4", "5") ~ "zenhan",
  run$km %in% c("6", "7", "8", "9", "10") ~ "kouhan",
  TRUE ~ "long"
)

run$kyori <- factor(
  run$kyori,
  levels = c("zenhan", "kouhan", "long")
)

model_3 <- lm(time_second ~ bpm + I(bpm^2) + kyori + bpm:kyori + I(bpm^2):kyori, data=run)
summary(model_3)
car::Anova(model_3, type = 3)

kyori_cols <- c(
  "zenhan" = "#1F78B4",
  "kouhan" = "#33A02C",
  "long" = "#E31A1C"
)

plot(
  run$time_second,run$bpm,
  col = kyori_cols[run$kyori],
  pch=19,
  cex = 0.8
)

run_temp <- na.omit(run)
model_temp <- lm(time_second ~ 
                   bpm + I(bpm^2) + temp, data = run_temp)
summary(model_temp)
car::Anova(model_temp, type = 3)