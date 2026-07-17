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

run$month <- factor(
  run$month,
  levels = c("26JUN", "25DEC", "26JAN", "26FEB", "26MAR", "26APR", "26MAY")
)

model <- lm(kmh ~ bpm + I(bpm^2) + month + bpm:month + I(bpm^2):month, data=run)
summary(model)
car::Anova(model, type = 3)

bpm_list <- c(150, 160, 170, 180)

result <- sapply(bpm_list, function(b) {
  3600 / predict(
    model,
    data.frame(
      bpm = b,
      month = c("25DEC", "26JAN", "26FEB","26MAR","26APR","26MAY","26JUN")
    )
  )
})

rownames(result) <- c("25DEC", "26JAN", "26FEB","26MAR","26APR","26MAY","26JUN")
colnames(result) <- paste0("bpm", bpm_list)

result

cols <- c(
  "25DEC" = "#1F78B4",  
  "26JAN" = "#A6CEE3",
  "26FEB" = "#33A02C",  
  "26MAR" = "#B2DF8A",  
  "26APR" = "#FFD92F",  
  "26MAY" = "#FF7F00",  
  "26JUN" = "#E31A1C"  
  #"26JUL" = "#6A3D9A"
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
  # run$month %in% c("26JUN","26JUL", "26AUG") ~ "26Summer",
  TRUE ~ as.character(run$month)
)

run$period <- factor(
  run$period,
  levels = c("26JUN", "25Winter", "26Spring")
)

model_2 <- lm(
  kmh ~ bpm + I(bpm^2) +
    period +
    bpm:period +
    I(bpm^2):period,
  data = run
)
car::Anova(model_2, type = 3)
vif(model_2)

summary(model_2)

result_2 <- sapply(bpm_list, function(b) {
  3600 / predict(
    model_2,
    data.frame(
      bpm = b,
      period = c("25Winter", "26Spring", "26JUN")
    )
  )
})

rownames(result_2) <- c("25Winter", "26Spring", "26JUN")
colnames(result_2) <- paste0("bpm", bpm_list)

result_2

cols_seasons <- c(
  "Winter" = "#1F78B4",  
  "Spring" = "#33A02C",
  "26JUN" = "#E66101"
  #"#7F3B08"
)

png("rundata_plot_period.png")
plot(run$bpm, run$kmh,
     col=cols_seasons[run$period],
     pch=19,
     cex = 0.8,
     xlab="bpm",
     ylab="km/h")

legend(
  "topleft",
  legend=names(cols_seasons),
  col=cols_seasons,
  pch=19
)
dev.off()

run$period <- factor(run$period)

bpm_range <- run |>
  filter(!is.na(bpm), !is.na(period)) |>
  group_by(period) |>
  summarise(
    bpm_min = min(bpm),
    bpm_max = max(bpm),
    .groups = "drop"
  )

newdata <- bpm_range |>
  mutate(
    bpm = map2(
      bpm_min,
      bpm_max,
      ~ seq(.x, .y, length.out = 200)
    )
  ) |>
  unnest(bpm) |>
  select(period, bpm)

pred <- predict(
  model_2,
  newdata = newdata,
  interval = "confidence"
)

newdata <- newdata |>
  mutate(
    fit = pred[, "fit"],
    lwr = pred[, "lwr"],
    upr = pred[, "upr"]
  )

ggplot() +
  geom_point(
    data = run,
    aes(bpm, kmh, color = period),
    alpha = 0.6,
    size = 1.2
  ) +
  geom_ribbon(
    data = newdata,
    aes(bpm, ymin = lwr, ymax = upr, fill = period),
    alpha = 0.15,
    color = NA
  ) +
  geom_line(
    data = newdata,
    aes(bpm, fit, color = period),
    linewidth = 1.2
  ) +
  labs(
    x = "Heart rate (bpm)",
    y = "Speed (km/h)",
    color = "Period",
    fill = "Period"
  ) +
  #geom_vline(xintercept = c(160,180))+
  theme_classic()
