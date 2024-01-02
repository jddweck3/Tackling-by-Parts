#Train the Expected Tackles Model

library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)

#Set weeks for train/val/test split
train_weeks <- 1:7
val_week <- 8
test_week <- 9

#finalize dataset for model training
for_modeling_3 <- model_molder %>%
  select(-o_p1_accel, -o_p2_accel, -o_p3_accel, -o_p4_accel, -o_p5_accel,
         -o_p6_accel, -o_p7_accel, -o_p8_accel, -o_p9_accel, -o_p10_accel,
         -o_p3_dir_ball, -o_p4_dir_ball, -o_p5_dir_ball, -o_p6_dir_ball,
         -o_p7_dir_ball, -o_p8_dir_ball, -o_p9_dir_ball, -o_p10_dir_ball,
         -o_p1_height, -o_p2_height, -o_p3_height, -o_p4_height, -o_p5_height,
         -o_p6_height, -o_p7_height, -o_p8_height, -o_p9_height, -o_p10_height,
         -s, -down, -yardsToGo, -quarter, -possessionTeam, -defensiveTeam,
         -game_clock, -field_side, -yds_ez, -yardlineNumber, -o_p1_x, -o_p2_x,
         -o_p3_x, -o_p4_x, -o_p5_x, -o_p6_x, -o_p7_x, -o_p8_x, -o_p9_x, -o_p10_x, -x,
         -los_x, -firstdown_x, -o_p1_pos, -o_p2_pos, -o_p3_pos, -o_p4_pos, -o_p5_pos,
         -o_p6_pos, -o_p7_pos, -o_p8_pos, -o_p9_pos, -o_p10_pos, -called_pass, -event, -max_speed,
         -game_sit, -height_inches, -weight)

#Training data for merging results back in
train_data <- model_molder %>%
  filter(week %in% train_weeks) %>%
  select(-o_p1_accel, -o_p2_accel, -o_p3_accel, -o_p4_accel, -o_p5_accel,
         -o_p6_accel, -o_p7_accel, -o_p8_accel, -o_p9_accel, -o_p10_accel,
         -o_p3_dir_ball, -o_p4_dir_ball, -o_p5_dir_ball, -o_p6_dir_ball,
         -o_p7_dir_ball, -o_p8_dir_ball, -o_p9_dir_ball, -o_p10_dir_ball,
         -o_p1_height, -o_p2_height, -o_p3_height, -o_p4_height, -o_p5_height,
         -o_p6_height, -o_p7_height, -o_p8_height, -o_p9_height, -o_p10_height,
         -s)

#Validation data for merging results back in
val_data <- model_molder %>%
  filter(week == val_week) %>%
  select(-o_p1_accel, -o_p2_accel, -o_p3_accel, -o_p4_accel, -o_p5_accel,
         -o_p6_accel, -o_p7_accel, -o_p8_accel, -o_p9_accel, -o_p10_accel,
         -o_p3_dir_ball, -o_p4_dir_ball, -o_p5_dir_ball, -o_p6_dir_ball,
         -o_p7_dir_ball, -o_p8_dir_ball, -o_p9_dir_ball, -o_p10_dir_ball,
         -o_p1_height, -o_p2_height, -o_p3_height, -o_p4_height, -o_p5_height,
         -o_p6_height, -o_p7_height, -o_p8_height, -o_p9_height, -o_p10_height,
         -s)

#Test data for merging results back in
test_data <- model_molder %>%
  filter(week == test_week) %>%
  select(-o_p1_accel, -o_p2_accel, -o_p3_accel, -o_p4_accel, -o_p5_accel,
         -o_p6_accel, -o_p7_accel, -o_p8_accel, -o_p9_accel, -o_p10_accel,
         -o_p3_dir_ball, -o_p4_dir_ball, -o_p5_dir_ball, -o_p6_dir_ball,
         -o_p7_dir_ball, -o_p8_dir_ball, -o_p9_dir_ball, -o_p10_dir_ball,
         -o_p1_height, -o_p2_height, -o_p3_height, -o_p4_height, -o_p5_height,
         -o_p6_height, -o_p7_height, -o_p8_height, -o_p9_height, -o_p10_height,
         -s)

#Training data
trainer <- for_modeling_3 %>%
  filter(week %in% train_weeks)
#Validation data
valer <- for_modeling_3 %>%
  filter(week == val_week)
#Testing data
tester <- for_modeling_3 %>%
  filter(week == test_week)


#Hyperparameters
nrounds <- 90
params <-
  list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = c("logloss"),
    eta = .10,
    gamma = .025,
    subsample = 1,
    max_depth = 12,
    min_child_weight = 12,
    colsample_bytree = .95
  )

#Train the model
full_train <- xgboost::xgb.DMatrix(as.matrix(trainer %>% select(-is_tackler_solo, -is_tackler_assist, -is_tackler_solo_ob,
                                                                -is_tackler_asst_ob, -gameId, -playId, -nflId, -frameId,
                                                                -week)),
                                   label = trainer$is_tackler_asst_ob
)

et_model_3 <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)

preds <- as.data.frame(
  matrix(predict(et_model_3, as.matrix(tester %>% select(-is_tackler_solo, -is_tackler_assist, -is_tackler_solo_ob,
                                                         -is_tackler_asst_ob, -gameId, -playId, -nflId, -frameId,
                                                         -week))))
) %>%
  dplyr::rename(x_tackle_assist_ob = V1)

cv_results <- bind_cols(test_data, preds) %>% mutate(week = x)

#Get error
check <- cv_results %>%
  mutate(bin = round(x_tackle_assist_ob / 0.05) * 0.05,
         in_cz = ifelse(distance_from_ball <= 3, 1, 0)) %>%
  group_by(in_cz, bin) %>%
  summarize(n_frames = n(),
            n_tackles = sum(is_tackler_asst_ob),
            bin_real_prob = n_tackles / n_frames)

print(check %>%
        ggplot() +
        geom_point(aes(x = bin, y = bin_real_prob, size = n_frames)) +
        geom_smooth(aes(x = bin, y = bin_real_prob), method = "loess") +
        geom_abline(slope = 1, intercept = 0, color = "black", lty = 2) +
        coord_equal() +
        scale_x_continuous(limits = c(0, 1)) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(
          size = "Number of Frames",
          x = "Estimated Tackle probability",
          y = "Observed Tackle probability"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_text(size = 10, angle = 90),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.position = "bottom"
        ) +
        facet_wrap(~in_cz))

cv_cal_error <- check %>%
  ungroup() %>%
  mutate(cal_diff = abs(bin - bin_real_prob)) %>%
  summarize(
    weight_cal_error = weighted.mean(cal_diff, n_frames, na.rm = TRUE)
  )
# errors_3 <- c(errors_3, round(with(cv_cal_error, mean(weight_cal_error)), 4))
print(round(with(cv_cal_error, mean(weight_cal_error)), 4))
glue::glue(
  "
    --CALIBRATION ERROR--
    
    {round(with(cv_cal_error, mean(weight_cal_error)), 4)}
    
    "
)

#Feature importance
et_importance_3 <- xgb.importance(model = et_model_3)
xgb.plot.importance(importance_matrix = et_importance_3[1:20])



saveRDS(et_model_3, "C:/Users/Joshua/Desktop/BDB 2023-24/Expected Tackles Model Inc Assist + Nearest OB V15.rds")
