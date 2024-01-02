#FUMBLE OPTIMIZATION METRICS/COMBINING EVERYTHING
#FIGURES 6, 8, 9

library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)
library(nflverse)
library(gridExtra)
library(ggrepel)
library(gt)
library(ggimage)
library(gtExtras)
library(e1071)

#Fit all the data to the Expected Tackles model
x_tackles <- readRDS("C:/Users/Joshua/Desktop/BDB 2023-24/Expected Tackles Model Inc Assist + Nearest OB V15.rds")

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

preds <- as.data.frame(
  matrix(predict(x_tackles, as.matrix(for_modeling_3 %>% select(-is_tackler_solo, -is_tackler_assist, -is_tackler_solo_ob,
                                                                -is_tackler_asst_ob, -gameId, -playId, -nflId, -frameId,
                                                                -week))))
) %>%
  dplyr::rename(x_tackle_assist_ob = V1)

#Bind the predictions to the data and include the first included frame of each play for time to tackle calculations
cv_results_0 <- bind_cols(model_molder, preds)
cv_results_pre <- cv_results_0 %>% 
  group_by(gameId, playId) %>%
  summarize(first_frame = min(frameId))
cv_results <- merge(cv_results_0, cv_results_pre, by = c("gameId", "playId"))
rm(cv_results_pre)
rm(cv_results_0)
#Find where the fumbles happen
fumble_plays <- tackles %>%
  group_by(gameId, playId) %>%
  summarize(fumble = sum(forcedFumble)) %>%
  mutate(fumble = ifelse(fumble > 1, 1, ifelse(is.na(fumble), 0, fumble)))

#Forced Fumble Rate at 3 yards
cz_thresh <- 3
fum_tack_td <- left_join(tack_td, fumble_plays, by = c("gameId", "playId")) %>%
  mutate(fumble = ifelse(is.na(fumble), 0, fumble)) %>%
  merge(cv_results, by.x = c("gameId", "playId", "tackle_frame"), by.y = c("gameId", "playId", "frameId")) %>%
  mutate(zone = ifelse(distance_from_ball <= cz_thresh, 1, 0),
         tackle_time = (tackle_frame - first_frame)/10) %>%
  select(gameId, playId, called_pass, first_frame, tackle_frame, tackle_time, distance_rank, distance_from_ball, zone, fumble, seconds_carry)
tack_zones <- fum_tack_td %>%
  group_by(gameId, playId) %>%
  summarize(fumble = max(fumble),
            threat_count = sum(zone),
            time_to_tackle = first(tackle_time))
fumble_number <- sum(tack_zones$fumble)
fumble_freq <- mean(tack_zones$fumble)
total_plays <- length(tack_zones$fumble)
threat_fumbles <- tack_zones %>%
  filter(threat_count > 0) %>%
  mutate(threats = ifelse(threat_count >= 4, "4+", as.character(threat_count))) %>%
  group_by(threats) %>%
  summarize(play_count = n(),
            fumble_count = sum(fumble),
            fumble_perc = fumble_count/play_count,
            perc_of_fumbles = fumble_count/fumble_number,
            play_perc = play_count/total_plays,
            bayes_fumble = fumble_perc * fumble_freq / play_perc,
            thresh = cz_thresh,
            span = mean(time_to_tackle))

#Find the plays with fumbles in the tracking data
data_fum <- data %>%
  group_by(gameId, playId, possessionTeam, defensiveTeam) %>%
  summarize(desc = first(playDescription),
            fumble = ifelse(sum(forcedFumble, na.rm = TRUE) >= 1, 1, 0)) %>%
  filter(fumble == 1)

#Use the text of the play description to find the part of the text where it says who recovered the fumble
after_last_reversal <- function(inp){
  parts <- strsplit(inp, "REVERSED")
  set_up <- parts[[1]][length(parts[[1]])]
  parts2 <- strsplit(set_up, "FUMBLES")
  set_up2 <- (parts2[[1]][2])
  return(set_up2)
}

#Get the team that recovered
get_rec_team <- function(inp2){
  return(str_match(toupper(inp2), "RECOVERED BY (\\w+)")[1,2])
}

#Apply the above 2 functions to the tracking data and determine if the defense recovered the fumble
data_fum <- data_fum %>%
  mutate(fum_text = after_last_reversal(desc),
         rec_team = case_when(grepl("and recovers", fum_text) ~ possessionTeam,
                              grepl("ball out of bounds", fum_text) ~ possessionTeam,
                              grepl("RECOVERED BY", toupper(fum_text)) ~ get_rec_team(fum_text),
                              T ~ "BLLLLL"),
         rec_team = case_when(rec_team == "BLT" ~ "BAL",
                              rec_team == "ARZ" ~ "ARI",
                              rec_team == "HST" ~ "HOU",
                              rec_team == "CLV" ~ "CLE",
                              T ~ rec_team),
         def_rec = ifelse(rec_team == defensiveTeam, 1, 0))

#Get the general defensive fumble recovery rate
recovery_rate <- mean(data_fum$def_rec)

#Combine fumble recovery data with some other data from the initial data cleaning to get what we need for analysis
fumbler <- merge(data_fum, fum_tack_td, by = c("gameId", "playId", "fumble")) %>%
  select(-zone)

#Fumble Recovery Rates inside 3 yards
cz_thresh <- 3
fum_rec <- fumbler %>%
  mutate(contact_zone = ifelse(distance_from_ball <= cz_thresh, 1, 0)) %>%
  group_by(gameId, playId) %>%
  summarize(threat_count = sum(contact_zone),
            def_rec = max(def_rec),
            time_to_tackle = first(tackle_time))
total_fumbles <- length(fum_rec$threat_count)
recovery_rate <- mean(fum_rec$def_rec)
def_recoveries <- sum(fum_rec$def_rec)
recovery <- fum_rec %>%
  filter(threat_count > 0) %>%
  mutate(threats = ifelse(threat_count >= 4, "4+", as.character(threat_count))) %>%
  group_by(threats) %>%
  summarize(fumbles = n(),
            recs = sum(def_rec),
            rec_perc = mean(def_rec),
            p_threat_rec = recs/def_recoveries,
            p_threat = fumbles / total_fumbles,
            bayes_recover = p_threat_rec * recovery_rate / p_threat,
            span = mean(time_to_tackle)) %>%
  mutate(thresh = cz_thresh)

#Merge fumbles and recoveries for analysis creating recovered fumbles
fumble_recover <- merge(recovery, threat_fumbles, by = c("threats", "thresh")) %>%
  rename(to_tackle = span.x, to_fumble = span.y) %>%
  arrange(thresh, threats) %>%
  mutate(turnover_prob = rec_perc * fumble_perc,
         turn_prev = lag(turnover_prob),
         turn_inc = turnover_prob - turn_prev,
         turn_inc_perc = turn_inc/turn_prev,
         turn_inc_graph = ifelse(threats == 1, paste(round(turnover_prob * 100, 2), "%", sep = ""), paste(round(turn_inc_perc * 100, 0), "%", sep = "")),
         turn_inc_graph = ifelse(threats %in% c("2", "3"), paste("+", turn_inc_graph, sep = ""), turn_inc_graph))

#Reduce down to just 1, 2, or 3 players
fum_rec_graph <- fumble_recover %>%
  filter(threats != "4+")

#Graph it - FIGURE 6
fum_rec_graph %>%
  ggplot(aes(x = threats, y = turnover_prob * 100)) +
  geom_bar(stat = "identity", fill = "#FB4F14", color = "#000000") +
  geom_text(aes(label = paste(round(turnover_prob * 100, 2), "%", sep = "")), vjust = 2, fontface = "bold", size = 10) +
  geom_segment(aes(x = as.numeric(lag(threats)) + .2, xend = as.numeric(lag(threats)) + .8,
                   y = lag(turnover_prob) * 80, yend = turnover_prob * 85),
               arrow = arrow(length = unit(0.5, "cm")), color = "black", linewidth = 2) +
  labs(x = "Players Around the Ball", y = "Fumble + Recovery Probability",
       caption = paste(fum_rec_graph$play_count[1], "Plays with 1 Defender Around the Ball,", fum_rec_graph$play_count[2], "Plays with 2 Defenders,", fum_rec_graph$play_count[3], "Plays with 3 Defenders\nTrends Hold on Both Run and Pass Plays, with Time to Tackle Increasing as Each Player Arrives at the Ball on Pass Plays and Decreasing on Run Plays")) +
  ggtitle("Turnover Percentage by Number of Players Within 3 Yards of the Ball At the Time of the Tackle",
          "More Players Around the Ball Create More Turnovers") +
  theme(panel.background = element_rect(fill = "#7eaf34"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(size = 10, face = "bold", color = "black"),
        axis.title = element_text(size = 12, face = "bold", color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(face = "bold.italic", size = 12),
        plot.caption = element_text(face = "bold", size = 9, hjust = 0)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1),
                     limits = c(0, 1.25))

rm(fum_rec_graph)


#BALL BOTHERING
#Find how frequently at least 3 players are within 3 or 6 yards from the ball at the tackle
ball_bother <- merge(tack_td, cv_results, by.x = c("gameId", "playId", "tackle_frame"), by.y = c("gameId", "playId", "frameId"))
ball_bother <- ball_bother %>%
  mutate(inside_3 = ifelse(distance_from_ball <= 3, 1, 0),
         inside_6 = ifelse(distance_from_ball <= 6, 1, 0)) %>%
  select(gameId, playId, defensiveTeam, called_pass, distance_rank, distance_from_ball, inside_3, inside_6) %>%
  group_by(gameId, playId, defensiveTeam, called_pass) %>%
  summarize(num_in_3 = sum(inside_3),
            num_in_6 = sum(inside_6),
            over_3_3 = ifelse(num_in_3 >= 3, 1, 0),
            over_3_6 = ifelse(num_in_6 >= 3, 1, 0))

#Separate into Run and Pass Plays
p_bother <- ball_bother %>%
  filter(called_pass == 1)
r_bother <- ball_bother %>%
  filter(called_pass == 0)

#Ball Annoyance Index - At Least 3 Players Within 6 Yards, Separated by Run and Pass and then Combined
r_annoyer <- r_bother %>%
  group_by(defensiveTeam) %>%
  summarize(plays = n(),
            over_3_count = sum(over_3_6),
            annoy_perc = over_3_count/plays) %>%
  arrange(-annoy_perc) %>%
  mutate(ann_rank = row_number(),
         ann_vs_min = annoy_perc - min(annoy_perc),
         annoy_index = ann_vs_min*10/max(ann_vs_min))
r_annoy_team <- r_annoyer %>%
  select(defensiveTeam, annoy_perc, ann_rank, ann_vs_min, annoy_index) %>%
  rename(team = defensiveTeam, r_annoy_perc = annoy_perc, r_ann_rank = ann_rank, r_ann_vs_min = ann_vs_min, r_annoy_index = annoy_index)

p_annoyer <- p_bother %>%
  group_by(defensiveTeam) %>%
  summarize(plays = n(),
            over_3_count = sum(over_3_6),
            annoy_perc = over_3_count/plays) %>%
  arrange(-annoy_perc) %>%
  mutate(ann_rank = row_number(),
         ann_vs_min = annoy_perc - min(annoy_perc),
         annoy_index = ann_vs_min*10/max(ann_vs_min))
p_annoy_team <- p_annoyer %>%
  select(defensiveTeam, annoy_perc, ann_rank, ann_vs_min, annoy_index) %>%
  rename(team = defensiveTeam, p_annoy_perc = annoy_perc, p_ann_rank = ann_rank, p_ann_vs_min = ann_vs_min, p_annoy_index = annoy_index)

annoy_team <- merge(r_annoy_team, p_annoy_team, by = c("team")) %>%
  mutate(annoy = (r_annoy_perc + p_annoy_perc)/2) %>%
  arrange(-annoy) %>%
  mutate(ann_rank = row_number(),
         ann_vs_min = annoy - min(annoy),
         annoy_index = ann_vs_min * 10/max(ann_vs_min))

#Ball Harassment Index - At Least 3 Players Within 3 Yards, Separated by Run and Pass then Combined
r_harrasser <- r_bother %>%
  group_by(defensiveTeam) %>%
  summarize(plays = n(),
            over_3_count = sum(over_3_3),
            harr_perc = over_3_count/plays) %>%
  arrange(-harr_perc) %>%
  mutate(harr_rank = row_number(),
         harr_vs_min = harr_perc - min(harr_perc),
         harr_index = harr_vs_min*10/max(harr_vs_min))
r_harrass_team <- r_harrasser %>%
  select(defensiveTeam, harr_perc, harr_rank, harr_vs_min, harr_index) %>%
  rename(team = defensiveTeam, r_harr_perc = harr_perc, r_harr_rank = harr_rank, r_harr_vs_min = harr_vs_min, r_harr_index = harr_index)

p_harrasser <- p_bother %>%
  group_by(defensiveTeam) %>%
  summarize(plays = n(),
            over_3_count = sum(over_3_3),
            harr_perc = over_3_count/plays) %>%
  arrange(-harr_perc) %>%
  mutate(harr_rank = row_number(),
         harr_vs_min = harr_perc - min(harr_perc),
         harr_index = harr_vs_min*10/max(harr_vs_min))
p_harrass_team <- p_harrasser %>%
  select(defensiveTeam, harr_perc, harr_rank, harr_vs_min, harr_index) %>%
  rename(team = defensiveTeam, p_harr_perc = harr_perc, p_harr_rank = harr_rank, p_harr_vs_min = harr_vs_min, p_harr_index = harr_index)

harrass_team <- merge(r_harrass_team, p_harrass_team, by = c("team")) %>%
  mutate(harrass = (r_harr_perc + p_harr_perc)/2) %>%
  arrange(-harrass) %>%
  mutate(harr_rank = row_number(),
         harr_vs_min = harrass - min(harrass),
         harr_index = harr_vs_min * 10/max(harr_vs_min))

#Combine Harassment and Annoyance to Create Ball Bothering Index
bother_index <- merge(annoy_team, harrass_team, by = c("team")) %>%
  arrange(-(annoy_index + harr_index)) %>%
  mutate(bother_perc = (harrass * 2 + annoy)/3,
         bother_vs_min = bother_perc - min(bother_perc),
         bother_index = bother_vs_min*10/max(bother_vs_min)) %>%
  arrange(-bother_index) %>%
  mutate(bother_rank = row_number())

#Merge with NFLVerse data for abbreviations and logos
bother_grapher <-  merge(bother_index, load_teams(current = TRUE), by.x = c("team"), by.y = c("team_abbr"))

#Dot Plot Harassment vs Annoyance
bother_1 <- bother_grapher %>%
  ggplot(aes(y = annoy_index, x = harr_index)) +
  geom_abline(intercept = 0, slope = 1, linetype = "longdash", linewidth = 0.75, color = "grey") +
  # geom_point() +
  geom_vline(xintercept = 5, color = "red", linewidth = 0.75, linetype = "longdash") +
  geom_hline(yintercept = 5, color = "red", linewidth = 0.75, linetype = "longdash") +
  # scale_color_manual(values = primary_colors) +
  scale_x_continuous(breaks = 0:10, labels = 0:10) +
  scale_y_continuous(breaks = 0:10, labels = 0:10) +
  labs(y = "Ball Harassment Index", x = "Ball Annoyance Index") +
  ggtitle("Bothering the Ballcarrier",
          "Harassment: At least 3 players within 3 yards of the ball at the time of the tackle\nAnnoyance: At least 3 players within 6 yards of the ball at the time of the tackle") +
  geom_rect(aes(xmin = 9, xmax = 10, ymin = 9, ymax = 10),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = 9.5, y = 9.5, label = "All Over\nThe Ball", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = 8.7, xmax = 10.3, ymin = 0, ymax = 1),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = 9.5, y = 0.5, label = "Fly Around\nNot Fast Enough", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = -0.2, xmax = 1.2, ymin = 9, ymax = 10),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = 0.5, y = 9.5, label = "All Pressure\nIs Heavy", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = -.2, xmax = 1.2, ymin = 0.75, ymax = 1.25),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = 0.5, y = 1, label = "No Pressure", color = "red", fontface = "bold") +
  geom_image(aes(image = team_logo_wikipedia), asp = 16/9) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA),
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", size = 12, hjust = 0.5),
        plot.background = element_rect(fill = "#FB4F14"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))

#Bar Graph Bothering Rankings
bother_2 <- bother_grapher %>%
  ggplot(aes(x = reorder(team, -bother_index), y = bother_index)) +
  geom_bar(stat = "identity", aes(fill = team, color = team)) +
  scale_color_manual(values = secondary_colors) +
  scale_fill_manual(values = primary_colors) +
  scale_y_continuous(breaks = 0:10, labels = 0:10) +
  geom_text(aes(label = bother_rank), vjust = -0.5, fontface = "bold") +
  labs(y = "Ball Bothering Index",
       caption = "Harassment: At least 3 players within 3 yards of the ball at the time of the tackle\nAnnoyance: At least 3 players within 6 yards of the ball at the time of the tackle\nBothering: (Harassment * 2 + Annoyance)/3\nTeam-Seasons Indexed from 0-10") +
  ggtitle("Ball Bothering Ratings",
          "Ball Bothering: Weighted Average of Ball Harassment and Ball Annoyance") +
  theme(axis.text.x = nflplotR::element_nfl_logo(size = 1),
        axis.title.x = ggplot2::element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
        plot.subtitle = element_text(face = "bold.italic", size = 12, hjust = 0.5),
        plot.background = element_rect(fill = "#FB4F14"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", face = "bold"),
        axis.title.y = element_text(face = "bold"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "white"),
        plot.caption = element_text(face = "bold", size = 9, hjust = 0))

#Combine bother_1 and bother_2 into 1 graphic - FIGURE 7
grid.arrange(bother_1, bother_2, ncol = 1)


#Put all the team level stats together in one dataframe
purs_tack <- merge(pursuit_rp, toe_cz_team, by = c("defensiveTeam"))
max_purs <- merge(purs_tack, bother_index, by.x = c("defensiveTeam"), by.y = c("team")) %>%
  merge(load_teams(current = T), by.x = c("defensiveTeam"), by.y = c("team_abbr")) %>%
  select(defensiveTeam, pursuit_pass, pursuit_run, toe_play, cz_count, avg_purs, purs_norm, pass_rate, purs_relative, r_annoy_index, p_annoy_index, annoy_index, r_harr_index, p_harr_index, harr_index, bother_index, team_logo_wikipedia)

#Regress Ball Bothering Index on Team Pursuit and Generate Numbers to Use for Graph
bother_pursuit_lm <- summary(lm(bother_index ~ purs_relative, data = max_purs))
bother_pursuit_b0 <- bother_pursuit_lm$coefficients[1]
bother_pursuit_b1 <- bother_pursuit_lm$coefficients[2]
bother_pursuit_rsq <- bother_pursuit_lm$r.squared

#Pursuit vs Ball Bothering With Regression - FIGURE 8
max_purs %>%
  ggplot(aes(x = purs_relative , y = bother_index)) +
  geom_abline(intercept = bother_pursuit_b0, slope = bother_pursuit_b1, linetype = "longdash", linewidth = 1, color = "blue") +
  geom_vline(xintercept = mean(max_purs$purs_relative), linetype = "longdash", linewidth = 1, color = "red") +
  geom_hline(yintercept = 5, linetype = "longdash", linewidth = 1, color = "red") +
  geom_image(aes(image = team_logo_wikipedia), asp = 16/9) +
  labs(x = "Team Pursuit", y = "Ball Bothering Index", caption = paste("R^2", round(bother_pursuit_rsq, 2), sep = " = ")) +
  ggtitle("Relationship Between Team Pursuit and Optimizing Recovered Fumble Creation",
          "Above the Line: Players gain less tackle probability but are frequently around the ball\nBelow the Line: Players gain more expected tackles but are not near the ball as often") +
  scale_y_continuous(breaks = 0:10, labels = 0:10) +
  geom_rect(aes(xmin = min(max_purs$purs_relative) - .01, xmax = min(max_purs$purs_relative) + .025, ymin = 9, ymax = 10),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = min(max_purs$purs_relative) + .0075, y = 9.5, label = "May Not Need\nTo Fly Around", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = min(max_purs$purs_relative) - .01, xmax = min(max_purs$purs_relative) + .025, ymin = 0, ymax = 1),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = min(max_purs$purs_relative) + .0075, y = 0.5, label = "Limited\nBall Pressure", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = max(max_purs$purs_relative) - .05, xmax = max(max_purs$purs_relative) - .025, ymin = 9, ymax = 10),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = max(max_purs$purs_relative) - 0.0375, y = 9.5, label = "ATTACK", color = "red", fontface = "bold") +
  geom_rect(aes(xmin = max(max_purs$purs_relative) - .065, xmax = max(max_purs$purs_relative) - .01, ymin = 1, ymax = 0),
            fill = "white", color = "black",
            linetype = "longdash") +
  geom_text(x = max(max_purs$purs_relative) - 0.0375, y = 0.5, label = "Fly Around\nDon't Get 3 to the Ball", color = "red", fontface = "bold") +
  theme(legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "#FB4F14"),
        axis.ticks = element_line(color = "black"),
        axis.text = element_text(color = "black", face = "bold"),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(face = "bold.italic", size = 12),
        plot.caption = element_text(face = "bold", size = 9, hjust = 0))

#Check how all the team level metrics correlate
cor(max_purs %>% select(-defensiveTeam, -team_logo_wikipedia))
