#TEAM PURSUIT METRICS
#FIGURE 4

library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)
library(nflverse)

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

cv_results <- bind_cols(model_molder, preds)

#Get player names
player_names <- players %>%
  select(nflId, displayName)

#Attach expected tacklers and player names to each player for each frame
team_tax <- cv_results %>%
  group_by(gameId, playId, frameId) %>%
  summarize(total_x_tack = sum(x_tackle_assist_ob)) %>%
  merge(cv_results, by = c("gameId", "playId", "frameId")) %>%
  merge(player_names, by = c("nflId"))

#Generate every number we need for the pursuit stats
max_tax_player <- team_tax %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId, defensiveTeam, nflId) %>%
  summarize(player_name = last(displayName),
            first_xtack = first(x_tackle_assist_ob),
            max_xtack = max(x_tackle_assist_ob),
            max_x_tacklers = max(total_x_tack),
            inital_x_tacklers = first(total_x_tack),
            final_x_tacklers = last(total_x_tack),
            is_pass = first(called_pass)) %>%
  mutate(net_p_xtack = max_xtack - first_xtack)

#Make Player Net for Each Play, Separated by Run vs Pass
team_player_net <- max_tax_player %>%
  group_by(gameId, playId, defensiveTeam, is_pass) %>%
  summarize(team_player_net_max_xtack = sum(net_p_xtack))

#Average for Each Team, Separated by Run vs Pass
team_player_net_rankings <- team_player_net %>%
  group_by(defensiveTeam, is_pass) %>%
  summarize(team_player_net_max_play = mean(team_player_net_max_xtack),
            num_plays = n()) %>%
  arrange(team_player_net_max_play)
View(team_player_net_rankings)

#Add NFLVerse data for abbreviations, logos
team_player_net_nflverse <- merge(team_player_net_rankings, load_teams(current = TRUE), by.x = c("defensiveTeam"), by.y = c("team_abbr"))

#Team Pursuit - Run Plays Only
team_player_net_nflverse %>%
  filter(is_pass == 0) %>%
  ggplot(aes(x = reorder(defensiveTeam, -team_player_net_max_play), y = team_player_net_max_play)) +
  geom_bar(stat = "identity", aes(color = defensiveTeam, fill = defensiveTeam), width = .75) +
  scale_color_manual(values = secondary_colors) +
  scale_fill_manual(values = primary_colors) +
  labs(y = "Team Pursuit") +
  ggtitle("Pursuit - Player Net Expected Tacklers per Play: Called Run Plays Only",
          "Player Net: Maximum Expected Tackles - Expected Tackles When the Target is Set, Each Player Counted Separately") +
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

#Team Pursuit - Pass Plays Only
team_player_net_nflverse %>%
  filter(is_pass == 1) %>%
  ggplot(aes(x = reorder(defensiveTeam, -team_player_net_max_play), y = team_player_net_max_play)) +
  geom_bar(stat = "identity", aes(color = defensiveTeam, fill = defensiveTeam), width = .75) +
  scale_color_manual(values = secondary_colors) +
  scale_fill_manual(values = primary_colors) +
  labs(y = "Team Pursuit") +
  ggtitle("Pursuit - Player Net Expected Tacklers per Play: Called Pass Plays Only",
          "Player Net: Maximum Expected Tackles - Expected Tackles When the Target is Set, Each Player Counted Separately") +
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



#Separate Pursuit into run vs pass in a way that can easily be combined
player_net_r <- team_player_net_rankings %>%
  filter(is_pass == 0) %>%
  rename(pursuit_run = team_player_net_max_play, run_plays = num_plays) %>%
  select(-is_pass)
player_net_r <- player_net_r %>%
  mutate(run_vs_avg = pursuit_run - mean(player_net_r$pursuit_run))


player_net_p <- team_player_net_rankings %>%
  filter(is_pass == 1) %>%
  rename(pursuit_pass = team_player_net_max_play, pass_plays = num_plays) %>%
  select(-is_pass)
player_net_p <- player_net_p %>%
  mutate(pass_vs_avg = pursuit_pass - mean(player_net_p$pursuit_pass))
# mean(player_net_r$pursuit_run)
# mean(player_net_p$pursuit_pass)

#Combine them and generate Relative Player Net Team Pursuit
pursuit_rp <- merge(player_net_r, player_net_p, by = c("defensiveTeam")) %>%
  mutate(avg_purs = (pursuit_run + pursuit_pass) / 2,
         purs_norm = ((pursuit_run * run_plays) + (pursuit_pass * pass_plays))/(run_plays + pass_plays),
         pass_rate = pass_plays/(run_plays + pass_plays),
         purs_relative = (run_vs_avg + pass_vs_avg) / 2)


#Add NFLVerse data for abbreviations, logos
pursuit_rp_player_net_nflverse <- merge(pursuit_rp, load_teams(current = TRUE), by.x = c("defensiveTeam"), by.y = c("team_abbr"))

#Graph Relative Player Net Team Pursuit Rankings
pursuit_rp_player_net_nflverse %>%
  ggplot(aes(x = reorder(defensiveTeam, -purs_relative), y = purs_relative)) +
  geom_bar(stat = "identity", aes(color = defensiveTeam, fill = defensiveTeam), width = .75) +
  scale_color_manual(values = secondary_colors) +
  scale_fill_manual(values = primary_colors) +
  labs(y = "Team Pursuit") +
  ggtitle("Pursuit - Relative Player Net Expected Tacklers per Play") +
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
