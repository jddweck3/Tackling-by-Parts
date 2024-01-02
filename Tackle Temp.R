#TACKLING METRICS - TEAM AND PLAYER LEVEL
#FIGURES 2, 3



library(tidyverse)
library(xgboost)
library(caret)
library(doParallel)
library(nflverse)
library(ggrepel)
library(gt)
library(ggimage)
library(gtExtras)

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

#Get the number of plays and games for each player
play_counts <- cv_results %>%
  mutate(gp = paste(gameId, playId)) %>%
  group_by(nflId) %>%
  summarize(play_num = length(unique(gp)),
            games_played = length(unique(gameId)))

#Get the number of plays with a contact zone for each player
zone_counts <- cv_results %>%
  mutate(gp = paste(gameId, playId)) %>%
  group_by(nflId) %>%
  summarize(plays_w_cz = length(unique(gp)))

#Merge the data onto the contact zones by the first frame of the contact zone
contact_zone_preds <- merge(cv_results, contact_zones, by.x = c("gameId", "playId", "nflId", "frameId", "week", "defensiveTeam"),
                            by.y = c("gameId", "playId", "nflId", "first_frame", "week", "team")) %>%
  mutate(make_tackle = ifelse(is_tackler_now == 1 | (is_tackler == 1 & frameId >= last_tackler), 1, 0),
         tack_oe = make_tackle - x_tackle_assist_ob)

#Generate all the per contact zone stats
toe_cz_play <- contact_zone_preds %>%
  mutate(gp = paste(gameId, playId)) %>%
  group_by(nflId) %>%
  summarize(player_name = last(player_name),
            player_team = last(defensiveTeam),
            player_position = last(player_position),
            toe_play = mean(tack_oe),
            avg_exp_tack = mean(x_tackle_assist_ob),
            cz_count = n(),
            tackles = sum(make_tackle),
            plays_w_cz = length(unique(gp))) %>%
  arrange(-toe_play)

#Put everything together and make the rankings with all the data you will need to visualize it
official_toe_rankings <- toe_cz_play %>%
  # filter(plays_w_cz > 20) %>%
  # mutate(rank = row_number()) %>%
  select(player_team, player_name, nflId, player_position, tackles, cz_count, plays_w_cz, avg_exp_tack, toe_play) %>%
  merge(play_counts, by = c("nflId")) %>%
  mutate(cz_play_perc = ifelse(plays_w_cz/play_num < .1, paste("0", round(plays_w_cz * 100/play_num, 1), "%", sep = ""), paste(round(plays_w_cz * 100/play_num, 1), "%", sep = "")))

#Rankings by position
toe_cz_pos <- contact_zone_preds %>%
  group_by(player_position) %>%
  summarize(toe_play = mean(tack_oe),
            cz_count = n(),
            tackles = sum(make_tackle)) %>%
  arrange(-toe_play) %>%
  mutate(rank = row_number()) %>%
  select(rank, player_position, tackles, cz_count, toe_play)

#Rankings by team
toe_cz_team <- contact_zone_preds %>%
  group_by(defensiveTeam) %>%
  summarize(toe_play = mean(tack_oe),
            cz_count = n(),
            tackles = sum(make_tackle)) %>%
  arrange(-toe_play) %>%
  mutate(rank = row_number()) %>%
  select(rank, defensiveTeam, tackles, cz_count, toe_play)

#Team colors for graphs
primary_colors <- c(
  "BAL" = "#241773",
  "CIN" = "#FB4F14",
  "CLE" = "#311D00",
  "PIT" = "#FFB612",
  "BUF" = "#00338D",
  "MIA" = "#008E97",
  "NE" = "#002244",
  "NYJ" = "#125740",
  "HOU" = "#03202F",
  "IND" = "#002C5F",
  "JAX" = "#006778",
  "TEN" = "#4B92DB",
  "DEN" = "#FB4F14",
  "KC" = "#E31837",
  "LV" = "#000000",
  "LAC" = "#0080C6",
  "CHI" = "#C83803",
  "DET" = "#0076B6",
  "GB" = "#203731",
  "MIN" = "#4F2683",
  "DAL" = "#003594",
  "NYG" = "#0B2265",
  "PHI" = "#004C54",
  "WAS" = "#5A1414",
  "ATL" = "#A71930",
  "CAR" = "#0085CA",
  "NO" = "#D3BC8D",
  "TB" = "#D50A0A",
  "ARI" = "#97233F",
  "LA" = "#003594",
  "SF" = "#AA0000",
  "SEA" = "#002244"
)
secondary_colors <- c(
  "BAL" = "#000000",
  "CIN" = "#000000",
  "CLE" = "#FF3C00",
  "PIT" = "#101820",
  "BUF" = "#C60C30",
  "MIA" = "#FC4C02",
  "NE" = "#C60C30",
  "NYJ" = "#FFFFFF",
  "HOU" = "#A71930",
  "IND" = "#A2AAAD",
  "JAX" = "#9F792C",
  "TEN" = "#8A8D8F",
  "DEN" = "#002244",
  "KC" = "#FFB81C",
  "LV" = "#A5ACAF",
  "LAC" = "#FFC20E",
  "CHI" = "#0B162A",
  "DET" = "#B0B7BC",
  "GB" = "#FFB612",
  "MIN" = "#FFC62F",
  "DAL" = "#7F9695",
  "NYG" = "#A71930",
  "PHI" = "#A5ACAF",
  "WAS" = "#FFB612",
  "ATL" = "#000000",
  "CAR" = "#101820",
  "NO" = "#101820",
  "TB" = "#B1BABF",
  "ARI" = "#000000",
  "LA" = "#FF8200",
  "SF" = "#B3995D",
  "SEA" = "#69BE28"
)

#Merge with NFLVerse data for team names/logos
toe_cz_team_nflverse <- merge(toe_cz_team, load_teams(current = TRUE), by.x = c("defensiveTeam"), by.y = c("team_abbr"))

#Team tackling rankings bar chart (FIGURE 3)
toe_cz_team_nflverse %>%
  arrange(-toe_play) %>%
  mutate(rank = row_number()) %>%
  # arrange(gameId, playId, frameId) %>%
  ggplot(aes(x = reorder(defensiveTeam, -toe_play), y = toe_play)) +
  geom_bar(stat = "identity", aes(color = defensiveTeam, fill = defensiveTeam), width = .75) +
  geom_text(aes(label = rank), vjust = -1) +
  scale_color_manual(values = secondary_colors) +
  scale_fill_manual(values = primary_colors) +
  labs(y = "Team Tacklling") +
  ggtitle("Tackling - Team Tackles Over Expected Per Contact Zone") +
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

#Merge the player rankings with the players from the 2022 season, clean everything up to make it look good in the chart,
#and calculate tackles per contact zone, expected tackles and total tackles over expected, and rankings by total tackles
official_toe_rankings_nflverse <- official_toe_rankings %>%
  merge(load_rosters(seasons = c(2022)), by.x = c("nflId"), by.y = c("gsis_it_id")) %>%
  mutate(exp_tackles = round(avg_exp_tack * cz_count, 2),
         tackles_oe = round(tackles - exp_tackles, 2),
         toe_play = round(toe_play, 3),
         avg_exp_tack = round(avg_exp_tack, 3),
         years_exp = ifelse(years_exp == 0, "R", as.character(years_exp)),
         height_ft = floor(as.numeric(height) / 12),
         height_in = as.numeric(height) %% 12,
         height = paste(height_ft, "'", height_in, '"', sep = ""),
         college = gsub("&amp;", "&", college),
         per_zone = round(tackles/cz_count, 3)) %>%
  merge(load_teams(current = TRUE), by.x = c("team"), by.y = c("team_abbr")) %>%
  filter(games_played >= 5, play_num >= games_played * 10) %>%
  arrange(-tackles_oe) %>%
  mutate(total_toe_rank = row_number(),
         posnum = case_when(position == "DL" ~ 0,
                            position == "LB" ~ 1,
                            position == "DB" ~ 2),
         position = case_when(position == "DL" ~ "DEFENSIVE LINEMEN",
                              position == "LB" ~ "LINEBACKERS",
                              position == "DB" ~ "DEFENSIVE BACKS")) %>%
  arrange(-toe_play) %>%
  mutate(rank = row_number())

official_toe_rankings_nflverse$position <- factor(official_toe_rankings_nflverse$position, levels = c("DEFENSIVE LINEMEN", "LINEBACKERS", "DEFENSIVE BACKS"))

#Generate means of tackles over expected and expected tackles by position
mean_toe_values <- aggregate(toe_play ~ position, data = official_toe_rankings_nflverse, FUN = median)
mean_xtack_values <- aggregate(avg_exp_tack ~ position, data = official_toe_rankings_nflverse, FUN = median)

#Expected Tackles vs Tackles Over Expected Per Contact Zone, Faceted By Position (FIGURE 1)
official_toe_rankings_nflverse %>%
  arrange(posnum) %>%
  ggplot(aes(x = avg_exp_tack, y = toe_play)) +
  # geom_hline(aes(yintercept = median(toe_play)), linetype = "longdash", linewidth = 1, color = "darkgray") +
  geom_hline(data = mean_toe_values, aes(yintercept = toe_play), linetype = "longdash", linewidth = 1, color = "darkgray") +
  geom_vline(data = mean_xtack_values, aes(xintercept = avg_exp_tack), linetype = "longdash", linewidth = 1, color = "darkgray") +
  geom_point(aes(color = player_team, fill = player_team, size = cz_count)) +
  labs(x = "Expected Tackles per Contact Zone",
       y = "Tackles Over Expected per Contact Zone",
       caption = "Point Size = Total Contact Zones\nGrey Dashed Lines at Median by Position") +
  ggtitle("Tackler Evaluation by Position",
          "Expected Tackles per Contact Zone vs Tackles Over Expected per Contact Zone\nAt least 5 games played in Weeks 1-9 of 2022, at least 10 plays per game") +
  scale_color_manual(values = primary_colors) +
  scale_fill_manual(values = secondary_colors) +
  geom_text_repel(aes(label = player_name), fontface = "bold") +
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
        plot.title = element_text(face = "bold", size = 21, hjust = 0.5),
        plot.subtitle = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.caption = element_text(face = "bold", size = 9, hjust = 0),
        strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "#7eaf34")) +
  facet_wrap(~position) +
  xlim(0, 0.75)


#Get the top and bottom 5 players by Tackles Over Expected Per Contact Zone and Put the Tackling Metrics in a GT Table (FIGURE 2)
toe_pick <- rbind(head(arrange(official_toe_rankings_nflverse, rank), 5), tail(arrange(official_toe_rankings_nflverse, rank), 5))
toe_pick  %>%
  arrange(rank) %>%
  gt(rowname_col = "player_name") %>%
  cols_hide(c("player_team", "position", "depth_chart_position",
              "full_name", "first_name", "last_name", "birth_date", "status",
              "espn_id", "gsis_id", "sportradar_id", "yahoo_id", "rotowire_id",
              "pff_id", "pfr_id", "fantasy_data_id", "sleeper_id", "ngs_position",
              "week", "game_type", "status_description_abbr", "football_name",
              "esb_id", "smart_id", "entry_year", "rookie_year", "draft_club",
              "draft_number", "season", "nflId", "height_ft", "height_in", "team",
              "team_name", "team_id", "team_nick", "team_conf", "team_color",
              "team_logo_squared", "team_league_logo", "team_conference_logo",
              "team_logo_espn", "team_logo_wikipedia", "team_color2", "team_color3",
              "team_color4", "team_division", "college", "height", "weight",
              "jersey_number", "years_exp", "play_num", "games_played", "posnum")) %>%
  gt_img_rows(headshot_url, height = 30) %>%
  gt_img_rows(team_wordmark, height = 30) %>%
  tab_header(title = md("**Tackles Over Expectation Player Rankings: Top 5 and Bottom 5**"),
             subtitle = md("*At least 5 games played in Weeks 1-9 of 2022, at least 10 plays per game*")) %>%
  tab_stubhead(label = "Name") %>%
  cols_move_to_start(headshot_url) %>%
  cols_move(rank, after = headshot_url) %>%
  cols_move(total_toe_rank, after = rank) %>%
  cols_move(team_wordmark, after = total_toe_rank) %>%
  cols_move(player_position, after = team_wordmark) %>%
  cols_move(tackles, after = player_position) %>%
  cols_move(cz_count, after = tackles) %>%
  cols_move(plays_w_cz, after = cz_count) %>%
  cols_move(cz_play_perc, after = plays_w_cz) %>%
  cols_move(exp_tackles, after = cz_play_perc) %>%
  cols_move(per_zone, after = avg_exp_tack) %>%
  cols_move(tackles_oe, after = exp_tackles) %>%
  cols_label(headshot_url = "",
             rank = "TOE/Zone Rank",
             player_name = "Player",
             team = "Team",
             player_position = "Position",
             tackles = "Tackles",
             cz_count = "Contact Zones",
             avg_exp_tack = "xTackles/Zone",
             toe_play = "Tackles OE/Zone",
             tackles_oe = "Tackles OE",
             exp_tackles = "xTackles",
             team_wordmark = "Team",
             per_zone = "Tackles/Zone",
             total_toe_rank = "Total TOE Rank",
             plays_w_cz = "Plays with Contact Zone",
             cz_play_perc = "% Plays with Contact Zone"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  data_color(toe_play, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(tackles_oe, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(cz_count, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(per_zone, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(exp_tackles, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(tackles, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(avg_exp_tack, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(plays_w_cz, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  data_color(cz_play_perc, palette = c("darkred", "#D2B450", "darkgreen")) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = list(cells_column_labels(),
                             cells_stubhead(),
                             cells_stub())) %>%
  tab_style(style = cell_borders(sides = c("bottom", "top")),
            locations = list(cells_stub())) %>%
  tab_style(style = cell_borders(sides = c("right"), color = "white"),
            locations = list(cells_stub())) %>%
  tab_style(style = cell_borders(sides = c("bottom", "top")),
            locations = list(cells_body())) %>%
  tab_style(style = cell_borders(sides = c("right")),
            locations = list(cells_body(headshot_url))) %>%
  tab_style(style = cell_borders(sides = c("right", "left"), color = "grey", weight = 0.5),
            locations = list(cells_body(c(cz_count, exp_tackles, tackles_oe, avg_exp_tack, per_zone, plays_w_cz, cz_play_perc)))) %>%
  tab_style(style = cell_borders(sides = c("right"), color = "grey", weight = 0.5),
            locations = list(cells_body(tackles))) %>%
  tab_style(style = cell_borders(sides = c("left"), color = "grey", weight = 0.5),
            locations = list(cells_body(c(toe_play)))) %>%
  tab_style(style = cell_borders(sides = c("bottom"), color = "black", weight = px(15)),
            locations = list(cells_body(columns = everything(), rows = rank == 5),
                             cells_stub(rows = rank == 5))) %>%
  tab_options(column_labels.border.bottom.color = "black",
              table_body.border.bottom.color = "black",
              table.border.top.style = "solid",
              table.border.top.color = "black",
              table.border.right.style = "solid",
              table.border.right.color = "black",
              table.border.left.style = "solid",
              table.border.left.color = "black",
              footnotes.border.bottom.style = "solid",
              footnotes.border.bottom.color = "black")

