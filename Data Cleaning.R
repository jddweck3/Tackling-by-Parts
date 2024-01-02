#DATA CLEANING/FEATURE ENGINEERING

library(tidyverse)

setwd("C:/Users/Joshua/Desktop/BDB 2023-24/NFL Big Data Bowl 2023-24")

#use the play description to determine whether they play is a called run or pass
is_pass <- function(blop){
  desc_words <- unlist(strsplit(blop, " "))
  if("pass" %in% desc_words | "scramble" %in% desc_words | "scrambles" %in% desc_words){
    desc_words <- c()
    return(1)
  } else{
    desc_words <- c()
    return(0)
  }
}

#Load and merge all the provided data files
files <- list.files()
games <- read_csv("games.csv")
players <- read_csv("players.csv")
plays <- read_csv("plays.csv") %>%
  mutate(called_pass = lapply(playDescription, is_pass))
rm(is_pass)
tackles <- read_csv("tackles.csv")
tracking <- read_csv("tracking_week_1.csv")
for(file in files[7:length(files)]){
  tracking <- rbind(tracking, read_csv(file))
}
tracking1 <- tracking %>%
  distinct()
if(length(tracking1$gameId) == length(tracking$gameId)){
  rm(tracking1)
}

tracking_game <- merge(tracking, games, by.x = c("gameId"), by.y = c("gameId"))
tracking_player <- left_join(tracking, players, by = c("nflId", "displayName"))
tracking_play <- merge(tracking, plays, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"))
tracking_tackles <- left_join(tracking, tackles, by = c("gameId", "playId", "nflId"))
play_game <- merge(plays, games, by.x = c("gameId"), by.y = c("gameId"))
player_tackle <- merge(tackles, players, by.x = c("nflId"), by.y = c("nflId"))

but_tackle <- merge(tracking_player, play_game, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"))

data <- left_join(but_tackle, tackles, by = c("gameId", "playId", "nflId"))
setwd("C:/Users/Joshua/Desktop/BDB 2023-24")

#Start cleaning - make sure every play is the same and establish where field benchmarks are
data <- data %>%
  mutate(on_offense = ifelse(club == possessionTeam, 1, ifelse(displayName == "football", -1, 0)),
         off_def = ifelse(on_offense == 1, "Offense", ifelse(displayName == "football", "Football", "Defense")),
         home_team = ifelse(club == homeTeamAbbr, 1, ifelse(displayName == "football", -1, 0)),
         field_side = ifelse(possessionTeam == yardlineSide, "-", "+"),
         yds_ez = ifelse(field_side == "-", 100 - yardlineNumber, yardlineNumber),
         game_clock = format(as.POSIXct(gameClock, format = "%H:%M:%S"), "%H:%M"),
         is_ball_carrier = ifelse(nflId == ballCarrierId, 1, ifelse(is.na(nflId), 0, 0)),
         carrier_set = ifelse(event %in% c("pass_forward", "pass_outcome_caught", "pass_arrived", "pass_shovel", "fumble_offense_recovered", "handoff", "run", "lateral", "snap_direct"), 1, 0),
         carrier_set_b = ifelse(event %in% c("pass_outcome_caught", "handoff", "run", "lateral", "snap_direct"), 1, 0),
         firstdown_yl = ifelse(possessionTeam == yardlineSide, 100 - yardlineNumber - yardsToGo, yardlineNumber - yardsToGo),
         los_x = case_when(playDirection == "left" & possessionTeam == yardlineSide ~ 100 - yardlineNumber + 10,
                           playDirection == "left" & defensiveTeam == yardlineSide ~ yardlineNumber + 10,
                           playDirection == "right" & defensiveTeam == yardlineSide ~ 100 - yardlineNumber + 10,
                           playDirection == "right" & possessionTeam == yardlineSide ~ yardlineNumber + 10),
         firstdown_x = ifelse(playDirection == "left", los_x - yardsToGo, los_x + yardsToGo),
         x_to_los = ifelse(playDirection == "left", los_x - x, x - los_x),
         x_to_firstdown = ifelse(playDirection == "left", firstdown_x - x, x - firstdown_x),
         x_to_gl = ifelse(playDirection == "left", x - 10, 110 - x),
         y_flat = y,
         y = ifelse(playDirection == "right", (160/3)-y, y))

#Generate max speed for each player and put it back into our dataframe
max_speeds <- data %>%
  group_by(nflId) %>%
  summarize(max_speed = max(s))

data <- merge(data, max_speeds, by = c("nflId")) %>%
  mutate(speed_vs_max = s/max_speed)

#How long it has been since the target was set
carry_time <- data %>%
  arrange(frameId) %>%
  group_by(gameId, playId) %>%
  filter(carrier_set == 1) %>%
  summarize(carrier_ball_time = first(time),
            frame = first(frameId))

data1 <- merge(carry_time, data, by = c("gameId", "playId")) %>%
  mutate(frames_carry = frameId - frame,
         seconds_carry = frames_carry * .1)

#Get Ball Carrier Data and Merge it Back into Rows
ball_c <- data1 %>%
  group_by(gameId, playId, frameId) %>%
  filter(is_ball_carrier == 1) %>%
  summarize(bc_x = first(x),
            bc_x_los = first(x_to_los),
            bc_x_1d = first(x_to_firstdown),
            bc_x_gl = first(x_to_gl),
            bc_y = first(y),
            bc_speed = first(s),
            bc_accel = first(a),
            bc_height = first(height),
            bc_weight = first(weight),
            bc_pos = first(position),
            bc_dis = dis,
            bc_orient = first(o),
            bc_dir = first(dir)) %>%
  mutate(bc_poser = case_when(bc_pos == "QB" ~ 1,
                              bc_pos == "RB" ~ 2,
                              bc_pos == "FB" ~ 2,
                              bc_pos == "WR" ~ 3,
                              bc_pos == "TE" ~ 4,
                              T ~ 5)) %>%
  filter(bc_poser != 5)
rm(data2)
rm(noter)
rm(carry_time)
rm(max_speeds)
data3 <- merge(data1, ball_c, by = c("gameId", "playId", "frameId")) %>%
  mutate(distance_from_ball = sqrt((x - bc_x) ** 2 + (y - bc_y) ** 2),
         orient_ball_raw = abs(dir - bc_dir),
         orient_ball = ifelse(orient_ball_raw > 180, 360 - orient_ball_raw, orient_ball_raw))
data3 <- data3 %>%
  mutate(height_inches = as.numeric(substr(height, 1, 1)) * 12 + as.numeric(substr(height, 3, nchar(height))),
         bc_height_inches = as.numeric(substr(bc_height, 1, 1)) * 12 + as.numeric(substr(bc_height, 3, nchar(bc_height))))

#Filter data down to just frames with target set, set up distance rank
data4 <- data3 %>%
  filter(seconds_carry >= 0) %>%
  group_by(gameId, playId, frameId, club) %>%
  arrange(distance_from_ball) %>%
  mutate(distance_rank = row_number() - 1) %>%
  ungroup() %>%
  arrange(gameId, playId, frameId, club, jerseyNumber)

#Data that will not be part of the cleaning but will need to go back in at the end
data_merge_end <- data4 %>%
  select(gameId, playId, frameId, nflId, displayName, off_def, x, x_to_los, x_to_firstdown, x_to_gl, y, s, max_speed, speed_vs_max, a, height_inches, weight, dis, o, dir, orient_ball, position,
         distance_from_ball, distance_rank, frames_carry, seconds_carry, bc_x, bc_x_los, bc_x_1d, bc_x_gl, bc_y, bc_speed, bc_accel, bc_height, bc_height_inches, bc_weight, bc_pos, bc_dis, bc_orient, bc_dir, bc_poser,
         tackle, assist, pff_missedTackle, forcedFumble, season, week, field_side, yardlineNumber, yds_ez, is_ball_carrier, carrier_set, club, home_team, on_offense, homeTeamAbbr, visitorTeamAbbr, possessionTeam, defensiveTeam, quarter, game_clock,
         down, yardsToGo, yardlineSide, preSnapHomeTeamWinProbability, preSnapHomeScore, preSnapVisitorScore, event, los_x, firstdown_x, called_pass)

#Make the dataframe as small as possible so that you don't kill your computer with this
data_inds <- data4 %>%
  select(gameId, playId, frameId, off_def, distance_rank, x, x_to_los, x_to_firstdown, x_to_gl, y, s, a, height_inches, weight, o, dir,
         orient_ball, position, distance_from_ball, nflId, displayName)
base <- names(data_inds)

#Offense Distance Rank 1
o_p1 <- data_inds %>%
  filter(off_def == "Offense", distance_rank == 1) %>%
  mutate(o_p1_x_los = x_to_los,
         o_p1_x_1d = x_to_firstdown,
         o_p1_x_gl = x_to_gl,
         o_p1_x = x,
         o_p1_y = y,
         o_p1_speed = s,
         o_p1_accel = a,
         o_p1_height = height_inches,
         o_p1_weight = weight,
         o_p1_orient = o,
         o_p1_dir = dir,
         o_p1_dir_ball = orient_ball,
         o_p1_pos = position,
         o_p1_dist_ball = distance_from_ball,
         o_p1_nflId = nflId,
         o_p1_name = displayName)
o_p1_merge <- o_p1 %>%
  select(gameId, playId, frameId, o_p1_x, o_p1_x_los, o_p1_x_1d, o_p1_x_gl, o_p1_y, o_p1_speed, o_p1_accel, o_p1_height, o_p1_weight, o_p1_orient, o_p1_dir, o_p1_dir_ball, o_p1_pos, o_p1_dist_ball, o_p1_nflId, o_p1_name)
data5 <- merge(data_inds, o_p1_merge, by = c("gameId", "playId", "frameId"))
rm(o_p1)

#Offense Distance Rank 2
o_p2 <- data5 %>%
  filter(off_def == "Offense", distance_rank == 2) %>%
  mutate(o_p2_x_los = x_to_los,
         o_p2_x_1d = x_to_firstdown,
         o_p2_x_gl = x_to_gl,
         o_p2_x = x,
         o_p2_y = y,
         o_p2_speed = s,
         o_p2_accel = a,
         o_p2_height = height_inches,
         o_p2_weight = weight,
         o_p2_orient = o,
         o_p2_dir = dir,
         o_p2_dir_ball = orient_ball,
         o_p2_pos = position,
         o_p2_dist_ball = distance_from_ball,
         o_p2_nflId = nflId,
         o_p2_name = displayName)
o_p2_merge <- o_p2 %>%
  select(gameId, playId, frameId, o_p2_x, o_p2_x_los, o_p2_x_1d, o_p2_x_gl, o_p2_y, o_p2_speed, o_p2_accel, o_p2_height, o_p2_weight, o_p2_orient, o_p2_dir, o_p2_dir_ball, o_p2_pos, o_p2_dist_ball, o_p2_nflId, o_p2_name)
data6 <- merge(data5, o_p2_merge, by = c("gameId", "playId", "frameId"))
rm(o_p2)

#Offense Distance Rank 3
o_p3 <- data6 %>%
  filter(off_def == "Offense", distance_rank == 3) %>%
  mutate(o_p3_x_los = x_to_los,
         o_p3_x_1d = x_to_firstdown,
         o_p3_x_gl = x_to_gl,
         o_p3_x = x,
         o_p3_y = y,
         o_p3_speed = s,
         o_p3_accel = a,
         o_p3_height = height_inches,
         o_p3_weight = weight,
         o_p3_orient = o,
         o_p3_dir = dir,
         o_p3_dir_ball = orient_ball,
         o_p3_pos = position,
         o_p3_dist_ball = distance_from_ball,
         o_p3_nflId = nflId,
         o_p3_name = displayName)
o_p3_merge <- o_p3 %>%
  select(gameId, playId, frameId, o_p3_x, o_p3_x_los, o_p3_x_1d, o_p3_x_gl, o_p3_y, o_p3_speed, o_p3_accel, o_p3_height, o_p3_weight, o_p3_orient, o_p3_dir, o_p3_dir_ball, o_p3_pos, o_p3_dist_ball, o_p3_nflId, o_p3_name)
data7 <- merge(data6, o_p3_merge, by = c("gameId", "playId", "frameId"))
rm(o_p3)
rm(data4)

#Offense Distance Rank 4
o_p4 <- data7 %>%
  filter(off_def == "Offense", distance_rank == 4) %>%
  mutate(o_p4_x_los = x_to_los,
         o_p4_x_1d = x_to_firstdown,
         o_p4_x_gl = x_to_gl,
         o_p4_x = x,
         o_p4_y = y,
         o_p4_speed = s,
         o_p4_accel = a,
         o_p4_height = height_inches,
         o_p4_weight = weight,
         o_p4_orient = o,
         o_p4_dir = dir,
         o_p4_dir_ball = orient_ball,
         o_p4_pos = position,
         o_p4_dist_ball = distance_from_ball,
         o_p4_nflId = nflId,
         o_p4_name = displayName)
o_p4_merge <- o_p4 %>%
  select(gameId, playId, frameId, o_p4_x, o_p4_x_los, o_p4_x_1d, o_p4_x_gl, o_p4_y, o_p4_speed, o_p4_accel, o_p4_height, o_p4_weight, o_p4_orient, o_p4_dir, o_p4_dir_ball, o_p4_pos, o_p4_dist_ball, o_p4_nflId, o_p4_name)
data8 <- merge(data7, o_p4_merge, by = c("gameId", "playId", "frameId"))
rm(o_p4)
rm(data5)
rm(data_inds)

#Offense Distance Rank 5
o_p5 <- data8 %>%
  filter(off_def == "Offense", distance_rank == 5) %>%
  mutate(o_p5_x_los = x_to_los,
         o_p5_x_1d = x_to_firstdown,
         o_p5_x_gl = x_to_gl,
         o_p5_x = x,
         o_p5_y = y,
         o_p5_speed = s,
         o_p5_accel = a,
         o_p5_height = height_inches,
         o_p5_weight = weight,
         o_p5_orient = o,
         o_p5_dir = dir,
         o_p5_dir_ball = orient_ball,
         o_p5_pos = position,
         o_p5_dist_ball = distance_from_ball,
         o_p5_nflId = nflId,
         o_p5_name = displayName)
o_p5_merge <- o_p5 %>%
  select(gameId, playId, frameId, o_p5_x, o_p5_x_los, o_p5_x_1d, o_p5_x_gl, o_p5_y, o_p5_speed, o_p5_accel, o_p5_height, o_p5_weight, o_p5_orient, o_p5_dir, o_p5_dir_ball, o_p5_pos, o_p5_dist_ball, o_p5_nflId, o_p5_name)
data9 <- merge(data8, o_p5_merge, by = c("gameId", "playId", "frameId"))
rm(o_p5)
rm(data6)

#Offense Distance Rank 6
o_p6 <- data9 %>%
  filter(off_def == "Offense", distance_rank == 6) %>%
  mutate(o_p6_x_los = x_to_los,
         o_p6_x_1d = x_to_firstdown,
         o_p6_x_gl = x_to_gl,
         o_p6_x = x,
         o_p6_y = y,
         o_p6_speed = s,
         o_p6_accel = a,
         o_p6_height = height_inches,
         o_p6_weight = weight,
         o_p6_orient = o,
         o_p6_dir = dir,
         o_p6_dir_ball = orient_ball,
         o_p6_pos = position,
         o_p6_dist_ball = distance_from_ball,
         o_p6_nflId = nflId,
         o_p6_name = displayName)
o_p6_merge <- o_p6 %>%
  select(gameId, playId, frameId, o_p6_x, o_p6_x_los, o_p6_x_1d, o_p6_x_gl, o_p6_y, o_p6_speed, o_p6_accel, o_p6_height, o_p6_weight, o_p6_orient, o_p6_dir, o_p6_dir_ball, o_p6_pos, o_p6_dist_ball, o_p6_nflId, o_p6_name)
data10 <- merge(data9, o_p6_merge, by = c("gameId", "playId", "frameId"))
rm(o_p6)
rm(data7)

#Offense Distance Rank 7
o_p7 <- data10 %>%
  filter(off_def == "Offense", distance_rank == 7) %>%
  mutate(o_p7_x_los = x_to_los,
         o_p7_x_1d = x_to_firstdown,
         o_p7_x_gl = x_to_gl,
         o_p7_x = x,
         o_p7_y = y,
         o_p7_speed = s,
         o_p7_accel = a,
         o_p7_height = height_inches,
         o_p7_weight = weight,
         o_p7_orient = o,
         o_p7_dir = dir,
         o_p7_dir_ball = orient_ball,
         o_p7_pos = position,
         o_p7_dist_ball = distance_from_ball,
         o_p7_nflId = nflId,
         o_p7_name = displayName)
o_p7_merge <- o_p7 %>%
  select(gameId, playId, frameId, o_p7_x, o_p7_x_los, o_p7_x_1d, o_p7_x_gl, o_p7_y, o_p7_speed, o_p7_accel, o_p7_height, o_p7_weight, o_p7_orient, o_p7_dir, o_p7_dir_ball, o_p7_pos, o_p7_dist_ball, o_p7_nflId, o_p7_name)
data11 <- merge(data10, o_p7_merge, by = c("gameId", "playId", "frameId"))
rm(o_p7)
rm(data8)

#Offense Distance Rank 8
o_p8 <- data11 %>%
  filter(off_def == "Offense", distance_rank == 8) %>%
  mutate(o_p8_x_los = x_to_los,
         o_p8_x_1d = x_to_firstdown,
         o_p8_x_gl = x_to_gl,
         o_p8_x = x,
         o_p8_y = y,
         o_p8_speed = s,
         o_p8_accel = a,
         o_p8_height = height_inches,
         o_p8_weight = weight,
         o_p8_orient = o,
         o_p8_dir = dir,
         o_p8_dir_ball = orient_ball,
         o_p8_pos = position,
         o_p8_dist_ball = distance_from_ball,
         o_p8_nflId = nflId,
         o_p8_name = displayName)
o_p8_merge <- o_p8 %>%
  select(gameId, playId, frameId, o_p8_x, o_p8_x_los, o_p8_x_1d, o_p8_x_gl, o_p8_y, o_p8_speed, o_p8_accel, o_p8_height, o_p8_weight, o_p8_orient, o_p8_dir, o_p8_dir_ball, o_p8_pos, o_p8_dist_ball, o_p8_nflId, o_p8_name)
data12 <- merge(data11, o_p8_merge, by = c("gameId", "playId", "frameId"))
rm(o_p8)
rm(data9)

#Offense Distance Rank 9
o_p9 <- data12 %>%
  filter(off_def == "Offense", distance_rank == 9) %>%
  mutate(o_p9_x_los = x_to_los,
         o_p9_x_1d = x_to_firstdown,
         o_p9_x_gl = x_to_gl,
         o_p9_x = x,
         o_p9_y = y,
         o_p9_speed = s,
         o_p9_accel = a,
         o_p9_height = height_inches,
         o_p9_weight = weight,
         o_p9_orient = o,
         o_p9_dir = dir,
         o_p9_dir_ball = orient_ball,
         o_p9_pos = position,
         o_p9_dist_ball = distance_from_ball,
         o_p9_nflId = nflId,
         o_p9_name = displayName)
o_p9_merge <- o_p9 %>%
  select(gameId, playId, frameId, o_p9_x, o_p9_x_los, o_p9_x_1d, o_p9_x_gl, o_p9_y, o_p9_speed, o_p9_accel, o_p9_height, o_p9_weight, o_p9_orient, o_p9_dir, o_p9_dir_ball, o_p9_pos, o_p9_dist_ball, o_p9_nflId, o_p9_name)
data13 <- merge(data12, o_p9_merge, by = c("gameId", "playId", "frameId"))
rm(o_p9)
rm(data10)

#Offense Distance Rank 10
o_p10 <- data13 %>%
  filter(off_def == "Offense", distance_rank == 10) %>%
  mutate(o_p10_x_los = x_to_los,
         o_p10_x_1d = x_to_firstdown,
         o_p10_x_gl = x_to_gl,
         o_p10_x = x,
         o_p10_y = y,
         o_p10_speed = s,
         o_p10_accel = a,
         o_p10_height = height_inches,
         o_p10_weight = weight,
         o_p10_orient = o,
         o_p10_dir = dir,
         o_p10_dir_ball = orient_ball,
         o_p10_pos = position,
         o_p10_dist_ball = distance_from_ball,
         o_p10_nflId = nflId,
         o_p10_name = displayName)
o_p10_merge <- o_p10 %>%
  select(gameId, playId, frameId, o_p10_x, o_p10_x_los, o_p10_x_1d, o_p10_x_gl, o_p10_y, o_p10_speed, o_p10_accel, o_p10_height, o_p10_weight, o_p10_orient, o_p10_dir, o_p10_dir_ball, o_p10_pos, o_p10_dist_ball, o_p10_nflId, o_p10_name)
data14 <- merge(data13, o_p10_merge, by = c("gameId", "playId", "frameId"))
print(Sys.time())
rm(o_p10)
rm(data11)
rm(data12)
rm(data13)
rm(o_p1_merge, o_p2_merge, o_p3_merge, o_p4_merge, o_p5_merge, o_p6_merge, o_p7_merge, o_p8_merge, o_p9_merge,
   tracking, tracking_game, tracking_play, tracking_player, tracking_tackles, ball_c, but_tackle, data1, data3,
   comparison, file, files, times, o_p10_merge)

#Merge everything back together
all_in <- merge(data14, data_merge_end, by = base)
rm(data14, base)

#Filter data down to just defenders, define whether the player is a tackler
just_defenders <- all_in %>%
  filter(off_def == "Defense") %>%
  mutate(is_tackler_solo = case_when(tackle == 1 ~ 1,
                                     forcedFumble == 1 ~ 1,
                                     T ~ 0),
         is_tackler_assist = case_when(assist == 1 ~ 1,
                                       T ~ is_tackler_solo),
         is_ob_nearest = ifelse(event == "out_of_bounds", ifelse(distance_rank == 1, 1, 0), 0))

ob_tack <- just_defenders %>%
  group_by(gameId, playId, nflId) %>%
  summarize(is_ob_tackler = sum(is_ob_nearest, na.rm = TRUE)) %>%
  ungroup()

almost_ready <- merge(ob_tack, just_defenders, by = c("gameId", "playId", "nflId")) %>%
  mutate(is_tackler_asst_ob = case_when(is_ob_tackler > 0 ~ 1,
                                        T ~ is_tackler_assist),
         is_tackler_solo_ob = case_when(is_ob_tackler > 0 ~ 1,
                                        T ~ is_tackler_solo))

#Add field situation
almost_ready <- almost_ready %>%
  mutate(field_position = case_when(field_side == "-" ~ 0 - yardlineNumber,
                                    T ~ yardlineNumber),
         dds = case_when(down == 1 & yardsToGo >= 10 & field_position <= -1 &  field_position >= -5 ~ "1&10+, Coming Out",
                         down == 1 & yardsToGo >= 10 & field_position <= -6 & field_position >= -49 ~ "1&10+, Open Field",
                         down == 1 & yardsToGo >= 10 & field_position <= 50 & field_position >= 36 ~ "1&10+, Plus Territory",
                         down == 1 & yardsToGo >= 10 & field_position <= 35 & field_position >= 26 ~ "1&10+, Fringe",
                         down == 1 & yardsToGo >= 10 & field_position <= 25 & field_position >= 13 ~ "1&10+, Red Area",
                         down == 1 & yardsToGo >= 10 & field_position <= 12 & field_position >= 4 ~ "1&10+, Near Red",
                         down == 1 & yardsToGo >= 10 & field_position <= 3 & field_position >= 1 ~ "1&10+, Goal Line",
                         down == 1 & yardsToGo < 10 & field_position <= -1 &  field_position >= -5 ~ "1&-10, Coming Out",
                         down == 1 & yardsToGo < 10 & field_position <= -6 & field_position >= -49 ~ "1&-10, Open Field",
                         down == 1 & yardsToGo < 10 & field_position <= 50 & field_position >= 36 ~ "1&-10, Plus Territory",
                         down == 1 & yardsToGo < 10 & field_position <= 35 & field_position >= 26 ~ "1&-10, Fringe",
                         down == 1 & yardsToGo < 10 & field_position <= 25 & field_position >= 13 ~ "1&-10, Red Area",
                         down == 1 & yardsToGo < 10 & field_position <= 12 & field_position >= 4 ~ "1&-10, Near Red",
                         down == 1 & yardsToGo < 10 & field_position <= 3 & field_position >= 1 ~ "1&-10, Goal Line",
                         down == 2 & yardsToGo >= 6 & field_position <= -1 &  field_position >= -5 ~ "2&6+, Coming Out",
                         down == 2 & yardsToGo >= 6 & field_position <= -6 & field_position >= -49 ~ "2&6+, Open Field",
                         down == 2 & yardsToGo >= 6 & field_position <= 50 & field_position >= 36 ~ "2&6+, Plus Territory",
                         down == 2 & yardsToGo >= 6 & field_position <= 35 & field_position >= 26 ~ "2&6+, Fringe",
                         down == 2 & yardsToGo >= 6 & field_position <= 25 & field_position >= 13 ~ "2&6+, Red Area",
                         down == 2 & yardsToGo >= 6 & field_position <= 12 & field_position >= 4 ~ "2&6+, Near Red",
                         down == 2 & yardsToGo >= 6 & field_position <= 3 & field_position >= 1 ~ "2&6+, Goal Line",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= -1 &  field_position >= -5 ~ "2&3-6, Coming Out",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= -6 & field_position >= -49 ~ "2&3-6, Open Field",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= 50 & field_position >= 36 ~ "2&3-6, Plus Territory",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= 35 & field_position >= 26 ~ "2&3-6, Fringe",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= 25 & field_position >= 13 ~ "2&3-6, Red Area",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= 12 & field_position >= 4 ~ "2&3-6, Near Red",
                         down == 2 & yardsToGo >= 3 & yardsToGo < 6 & field_position <= 3 & field_position >= 1 ~ "2&3-6, Goal Line",
                         down == 2 & yardsToGo < 3 & field_position <= -1 &  field_position >= -5 ~ "2&-3, Coming Out",
                         down == 2 & yardsToGo < 3 & field_position <= -6 & field_position >= -49 ~ "2&-3, Open Field",
                         down == 2 & yardsToGo < 3 & field_position <= 50 & field_position >= 36 ~ "2&-3, Plus Territory",
                         down == 2 & yardsToGo < 3 & field_position <= 35 & field_position >= 26 ~ "2&-3, Fringe",
                         down == 2 & yardsToGo < 3 & field_position <= 25 & field_position >= 13 ~ "2&-3, Red Area",
                         down == 2 & yardsToGo < 3 & field_position <= 12 & field_position >= 4 ~ "2&-3, Near Red",
                         down == 2 & yardsToGo < 3 & field_position <= 3 & field_position >= 1 ~ "2&-3, Goal Line",
                         down >= 3 & yardsToGo >= 7 & field_position <= -1 &  field_position >= -5 ~ "3/4&7+, Coming Out",
                         down >= 3 & yardsToGo >= 7 & field_position <= -6 & field_position >= -39 ~ "3/4&7+, Minus Territory",
                         down >= 3 & yardsToGo >= 7 & field_position <= -40  ~ "3/4&7+, 4Q",
                         down >= 3 & yardsToGo >= 7 & field_position <= 50 & field_position >= 36 ~ "3/4&7+, 4Q",
                         down >= 3 & yardsToGo >= 7 & field_position <= 35 & field_position >= 26 ~ "3/4&7+, Fringe",
                         down >= 3 & yardsToGo >= 7 & field_position <= 25 & field_position >= 13 ~ "3/4&7+, Red Area",
                         down >= 3 & yardsToGo >= 7 & field_position <= 12 & field_position >= 4 ~ "3/4&7+, Near Red",
                         down >= 3 & yardsToGo >= 7 & field_position <= 3 & field_position >= 1 ~ "3/4&7+, Goal Line",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= -1 &  field_position >= -5 ~ "3/4&3-6, Coming Out",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= -6 & field_position >= -39 ~ "3/4&3-6, Minus Territory",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= -40  ~ "3/4&3-6, 4Q",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= 50 & field_position >= 36 ~ "3/4&3-6, 4Q",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= 35 & field_position >= 26 ~ "3/4&3-6, Fringe",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= 25 & field_position >= 13 ~ "3/4&3-6, Red Area",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= 12 & field_position >= 4 ~ "3/4&3-6, Near Red",
                         down >= 3 & yardsToGo >= 3 & yardsToGo < 7 & field_position <= 3 & field_position >= 1 ~ "3/4&3-6, Goal Line",
                         down >= 3 & yardsToGo < 3 & field_position <= -1 &  field_position >= -5 ~ "3/4&-3, Coming Out",
                         down >= 3 & yardsToGo < 3 & field_position <= -6 & field_position >= -39 ~ "3/4&-3, Minus Territory",
                         down >= 3 & yardsToGo < 3 & field_position <= -40  ~ "3/4&-3, 4Q",
                         down >= 3 & yardsToGo < 3 & field_position <= 50 & field_position >= 36 ~ "3/4&-3, 4Q",
                         down >= 3 & yardsToGo < 3 & field_position <= 35 & field_position >= 26 ~ "3/4&-3, Fringe",
                         down >= 3 & yardsToGo < 3 & field_position <= 25 & field_position >= 13 ~ "3/4&-3, Red Area",
                         down >= 3 & yardsToGo < 3 & field_position <= 12 & field_position >= 4 ~ "3/4&-3, Near Red",
                         down >= 3 & yardsToGo < 3 & field_position <= 3 & field_position >= 1 ~ "3/4&-3, Goal Line",
                         T ~ "WAAAAAAAAA"),
         field_sit = case_when(dds == "1&10+, Coming Out" ~ 0,
                               dds == "1&10+, Open Field" ~ 1,
                               dds == "1&10+, Plus Territory" ~ 2,
                               dds == "1&10+, Fringe" ~ 3,
                               dds == "1&10+, Red Area" ~ 4,
                               dds == "1&10+, Near Red" ~ 5,
                               dds == "1&10+, Goal Line" ~ 6,
                               dds == "1&-10, Coming Out" ~ 7,
                               dds == "1&-10, Open Field" ~ 8,
                               dds == "1&-10, Plus Territory" ~ 9,
                               dds == "1&-10, Fringe" ~ 10,
                               dds == "1&-10, Red Area" ~ 11,
                               dds == "1&-10, Near Red" ~ 12,
                               dds == "1&-10, Goal Line" ~ 13,
                               dds == "2&6+, Coming Out" ~ 14,
                               dds == "2&6+, Open Field" ~ 15,
                               dds == "2&6+, Plus Territory" ~ 16,
                               dds == "2&6+, Fringe" ~ 17,
                               dds == "2&6+, Red Area" ~ 18,
                               dds == "2&6+, Near Red" ~ 19,
                               dds == "2&6+, Goal Line" ~ 20,
                               dds == "2&3-6, Coming Out" ~ 21,
                               dds == "2&3-6, Open Field" ~ 22,
                               dds == "2&3-6, Plus Territory" ~ 23,
                               dds == "2&3-6, Fringe" ~ 24,
                               dds == "2&3-6, Red Area" ~ 25,
                               dds == "2&3-6, Near Red" ~ 26,
                               dds == "2&3-6, Goal Line" ~ 27,
                               dds == "2&-3, Coming Out" ~ 28,
                               dds == "2&-3, Open Field" ~ 29,
                               dds == "2&-3, Plus Territory" ~ 30,
                               dds == "2&-3, Fringe" ~ 31,
                               dds == "2&-3, Red Area" ~ 32,
                               dds == "2&-3, Near Red" ~ 33,
                               dds == "2&-3, Goal Line" ~ 34,
                               dds == "3/4&7+, Coming Out" ~ 35,
                               dds == "3/4&7+, Minus Territory" ~ 36,
                               dds == "3/4&7+, 4Q" ~ 37,
                               dds == "3/4&7+, Fringe" ~ 38,
                               dds == "3/4&7+, Red Area" ~ 39,
                               dds == "3/4&7+, Near Red" ~ 40,
                               dds == "3/4&7+, Goal Line" ~ 41,
                               dds == "3/4&3-6, Coming Out" ~ 42,
                               dds == "3/4&3-6, Minus Territory" ~ 43,
                               dds == "3/4&3-6, 4Q" ~ 44,
                               dds == "3/4&3-6, Fringe" ~ 45,
                               dds == "3/4&3-6, Red Area" ~ 46,
                               dds == "3/4&3-6, Near Red" ~ 47,
                               dds == "3/4&3-6, Goal Line" ~ 48,
                               dds == "3/4&-3, Coming Out" ~ 49,
                               dds == "3/4&-3, Minus Territory" ~ 50,
                               dds == "3/4&-3, 4Q" ~ 51,
                               dds == "3/4&-3, Fringe" ~ 52,
                               dds == "3/4&-3, Red Area" ~ 53,
                               dds == "3/4&-3, Near Red" ~ 54,
                               dds == "3/4&-3, Goal Line" ~ 55,
                               T ~ 56))

#CONTACT ZONE SETUP

#Define where contact zones start and end
ze <- almost_ready %>%
  arrange(gameId, playId, nflId, frameId) %>%
  mutate(contact_zone = ifelse(distance_from_ball <= 3, 1, 0),
         cz_min_1 = lag(contact_zone, 1),
         cz_min_2 = lag(contact_zone, 2),
         cz_min_3 = lag(contact_zone, 3),
         zone_entry = case_when(seconds_carry == 0 & contact_zone == 1 ~ 1,
                                seconds_carry == .1 & cz_min_1 != 1 & contact_zone == 1 ~ 1,
                                seconds_carry == .2 & cz_min_1 != 1 & cz_min_2!= 1 & contact_zone == 1 ~ 1,
                                seconds_carry >= .3 & contact_zone == 1 & cz_min_1 != 1 & cz_min_2 != 1 & cz_min_3 != 1 ~ 1,
                                T ~ 0),
         zone_exit = ifelse(seconds_carry >= .3 & contact_zone == 0 & cz_min_1 != 1 & cz_min_2 != 1 & cz_min_3 == 1, 1, 0),
         event = case_when(gameId == 2022103009 & playId == 2259 & event == "autoevent_passforward" ~ "fumble",
                           T ~ event),
         tackle_now = ifelse(event %in% c("tackle", "qb_slide", "out_of_bounds", "fumble", "safety"), 1, 0),
         td_now = ifelse(event %in% c("touchdown", "pass_outcome_touchdown"), 1, 0),
         combiner = paste(gameId, playId))

#Find the frame with the tackle
tackle_frame <- ze %>%
  group_by(gameId, playId)  %>%
  filter(tackle_now == 1) %>%
  summarise(tackle_frame = first(frameId)) %>%
  ungroup()

#Include touchdowns - these are when everyone misses their tackle
td_frame <- ze %>%
  group_by(gameId, playId)  %>%
  filter(td_now == 1) %>%
  summarise(tackle_frame = first(frameId)) %>%
  ungroup()
tack_td <- rbind(tackle_frame, td_frame)

#Merge the contact zone starts and ends into the tackles
ze_tf <- merge(tack_td, ze, by = c("gameId", "playId"))
rm(ze)

#Determine how many zone entries and exits a player has had by a certain frame
zone_entries <- ze_tf %>%
  arrange(gameId, playId, frameId) %>%
  group_by(gameId, playId, nflId) %>%
  mutate(contact_zones = cumsum(zone_entry),
         zone_exits = cumsum(zone_exit)) %>%
  ungroup()
rm(ze_tf)

#Figure out whether the player is the tackler and whether the tackle is in this contact zone
contact_zone_base <- zone_entries %>%
  group_by(gameId, playId, nflId, contact_zones) %>%
  filter(zone_exits < contact_zones) %>%
  summarize(player_name = last(displayName),
            team = first(club),
            player_position = first(position),
            week = first(week),
            first_frame = first(frameId),
            last_frame = last(frameId),
            is_tackler = max(is_tackler_asst_ob),
            tackle_frame = first(tackle_frame)) %>%
  mutate(tack_in_zone = ifelse(tackle_frame >= first_frame & tackle_frame <= last_frame + 2, 1, 0),
         is_tackler_now = ifelse(tack_in_zone == 1 & is_tackler == 1, 1, 0))

#Make sure that the last contact zone for a tackler is counted as a contact zone for a tackler
made_tackle <- contact_zone_base %>%
  filter(is_tackler == 1, first_frame < tackle_frame) %>%
  arrange(gameId, playId, first_frame)
last_tackle_cz <- made_tackle %>%
  group_by(gameId, playId, nflId) %>%
  summarize(last_entry = last(first_frame),
            last_exit = last(last_frame)) %>%
  arrange(gameId, playId, last_entry) %>%
  ungroup() %>%
  group_by(gameId, playId) %>%
  summarize(last_tackler_entry = last(last_entry)) %>%
  ungroup()

#Put everything together and collect all the contact zones
tacklers <- left_join(made_tackle, last_tackle_cz, by = c("gameId" = "gameId", "playId" = "playId")) %>%
  select(gameId, playId, last_frame, last_tackler_entry) %>%
  group_by(gameId, playId) %>%
  summarize(last_tackler = last(last_tackler_entry))

contact_zones_lt_cz <- left_join(contact_zone_base, tacklers, by = c("gameId", "playId"))

contact_zones <- contact_zones_lt_cz %>%
  filter(tackle_frame >= first_frame, !(first_frame > last_tackler))


#END CONTACT ZONE SETUP

rm(all_in)
#Define game situation and defender orientation relative to ball
almost_ready <- almost_ready %>%
  mutate(potential_1h_2_min_time = ifelse(format(as.POSIXct(game_clock, format = "%H:%M"), format = "%H:%M") < format(as.POSIXct("02:00", format = "%H:%M"), format = "%H:%M"), 1, 0),
         potential_2h_2_min_time = ifelse(format(as.POSIXct(game_clock, format = "%H:%M"), format = "%H:%M") < format(as.POSIXct("04:00", format = "%H:%M"), format = "%H:%M"), 1, 0))
almost_ready <- almost_ready %>%
  mutate(game_sit = case_when(quarter == 2 & potential_1h_2_min_time == 1 ~ 0,
                              quarter == 4 & potential_2h_2_min_time == 1 & preSnapHomeTeamWinProbability >= (.33) & preSnapHomeTeamWinProbability <= (.67) & homeTeamAbbr == possessionTeam & preSnapHomeScore <= preSnapVisitorScore ~ 1,
                              quarter == 4 & potential_2h_2_min_time == 1 & preSnapHomeTeamWinProbability >= (.33) & preSnapHomeTeamWinProbability <= (.67) & homeTeamAbbr != possessionTeam & preSnapHomeScore >= preSnapVisitorScore ~ 1,
                              quarter == 4 & potential_2h_2_min_time == 1 & preSnapHomeTeamWinProbability >= (1/3) & preSnapHomeTeamWinProbability <= (2/3) & homeTeamAbbr == possessionTeam & preSnapHomeScore > preSnapVisitorScore ~ 2,
                              quarter == 4 & potential_2h_2_min_time == 1 & preSnapHomeTeamWinProbability >= (1/3) & preSnapHomeTeamWinProbability <= (2/3) & homeTeamAbbr != possessionTeam & preSnapHomeScore < preSnapVisitorScore ~ 2,
                              preSnapHomeTeamWinProbability >= .2 & preSnapHomeTeamWinProbability <= .8 ~ 3,
                              T ~ 4),
         dir_ball = orient_ball,
         orient_ball_base = abs(o - bc_orient),
         orient_ball = ifelse(orient_ball_base > 180, 360 - orient_ball_base, orient_ball_base))


#Select for only what you need to build the model and do the analysis work
model_molder <- almost_ready %>%
  select(distance_rank	,
         x	,
         x_to_los,
         x_to_firstdown,
         x_to_gl,
         y	,
         s	,
         speed_vs_max,
         max_speed,
         a	,
         height_inches	,
         weight	,
         dir_ball	,
         orient_ball,
         distance_from_ball	,
         o_p1_x_los	,
         o_p1_x_1d	,
         o_p1_x_gl	,
         o_p1_y	,
         o_p1_speed	,
         o_p1_accel	,
         o_p1_height	,
         o_p1_weight	,
         o_p1_dir_ball	,
         o_p1_dist_ball	,
         o_p2_x_los	,
         o_p2_x_1d	,
         o_p2_x_gl	,
         o_p2_y	,
         o_p2_speed	,
         o_p2_accel	,
         o_p2_height	,
         o_p2_weight	,
         o_p2_dir_ball	,
         o_p2_dist_ball	,
         o_p3_x_los	,
         o_p3_x_1d	,
         o_p3_x_gl	,
         o_p3_y	,
         o_p3_speed	,
         o_p3_accel	,
         o_p3_height	,
         o_p3_weight	,
         o_p3_dir_ball	,
         o_p3_dist_ball	,
         o_p4_x_los	,
         o_p4_x_1d	,
         o_p4_x_gl	,
         o_p4_y	,
         o_p4_speed	,
         o_p4_accel	,
         o_p4_height	,
         o_p4_weight	,
         o_p4_dir_ball	,
         o_p4_dist_ball	,
         o_p5_x_los	,
         o_p5_x_1d	,
         o_p5_x_gl	,
         o_p5_y	,
         o_p5_speed	,
         o_p5_accel	,
         o_p5_height	,
         o_p5_weight	,
         o_p5_dir_ball	,
         o_p5_dist_ball	,
         o_p6_x_los	,
         o_p6_x_1d	,
         o_p6_x_gl	,
         o_p6_y	,
         o_p6_speed	,
         o_p6_accel	,
         o_p6_height	,
         o_p6_weight	,
         o_p6_dir_ball	,
         o_p6_dist_ball	,
         o_p7_x_los	,
         o_p7_x_1d	,
         o_p7_x_gl	,
         o_p7_y	,
         o_p7_speed	,
         o_p7_accel	,
         o_p7_height	,
         o_p7_weight	,
         o_p7_dir_ball	,
         o_p7_dist_ball	,
         o_p8_x_los	,
         o_p8_x_1d	,
         o_p8_x_gl	,
         o_p8_y	,
         o_p8_speed	,
         o_p8_accel	,
         o_p8_height	,
         o_p8_weight	,
         o_p8_dir_ball	,
         o_p8_dist_ball	,
         o_p9_x_los	,
         o_p9_x_1d	,
         o_p9_x_gl	,
         o_p9_y	,
         o_p9_speed	,
         o_p9_accel	,
         o_p9_height	,
         o_p9_weight	,
         o_p9_dir_ball	,
         o_p9_dist_ball	,
         o_p10_x_los	,
         o_p10_x_1d	,
         o_p10_x_gl	,
         o_p10_y	,
         o_p10_speed	,
         o_p10_accel	,
         o_p10_height	,
         o_p10_weight	,
         o_p10_dir_ball	,
         o_p10_dist_ball	,
         seconds_carry	,
         bc_x_los	,
         bc_x_1d	,
         bc_x_gl	,
         bc_y	,
         bc_speed	,
         bc_accel	,
         bc_height_inches	,
         bc_weight	,
         bc_poser	,
         bc_orient	,
         field_sit	,
         game_sit	,
         is_tackler_solo,
         is_tackler_assist,
         is_tackler_solo_ob,
         is_tackler_asst_ob,
         week,
         gameId,
         playId,
         nflId,
         frameId,
         down,
         yardsToGo,
         quarter,
         possessionTeam,
         defensiveTeam,
         game_clock,
         field_side,
         yds_ez,
         yardlineNumber,
         o_p1_x,
         o_p2_x,
         o_p3_x,
         o_p4_x,
         o_p5_x,
         o_p6_x,
         o_p7_x,
         o_p8_x,
         o_p9_x,
         o_p10_x,
         bc_x,
         los_x,
         firstdown_x,
         called_pass,
         o_p1_pos,
         o_p2_pos,
         o_p3_pos,
         o_p4_pos,
         o_p5_pos,
         o_p6_pos,
         o_p7_pos,
         o_p8_pos,
         o_p9_pos,
         o_p10_pos,
         event
  ) %>%
  mutate(dist_o1 = sqrt((x - o_p1_x) ** 2 + (y - o_p1_y) ** 2),
         dist_o2 = sqrt((x - o_p2_x) ** 2 + (y - o_p2_y) ** 2),
         dist_o3 = sqrt((x - o_p3_x) ** 2 + (y - o_p3_y) ** 2),
         dist_o4 = sqrt((x - o_p4_x) ** 2 + (y - o_p4_y) ** 2),
         dist_o5 = sqrt((x - o_p5_x) ** 2 + (y - o_p5_y) ** 2),
         dist_o6 = sqrt((x - o_p6_x) ** 2 + (y - o_p6_y) ** 2),
         dist_o7 = sqrt((x - o_p7_x) ** 2 + (y - o_p7_y) ** 2),
         dist_o8 = sqrt((x - o_p8_x) ** 2 + (y - o_p8_y) ** 2),
         dist_o9 = sqrt((x - o_p9_x) ** 2 + (y - o_p9_y) ** 2),
         dist_o10 = sqrt((x - o_p10_x) ** 2 + (y - o_p10_y) ** 2),
         rb1 = ifelse(o_p1_pos %in% c("RB", "FB"), 1, 0),
         rb2 = ifelse(o_p2_pos %in% c("RB", "FB"), 1, 0),
         rb3 = ifelse(o_p3_pos %in% c("RB", "FB"), 1, 0),
         rb4 = ifelse(o_p4_pos %in% c("RB", "FB"), 1, 0),
         rb5 = ifelse(o_p5_pos %in% c("RB", "FB"), 1, 0),
         rb6 = ifelse(o_p6_pos %in% c("RB", "FB"), 1, 0),
         rb7 = ifelse(o_p7_pos %in% c("RB", "FB"), 1, 0),
         rb8 = ifelse(o_p8_pos %in% c("RB", "FB"), 1, 0),
         rb9 = ifelse(o_p9_pos %in% c("RB", "FB"), 1, 0),
         rb10 = ifelse(o_p10_pos %in% c("RB", "FB"), 1, 0),
         te1 = ifelse(o_p1_pos %in% c("TE"), 1, 0),
         te2 = ifelse(o_p2_pos %in% c("TE"), 1, 0),
         te3 = ifelse(o_p3_pos %in% c("TE"), 1, 0),
         te4 = ifelse(o_p4_pos %in% c("TE"), 1, 0),
         te5 = ifelse(o_p5_pos %in% c("TE"), 1, 0),
         te6 = ifelse(o_p6_pos %in% c("TE"), 1, 0),
         te7 = ifelse(o_p7_pos %in% c("TE"), 1, 0),
         te8 = ifelse(o_p8_pos %in% c("TE"), 1, 0),
         te9 = ifelse(o_p9_pos %in% c("TE"), 1, 0),
         te10 = ifelse(o_p10_pos %in% c("TE"), 1, 0),
         off_pers = (rb1 + rb2 + rb3 + rb4 + rb5 + rb6 + rb7 + rb8 + rb9 + rb10) * 10 + (te1 + te2 + te3 + te4 + te5 + te6 + te7 + te8 + te9 + te10),
         off_pers = ifelse(bc_poser == 2, off_pers + 10, ifelse(bc_poser == 4, off_pers + 1, off_pers))) %>%
  select(-rb1, -rb2, -rb3, -rb4, -rb5, -rb6, -rb7, -rb8, -rb9, -rb10, -te1, -te2, -te3, -te4, -te5, -te6, -te7, -te8, -te9, -te10)

model_molder$called_pass <- as.numeric(model_molder$called_pass)
