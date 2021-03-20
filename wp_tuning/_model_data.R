library(tidyverse)
future::plan("multisession")

data <- nflfastR::load_pbp(2001:2020) %>%
  mutate(
    # to fix bug in pit-ari game
    result = home_score - away_score,
    Winner = if_else(home_score > away_score, home_team,
                     if_else(home_score < away_score, away_team, "TIE"))
  )

#for estimating the models, apply some filters
pbp_data <- data %>%
  filter(
    !is.na(down), 
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    !is.na(score_differential),
    qtr <= 4,
    !is.na(result),
    !is.na(posteam)
    ) %>%
  #to keep file size manageable
  select(
    game_id,
    play_type,
    game_seconds_remaining,
    half_seconds_remaining,
    yardline_100,
    roof,
    posteam,
    defteam,
    home_team,
    ydstogo,
    season,
    qtr,
    down,
    week,
    drive,
    ep,
    score_differential,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    desc,
    Winner,
    spread_line,
    total_line
  )

#for doing calibation etc
saveRDS(pbp_data, 'wp_tuning/cal_data.rds')
