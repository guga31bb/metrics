suppressMessages(
  library(tidyverse)
)
source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_ep_wp.R')
source('https://raw.githubusercontent.com/mrcaseb/nflfastR/master/R/helper_add_nflscrapr_mutations.R')

set.seed(2013)

if (grepl("Documents", getwd())){
  thread <- 4
} else { ### server
  thread <- 6
}

# total points scored

model_data <-
  # readRDS('cal_data.rds') %>%
  readRDS(url('https://github.com/guga31bb/nflfastR-data/blob/master/models/cal_data.rds?raw=true')) %>%
  filter(Winner != "TIE") %>%
  make_model_mutations() %>%
  prepare_wp_data() %>%
  # new- need to change in wp function and elsewhere too
  mutate(
    receive_2h_ko = if_else(qtr > 2, 2, receive_2h_ko),
    # win conditions
    can_win = case_when(
      down == 1 & score_differential > 0 & game_seconds_remaining < 120 & defteam_timeouts_remaining == 0 ~ 1,
      down == 1 & score_differential > 0 & game_seconds_remaining < 84 & defteam_timeouts_remaining == 1 ~ 1,
      down == 1 & score_differential > 0 & game_seconds_remaining < 42 & defteam_timeouts_remaining == 2 ~ 1,
      TRUE ~ 0
    )
    ) %>%
  #
  mutate(label = ifelse(posteam == Winner, 1, 0)) %>%
  filter(!is.na(ep) & !is.na(score_differential) & !is.na(play_type) & !is.na(label) & !is.na(yardline_100), qtr <= 4) %>%
  select(
    label,
    receive_2h_ko,
    spread_time,
    total_line,
    home,
    half_seconds_remaining,
    game_seconds_remaining,
    ExpScoreDiff_Time_Ratio,
    score_differential,
    down,
    ydstogo,
    yardline_100,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    can_win,
    season
  )


folds <- map(0:9, function(x) {
  f <- which(model_data$season %in% c(2000 + x, 2010 + x))
  return(f)
})


full_train = xgboost::xgb.DMatrix(model.matrix(~.+0, data = model_data %>% select(-label, -season)),
                                  label = model_data$label)

#params
nrounds = 15000


# #################################################################################
# try tidymodels

grid <- dials::grid_latin_hypercube(
  dials::finalize(dials::mtry(), model_data %>% select(-season, -label)),
  dials::min_n(),
  dials::tree_depth(),
  dials::learn_rate(range = c(-3, -1), trans = scales::log10_trans()),
  dials::loss_reduction(),
  sample_size = dials::sample_prop(),
  size = 40
) %>%
  mutate(
    # has to be between 0 and 1
    mtry = mtry / length(model_data  %>% select(-season, -label))
  )

# # bonus round at the end: do more searching after finding good ones
# grid <- grid %>%
#   head(6) %>%
#   mutate(
#     learn_rate = c(0.01, 0.02, .03, .04, .05, .06),
#     min_n = 14,
#     tree_depth = 5,
#     mtry = 0.5714286,
#     loss_reduction = 3.445502e-01,
#     sample_size = 0.7204741
#   )

grid %>%
  head(20)

# function to search over hyperparameter grid
get_metrics <- function(df, row = 1) {
  
  # testing only
  # df <- grid %>% dplyr::slice(1)
  
  params <-
    list(
      booster = "gbtree",
      objective = "binary:logistic",
      eval_metric = c("logloss"),
      eta = df$learn_rate,
      gamma = df$loss_reduction,
      subsample= df$sample_size,
      colsample_bytree= df$mtry,
      max_depth = df$tree_depth,
      min_child_weight = df$min_n,
      monotone_constraints = 
        "(0, 0, 0, 1, 0, 0, 1, 1, -1, -1, -1, 1, -1, 0)",
      nthread = thread
    )
  # 
  # receive_2h_ko, 0
  # spread_time, 0
  # total_line, 0
  # home, 1
  # half_seconds_remaining, 0
  # game_seconds_remaining, 0
  # ExpScoreDiff_Time_Ratio, 1
  # score_differential, 1
  # down, -1 
  # ydstogo, -1
  # yardline_100, -1
  # posteam_timeouts_remaining, 1
  # defteam_timeouts_remaining, -1
  # can_win, 0

  #train
  wp_cv_model <- xgboost::xgb.cv(data = full_train, params = params, nrounds = nrounds,
                                 folds = folds, metrics = list("logloss"),
                                 early_stopping_rounds = 20, print_every_n = 50)
  
  output <- params
  output$iter = wp_cv_model$best_iteration
  output$logloss = wp_cv_model$evaluation_log[output$iter]$test_logloss_mean
  output$error = wp_cv_model$evaluation_log[output$iter]$test_error_mean
  
  this_param <- bind_rows(output)
  
  if (row == 1) {
    saveRDS(this_param, "modeling.rds")
  } else {
    prev <- readRDS("modeling.rds")
    for_save <- bind_rows(prev, this_param)
    saveRDS(for_save, "modeling.rds")
  }
  
  return(this_param)
  
}

# get results
results <- map_df(1 : nrow(grid), function(x) {
  
  message(glue::glue("Row {x}"))
  get_metrics(grid %>% dplyr::slice(x), row = x)
  
})

# plot
# results %>%
#   select(logloss, eta, gamma, subsample, colsample_bytree, max_depth, min_child_weight) %>%
#   pivot_longer(eta:min_child_weight,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, logloss, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE, size = 3) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "logloss") +
#   theme_minimal()


# final best model
#
# eta 0.02
# gamma 0.3445502
# subsample 0.7204741
# colsample_bytree 0.5714286
# max_depth 5
# min_child_weight 14
# iter 760
# logloss 0.4485878


# https://parsnip.tidymodels.org/reference/boost_tree.html
# https://xgboost.readthedocs.io/en/latest/parameter.html
