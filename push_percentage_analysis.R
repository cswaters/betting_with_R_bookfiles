library(tidyverse)

nfl <- read_csv('data/nfl-book.csv')

get_push_results <- function(df, .ps, .wnd) {
  df %>%
    mutate(push_ps_home = (between(sprv, .ps - .wnd, .ps + .wnd) &
                             ((ptsh - ptsv) == .ps)),
           push_ps_away = (between(sprv, -.ps - .wnd, -.ps + .wnd) &
                             ((ptsv - ptsh) == .ps))) %>%
    group_by(seas) %>%
    summarise(
      games = n(),
      ps_h = sum(between(sprv, .ps - .wnd, .ps + .wnd)),
      ps_v = sum(between(sprv, -.ps - .wnd, -.ps + .wnd)),
      push_h = sum(push_ps_home),
      push_v = sum(push_ps_away)
    ) %>%
    ungroup() %>%
    mutate(ps_games = ps_h + ps_v,
           push_tot = push_h + push_v) %>%
    
    mutate(
      tot_ps_games = cumsum(ps_games),
      tot_push = cumsum(push_tot),
      tot_push_pct = tot_push / tot_ps_games
    ) %>%
    select(seas,
           games,
           ps_games,
           push_tot,
           tot_ps_games,
           tot_push,
           tot_push_pct)
  
}

# Point spreads to analyze
point_spreads <- seq(1, 14)

# Create push result dataframe and combine them (add new column for spread)
push_results <- map(point_spreads,
                    ~ get_push_results(nfl, .x, 0) %>%
                      add_column(ps = .x, .before = 1, )) %>%
  bind_rows()

# Get seasons for column names
seasons <- paste0('seas_', unique(nfl$seas))

# create grid search space
p_grid <- seq(0.02, .15, .01)
n <- length(p_grid)

# Calculate value of push in grid space
calc_push_val <- function(df, p_grid) {
  map_dfc(c(1:nrow(df)),
          function(x) {
            prior <- rep(1, length(p_grid))
            likelihood <- dbinom(
              x = df$tot_push[x],
              size = df$tot_ps_games[x],
              prob = p_grid
            )
            bayes_numerator <- likelihood * prior
            posterior <-
              bayes_numerator / sum(bayes_numerator)
            result <- data_frame(posterior)
            return(result)
          })
  
}

# Plot cum probability
plot_push_probs <- function(df, p_grid){
  n <- length(p_grid)
  data.frame(p_grid = p_grid,
             prior = rep(1/n, n)) %>% 
    bind_cols(df) %>% 
    gather(key , val, -p_grid) %>% 
    ggplot(aes(p_grid, val)) +
    geom_point(col = '#303952') +
    geom_line(col = '#303952') + 
    scale_y_continuous(labels = scales::percent_format()) +
    facet_wrap(~key) +
    labs(x = 'Push %', 
         y = 'Posterior probability'
    ) +
    theme_bw()
}

# filter push results for point spread of 3
three <- filter(push_results, ps == 3)
seven <- filter(push_results, ps == 7)

get_push_estimates <- function(df, p_grid) {
  pushes <- calc_push_val(df, p_grid) %>%
    set_names(seasons)
  
  plot_push_probs(pushes, p_grid)
  
}

get_push_estimates(three, p_grid)
get_push_estimates(seven, p_grid)

p_grid[which.max(calc_push_val(three, p_grid)[n] %>% pull())]
p_grid[which.max(calc_push_val(seven, p_grid)[n] %>% pull())]


# Using Window of two points
push_results <- map(point_spreads,
                    ~ get_push_results(nfl, .x, 2) %>%
                      add_column(ps = .x, .before = 1, )) %>%
  bind_rows()


three <- filter(push_results, ps == 3)
seven <- filter(push_results, ps == 7)

get_push_estimates(three, p_grid)
get_push_estimates(seven, p_grid)



# windowing adds another percentage to the likelihood of a push
p_grid[which.max(calc_push_val(three, p_grid)[n] %>% pull())]
p_grid[which.max(calc_push_val(seven, p_grid)[n] %>% pull())]
