library(tidyverse)
nfl_games <- read_csv('data/nfl-book.csv') %>% 
  select(seas, wk, h, v, ptsh, ptsv, sprv, ou)
nfl_games <- filter(nfl_games, seas >= 2008)

nfl <- nfl_games %>% 
  mutate(mov = ptsh - ptsv,
         home_fav = as.integer(sprv >= 0),
         road_fav = as.integer(sprv < 0),
         home_cover = as.integer(mov > sprv),
         road_cover = as.integer(mov < sprv),
         fav_cover = as.integer((home_fav + home_cover == 2) | (road_fav + road_cover == 2)),
         dog_cover = as.integer((home_fav + road_cover == 2) | (road_fav + home_cover == 2)),
         push = as.integer(mov == sprv)) %>% 
  mutate(spread_group = case_when( # category of spreads crossing 3/7/10/14
    abs(sprv) >= 0 & abs(sprv) < 3 ~ "0 to 2.5",
    abs(sprv) >= 3 & abs(sprv) < 7 ~ "3 to 6.5",
    abs(sprv) >= 7 & abs(sprv) < 10 ~ "7 to 9.5",
    abs(sprv) >= 10 & abs(sprv) < 14 ~ "10 to 13.5",
    TRUE ~ "14+"
  )) 

nfl_reshape <- nfl %>% 
  transmute(fav_team = ifelse(sprv >= 0, h, v),
            dog_team = ifelse(sprv < 0, h, v),
            favored = case_when(fav_team == h ~ 'H',
                                fav_team == v ~ 'A',
                                TRUE ~ 'E'),
            fav_score = ifelse(fav_team == h, ptsh, ptsv),
            dog_score = ifelse(fav_team == h, ptsv, ptsh),
            mov_fav = fav_score - dog_score,
            point_spread = abs(sprv),
            total = ou)

df <-  nfl_reshape %>% 
  select(favored, 
         fav_score, 
         dog_score, 
         mov=mov_fav, 
         ps=point_spread) 


get_ps_push_rates <- function(df,pt_sprd){
  df %>% 
    mutate(fav_cover = mov > ps,
           dog_cover = ps > mov,
           push = ps == mov) %>% 
    group_by(favored,ps) %>% 
    summarise(games = n(),
              fav = mean(fav_cover),
              dog = mean(dog_cover),
              push = mean(push)) %>% 
    ungroup() %>% 
    filter(ps == pt_sprd)
}

df %>%
  mutate(
    fav_cover = mov > ps,
    dog_cover = ps > mov,
    push = ps == mov
  )


get_teaser_results <- function(df, tsr_pts) {
  df %>%
    transmute(
      fav_tsr_cover = mov > (ps - tsr_pts),
      fav_tsr_push = mov == (ps - tsr_pts),
      dog_tsr_cover = (ps + tsr_pts) > mov,
      dog_tsr_push = (ps + tsr_pts) == mov
    )
}

df %>% 
  mutate(fav_cover = mov > ps,
         dog_cover = ps > mov,
         push = ps == mov) %>% 
  bind_cols(
    get_teaser_results(df,6)
  ) %>% 
  transmute(ps,
            fav_ratio = (fav_cover == FALSE | push) & fav_tsr_cover == TRUE,
            dog_ratio = (dog_cover == FALSE | push) & dog_tsr_cover == TRUE) %>% 
  group_by(ps) %>% 
  summarise(games = n(),
            fav = mean(fav_ratio),
            dog = mean(dog_ratio)) %>% 
  gather(side, pct, -ps,-games) %>% 
  filter(ps <= 14) %>% 
  ggplot(aes(factor(ps),pct,
             col=side,
             size=games,
             shape=side)) +
  geom_point() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = seq(0,.6,.1)) +
  labs(x='Point Spread', y = 'Teaser Efficiency Pct', 
       color='Side',shape='Side',size='Games',
       title = '6 Point Teasers',
       subtitle = 'Games where a push or loss is turned into a win.') +
  theme_minimal()




build_teaser_table <- function(tsr_pts) {
  df %>%
    mutate(
      fav_cover = mov > ps,
      dog_cover = ps > mov,
      push = ps == mov
    ) %>%
    bind_cols(get_teaser_results(df, tsr_pts)) %>%
    transmute(
      ps,
      favored,
      fav_tsr_cover,
      dog_tsr_cover,
      fav_ratio = (fav_cover == FALSE |
                     push) & fav_tsr_cover == TRUE,
      dog_ratio = (dog_cover == FALSE |
                     push) & dog_tsr_cover == TRUE
    ) %>%
    add_column(.before = 1, teaser_pts = tsr_pts)
}


teaser_pts <- c(6,6.5,7,10,13,14)
teaser_table <- map_df(teaser_pts, build_teaser_table)
