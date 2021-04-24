library(StatsBombR)
library(tidyverse)
library(rstanarm)

Comp <- FreeCompetitions() %>% 
  filter(competition_id== 11, season_id== 26)

Matches <- FreeMatches(Comp) %>%
  filter(home_team.home_team_name == "Barcelona" || 
           away_team.away_team_name == "Barcelona")

StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T) %>%
  allclean()

shots_goals <- StatsBombData %>%
  group_by(team.name, match_id) %>%
  filter(team.name == "Barcelona",
         type.name == "Shot") %>%
  summarise(shots = sum(type.name=="Shot",
                        na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal",
                        na.rm = TRUE),
            XG = sum(shot.statsbomb_xg,
                     na.rm = TRUE),
            .groups = "drop") %>%
  select(shots, goals, XG)

shots_goals_p90 <- StatsBombData %>%
  group_by(team.name) %>%
  filter(team.name == "Barcelona",
         type.name == "Shot") %>% 
  summarise(shots_p90 = sum(type.name=="Shot",
                            na.rm =TRUE)/n_distinct(match_id),
            goals_p90 = sum(shot.outcome.name=="Goal",
                            na.rm =TRUE)/n_distinct(match_id),
            XG_p90 = sum(shot.statsbomb_xg,
                         na.rm =TRUE)/n_distinct(match_id),
            .groups = "drop") %>% 
  select(shots_p90, goals_p90, XG_p90)



newobs_4<- tibble(shots = 1,
                  XG = 1)

fit_4 <-stan_glm(data = shots_goals,
                 formula = goals ~ (shots*XG),
                 family = gaussian,
                 seed = 25,
                 refresh = 0)

fit_4_tibble <-posterior_epred(fit_4,
                               newobs_4) %>%
  as_tibble() %>%
  filter(`1` >0) %>%
  mutate_all(as.numeric)

fit_4_plot <- fit_4_tibble %>%
  ggplot(aes(x = `1`)) +
  geom_histogram(bins = 100,
                 aes(y = after_stat(count/sum(count)))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = .001)) +
  scale_x_continuous(labels = scales::number_format(accuracy = .001)) +
  theme_classic() +
  labs(title = "Posterior for Number of Goals Scored",
       subtitle = "Estimated posterior using (xG* #of shots)",
       x = "Goals",
       y = "Probability",
       caption = "Source: StatsBomb")