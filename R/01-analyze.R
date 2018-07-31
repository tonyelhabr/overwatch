
library("tidyverse")
library("rlang")
library("teplot")

config <-
  list(
    export_data = TRUE,
    dir_data = "data",
    export_viz = TRUE,
    dir_viz = "figs"
  )

path_data_raw <-
  list.files(
    path = "data-raw",
    pattern = "*csv$",
    recursive = FALSE,
    full.names = TRUE
  )
data_raw <-
  path_data_raw %>%
  teproj::import_path_cleanly()

data <-
  data_raw %>%
  mutate(
    hours_played =
      time_played %>%
      str_replace_all("h.*", "") %>%
      as.integer(),
    minutes_played =
      time_played %>%
      str_replace_all("h\\s", "") %>%
      str_replace_all("([0-9]+)(m)", "\\1") %>%
      as.integer()
  ) %>%
  mutate_at(vars(matches("s_played")), funs(coalesce(., 0L))) %>%
  mutate_at(vars(hours_played), funs(. + (minutes_played / 60L))) %>%
  select(-matches("time|minutes"))
data
data %>%
  filter(is.na(hours_played))
data %>% count(team, sort = TRUE)

data_tidy <-
  data %>%
  mutate_at(vars(role), funs(factor(.) %>% as.integer())) %>%
  gather(metric, value, -player, -team)

viz_metrics <-
  data_tidy %>%
  ggplot(aes(x = value, fill = metric)) +
  geom_histogram(alpha = 0.8, bins = 30) +
  facet_wrap( ~ metric, scales = "free") +
  teplot::theme_te()
viz_metrics

viz_xy <-
  function(data, x, y) {
    x <- enquo(x)
    y <- enquo(y)
    data %>%
      ggplot(aes(x = !!x, y = !!y, color = role)) +
      teplot::scale_color_set1() +
      geom_point(size = 3) +
      geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
      # geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
      # geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +
      guides(color = guide_legend(override.aes = list(size = 3))) +
      teplot::theme_te()
  }

.pull_f <- function(data, col, f) {
  col <- enquo(col)
  data %>%
    summarise(value = max(!!col, na.rm = TRUE)) %>%
    pull(value)
}

viz_xy_facet <-
  function(data, x, y) {
    x <- enquo(x)
    y <- enquo(y)
    max_x <- data %>% .pull_f(!!x, max)
    max_y <- data %>% .pull_f(!!y, max)
    max_x <- ceiling(1.1 * max_x)
    max_y <- ceiling(1.1 * max_y)
    minx_xy <- 0

    data %>%
      ggplot(aes(x = !!x, y = !!y, color = role)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, color = "black", size = 1) +
      geom_smooth(method = "lm", se = FALSE, color = "blue", size = 1) +
      geom_smooth(method = "loess", se = FALSE, color = "red", size = 1) +
      facet_wrap( ~ role, scales = "free") +
      coord_cartesian(xlim = c(minx_xy, max_x), ylim = c(minx_xy, max_y)) +
      teplot::theme_te()
  }

data %>%
  viz_xy(elim, deaths)

data %>%
  viz_xy(damage, healing)

viz_kd <-
  data %>%
  viz_xy_facet(elim, deaths)
viz_kd

viz_dh <-
  data %>%
  viz_xy_facet(damage, healing)
viz_dh

data %>%
  count(role, sort = TRUE)

summ_byrole <-
  data %>%
  gather(metric, value, -team, -player, -role) %>%
  group_by(role, metric) %>%
  tetidy::summarise_stats(value, tidy = TRUE)
summ_byrole %>%
  ggplot(aes(x = role, y = value, fill = role)) +
  geom_col(position = position_dodge()) +
  facet_grid(metric ~ stat, scales = "free") +
  theme(legend.position = "bottom") +
  teplot::theme_te()
data %>%
  ggplot(aes(x = role, y = hours_played)) +
  geom_boxplot() +
  teplot::theme_te()

