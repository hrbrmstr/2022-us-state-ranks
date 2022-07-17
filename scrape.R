library(stringi)
library(hrbragg)
library(patchwork)
library(rvest)
library(tidyverse)

pg <- read_html("https://www.cnbc.com/2022/07/13/americas-top-states-for-business-2022-the-full-rankings.html")

html_node(pg, "table") |>
  html_table() |>
  janitor::clean_names() -> cnbc

write_csv(cnbc, "2022-cnbc-us-state-ranks.csv")
jsonlite::stream_out(cnbc, file("2022-cnbc-us-state-ranks.json"))

pg <- read_html("https://www.usnews.com/news/best-states/rankings")
html_node(pg, "table.TableTabular__TableContainer-impg-0") |>
  html_table() |>
  select(seq(1, 19, 2)) |>
  janitor::clean_names() |>
  filter(!is.na(rank)) |> 
  mutate(
    state = stri_replace_all_regex(state, "([:upper:])", " $1") |>
      stri_replace_all_regex("[[:space:]]+", " ") |>
      stri_split_fixed(" ") |>
      map_chr(~paste(unique(.x), collapse = " "))
  ) -> usnews

write_csv(usnews, "2021-usnews-us-state-ranks.csv")
jsonlite::stream_out(usnews, file("2021-usnews-us-state-ranks.json"))

cnbc |> 
  select(-overall_rank) |>
  gather(measure, value, -state) |>
  mutate(
    state = fct_reorder(state, value, sum) |> fct_rev()
  ) |> 
  ggplot() +
  geom_boxplot(
    aes(state, value),
    color = "black", fill = "black",
    linewidth = 0.125, width = 0.4,
    outlier.size = 0.25
  ) +
  scale_y_continuous(sec.axis = dup_axis())+
  coord_flip() +
  labs(
    y = "Rank Range", x = NULL,
    title = "CNBC 2022 U.S. State Rankings"
  ) +
  theme_cs(grid = "X", mode = "light") -> gg_cnbc

usnews |>
  gather(measure, value, -state) |>
  mutate(
    state = fct_reorder(state, value, sum) |> fct_rev()
  ) |>
  ggplot() +
  geom_boxplot(
    aes(state, value),
    color = "black", fill = "black",
    linewidth = 0.125, width = 0.4,
    outlier.size = 0.25
  ) +
  scale_y_continuous(sec.axis = dup_axis())+
  coord_flip() +
  labs(
    y = "Rank Range", x = NULL,
    title = "U.S. News 2021 U.S. State Rankings"
  ) +
  theme_cs(grid = "X", mode = "light") -> gg_usnews

gg_cnbc + gg_usnews
