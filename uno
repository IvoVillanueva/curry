library(tidyverse)
library(rvest)
library(ggtext)
library(janitor)
library(ggimage)


# funciones ---------------------------------------------------------------



year <- 2010:2022

curry <- function(year){
url <- paste0("https://www.basketball-reference.com/teams/GSW/",year, ".html") %>%
  read_html()
df <- url %>%
  html_nodes(xpath = '//comment()') %>%
  html_text() %>%
  paste(collapse='') %>%
  read_html() %>%
  html_node("table#per_poss") %>%
  html_table()%>%
  clean_names()

df$year <- year
return(df)

}

currydf <- map_df(year, curry)

liga <- function(year){
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, "_per_poss.html") %>%
  read_html()

df <- url %>%
  html_element("#per_poss_stats") %>%
  html_table() %>%
  clean_names() %>%
  filter(player != "Player")

df$year <- year
return(df)

}

liga_df <- map_df(year, liga)




liga_equipos <- function(year){
  url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html") %>%
    read_html()
  df <- url %>%
    html_element("#per_poss-team") %>%
    html_table() %>%
    clean_names()

  df$year <- year
  return(df)


}

liga_equipos_df <- map_df(year, liga_equipos)


# tablas ------------------------------------------------------------------



equiposdf <- liga_equipos_df %>%
  group_by(year) %>%
  summarise(
    x3ptott  = mean(x3p),
    x3patott = mean(x3pa),
    stltott  = mean(stl),
    tovtott  = mean(tov),
    .groups   = "drop"
  )



ligadf <- liga_df %>%
  group_by(year) %>%
  mutate_at(vars(mp, x3p, x3pa, stl, tov), as.numeric) %>%
  filter(mp >= mean(mp)) %>%
  summarise(
    x3ptotl  = mean(x3p),
    x3patotl = mean(x3pa),
    stltotl  = mean(stl),
    tovtotl  = mean(tov),
    .groups   = "drop"
  )

teams <- equiposdf %>%
  left_join(ligadf) %>%
  mutate(
    totalt = x3patotl / x3patott,
    total3pt = x3ptotl / x3ptott,
    total_stlt = stltotl / stltott,
    totaltovt = tovtotl / tovtott
  ) %>% select(year, totalt, total3pt, total_stlt, totaltovt)


curry_df_total <- currydf %>%
  group_by(year) %>%
  summarise(
    x3ptotalc  = sum(x3p),
    x3patotalc = sum(x3pa),
    stltotalc  = sum(stl),
    tovtotalc  = sum(tov),
    .groups   = "drop"
  )



curry_df <- currydf %>%
  rename(name = x) %>%
  filter(name == "Stephen Curry") %>%
  select(year, x3p, x3pa, stl, tov)

curry_df <- curry_df %>%
  left_join(curry_df_total) %>%
  mutate(total = x3pa/x3patotalc,
         total3p = x3p/x3ptotalc,
         total_stl = stl/stltotalc,
         totaltov = tov/tovtotalc) %>%
  left_join(teams)
