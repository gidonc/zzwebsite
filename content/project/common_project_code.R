## ----setup----
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(ggrepel)
library(arm)
library(formattable)
#library(rstanarm)
library(kableExtra)
drive_auth("gidon.d.cohen@gmail.com")
sheets_auth(token=drive_token())

myclean <- function(x, illegal="[!. @$£()*&^~#]", replacement =""){
  gsub(illegal, replacement, x)
}

zz_matches <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=3)
zz_results <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=4, col_types="c", skip = 3) %>%
  mutate(team="Zig-Zag RZR")

gs_matches <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=3)
gs_results <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=4, col_types="c", skip = 3) %>%
  mutate(team="GS")


cb <- function(x) {
  range <- max(abs(x))
  width <- round(abs(x / range * 50), 2)
  ifelse(
    x > 0,
    paste0(
      '<span style="display: inline-block; border-radius: 2px; ', 
      'padding-right: 2px; background-color: lightgreen; width: ', 
      width, '%; margin-left: 50%; text-align: left;">', x, '</span>'
    ),
    paste0(
      '<span style="display: inline-block; border-radius: 2px; ', 
      'padding-right: 2px; background-color: lightpink; width: ', 
      width, '%; margin-right: 50%; text-align: right; float: right; ">', x, '</span>'
    )
  )
}


##----cleancalculate----
zz_results <- zz_results %>%
  mutate(PlayerTeam=paste0(Players, "(", team, ")")) 

zz_matches <- zz_matches %>%
  mutate(event_round=tolower(event_round))

zz_res_long <- zz_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(zz_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

gs_results <- gs_results %>% 
  mutate(PlayerTeam=paste0(Players, "(", team, ")")) 

gs_matches <- gs_matches %>%
  mutate(event_round=tolower(event_round))

gs_res_long <- gs_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(gs_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

results <- bind_rows(zz_results, gs_results) %>%
  mutate(
    safeplayername = myclean(fs::path_sanitize(Players)),
    playerprofileurl = paste0("playerprofiles/", safeplayername, "/"))

res_long <- bind_rows(zz_res_long, gs_res_long) %>%
  group_by(event_round) %>%
  mutate(
    GP=as.numeric(GP),
    std_GP = (GP-mean(GP, na.rm=TRUE))/sd(GP, na.rm=TRUE),
    score = as.numeric(score),
    event_sd = sd(score, na.rm=TRUE), 
    event_mu = mean(score, na.rm=TRUE),
    std_score = (score-event_mu)/event_sd,
    has_zig_zag = sum(team=="Zig-Zag Racers"&!is.na(score), na.rm=TRUE)>0,
    has_gs = sum(team=="GS"&!is.na(score), na.rm=TRUE)>0)


gp_sd = sd(as.numeric(results$GP), na.rm=TRUE)
gp_mu = mean(as.numeric(results$GP), na.rm=TRUE)
score_sd = sd(as.numeric(res_long$score), na.rm=TRUE)
score_mu = mean(as.numeric(res_long$score), na.rm=TRUE)

most.recent.gs <- gs_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz1 <- zz_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 

f1 <- lmer(std_score~ has_zig_zag + has_gs + (1|PlayerTeam), res_long)
res_long_recent <- filter(res_long, matches_ago<16)
f2 <- lmer(std_score~ has_zig_zag + has_gs + (1|PlayerTeam), res_long_recent)
