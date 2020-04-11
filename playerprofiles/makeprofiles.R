myrmd <- readLines(con="playerprofiles/template.Rmd")

source(paste0(rprojroot::find_rstudio_root_file(), "/content/project/common_project_code.R"))
# library(googledrive)
# library(googlesheets4)
# library(tidyverse)
# library(ggrepel)
# library(arm)
# library(formattable)
# #library(rstanarm)
# library(kableExtra)
# drive_auth("gidon.d.cohen@gmail.com")
# sheets_auth(token=drive_token())
# 
# 
# zz_matches <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=3)
# zz_results <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=4, col_types="c", skip = 3) %>%
#   mutate(team="Zig-Zag RZR")
# 
# gs_matches <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=3)
# gs_results <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=4, col_types="c", skip = 3) %>%
#   mutate(team="GS")
# 
# gt_matches <- read_sheet("1oGTfeDSjWoqHaOZYrw09kMxP42bo_sBBVVKJbEThsMI", sheet=3)
# gt_results <- read_sheet("1oGTfeDSjWoqHaOZYrw09kMxP42bo_sBBVVKJbEThsMI", sheet=4, col_types="c", skip = 3) %>%
#   mutate(team="GT")
# 
# myclean <- function(x, illegal="[!. @$£()*&^~#]", replacement =""){
#   gsub(illegal, replacement, x)
# }
# cb <- function(x) {
#   range <- max(abs(x))
#   width <- round(abs(x / range * 50), 2)
#   ifelse(
#     x > 0,
#     paste0(
#       '<span style="display: inline-block; border-radius: 2px; ', 
#       'padding-right: 2px; background-color: lightgreen; width: ', 
#       width, '%; margin-left: 50%; text-align: left;">', x, '</span>'
#     ),
#     paste0(
#       '<span style="display: inline-block; border-radius: 2px; ', 
#       'padding-right: 2px; background-color: lightpink; width: ', 
#       width, '%; margin-right: 50%; text-align: right; float: right; ">', x, '</span>'
#     )
#   )
# }
# 
# zz_results <- zz_results %>%
#   mutate(PlayerTeam=paste0(Players, "(", team, ")")) 
# 
# zz_matches <- zz_matches %>%
#   mutate(event_round=tolower(event_round), event_std=tolower(event))
# 
# zz_res_long <- zz_results %>%
#   pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
#   mutate(event_round=tolower(event_round))%>%
#   inner_join(zz_matches, by="event_round") %>%
#   mutate(matches_ago = max(match_number) + 1 - match_number)
# 
# gs_results <- gs_results %>% 
#   mutate(PlayerTeam=paste0(Players, "(", team, ")")) 
# 
# gs_matches <- gs_matches %>%
#   mutate(event_round=tolower(event_round), event_std=tolower(event))
# 
# gs_res_long <- gs_results %>%
#   pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
#   mutate(event_round=tolower(event_round))%>%
#   inner_join(gs_matches, by="event_round") %>%
#   mutate(matches_ago = max(match_number) + 1 - match_number)
# 
# gt_results <- gt_results %>%
#   mutate(PlayerTeam=paste0(Players, "(", team, ")")) 
# 
# gt_matches <- gt_matches %>%
#   mutate(event_round=tolower(event_round), event_std=tolower(event))
# 
# gt_res_long <- gt_results %>%
#   pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
#   mutate(event_round=tolower(event_round))%>%
#   inner_join(gt_matches, by="event_round") %>%
#   mutate(matches_ago = max(match_number) + 1 - match_number)
# 
# 
# results <- bind_rows(zz_results, gs_results, gt_results)
# 
# 
# events <- gs_matches %>%
#   group_by(event_std) %>%
#   summarize(no = min(match_number)) %>%
#   arrange(no) %>%
#   mutate(event_id = row_number())
# 
# res_long <- bind_rows(zz_res_long, gs_res_long, gt_res_long) %>%
#   mutate(event_std=tolower(event)) %>%
#   left_join(events, by="event_std") %>%
#   group_by(event_id, round) %>%
#   mutate(overall_match_number = group_indices()) %>%
#   group_by(event_round) %>%
#   mutate(
#     safeplayername = myclean(fs::path_sanitize(Players)),
#     GP=as.numeric(GP),
#     std_GP = (GP-mean(GP, na.rm=TRUE))/sd(GP, na.rm=TRUE),
#     score = as.numeric(score),
#     event_sd = sd(score, na.rm=TRUE), 
#     event_mu = mean(score, na.rm=TRUE),
#     std_score = (score-event_mu)/event_sd,
#     has_zig_zag = sum(team=="Zig-Zag RZR"&!is.na(score), na.rm=TRUE)>0,
#     has_gs = sum(team=="GS"&!is.na(score), na.rm=TRUE)>0) 
# 


gp_sd = sd(as.numeric(results$GP), na.rm=TRUE)
gp_mu = mean(as.numeric(results$GP), na.rm=TRUE)
score_sd = sd(as.numeric(res_long$score), na.rm=TRUE)
score_mu = mean(as.numeric(res_long$score), na.rm=TRUE)


most.recent.gs <- gs_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz1 <- zz_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz3 <- gt_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1)

findbe <- function(score, event_round){
  if (sum(is.na(score)) == length(score)){
    res <- "NA"
  } else {
    res <- stringr::str_to_title(event_round[which.max(score)])
  }
  res
}
players <- filter(res_long, Current=="y") %>% 
  group_by(safeplayername, Players, team) %>%
  summarize(maxscore=max(score, na.rm=TRUE), 
            bestevent=findbe(score, event_round)) %>%
  ungroup()

# write templates

do.call(file.remove, list(list.files("content/playerprofiles", full.names = TRUE)))

for (n in 1:nrow(players)){
  this_safeplayer <- slice(players, n) %>% 
    pull(safeplayername)
  playername <- slice(players, n) %>%
    pull(Players)
  teamname <- slice(players, n) %>%
    pull(team)
  maxscore <- slice(players, n) %>% 
    pull(maxscore)
  bestevent <- slice(players, n) %>%
    pull(bestevent)
  print(this_safeplayer)
  this_template <- sub("qqplayernameqq", playername, myrmd)
  this_template <- sub("qqpathtoimageqq", paste0(this_safeplayer, ".jpg"), this_template)
  this_template <- sub("qqcurrentteamqq", teamname, this_template)
  this_template <- sub("qqmaxscoreqq", maxscore, this_template)
  this_template <- sub("qqbesteventqq", bestevent, this_template)
  thisplayerdat <- res_long %>%
    filter(safeplayername==this_safeplayer, Current=="y") %>%
    dplyr::select(-team)
  
  res_long %>%
    dplyr::group_by(round_number, event_std, round, event_round, team) %>%
    summarize(med = median(score, na.rm=TRUE), high=quantile(score, probs=.9, na.rm=TRUE), low=quantile(score, probs=.1, na.rm=TRUE)) %>%
    ggplot(aes(round_number, med)) +
    facet_wrap(~team)+
    geom_line(colour="grey70") +
    geom_ribbon(fill="grey50", alpha=.2, aes(ymin=low, ymax=high))+
    theme_bw() +
    geom_line(colour="red", data=thisplayerdat, aes(x=round_number, y=score))
  
  relfolder <- paste0("content/playerprofiles/", this_safeplayer)
  fs::dir_create(relfolder)
  relpath <- paste0(relfolder, "/", this_safeplayer, ".jpg")
  
    ggsave(filename= relpath, device="jpeg")
    
    writeLines(this_template, con=paste0("content/playerprofiles/", this_safeplayer, ".Rmd"))
  
}


