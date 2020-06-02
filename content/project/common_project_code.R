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

# Re-read the data if the stored data is more than three hours old (established date from just one of the stored data files)
readdata <- Sys.time() - file.info(paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_results.RDS"))$mtime > lubridate::hours(3)

myclean <- function(x, illegal="[!. @$£()*&^~#]", replacement =""){
  gsub(illegal, replacement, x)
}

if(readdata==TRUE){
  zz_matches <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=3) %>%
    mutate(teamname="RZR")
  zz_results <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=4, col_types="c", skip = 3) %>%
    mutate(team="Zig-Zag RZR")
  
  transfers <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=7, col_types="c")
  
  zz_oldscores <- read_sheet("1ftRMXTRL84SO-eedX8EIof8i_5C64qemwskffr8DUXA", sheet=6, col_types="c")
  
  
  gs_matches <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=3) %>%
    mutate(teamname="GS")
  gs_results <- read_sheet("1VfIqaB--8Y-pF06zp1KEBrTJyX1A_33XKG30n38Da14", sheet=4, col_types="c", skip = 3) %>%
    mutate(team="GS")
  
  gt_gts_matches <- read_sheet("1oGTfeDSjWoqHaOZYrw09kMxP42bo_sBBVVKJbEThsMI", sheet=3) %>%
    mutate(teamname=ifelse(match_number < 71, "GT", "GTS"))
  gt_gts_results <- read_sheet("1oGTfeDSjWoqHaOZYrw09kMxP42bo_sBBVVKJbEThsMI", sheet=4, col_types="c", skip = 3) 
  readr::write_rds(zz_matches, paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_matches.RDS"))
  readr::write_rds(zz_results, paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_results.RDS"))
  readr::write_rds(zz_oldscores, paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_oldscores.RDS"))
  readr::write_rds(gs_matches, paste0(rprojroot::find_rstudio_root_file(), "/resources/gs_matches.RDS"))
  readr::write_rds(gs_results, paste0(rprojroot::find_rstudio_root_file(), "/resources/gs_results.RDS"))
  readr::write_rds(gt_gts_matches, paste0(rprojroot::find_rstudio_root_file(), "/resources/gt_gts_matches.RDS"))
  readr::write_rds(gt_gts_results, paste0(rprojroot::find_rstudio_root_file(), "/resources/gt_gts_results.RDS"))
  readr::write_rds(transfers, paste0(rprojroot::find_rstudio_root_file(), "/resources/transfers.RDS"))
} else {
  zz_matches <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_matches.RDS"))
  zz_results <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_results.RDS"))
  zz_oldscores <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/zz_oldscores.RDS"))
  gs_matches <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/gs_matches.RDS"))
  gs_results <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/gs_results.RDS"))
  gt_gts_matches <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/gt_gts_matches.RDS"))
  gt_gts_results <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/gt_gts_results.RDS"))
  transfers <- readr::read_rds(paste0(rprojroot::find_rstudio_root_file(), "/resources/transfers.RDS"))
}

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
  mutate(event_round=tolower(event_round),
         event_std = tolower(event))


zz_res_long <- zz_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(zz_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

common_cols <- c("Players", "GP", "Current", "Destination")

gt_matches <- gt_gts_matches %>% filter(teamname=="GT")
gts_matches <- gt_gts_matches %>% filter(teamname=="GTS")
gt_cols <- c(common_cols,  gt_matches %>% pull(event_round))
gts_cols <- c(common_cols, gts_matches %>% pull(event_round))
gt_results <- gt_gts_results %>%
  dplyr::select(one_of(gt_cols)) %>%
  mutate(team="GT", PlayerTeam=paste0(Players, "(", team, ")")) 

gts_results <- gt_gts_results %>%
  dplyr::select(one_of(gts_cols)) %>%
  mutate(team="GTS", PlayerTeam=paste0(Players, "(", team, ")")) 

gt_matches <- gt_matches %>%
  mutate(event_round=tolower(event_round),
         event_std = tolower(event))

gts_matches <- gts_matches %>%
  mutate(event_round=tolower(event_round),
         event_std = tolower(event))

gt_res_long <- gt_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(gt_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

gts_res_long <- gts_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(gts_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

gs_results <- gs_results %>% 
  mutate(PlayerTeam=paste0(Players, "(", team, ")")) 

gs_matches <- gs_matches %>%
  mutate(event_round=tolower(event_round),
         event_std = tolower(event))




gs_res_long <- gs_results %>%
  pivot_longer(-c(Players, PlayerTeam, GP, Current, team), names_to="event_round", values_to = "score") %>%
  mutate(event_round=tolower(event_round))%>%
  inner_join(gs_matches, by="event_round") %>%
  mutate(matches_ago = max(match_number) + 1 - match_number)

# results <- bind_rows(zz_results, gs_results, gt_results) %>%
#   mutate(
#     safeplayername = myclean(fs::path_sanitize(PlayerTeam)),
#     playerprofileurl = paste0("playerprofiles/", safeplayername, "/"))

results <- bind_rows(zz_results, gts_results) %>%
  mutate(
    safeplayername = myclean(fs::path_sanitize(PlayerTeam)),
    playerprofileurl = paste0("playerprofiles/", safeplayername, "/"))

# res_long <- bind_rows(zz_res_long, gs_res_long, gt_res_long) %>%
#   group_by(event_round) %>%
#   mutate(     
#     participated = ifelse(is.na(score), NA_real_, ifelse(score=="NP", 0, 1)),
#     score = ifelse(is.na(participated), NA_real_, ifelse(participated==1, score, 0)),
#     safeplayername = myclean(fs::path_sanitize(PlayerTeam)),
#     GP=as.numeric(GP),
#     std_GP = (GP-mean(GP, na.rm=TRUE))/sd(GP, na.rm=TRUE),
#     score = as.numeric(score),
#     event_sd = sd(score, na.rm=TRUE), 
#     event_mu = mean(score, na.rm=TRUE),
#     std_score = (score-event_mu)/event_sd,
#     has_zig_zag = sum(team=="Zig-Zag Racers"&!is.na(score), na.rm=TRUE)>0,
#     has_gt = sum(team=="GT"&!is.na(score), na.rm=TRUE)>0,
#     has_gs = sum(team=="GS"&!is.na(score), na.rm=TRUE)>0)

res_long <- bind_rows(zz_res_long, gts_res_long, gs_res_long, gt_res_long) %>%
  group_by(event_round) %>%
  mutate(     
    participated = ifelse(is.na(score), NA_real_, ifelse(score=="NP", 0, 1)),
    score = ifelse(is.na(participated), NA_real_, ifelse(participated==1, as.numeric(score), 0)),
    safeplayername = myclean(fs::path_sanitize(PlayerTeam)),
    GP=as.numeric(GP),
    std_GP = (GP-mean(GP, na.rm=TRUE))/sd(GP, na.rm=TRUE),
    score = as.numeric(score),
    event_sd = sd(score, na.rm=TRUE), 
    event_mu = mean(score, na.rm=TRUE),
    std_score = (score-event_mu)/event_sd,
    has_zig_zag = sum(team=="Zig-Zag Racers"&!is.na(score), na.rm=TRUE)>0,
    has_gts = sum(team=="GTS"&!is.na(score), na.rm=TRUE)>0)


gp_sd = sd(as.numeric(results$GP), na.rm=TRUE)
gp_mu = mean(as.numeric(results$GP), na.rm=TRUE)
score_sd = sd(as.numeric(res_long$score), na.rm=TRUE)
score_mu = mean(as.numeric(res_long$score), na.rm=TRUE)

most.recent.gs <- gs_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz1 <- zz_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.gt <- gt_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.gts <- gts_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 

f1 <- lmer(std_score~ has_zig_zag + has_gts + (1|PlayerTeam), res_long)
res_long_recent <- filter(res_long, matches_ago<16)
f2 <- lmer(std_score~ has_zig_zag + has_gts + (1|PlayerTeam), res_long_recent)


zz_oldscores <- zz_oldscores %>% 
  mutate(zz_cups_before = as.numeric(zz_cups_before),
         zz_points = as.numeric(zz_points),
         opp_points = as.numeric(opp_points),
         zz_cup_change=as.numeric(zz_cup_change),
         zzcupst=zz_cups_before/1000,
         result = ifelse(zz_points>opp_points, "wins", "losses"),
         teamname = ifelse(team=="zz", "zig-zag", "rizzla"),
         winmargin=abs(zz_points-opp_points),
         magnitude_cupchange=abs(zz_cup_change),
         match_number=as.numeric(match_number))
zz_oldnodups <- zz_oldscores %>% slice(10:nrow(.))


zz_scores <- zz_matches %>% 
  mutate(zzcupst=zz_cups_before/1000,
         result = ifelse(zz_points>opp_points, "wins", "losses"),
         team = "zz",
         winmargin=abs(zz_points-opp_points),
         magnitude_cupchange=abs(zz_cup_change),
         wincup = ifelse(result=="wins", zz_cups_before, opp_cups_before),
         losecup = ifelse(result=="wins", opp_cups_before, zz_cups_before),
         winrank = ifelse(result=="wins", zz_rank_before, opp_rank_before),
         loserank = ifelse(result=="wins", opp_rank_before, zz_rank_before),
         wingp = ifelse(result=="wins", 5427, gp_average),
         losegp = ifelse(result=="wins", gp_average, 5427),
         cupdiff = wincup-losecup,
         gpdiff = wingp-losegp,
         rankdiff = loserank-winrank,
         cupratio = wincup/losecup,
         gpratio = wingp/losegp,
         rankratio = winrank/loserank,
         res=ifelse(result=="wins", 1, 0),
         res=ifelse(is.na(res)&zz_cup_change<0, 0, res),
         result=ifelse(is.na(result)&zz_cup_change<0, "losses", result)
  )


zz_oldscores <- zz_oldscores %>% 
  mutate(zz_cups_before = as.numeric(zz_cups_before),
         zz_points = as.numeric(zz_points),
         opp_points = as.numeric(opp_points),
         zz_cup_change=as.numeric(zz_cup_change),
         zzcupst=zz_cups_before/1000,
         result = ifelse(zz_points>opp_points, "wins", "losses"),
         teamname = ifelse(team=="zz", "zig-zag", "rizzla"),
         winmargin=abs(zz_points-opp_points),
         magnitude_cupchange=abs(zz_cup_change),
         match_number=as.numeric(match_number))
zz_oldnodups <- zz_oldscores %>% slice(10:nrow(.))

zz_scores <- zz_matches %>% 
  mutate(zzcupst=zz_cups_before/1000,
         result = ifelse(zz_points>opp_points, "wins", "losses"),
         team = "zz",
         winmargin=abs(zz_points-opp_points),
         magnitude_cupchange=abs(zz_cup_change),
         wincup = ifelse(result=="wins", zz_cups_before, opp_cups_before),
         losecup = ifelse(result=="wins", opp_cups_before, zz_cups_before),
         winrank = ifelse(result=="wins", zz_rank_before, opp_rank_before),
         loserank = ifelse(result=="wins", opp_rank_before, zz_rank_before),
         wingp = ifelse(result=="wins", 5427, gp_average),
         losegp = ifelse(result=="wins", gp_average, 5427),
         cupdiff = wincup-losecup,
         gpdiff = wingp-losegp,
         rankdiff = loserank-winrank,
         cupratio = wincup/losecup,
         gpratio = wingp/losegp,
         rankratio = winrank/loserank,
         res=ifelse(result=="wins", 1, 0),
         res=ifelse(is.na(res)&zz_cup_change<0, 0, res),
         result=ifelse(is.na(result)&zz_cup_change<0, "losses", result)
  )


all_zz_matches <- zz_oldnodups %>%
  filter(team=="zz") %>%
  mutate(zz_rank_before=as.numeric(zz_rank_before),
         opp_cups_before=as.numeric(opp_cups_before)) %>%
  #select(zz_cups_before, opposition, match_number, opp_cups_before)  %>%
  bind_rows(zz_scores) %>% 
  mutate(actual_match_no=rank(match_number, desc)) %>%
  mutate(event=ifelse(is.na(event), "unrecorded", event),
         event_std=ifelse(is.na(event_std), "unrecorded", event_std)) %>%
  mutate(teamname="RZR")


gs_events <- gs_matches %>%
  dplyr::rename(gs_event=event) %>%
  group_by(gs_event, event_std) %>%
  summarize(gs_no = min(match_number)) %>%
  arrange(gs_no) %>%
  ungroup() %>%
  mutate(gs_event_id = row_number())


gt_events <- gt_matches %>%
  dplyr::rename(gt_event=event) %>%
  group_by(gt_event, event_std) %>%
  summarize(gt_no = min(match_number)) %>%
  arrange(gt_no) %>%
  ungroup() %>%
  mutate(gt_event_id = row_number())

gts_events <- gts_matches %>%
  dplyr::rename(gts_event=event) %>%
  group_by(gts_event, event_std) %>%
  summarize(gts_no = min(match_number)) %>%
  arrange(gts_no) %>%
  ungroup() %>%
  mutate(gt_event_id = row_number())

zz_events <- all_zz_matches %>%
  dplyr::rename(zz_event=event) %>%
  group_by(zz_event, event_std) %>%
  summarize(zz_no = min(match_number)) %>%
  arrange(zz_no) %>%
  ungroup()%>%
  mutate(zz_event_id = row_number())

events <- dplyr::full_join(gs_events, zz_events, by="event_std") %>%
  dplyr::full_join(gts_events, by="event_std") %>%
  dplyr::full_join(gt_events, by="event_std") %>%
  arrange(zz_no, gts_no, gs_no, gt_no) %>%
  mutate(event_no = row_number())

all_zz_matches <-  all_zz_matches%>% left_join(events %>% dplyr::select(event_std, event_no), by="event_std")
gt_matches <- gt_matches %>% left_join(events %>% dplyr::select(event_std, event_no), by="event_std") 
gts_matches <- gts_matches %>% left_join(events %>% dplyr::select(event_std, event_no), by="event_std") 
gs_matches <- gs_matches %>% left_join(events %>% dplyr::select(event_std, event_no), by="event_std")

all_matches <- bind_rows(all_zz_matches, gs_matches, gt_matches, gts_matches)%>%
  mutate(special_round=ifelse(event_std=="unrecorded", actual_match_no, round))

all_event_rounds <- all_matches  %>%
  group_by(event_std, event_no, special_round) %>%
  summarize() %>%
  arrange(event_no, special_round) %>%
  ungroup()%>%
  mutate(round_number=row_number())

all_matches <- all_matches %>%
  dplyr::left_join(all_event_rounds, by=c("event_std", "event_no", "special_round"))  %>% 
  mutate(zzcupst=zz_cups_before/1000,
         result = ifelse(zz_points>opp_points, "wins", "losses"),
         winmargin=abs(zz_points-opp_points),
         magnitude_cupchange=abs(zz_cup_change),
         wincup = ifelse(result=="wins", zz_cups_before, opp_cups_before),
         losecup = ifelse(result=="wins", opp_cups_before, zz_cups_before),
         winrank = ifelse(result=="wins", zz_rank_before, opp_rank_before),
         loserank = ifelse(result=="wins", opp_rank_before, zz_rank_before),
         cupdiff = wincup-losecup,
         gpdiff = wingp-losegp,
         rankdiff = loserank-winrank,
         cupratio = wincup/losecup,
         gpratio = wingp/losegp,
         rankratio = winrank/loserank,
         res=ifelse(result=="wins", 1, 0),
         res=ifelse(is.na(res)&zz_cup_change<0, 0, res),
         result=ifelse(is.na(result)&zz_cup_change<0, "losses", result)
  )


ordered_events <- events %>% arrange(event_no)

topscores<-all_matches %>% filter(!is.na(Rank1Cups))

transfers_long <- transfers %>%
  rowid_to_column("crossprofile_id") %>%
  pivot_longer(names_to="team", 
               cols=-crossprofile_id) %>%
  separate(team, into=c("team", "profile")) %>%
  group_by(crossprofile_id, profile) %>%
  mutate(profile_id = dplyr::row_number()) %>%
  mutate(crossteam_name = value[!is.na(value)][1]) %>%
  ungroup() %>%
  mutate(team=ifelse(team=="zz1", "Zig-Zag RZR", ifelse(team=="zz2", "GS", "GT")))

res_long <- res_long %>%
  left_join(all_matches %>% dplyr::select(teamname, round, event_std, round_number)) 
res_long <- res_long%>%
  mutate(event_std_fct = factor(event_std, levels=ordered_events$event_std))

res_long <- res_long %>%
  filter(!is.na(score)) %>%
  group_by(round_number) %>%
  arrange(-score) %>%
  mutate(global_rank = dplyr::row_number()) %>%
  mutate(global_count = n()) %>%
  group_by(team, round_number) %>%
  mutate(team_rank = dplyr::row_number()) %>%
  mutate(team_count=n()) %>%
  mutate(global_rank_out = paste(global_rank, "of", global_count),
         team_rank_out = paste(team_rank, "of", team_count))

f1 <- lmer(std_score~ has_zig_zag +  has_gts + (1|PlayerTeam), res_long)
res_long_recent <- filter(res_long, matches_ago<16)
f2 <- lmer(std_score~ has_zig_zag + has_gts + (1|PlayerTeam), res_long_recent)

results_summary <- res_long %>%
  filter(!is.na(score)) %>%
  group_by(PlayerTeam) %>%
  summarize(n=n(), Score=mean(score, na.rm = TRUE), Participation=mean(participated)*100)

recent_results_summary <- res_long_recent %>%
  filter(!is.na(score)) %>%
  group_by(PlayerTeam) %>%
  summarize(n=n(), Score=mean(score, na.rm = TRUE), Participation=mean(participated)*100)

re.playersf1<-ranef(f1)$PlayerTeam %>% 
  rownames_to_column("PlayerTeam") %>%
  as_tibble() %>%
  rename(StdScore=2) %>%
  mutate(StdScore=round(StdScore*score_sd + score_mu, 0)) %>%
  arrange(-StdScore) %>%
  left_join(results) %>%
  left_join(results_summary) %>%
  arrange(-StdScore)  %>%
  filter(Current=="y") %>%
  rowid_to_column("Rank") %>%
  group_by(team) %>%
  mutate(Team_rank = dplyr::row_number(),
         bottom_10 = Team_rank>(max(Team_rank)-11),
         bottom_15 = Team_rank>(max(Team_rank)-16)) %>%
  ungroup()

re.playersf2<-ranef(f2)$PlayerTeam %>% 
  rownames_to_column("PlayerTeam") %>%
  as_tibble() %>%
  rename(StdScore=2) %>%
  mutate(StdScore=round(StdScore*score_sd + score_mu, 0)) %>%
  arrange(-StdScore) %>%
  left_join(results) %>%
  left_join(recent_results_summary) %>%
  arrange(-StdScore)  %>%
  filter(Current=="y") %>%
  rowid_to_column("Rank") %>%
  group_by(team) %>%
  mutate(Team_rank = dplyr::row_number(),
         bottom_10 = Team_rank>(max(Team_rank)-11),
         bottom_15 = Team_rank>(max(Team_rank)-16)) %>%
  ungroup()

