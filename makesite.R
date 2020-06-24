myrmd <- readLines(con="playerprofiles/template.Rmd")

source(paste0(rprojroot::find_rstudio_root_file(), "/content/project/common_project_code.R"))

gp_sd = sd(as.numeric(results$GP), na.rm=TRUE)
gp_mu = mean(as.numeric(results$GP), na.rm=TRUE)
score_sd = sd(as.numeric(res_long$score), na.rm=TRUE)
score_mu = mean(as.numeric(res_long$score), na.rm=TRUE)


most.recent.gs <- gs_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz1 <- zz_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1) 
most.recent.zz3 <- gt_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1)
most.recent.gts <- gts_res_long %>% filter(!is.na(score)) %>% arrange(-match_number) %>% slice(1)

findbe <- function(score, event_round){
  if (sum(is.na(score)) == length(score)){
    res <- "NA"
  } else {
    res <- stringr::str_to_title(event_round[which.max(score)])
  }
  res
}
players <- filter(res_long, Current=="y", team %in% c("GTS", "Zig-Zag RZR")) %>% 
  group_by(safeplayername, Players, team) %>%
  summarize(maxscore=max(score, na.rm=TRUE), 
            bestevent=findbe(score, event_round)) %>%
  ungroup() 

# write templates

do.call(file.remove, list(list.files("content/playerprofiles", full.names = TRUE)))

for (n in 1:nrow(players)){
  this_safeplayer <- dplyr::slice(players, n) %>% 
    dplyr::pull(safeplayername)
  playername <- dplyr::slice(players, n) %>%
    dplyr::pull(Players)
  teamname <- dplyr::slice(players, n) %>%
    dplyr::pull(team)
  maxscore <- dplyr::slice(players, n) %>% 
    dplyr::pull(maxscore)
  bestevent <- dplyr::slice(players, n) %>%
    dplyr::pull(bestevent)
  print(this_safeplayer)
  this_template <- sub("qqplayernameqq", playername, myrmd)
  this_template <- sub("qqpathtoimageqq", paste0(this_safeplayer, ".jpg"), this_template)
  this_template <- sub("qqcurrentteamqq", teamname, this_template)
  this_template <- sub("qqmaxscoreqq", maxscore, this_template)
  this_template <- sub("qqbesteventqq", bestevent, this_template)
  thisplayerdat <- res_long %>%
    ungroup() %>%
    filter(safeplayername==this_safeplayer, Current=="y") %>%
    dplyr::select(-team)
  
  res_long %>%
    filter(team %in% c("GTS", "Zig-Zag RZR")) %>%
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

readr::write_rds(res_long, paste0(rprojroot::find_rstudio_root_file(), "/playerprofiles/res_long.RDS"))
readr::write_rds(re.playersf1, paste0(rprojroot::find_rstudio_root_file(), "/playerprofiles/re.playersf1.RDS"))
readr::write_rds(re.playersf2, paste0(rprojroot::find_rstudio_root_file(), "/playerprofiles/re.playersf2.RDS"))


# blogdown::build_site()

bo <- 1
while(bo!=100){
  x = try(blogdown::build_site(),silent=TRUE)
  if (class(x)=="try-error") {
    cat("ERROR1: ", x, "\n")
    Sys.sleep(1)
    print("retrying...")
    bo <- bo+1
    print(bo)
  } else {
    break
  } 
}

gitadd <- function(dir = rprojroot::find_rstudio_root_file()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git add -A"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

gitcommit <- function(msg = "commit from Rstudio", dir = rprojroot::find_rstudio_root_file()){
  cmd = sprintf("git commit -m\"%s\"",msg)
  system(cmd)
}

# Git push.
gitpush <- function(dir = rprojroot::find_rstudio_root_file()){
  cmd_list <- list(
    cmd1 = tolower(substr(dir,1,2)),
    cmd2 = paste("cd",dir),
    cmd3 = "git push origin master"
  )
  cmd <- paste(unlist(cmd_list),collapse = " & ")
  shell(cmd)
}

git2r::config(user.name = "gidonc",user.email = "gidon.cohen@durham.ac.uk")
gitadd()
gitcommit()
gitpush()
