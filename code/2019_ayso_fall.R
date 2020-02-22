# connect to google sheet:
# Player Time Tracker (Responses)

library(tidyverse)
library(googlesheets)
library(janitor)
# library(googledrive)

# read in sheet:
gsTI <- gs_title("Player Time Tracker (Responses)")

# googlesheets::gs_browse(gsTI) # open spreadsheet

dat <- gsTI %>% gs_read(ws = 1) %>% 
  clean_names() %>% 
  select(timestamp:game_no, starts_with("player_time"))

# filter/slice columns for category
dat_s <- dat %>% filter(!game_no=="Scrimmage") #%>% slice(1:6)

# now tidy
dats2 <- dat_s %>%  gather(player, ptime, starts_with("player_time_")) %>%
  mutate(player=gsub("player_time_","", player)) %>%
  separate_rows(ptime) %>%
  mutate_at(vars(starts_with("ptime")), ~gsub("Q", replacement = "", .)) %>% 
  mutate(ptime=as.integer(ptime)) %>% 
  rename(Q_played=ptime) %>% 
  filter(!is.na(Q_played)) %>% 
  group_by(game_no, player) %>% 
  add_count(name = "Q_game_total") #%>% # total played in single game

# Adjust for Total Games Played -------------------------------------------

# total games played
(dats3 <- 
   dats2 %>%   
   group_by(game_no, player) %>% 
   filter(!is.na(Q_played))  %>%
   distinct(game_no, .keep_all = F) %>% 
   group_by(player) %>% add_count(name = "TotalGames") %>% 
   select(player, TotalGames) %>% 
   distinct(player, .keep_all = T))

# rejoin with orig dataset:
dats_final <- left_join(dats2, dats3, by=c("player")) %>% 
  filter(!is.na(TotalGames)) #%>% 
  
dats_cumulative <- dats_final %>% 
  distinct(player, game_no, Q_game_total, TotalGames, .keep_all = T) %>% 
  select(-timestamp, -Q_played) %>% 
  group_by(player) %>% 
  summarize("Q_total" = sum(Q_game_total)) %>% 
  left_join(., dats3, by="player") %>% 
  mutate("Percent_Played" = (Q_total/(TotalGames*4)))


# Total Quarters Played IN a Game -----------------------------------------

# plot quarters played by total for a given game
(gg_gameQ <- dats_final %>% 
  ggplot(.) + geom_tile(aes(x=player, y=Q_played, fill=Q_game_total), col="blue",alpha=0.7, show.legend = T) + facet_wrap(.~game_no) +
  labs(y="Quarters Played", x="") +
  scale_fill_viridis_c("Total Qtrs \n Played" , direction = -1)+
  theme_bw() +
  theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = "figs/quarters_played_per_game.png", width = 11, height = 8, dpi=200)



# Cumulative Quarters Played ----------------------------------------------

# plot total quarters played across ALL games
(gg_totQ <- dats_cumulative %>% 
   ggplot(.) + geom_col(aes(x=player, y=Q_total, fill=as_factor(TotalGames)), col="gray",alpha=0.7, show.legend = T) +
   scale_fill_viridis_d("Games Played", direction=-1) +
   theme_bw() + labs(x="", y="Total Quarters Played") +
   theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = "figs/cumulative_qtrs_played.png", width = 11, height = 8, dpi=200)
  
# percent time played
(gg_totPercent <- dats_cumulative %>% 
    ggplot(.) + geom_col(aes(x=player, y=Percent_Played, fill=as_factor(TotalGames)), col="gray",alpha=0.7, show.legend = T) +
    scale_fill_viridis_d("Games Played", direction=-1) +
    theme_bw() + labs(x="", y="Total Quarters Played") +
    labs(title="Percent Played",subtitle="Adjusted for Games played") +
    theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = "figs/cumulative_percent_played.png", width = 11, height = 8, dpi=200)

# Frequency of Quarters Played --------------------------------------------

# show frequency of which players play certain quarters most.
(gg_freq <- dats_final %>% group_by(player, Q_played) %>% tally %>% 
  ggplot(.) + 
  geom_point(aes(x=player, y=Q_played, size=as.factor(n), color=as.factor(n)), show.legend = T) +
  ylim(1,4) + ylab("Quarters") + xlab("")+
  #scale_size_discrete("Qtrs Played", breaks=c(2,4,5,6,7,8))+
  ggdark::dark_theme_classic(base_family = "Roboto Condensed") +
  viridis::scale_color_viridis(discrete = T, option = "D", direction = 1)+
  guides(color=guide_legend("Qtrs Played"), size=F)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)))

ggsave(filename = "figs/frequency_qtrs_played.png", width = 11, height = 8, dpi=200)



