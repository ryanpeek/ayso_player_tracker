# connect to google sheet:
# PlayerTime Select

library(tidyverse)
library(tidylog)
library(googlesheets4)
library(janitor)
library(ggsoccer)
# https://github.com/FCrSTATS/StatsBomb_WomensData
# https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/C1_UnderlyingMechanics.md

# Get Data ----------------------------------------------------------------

# id: 1BZgFn0cwFgaZB5_xUQ7TQvHiMsVnxP3WseguvuH5tAE
# read in sheet:
gs_pt_url <- "https://docs.google.com/spreadsheets/d/1BZgFn0cwFgaZB5_xUQ7TQvHiMsVnxP3WseguvuH5tAE/edit?usp=sharing"
data <- sheets_get(gs_pt_url)

dat_clean <- read_sheet(data$spreadsheet_id) %>% 
  clean_names()  %>% 
  pivot_longer(cols = starts_with("q"), names_to = "quarter", values_to = "player")

# write to rdata
save(dat_clean, file = paste0(here::here(), "/data/gs_dat_clean.rda"))

dat_filt <- dat_clean %>% 
  filter(grepl("^t", game))




# Get Position Locations --------------------------------------------------

ppos <- tibble(pos=c("GK","RB","CB","LB","RM", "CM", "LM", "Striker"), 
               xc=c(4, 17, 15, 17, 34, 32, 34, 45),
               yc=c(50, 21, 50, 79, 18, 50, 82, 50))


# Plot a Pitch ------------------------------------------------------------

(posplot <- ggplot() +
  annotate_pitch(fill = "palegreen4", colour = "white") +
  geom_point(data=ppos, aes(x=xc, y=yc), fill="orange", pch=21, size=3)+
  ggrepel::geom_label_repel(data=ppos, aes(x=xc, y=yc, label=pos))+
  theme_pitch(aspect_ratio = 70/110))

# plotly::ggplotly(posplot)

# Point Density -----------------------------------------------------------

ggplot(data=ppos, aes(x=xc, y=yc)) +
  annotate_pitch(fill = "palegreen4", colour = "white") +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=0.7, show.legend = FALSE)+
  geom_point(data=ppos, aes(x=xc, y=yc), fill="white", pch=21, size=3, alpha=0.8)+
  labs(subtitle="Soccer Density Plot") +
  scale_fill_viridis_c(option = "D")+
  theme_bw(base_family = "Roboto Condensed")+
  theme_pitch(aspect_ratio = 70/110)


# Now add Real Data -------------------------------------------------------
currDate <- Sys.Date()


dat_df <- left_join(dat, ppos, by="pos") %>% 
  mutate(pos = factor(pos, levels=c("GK","RB","CB","LB","RM", "CM", "LM", "Striker")))
levels(dat_df$pos)

# make a player map

pA <- "Armon"
dat %>% left_join(., ppos, by="pos") %>% 
  filter(player==pA) %>% 
  filter(!is.na(xc)) %>% 
  arrange(quarter) -> dat_df1

ggplot(data=dat_df1, aes(x=jitter(xc, 8), y=jitter(yc, 8))) +
  annotate_pitch(fill = "palegreen4", colour = "white") +
  theme_bw(base_family = "Roboto Condensed")+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=0.7, show.legend = FALSE) +
  geom_point(data=dat_df1 %>% distinct(pos, .keep_all=TRUE), aes(x=xc, y=yc), fill="white", pch=21, size=2, alpha=0.8)+
  ggrepel::geom_text_repel(data=dat_df1 %>% distinct(pos, .keep_all=TRUE), aes(x=xc, y=yc, label=pos), color="black", family="Roboto Condensed")+
  labs(subtitle=glue::glue("Soccer Density Plot: {pA}"),
       caption=glue::glue("updated: {currDate}")) +
  scale_fill_viridis_c(option = "D")+
  theme_pitch(aspect_ratio = 70/110)


# How many Times a Player has played a quarter ------------------------

TotalGames <- dat_df %>% distinct(game) %>% tally() %>% pull(n)
TotalQtrs <- TotalGames*4

# cumulative quarters
df_cumul <- dat_df %>% 
  filter(pos!="Sub", pos!="Gone", !is.na(pos)) %>% 
  distinct(player, game, quarter, .keep_all = TRUE) %>% 
  select(-xc, -yc) %>% 
  group_by(player) %>% tally(name="Q_total") %>% 
  ungroup() %>% 
  left_join(., dat_df[,c(5,4, 2)], by="player") %>% 
  mutate("PrcntPlay" = (Q_total/(TotalGames*4))) 

df_final <- dat_df %>% 
  filter(pos!="Sub", pos!="Gone", !is.na(pos)) %>% 
  distinct(player, game, quarter, .keep_all = TRUE) %>% 
  select(-xc, -yc) %>% 
  group_by(player, game) %>% tally(name="q_game_total") %>% 
  group_by(player) %>% add_count(name="totgames") %>% 
  right_join(., df_cumul)


# add manual colors:
poscols <- c("GK"="darkorange2", 
             "RB"="darkseagreen", "LB"="darkseagreen",
             "RM"="royalblue", "LM"="royalblue",
             "CM"="royalblue4", "Striker"="deepskyblue")

# plot total quarters played by game
  (gg_gameQ <- dat_df %>% filter(!is.na(xc)) %>% 
     ggplot(.) + 
     geom_tile(aes(x=player, y=quarter, fill=pos),
                           col="gray40", alpha=0.9, show.legend = T) + 
     facet_grid(.~game) +
     scale_fill_manual("Game", values = poscols)+
     labs(y="Quarters Played", x="",
          caption=glue::glue("updated: {currDate}")) +
     #scale_fill_viridis_d("Game" , direction = -1)+
     theme_bw() +
     theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = glue::glue("figs/presidents_quarters_played_per_game_pos_{currDate}.png"), width = 11, height = 8, dpi=200)

# plot total percent played
(gg_gamePrcnt <- df_final %>% 
    ggplot(.) + 
    geom_col(aes(x=player, y=Q_total), width = .1, 
             fill="royalblue", alpha=0.5) +
    ggimage::geom_emoji(aes(x=player, y=Q_total), image = "26bd") +
    geom_point(aes(x=player, y=Q_total, color=PrcntPlay),
               show.legend = T, size=7, alpha=0.1) + 
    labs(y="Quarters Played", x="",
         subtitle="Total Quarters/Percent Played",
         caption=glue::glue("updated: {currDate}")) +
    ylim(0,TotalQtrs)+
    scale_color_viridis_c("Percent  \nPlayed" , direction = -1)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = glue::glue("figs/presidents_percent_played_{currDate}.png"), width = 11, height = 8, dpi=200)
