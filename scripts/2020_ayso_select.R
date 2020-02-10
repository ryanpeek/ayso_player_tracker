# connect to google sheet:
# PlayerTime Select

library(tidyverse)
library(tidylog)
library(googlesheets)
library(janitor)
library(ggsoccer)
# https://github.com/FCrSTATS/StatsBomb_WomensData
# https://github.com/FCrSTATS/StatsBomb_WomensData/blob/master/C1_UnderlyingMechanics.md

# Get Data ----------------------------------------------------------------

# read in sheet:
gsTI <- gs_title("PlayerTime")

# googlesheets::gs_browse(gsTI) # open spreadsheet

dat <- gsTI %>% gs_read(ws = 1) %>% 
  clean_names()  %>% 
  pivot_longer(cols = starts_with("q"), names_to = "quarter", values_to = "player")


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

pA <- "Connor"
dat %>% left_join(., ppos, by="pos") %>% 
  filter(player==pA) %>% 
  filter(!is.na(xc)) -> dat_df1

ggplot(data=dat_df1, aes(x=xc, y=yc)) +
  annotate_pitch(fill = "palegreen4", colour = "white") +
  theme_bw(base_family = "Roboto Condensed")+
  stat_density_2d(aes(fill = ..level..), geom = "polygon", alpha=0.7, show.legend = FALSE)+
  geom_point(data=dat_df1 %>% distinct(pos, .keep_all=TRUE), aes(x=xc, y=yc), fill="white", pch=21, size=2, alpha=0.8)+
  ggrepel::geom_text_repel(data=dat_df1 %>% distinct(pos, .keep_all=TRUE), aes(x=xc, y=yc, label=pos), color="black", family="Roboto Condensed")+
  labs(subtitle=glue::glue("Soccer Density Plot: {pA}"),
       caption=glue::glue("updated: {currDate}")) +
  scale_fill_viridis_c(option = "D")+
  theme_pitch(aspect_ratio = 70/110)


# How many Times a Player has played a quarter ------------------------

TotalGames <- dat_df %>% distinct(game) %>% tally() %>% pull(n)
TotalQtrs <- TotalGames*4

df_cumul <- dat_df %>% 
  filter(pos!="Sub", pos!="Gone") %>% 
  distinct(player, game, quarter, .keep_all = TRUE) %>% 
  select(-xc, -yc) %>% 
  group_by(player) %>% tally(name="Q_total") %>% 
  ungroup() %>% 
  left_join(., dat_df[,c(5,4, 2)], by="player") %>% 
  mutate("PrcntPlay" = (Q_total/(TotalGames*4))) 

df_final <- dat_df %>% 
  filter(pos!="Sub", pos!="Gone") %>% 
  distinct(player, game, quarter, .keep_all = TRUE) %>% 
  select(-xc, -yc) %>% 
  group_by(player, game) %>% tally(name="q_game_total") %>% 
  group_by(player) %>% add_count(name="totgames") %>% 
  right_join(., df_cumul)

# plot total quarters played by game
(gg_gameQ <- dat_df %>% filter(!is.na(xc)) %>% 
    ggplot(.) + geom_tile(aes(x=player, y=quarter, fill=pos),
                          col="gray40", alpha=0.7, show.legend = T) + 
    facet_grid(.~game) +
    labs(y="Quarters Played", x="",
         caption=glue::glue("updated: {currDate}")) +
    scale_fill_viridis_d("Game" , direction = -1)+
    theme_bw() +
    theme(axis.text.x = element_text(angle=70, hjust = 1, vjust=1)))

ggsave(filename = glue::glue("figs/quarters_played_per_game_pos_{currDate}.png"), width = 11, height = 8, dpi=200)

# plot total percent played
(gg_gamePrcnt <- df_final %>% 
    ggplot(.) + 
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

ggsave(filename = glue::glue("figs/percent_played_{currDate}.png"), width = 11, height = 8, dpi=200)
