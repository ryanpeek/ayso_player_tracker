# save/auth
library(googledrive)
library(googlesheets4)
library(shiny)
library(dplyr)
library(tibble)

# SET OPTIONS -------------------------------------------------------------

# options(gargle_quiet = FALSE) # see the messages for debugging

# set these at top
options(gargle_oauth_cache = here::here(".secrets")) #proj specific cache

#gargle::gargle_oauth_cache() # double check location
#gargle::gargle_oauth_sitrep()

# DO ONCE: SAVE TOKEN --------------------------------------------------

# Authenticate to produce the token in the cache folder
# drive_auth(email = "ryan.a.peek@gmail.com",
#            scopes="https://www.googleapis.com/auth/spreadsheets.readonly")

# list.files(here::here(".secrets/")) # double check token
# sheets_auth(token = drive_token())

# READ IN -----------------------------------------------------------------

# id: 1BZgFn0cwFgaZB5_xUQ7TQvHiMsVnxP3WseguvuH5tAE
drive_auth(cache = here::here(".secrets"), email="ryan.a.peek@gmail.com")
sheets_auth(token = drive_token())

# read the Spawning google sheet by ID
wb <- drive_get("PlayerTime")
data_clean <- read_sheet(wb, sheet = 1) %>% 
  janitor::clean_names()  %>% 
  pivot_longer(cols = starts_with("q"), names_to = "quarter", values_to = "player")


ppos <- tibble::tibble(pos=c("GK","RB","CB","LB","RM", "CM", "LM", "Striker"), 
               xc=c(4, 17, 15, 17, 34, 32, 34, 45),
               yc=c(50, 21, 50, 79, 18, 50, 82, 50)) %>% 
  mutate(pos = factor(pos, levels=c("GK","RB","CB","LB","RM", "CM", "LM", "Striker")))
currDate <- Sys.Date()

# AFTER -------------------------------------------------------------------

# Authenticate in ShinyApps.io
# rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# setwd() in your App directory
# library(rsconnect)
# deployApp()

