
library(shiny)
library(tidyverse)
library(googlesheets4)
library(ggsoccer)
library(janitor)

# load data
#load(here::here(), "/data/gs_dat_clean.rda")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$ggSoccerPlot <- renderPlot({
    
        data_df <- data_clean %>%
                filter(grepl(pattern = {input$g}, x = game)) %>% 
                left_join(., ppos, by="pos") %>% 
                filter(player==input$plyrA) %>% 
                filter(!is.na(xc)) %>% 
                arrange(quarter)
        
        # ggplot
        ggplot(data=data_df, aes(x=jitter(xc, 8), y=jitter(yc, 8))) +
            annotate_pitch(fill = "palegreen4", colour = "white") +
            theme_bw(base_family = "Roboto Condensed")+
            stat_density_2d(aes(fill = ..level..), geom = "polygon", 
                            alpha=0.7, show.legend = FALSE) +
            geom_point(data=data_df %>% distinct(pos, .keep_all=TRUE), 
                       aes(x=xc, y=yc), fill="white", pch=21, 
                       size=2, alpha=0.8)+
            ggrepel::geom_text_repel(data=data_df %>% 
                                         distinct(pos, .keep_all=TRUE), 
                                     aes(x=xc, y=yc, label=pos), 
                                     color="black", 
                                     family="Roboto Condensed")+
            labs(subtitle=glue::glue("Soccer Density Plot: {input$plyrA}"),
                 caption=glue::glue("updated: {currDate}")) +
            scale_fill_viridis_c(option = "D")+
            theme_pitch(aspect_ratio = 70/110)
    })

})
