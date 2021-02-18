library(tidyverse)
library(extrafont)
library(showtext)
font.add("FuturaDisplay", "C:/Users/Work/Downloads/Futura Display BQ Regular.ttf")
showtext.auto()

# Load data ----
mha <- read_csv('MHA_imdb.csv')
mha <- mha[-c(1,6,7)]
mha$ep_num <- c(1:89)
mha$Season <- as.factor(mha$Season)

# Prepare the data -----
mha_episodes <- mha %>% 
  distinct(Season, Episode, ep_num, Title, `IMDB Rating`) %>%
  group_by(Season) %>% 
  mutate(season_avg_rating = mean(`IMDB Rating`))

# for the average IMDB rating per season line - geom_line
season_avg <- mha_episodes %>% 
  summarize(start_x = min(ep_num) - 0.1,
            end_x = max(ep_num) + 0.1,
            y = unique(season_avg_rating)) %>% 
  pivot_longer(cols = c(start_x, end_x),
               names_to = "type",
               values_to = "x")

# for the season labels
mha_labels <- mha_episodes %>% 
  summarise(x = min(ep_num),
            y = unique(season_avg_rating))


# logo and episode pics
bg <- png::readPNG('plus_ultra.png')
bg <-  grid::rasterGrob(bg, interpolate=TRUE)


logo <- png::readPNG('mha_logo.png')
logo <-  grid::rasterGrob(logo, interpolate=TRUE)

s1 <- png::readPNG('s1_e12.png')
s1 <-  grid::rasterGrob(s1, interpolate=TRUE)

s2 <- png::readPNG('s2_e11.png')
s2 <-  grid::rasterGrob(s2, interpolate=TRUE)

s3 <- png::readPNG('s3_e11.png')
s3 <-  grid::rasterGrob(s3, interpolate=TRUE)

s4 <- png::readPNG('s4_e25.png')
s4 <-  grid::rasterGrob(s4, interpolate=TRUE)


# Theme update
theme_set(theme_dark())
theme_update(panel.grid.minor = element_blank(),
             panel.grid.major.x = element_blank(),
             legend.position = "none",
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_text(size = 50, family = "FuturaDisplay"),
             axis.title.y = element_text(size = 126, family = "FuturaDisplay"))

mha_episodes %>% 
  ggplot(aes(ep_num, `IMDB Rating`, color = Season)) +
  annotation_custom(grob = bg) + 
  labs(y = "Episode Rating", caption = "by @xndrmcw") +
  # added horizontal lines over the background
  geom_hline(yintercept = c(6, 7, 8, 9, 10), color = "white") +
  # added the images to the graph. make sure to do this after you 
  # decide on your dimensions otherwise thinks get annoying
  annotation_custom(grob = s1, xmin = -3,
                    xmax = 16.5, ymin = 9.4, ymax = 10.1) +
  annotation_custom(grob = s2, xmin = 22,
                    xmax = 40.5, ymin = 9.7, ymax = 10.4) +
  annotation_custom(grob = s3, xmin = 48,
                    xmax = 66.5, ymin = 9.9, ymax = 10.6) +
  annotation_custom(grob = s4, xmin = 73,
                    xmax = 91, ymin = 9.9, ymax = 10.6) +
  geom_segment(aes(xend = ep_num, yend = season_avg_rating, color = Season), 
               size = 2) +
  geom_line(data = season_avg,
            aes(x, y), size = 1.5) +
  geom_point(size = 3.5) +
  annotation_custom(grob = logo, xmin = 33, xmax = 56, ymin = 6, ymax = 6.8) +
  scale_y_continuous(limits = c(6,10.5)) +
  scale_color_manual(values = c("1" = "#F8EC8A", "2" = "#2F8B65",
                                "3" = "#1F2F77", "4" = "#EE9654")) +
  annotate("text", x = 63, y = 6.2, label = "1 Dot = 1 Episode",
           size = 28, family = "FuturaDisplay", ) +
  annotate(geom = "label", x = 6, y = 9.2, size = 20, ## label
           label = " S1:E12 [9.3/10]\nAll Might", 
           family = "FuturaDisplay",
           lineheight = 0.3,
           color = "#D12028",
           #label.size = NA,
           fill = "#FFDD00") +
  annotate(geom = "label", x = 32, y = 9.5, size = 20, ## label
           label = " S2:E10 [9.6/10]\nShoto Todoroki: Origin", 
           family = "FuturaDisplay",
           lineheight = 0.3,
           color = "#D12028",
           #label.size = NA,
           fill = "#FFDD00") +
  annotate(geom = "label", x = 57, y = 9.7, size = 20, ## label
           label = " S3:E11 [9.8/10]\nOne For All", 
           family = "FuturaDisplay",
           lineheight = 0.3,
           color = "#D12028",
           #label.size = NA,
           fill = "#FFDD00") +
  annotate(geom = "label", x = 82, y = 9.7, size = 20, ## label
           label = " S4:E25 [9.8/10]\nHis Start", 
           family = "FuturaDisplay",
           lineheight = 0.3,
           color = "#D12028",
           #label.size = NA,
           fill = "#FFDD00") +
  ggsave(here::here("graph.png"), device = "png",
         type = "cairo", width = 17.3, height = 9.6, dpi = 300)


