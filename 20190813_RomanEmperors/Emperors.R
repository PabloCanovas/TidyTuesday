library(tidyverse)
library(lubridate)
library(viridis)

emperors_original <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors <- emperors_original %>%
  mutate_at(vars(reign_start, reign_end, birth, death), ~ ymd(.)) %>% 
  mutate(birth = if_else(notes %>% str_detect("BCE"), birth %>% magrittr::add(years(-2*year(birth))), birth, birth),
         reign_start = if_else(name == "Augustus", reign_start %>% magrittr::add(years(-2*year(reign_start))), reign_start),
         name = fct_reorder(name, reign_start)) %>%
  select(name, birth, death, reign_start, reign_end, dynasty) 

emperors %>% 
  ggplot() + 
  geom_segment(aes(x = reign_start, xend = reign_end, y = name, yend = name, color = fct_reorder(dynasty, reign_start)),
               size = 2, lineend = "round") + 
  geom_segment(aes(x = birth, xend = death, y = name, yend = name, color = fct_reorder(dynasty, reign_start)),
               size = 3, lineend = "round", alpha = .35) + 
  labs(title = "The reign of Roman Emperors by dynasty",
       x = NULL, y = NULL, 
       color = "Dynasty") +
  scale_color_viridis_d(option = "B", end = .9) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "ivory1"),
        plot.title = element_text(size = 25 ,face = "bold", hjust = 0.5, family = "serif"),
        legend.title = element_text(size = 20 ,face = "bold"),
        text = element_text(size=16, family="serif"),
        axis.text.x = element_text(size = 20, face = "bold")) +
  scale_x_date(breaks = c(ymd("00000101"), ymd("01000101"), ymd("02000101"), ymd("03000101"), ymd("04000101")),
               labels = c("0", "I a.C", "II a.C", "III a.C", "IV a.C")) +
  
  geom_vline(xintercept = ymd("00690101"), col = "black", size = 1) + 
  geom_curve(aes(x = ymd("00460101"), y = 35, xend = ymd("00650101"), yend = 32),
             arrow = arrow(length = unit(0.1, "inch")), size = 1, color = "black", curvature = 0.3) +
  annotate(geom = "text", col = "black", x = ymd("00460101"), y = 37, 
           size = 3.7, label = "Year of the Four\n Emperors", fontface = "bold") +
  
  geom_curve(aes(x = ymd("01320101"), y = 7.5, xend = ymd("01420101"), yend = 11),
             arrow = arrow(length = unit(0.1, "inch")), size = 1, col = "mediumpurple4", curvature = 0.3) +
  annotate(geom = "text", col = "mediumpurple4", x = ymd("01320101"), y = 6, 
           size = 3.7, label = "The Five Good Emperors", fontface = "bold") +
  annotate(geom = "rect", xmin = ymd("00960101"), xmax = ymd("01810601"), ymin = -Inf, ymax = Inf,
           fill = "mediumpurple4", alpha = 0.1) + 
  
  geom_curve(aes(x = ymd("03050101"), y = 25, xend = ymd("02750101"), yend = 22),
             arrow = arrow(length = unit(0.1, "inch")), size = 1, color = "indianred3", curvature = -0.3) +
  annotate(geom = "text", col = "indianred3", x = ymd("03050101"), y = 27, 
           size = 3.7, label = "Crisis of the \nThird Century", fontface = "bold") + 
  annotate(geom = "rect", xmin = ymd("02350101"), xmax = ymd("02840101"), ymin = -Inf, ymax = Inf, 
           fill = "indianred3", alpha = 0.1) +
  
  geom_vline(xintercept = ymd("03941231"), col = "orangered1", size = 1) + 
  geom_curve(aes(x = ymd("03700101"), y = 42, xend = ymd("03900101"), yend = 39),
             arrow = arrow(length = unit(0.1, "inch")), size = 1,  col = "orangered1", curvature = 0.3) +
  annotate(geom = "text", col = "orangered1", x = ymd("03700101"), y = 44, 
           size = 3.7, label = "Fall of the \nWestern Empire", fontface = "bold") 

ggsave(filename = "RomanEmperors.png", width = 16, height = 9, dpi = 100)
  
  
         