# source: https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-02-22
library(tidyverse)
library(patchwork)
library(sysfonts)

font_add_google("Oswald")

freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv') %>% 
  janitor::clean_names()


freedom_status = freedom %>% 
  count(country, status)


freedom_status_par = freedom_status %>% 
  mutate(status_binary = case_when(status == "F" ~ 1,
                          TRUE ~ 0)) %>% 
  group_by(country) %>% 
  mutate(sum_status = sum(status_binary))

freedom_nofree = freedom_status_par %>% 
  filter(sum_status == 0) %>% 
  select(country) %>% 
  left_join(freedom)


freedom_free = freedom_status_par %>% 
  filter(sum_status == 1)%>% 
  select(country) %>% 
  left_join(freedom)


# countries which did not have status of freedom in any given years
nofree_pr = ggplot(freedom_nofree, aes(x = year, y = pr)) +
  geom_point(aes(color = region_name),
             alpha = 0.02,
             show.legend = FALSE) +
  geom_line(aes(color = region_name, group = interaction(country)),
            alpha = 0.2,
            show.legend = FALSE) +
  scale_y_reverse(breaks = rev(c(1,2,3,4,5,6,7)),
                  limits = c(7,1),
                  labels = c("Low","6","5","4","3","2","High"))+
  theme_minimal()+
  labs(x = "",
       y = "Political rights",
       title = "Countries haven't been free")+
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        text = element_text("Oswald"),
        plot.title = element_text(hjust = 0.5))


nofree_cl = ggplot(freedom_nofree, aes(x = year, y = cl)) +
  geom_point(aes(color = region_name),
             alpha = 0.02,
             show.legend = FALSE) +
  geom_line(aes(color = region_name, group = interaction(country)),
            alpha = 0.2,
            show.legend = FALSE) +
  scale_y_reverse(breaks = rev(c(1,2,3,4,5,6,7)),
                     limits = c(7,1),
                  labels = c("Low","6","5","4","3","2","High")) +
  theme_minimal()+
  labs(x = "Year",
       y = "Civil Liberties")+
  theme(panel.grid = element_blank(),
        text = element_text(family = "Oswald"))


# countries which have been free
free_pr = ggplot(freedom_free, aes(x = year, y = pr)) +
  geom_point(aes(color = region_name),
             alpha = 0.02) +
  geom_line(aes(color = region_name, group = interaction(country)),
            alpha = 0.2) +
  scale_y_reverse(breaks = rev(c(1,2,3,4,5,6,7)),
                     limits = c(7,1))+
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(alpha = 0.5) ) )+
  labs(color = "Region",
       x = "",
       y = "",
       title = "Countries have been free")+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        text = element_text("Oswald"),
        plot.title = element_text(hjust = 0.5))


free_cl = ggplot(freedom_free, aes(x = year, y = cl)) +
  geom_point(aes(color = region_name),
             alpha = 0.02,
             show.legend = FALSE) +
  geom_line(aes(color = region_name, group = interaction(country)),
            alpha = 0.2,
            show.legend = FALSE) +
  scale_y_reverse(breaks = rev(c(1,2,3,4,5,6,7)),
                     limits = c(7,1)) +
  theme_minimal()+
  labs(color = "Region",
       x = "Year",
       y = "")+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        text = element_text("Oswald"))


p1 = (nofree_pr + free_pr) / (nofree_cl + free_cl)

p1

ggsave("freedom_diff_across_countries.png", p1, width = 8,height=6)

