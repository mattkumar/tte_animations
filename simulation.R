library(tidyverse)
library(gganimate)
library(ggthemes)
#library(ggbrace)

set.seed(32411)

# pretty data
dat <- data.frame(t = rep(seq(-10, 10, 0.1), each = 2),
                  id = rep(c("Theoretical", "Simulated"))) %>%
  mutate(fn = case_when(id == "Theoretical" ~ exp(-abs(t)) * sin(t-pi),
                        id == "Simulated" ~ exp(-abs(t)) * sin(t-pi) + rnorm(201, mean = 0, sd = 0.01)))  

# define a range where we want big differences between groups
dat <- dat %>%
  mutate(fn = ifelse(id == "Simulated" & between(t, -3, 3), exp(-abs(t)) * sin(t-pi) + rnorm(201, mean = 0, sd = 0.09), fn))

# create a label
df1 <- dat %>% filter(id == "Simulated")
df2 <- dat %>% filter(id != "Simulated")
dat_labels <- data.frame(t = df2$t, id = df1$id, label = df1$fn - df2$fn, fn = df1$fn)

dat <- dat %>% left_join(dat_labels)


gg <- ggplot(data = dat, aes(x = t, y = fn, color = id, group = id)) +
  geom_path(aes(linetype = id , size = id), show.legend = FALSE) +
  scale_x_continuous(breaks = seq(-10,10,1), limits = c(-10, 13)) +
  scale_linetype_manual(values = c('dotted','solid')) +
  scale_size_manual(values = c(0.5,1)) +
  scale_color_manual(values = c('#017BAA','#F7043E')) +
  geom_point(size = 1) +
  
  geom_point(stroke = 1, size = 0.5, aes(group = seq_along(t)), show.legend = FALSE) +
  geom_segment(data = dat_labels, xend = 11, aes(yend = fn), linetype = 2) +
  geom_label(data = dat_labels, x = 12, aes(label = paste("Difference:", round(label, 3))), size = 3, show.legend = FALSE) +
  theme_fivethirtyeight() +
  theme(legend.position = "top") +
  labs(title = "Theoretical vs Simulation-based Estimates",
       subtitle = "Over a pre-defined range of values",
       color = "")
  #geom_brace(aes(c(-2, -0.8), c(0.319909, 0.5719746)), color = "green" , rotate = 270, inherit.data=F)

gg

anim <- gg + 
  transition_reveal(t) 

animate(anim, renderer = gifski_renderer(), nframes = 200, height = 900, width = 1250, res = 120, end_pause = 20)

anim_save("curve.gif", animation = last_animation())
