# Prelims:
# sample data and analysis
library(KMsurv)
library(survival)
library(survminer)

# data wrangling
library(tidyr)
library(dplyr)

# animation, plotting
library(ggplot2)
library(gganimate)
library(ggtext)
library(hrbrthemes)

# Analysis:
# sample data from KMsurv
data(bmt)

# Kaplan Meier
fit <- survfit(Surv(t1, d1) ~ group, data = bmt)

# use ggsurvplot() and return underlying data for wrangling + plotting
# set break.x.by to 1 so we get underlying data at each unit time point
ggsurv <- ggsurvplot(fit, 
                     data = bmt, 
                     risk.table = TRUE, 
                     cumevents = TRUE,
                     break.x.by = 1)

# Data Wrangling:
# get survival data
surv_data <- ggsurv$plot$data %>%
  select(strata, time, surv)

# get contextual event data
event_data <- ggsurv$cumevents$data %>%
  select(strata, time, cum_n_risk = n.risk, cum_n_event = cum.n.event, censor = n.censor)

# merge survival + event data
# fill in intermediate survival estimates
step1 <- event_data %>%
  left_join(surv_data) %>%
  fill(surv) 

# specify at which times you want the animation to "pause" - https://stackoverflow.com/questions/47681853/pause-between-gganimate-loops
# I specify I want the animation to pause at time = 1000, and 2640, corresponding to the first and final dates of analysis
# I want a (briefer) pause at the median survival times (based on the final analysis) for each group: survminer::surv_median(fit)
# Lastly, I specify the text for the label to be displayed at time = 1000 and 2640
step2 <- step1 %>%
  mutate(show_time = case_when(time %in% c(1000,2640) ~ 900,
                               time %in% c(466,2204,265) ~ 100,
                               TRUE ~ 1)) %>%
  uncount(show_time) %>%
  group_by(strata) %>%
  mutate(reveal_time = row_number()) %>%
  ungroup() %>%
  mutate(refline_time = case_when(
    time == 1000 ~ 1000,
    time == 2640 ~ 2640
  )) %>%
  mutate(refline_name = case_when(
    time == 1000 ~ "**Analysis 1**: The first interim analysis was conducted on January 1, 2017. No serious adverse events were observed. It was determined the analysis would continue. <br><br>The **log-rank p-value** was 0.00432.",
    time == 2640 ~ "**Analysis 2**: The final analysis was conducted on June 29, 2021. Both Treatment A and B Groups showed a survival benefit relative to the Control Group. <br><br>The **log-rank p-value** was 0.004"    
  ))

# here i specify the times at which the median estimates should occurr
# unlike the labels at time = 1000, and 2640, they are retained for the entire animation once they initially appear
step3 <- step2 %>%
  mutate(median = case_when(strata == 'group=1' & time >= 466 ~ 466,
                            strata == 'group=2' & time >= 2204 ~ 2204,
                            strata == 'group=3' & time >= 265 ~ 265))


# Plotting:
# use step3 data to begin constructing the Kaplan-Meier curve
# i've broken things into 'chunks' so it's easier to look at things

# setup plotting space
km <- ggplot(data = step3, aes(x = time, y = surv, group = strata, color = strata, shape = as.factor(censor))) +
  scale_color_manual(labels = c('Treatment A', 'Treatment B', 'Control'), values = c("#94167F","#E93479","#F9AC53")) +
  scale_y_continuous(breaks=seq(0, 1, 0.1),    limits=c(0, 1)) +
  scale_x_continuous(breaks=seq(0, 2750, 250), limits=c(0, 3200)) + 
  labs(x="Time (days)", y="Survival Probability\n", title="Kaplan Meier Plot", subtitle="An animated take on survival analysis", caption="@Mattkumar_", color = "") +
  theme_modern_rc() +
  theme(legend.position = "bottom", legend.text=element_text(size=12))

# add in KM curves, points
# here I use two geom_point calls: one for the "leading" point in the animation, the other for the "censored" points
km1 <- km + 
  geom_step(size = 1.25, alpha = 0.7, show.legend=FALSE) +
  geom_point(shape = 21, aes(color = strata), fill = 'white', stroke = 2, size  = 3) +
  geom_point(stroke = 1.15, size = 2, aes(group = seq_along(time)), show.legend=FALSE) +
  scale_shape_manual("", values=c(32,3))

# add in point tracking pieces - the boxes to the right of the graph that tracks with contextual information
km2 <- km1 +
  geom_segment(xend = 3000, aes(yend = surv), linetype = 2, show.legend = FALSE) +
  geom_label(x = 3100, aes(label = paste("Cumulative Events:",cum_n_event, "\nNumber At Risk:", cum_n_risk)), size = 3, show.legend = FALSE) 

# add in the analysis messages and median survival estimates
km3 <- km2 +
  
  # analysis messages @ time = 1000, 2640 (line 64)
  geom_vline(aes(xintercept = refline_time),  linetype = "solid" , color = "#00b8ff", size = 0.8, show.legend=FALSE) +
  geom_textbox(aes(x = refline_time, y = 1, label = refline_name), size = 2.5, vjust = 1, color = "#00b8ff", orientation = "upright", fill = "cornsilk") +
  
  # median estimates 
  geom_segment(aes(x = 0, y = 0.5, xend = median, yend = 0.5, color = strata), linetype = "dotted", size = 0.75, show.legend=FALSE,) +
  geom_segment(aes(x = median, xend = median, y = 0, yend=0.5, color = strata), linetype = "dotted", size = 0.75, show.legend=FALSE,) +
  geom_text(show.legend = FALSE, aes(color = strata, x = median, y=0, label = paste('Median OS:', median)), angle = 90, size = 3, vjust = -1, hjust = -0.025, fontface = "bold")


# Animate:
temp <- km3 + transition_reveal(reveal_time)  
animate(temp, renderer = gifski_renderer(), nframes = 220, height = 950, width = 1250, res = 120, end_pause = 10)

# Save:
anim_save("km.gif", animation = last_animation())
