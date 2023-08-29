## run apsa_outputs.qmd before running this script 
library(tidyverse)
library(broom)
# score by platform 
ggplot(regression, aes(platform, mist_score, fill = platform))+
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(values = c("#4267B2", "#833AB4","#FF5700", "#ff0050","#1DA1F2"), guide = "none")+
  labs(
    y = "MIST Score",
  )+
  theme_minimal()+
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold")
  )


ggplot(regression, aes(platform, mist_score, fill = platform))+
  ggdist::stat_halfeye(adjust = .5, width = .3, .width = c(0.5, 1)) + 
  ggdist::stat_dots(side = "left", dotsize = .5, justification = 1.05, binwidth = .1)+
  theme_minimal()

ggplot(regression, aes(mist_score, fill = platform))+
  geom_bar()+
  scale_fill_manual(values = c("#4267B2", "#833AB4","#FF5700", "#ff0050","#1DA1F2"), guide = "none")+
  coord_flip()+
  facet_grid(~platform)+
  theme_minimal()

regression %>% 
  select(platform, mist_score) %>% 
  group_by(platform, mist_score) %>% 
  summarize(n = n()) %>% 
  group_by(platform) %>% 
  mutate(n_platform = sum(n),
         mist_pct = n/n_platform*100) %>% 
  ggplot(aes(mist_score, mist_pct, fill = platform))+
  geom_col()+
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  scale_fill_manual(values = c("#4267B2", "#833AB4","#FF5700", "#ff0050","#1DA1F2"), guide = "none")+
  facet_grid(~platform)+
  labs(
    x = "MIST Score",
    y = "Percent of Platform Users"
  )+
  theme_minimal()+
  theme(
    strip.text.x.top = element_text(size = 10, face = "bold")
  )

## model plots 
reg_plots = tidy(fb_reg) %>% 
  bind_rows(tidy(ig_reg), tidy(re_reg), tidy(tt_reg), tidy(tw_reg)) %>% 
  filter(term %in% c("facebook", "instagram", "reddit", "tiktok", "twitter")) %>% 
  mutate(platform = case_when(
    term == "facebook" ~ "Facebook",
    term == "instagram" ~ "Instagram",
    term == "reddit" ~ "Reddit",
    term == "tiktok" ~ "TikTok",
    term == "twitter" ~ "Twitter",
    TRUE ~ NA
  ),
  upper95 = estimate + 1.96 * std.error,
  lower95 = estimate - 1.96 * std.error,
  upper90 = estimate + 1.64 * std.error,
  lower90 = estimate - 1.64 * std.error) %>% 
  select(platform, estimate, upper95, lower95, upper90, lower90)

ggplot(reg_plots, aes(platform, estimate, color = platform))+
  geom_point(size = 4)+
  geom_linerange(aes(ymin = lower90, ymax = upper90), size = 2)+
  geom_errorbar(aes(ymin = lower95, ymax = upper95), width = 0.25)+
  scale_color_manual(values = c("#4267B2", "#833AB4","#FF5700", "#ff0050","#1DA1F2"), guide = "none")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(face = "bold", size = 10)
  )+
  labs(
    x = NULL,
    y = "OLS Estimate",
    caption = "Confidence Intervals: 90% bold, 95% full error bar"
  )
  
            