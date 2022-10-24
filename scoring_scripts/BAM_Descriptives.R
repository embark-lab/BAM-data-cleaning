
library(knitr)


BAM_redcap_long %>% 
  group_by(group, timepoint) %>% 
  summarize (epsi_neg = round(mean(espi_neg_obse_25, na.rm = TRUE),2)) %>% 
  pivot_wider(names_from = timepoint, values_from = epsi_neg) %>% 
  kable()

