library(tidyverse)

sii_argentina_itaqg_university_block <- buenosaires %>% #SII at census block level
  
  st_drop_geometry() %>% 
  
  drop_na(university_quantile) %>% 
  
  group_by(university_quantile) %>% 
  
  summarize(pop = n(), 
            prop_p = pop/nrow(buenosaires),
            pm2.5_mean = mean(APSPM25MEAN2010L3)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

lm_sii_argentina_itaqg_university_block <- lm(pm2.5_mean~rank,
                                              data = sii_argentina_itaqg_university_block)

summary(lm_sii_argentina_itaqg_university_block)

m1 <- lm(APSPM25MEAN2010L3~factor(university_quantile), weights = CNSPOPL3,
   data = buenosaires) #usar para plots estimate factor 5 y conf int

m2 <- buenosaires %>% 
  mutate(new_un = CNSMINUN_L3/100) %>% 
  lm(APSPM25MEAN2010L3~new_un, weights = CNSPOPL3, data=.)
tidy(m1, conf.int = T)
tidy(m2, conf.int = T)
tidy(lm_sii_argentina_itaqg_university_block, conf.int = T)

buenosaires %>% 
  st_drop_geometry() %>% 
  group_by(university_quantile) %>% 
  summarise(m=mean(APSPM25MEAN2010L3))

summary(buenosaires$CNSMINUN_L3)

buenosaires %>% 
  mutate(new_un = CNSMINUN_L3/100) %>%
  ggplot(aes(x=new_un, y=APSPM25MEAN2010L3)) +
  geom_point() +
  geom_smooth()

buenosaires %>% 
  st_drop_geometry() %>% 
  group_by(university_quantile) %>% 
  summarise(mean_un = mean(CNSMINUN_L3))
