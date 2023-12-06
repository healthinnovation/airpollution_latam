#2. SII analysis ----------

edson <- function(x, pais = "peru"){
  s1 <- summary(x)
  df_coef <- as.data.frame(s1$coefficients)
  rank <- df_coef[["Estimate"]][5]
  confid <- tidy(x, conf.int = T)
  conf.low <- confid[["conf.low"]][5]
  conf.high <- confid[["conf.high"]][5]
  table_model <- data.frame(rank = rank,
                            conf.low = conf.low,
                            conf.high = conf.high,
                            country = pais)
  return(table_model)
}

##2.1 argentina ----

df_argentina <- df_argentina %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_argentina_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                              data = df_argentina)

rank_lm_sii_argentina_Water <- edson(sii_argentina_Water, "argentina")

#Sewage school

sii_argentina_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                          data = df_argentina)

rank_lm_sii_argentina_Sewage <- edson(sii_argentina_Sewage, "argentina")

#Unemployment school

sii_argentina_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                          data = df_argentina)

rank_lm_sii_argentina_Unemployment <- edson(sii_argentina_Unemployment, "argentina")

#Labor_force school

sii_argentina_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                          data = df_argentina)

rank_lm_sii_argentina_Labor_force <- edson(sii_argentina_Labor_force, "argentina")

##2.2 brasil ----

df_brasil <- df_brasil %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_brasil_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                          data = df_brasil)

rank_lm_sii_brasil_Water <- edson(sii_brasil_Water, "brasil")

#Sewage school

sii_brasil_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                           data = df_brasil)

rank_lm_sii_brasil_Sewage <- edson(sii_brasil_Sewage, "brasil")

#Unemployment school

sii_brasil_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                                 data = df_brasil)

rank_lm_sii_brasil_Unemployment <- edson(sii_brasil_Unemployment, "brasil")

#Labor_force school

sii_brasil_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                                data = df_brasil)

rank_lm_sii_brasil_Labor_force <- edson(sii_brasil_Labor_force, "brasil")

##2.3 chile ----

df_chile <- df_chile %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_chile_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                       data = df_chile)

rank_lm_sii_chile_Water <- edson(sii_chile_Water, "chile")

#Unemployment school

sii_chile_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                              data = df_chile)

rank_lm_sii_chile_Unemployment <- edson(sii_chile_Unemployment, "chile")

#Labor_force school

sii_chile_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                             data = df_chile)

rank_lm_sii_chile_Labor_force <- edson(sii_chile_Labor_force, "chile")

##2.4 colombia ----

df_colombia <- df_colombia %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_colombia_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                      data = df_colombia)

rank_lm_sii_colombia_Water <- edson(sii_colombia_Water, "colombia")

#Sewage school

sii_colombia_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                       data = df_colombia)

rank_lm_sii_colombia_Sewage <- edson(sii_colombia_Sewage, "colombia")

#Unemployment school

sii_colombia_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                             data = df_colombia)

rank_lm_sii_colombia_Unemployment <- edson(sii_colombia_Unemployment, "colombia")

#Labor_force school

sii_colombia_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                            data = df_colombia)

rank_lm_sii_colombia_Labor_force <- edson(sii_colombia_Labor_force, "colombia")

##2.5 costarica ----

df_costarica <- df_costarica %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_costarica_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                         data = df_costarica)

rank_lm_sii_costarica_Water <- edson(sii_costarica_Water, "costarica")

#Sewage school

sii_costarica_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                          data = df_costarica)

rank_lm_sii_costarica_Sewage <- edson(sii_costarica_Sewage, "costarica")

#Unemployment school

sii_costarica_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                                data = df_costarica)

rank_lm_sii_costarica_Unemployment <- edson(sii_costarica_Unemployment, "costarica")

#Labor_force school

sii_costarica_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                               data = df_costarica)

rank_lm_sii_costarica_Labor_force <- edson(sii_costarica_Labor_force, "costarica")

##2.6 guatemala ----

df_guatemala <- df_guatemala %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_guatemala_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                          data = df_guatemala)

rank_lm_sii_guatemala_Water <- edson(sii_guatemala_Water, "guatemala")

#Sewage school

sii_guatemala_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                           data = df_guatemala)

rank_lm_sii_guatemala_Sewage <- edson(sii_guatemala_Sewage, "guatemala")

#Unemployment school

sii_guatemala_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                                 data = df_guatemala)

rank_lm_sii_guatemala_Unemployment <- edson(sii_guatemala_Unemployment, "guatemala")

#Labor_force school

sii_guatemala_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                                data = df_guatemala)

rank_lm_sii_guatemala_Labor_force <- edson(sii_guatemala_Labor_force, "guatemala")

##2.7 mexico ----

df_mexico <- df_mexico %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Sewage school

sii_mexico_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                           data = df_mexico)

rank_lm_sii_mexico_Sewage <- edson(sii_mexico_Sewage, "mexico")

#Unemployment school

sii_mexico_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                                 data = df_mexico)

rank_lm_sii_mexico_Unemployment <- edson(sii_mexico_Unemployment, "mexico")

#Labor_force school

sii_mexico_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                                data = df_mexico)

rank_lm_sii_mexico_Labor_force <- edson(sii_mexico_Labor_force, "mexico")

##2.8 panama ----

df_panama <- df_panama %>%
  mutate(Water_quantile = ntile(Water, 5)) %>% 
  mutate(Sewage_quantile = ntile(Sewage, 5)) %>% 
  mutate(Unemployment_quantile = ntile(Unemployment, 5)) %>% 
  mutate(Labor_force_quantile = ntile(Labor_force, 5))

#Water school

sii_panama_Water <- lm(PM2.5~factor(Water_quantile), weights = Population,
                          data = df_panama)

rank_lm_sii_panama_Water <- edson(sii_panama_Water, "panama")

#Sewage school

sii_panama_Sewage <- lm(PM2.5~factor(Sewage_quantile), weights = Population,
                           data = df_panama)

rank_lm_sii_panama_Sewage <- edson(sii_panama_Sewage, "panama")

#Unemployment school

sii_panama_Unemployment <- lm(PM2.5~factor(Unemployment_quantile), weights = Population,
                                 data = df_panama)

rank_lm_sii_panama_Unemployment <- edson(sii_panama_Unemployment, "panama")

#Labor_force school

sii_panama_Labor_force <- lm(PM2.5~factor(Labor_force_quantile), weights = Population,
                                data = df_panama)

rank_lm_sii_panama_Labor_force <- edson(sii_panama_Labor_force, "panama")

#3. SII Graphics----------------------------------

## Forest plot GG -------------------
rank_lm_sii_Water <- rbind(rank_lm_sii_argentina_Water,
                               rank_lm_sii_brasil_Water,
                               rank_lm_sii_chile_Water,
                               rank_lm_sii_colombia_Water,
                               rank_lm_sii_costarica_Water,
                               rank_lm_sii_guatemala_Water,
                               rank_lm_sii_panama_Water) %>% 
  mutate(exposure = "Water access %")

rank_lm_sii_Sewage <- rbind(rank_lm_sii_argentina_Sewage,
                             rank_lm_sii_brasil_Sewage,
                             rank_lm_sii_colombia_Sewage,
                             rank_lm_sii_costarica_Sewage,
                             rank_lm_sii_guatemala_Sewage,
                             rank_lm_sii_mexico_Sewage,
                             rank_lm_sii_panama_Sewage) %>% 
  mutate(exposure = "Sewage access %")

rank_lm_sii_Unemployment <- rbind(rank_lm_sii_argentina_Unemployment,
                          rank_lm_sii_brasil_Unemployment,
                          rank_lm_sii_chile_Unemployment,
                          rank_lm_sii_colombia_Unemployment,
                          rank_lm_sii_costarica_Unemployment,
                          rank_lm_sii_guatemala_Unemployment,
                          rank_lm_sii_panama_Unemployment) %>% 
  mutate(exposure = "Unemployment %")

rank_lm_sii_Labor_force <- rbind(rank_lm_sii_argentina_Labor_force,
                                rank_lm_sii_brasil_Labor_force,
                                rank_lm_sii_chile_Labor_force,
                                rank_lm_sii_colombia_Labor_force,
                                rank_lm_sii_costarica_Labor_force,
                                rank_lm_sii_guatemala_Labor_force,
                                rank_lm_sii_panama_Labor_force) %>% 
  mutate(exposure = "Population able to work")

rank_lm_sii <- rbind(rank_lm_sii_Water,
                     rank_lm_sii_Sewage,
                     rank_lm_sii_Unemployment,
                     rank_lm_sii_Labor_force) %>% 
  mutate(exposure_r = factor(exposure,levels = c(
    "Water access %",
    "Sewage access %",
    "Unemployment %",
    "Population able to work"
  )) ) %>% 
  mutate(country_r = fct_rev(country))


ggplot(rank_lm_sii, aes(x=rank, y=country_r, color=country_r)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (population weighted) of SES by country", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure_r, ncol=2)

ggsave("rank_lm_sii_otherSES.png",
       last_plot(),
       dpi = 300,
       bg = "white",
       width = 9,
       height = 6)
##
## MAPS =======================================
# 3. Bogota attending -----------------------------------------------

data1 <- bi_class(
  bogota,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSWATNETL3)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = bogota,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data1,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end1 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.2, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 4. Buenos aires attending -----------------------------------------------

data2 <- bi_class(
  buenosaires,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data2 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = buenosaires,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data2,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end2 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.13, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 5. Guatemala attending -----------------------------------------------

data3 <- bi_class(
  guatemala,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data3 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = guatemala,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data3,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end3 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.2, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.22, -0.05, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 6. Panama attending -----------------------------------------------

data4 <- bi_class(
  panama,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data4 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = panama,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data4,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end4 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.25, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 7. San jose attending -----------------------------------------------

data5 <- bi_class(
  sanjose,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data5 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = sanjose,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data5,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end5 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 8. Santiago attending -----------------------------------------------

data6 <- bi_class(
  santiago,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data6 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = santiago,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data6,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end6 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.015, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 9. Sao paolo attending -----------------------------------------------

data7 <- bi_class(
  saopaolo,
  x = CNSWATNETL3,
  y = PM2.5,
  style = "quantile",
  dim = 3
)

newdata <- data7 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = saopaolo,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data7,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end7 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

# 10. Mexico attending -----------------------------------------------

data8 <- bi_class(
  mexico,
  x = CNSWATNETL3,
  y = APSPM25MEAN2010L3,
  style = "quantile",
  dim = 3
)

newdata <- data8 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat1,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat1,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of census tracks",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school →",
    x = 0.34,
    y = 0.06,
    size = 7
  ) + 
  draw_plot_label(
    label = "PM2.5 →",
    x = 0.90,
    y = -0.1,
    angle = 90,
    size = 7
  )

barras

# Biscale map  ---------------------------------------------------------
##> Customization of color palette
paleta <- bi_pal2(
  pal = "DkViolet",
  dim = 3
) + 
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank()
  ) +
  labs(
    x = "% of attending school → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = mexico,
    lwd = 0.5,
    fill = "#d9d9d9",
    show.legend = FALSE,
    color = "white"
  ) +
  geom_sf(
    data = data8,
    lwd = 0.0,
    aes(fill = bi_class),
    show.legend = FALSE
  ) +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  bi_scale_fill2(pal = "DkViolet", dim = 3)

end8 <- ggdraw() +
  # Map - - - - - - - - - - - - - - - - - - - -
  draw_plot(
    basemap + theme(
      legend.justification = "top",
      plot.background = element_blank()
    ), 0.17, 0, 1, 1,scale = 1.0
  ) +
  # bar plot - - - - - - - - - - - - - - - - - - 
  draw_plot(
    barras + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    -0.23, -0.09, 0.9, 0.60,
    scale = 0.4
  ) +
  theme_bw()

end9 <- ggdraw() +
  # legend - - - - - - - - - - - - - - - - - - - 
  draw_plot(
    paleta + theme(
      legend.justification = "bottom",
      plot.background = element_blank()
    ),
    scale = 1
  ) +
  theme_bw()

gg1 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end8, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", "H. Mexico city (Mexico", ""),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_attending.png",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_attending.pdf",
  plot = gg1,
  width = 20.5,
  height = 16,
  bg = "white"  
)
