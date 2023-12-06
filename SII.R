library(biscale)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(sf)
library(ggpubr)
library(table1)
library(ggstatsplot)
library(innovar)
library(ggmosaic)
library(readr)
library(readxl)
library(broom)
source("utils2.R")

#1. Import data -----------

sn_argentina <- read.csv("Data2/ses/AR_L2_5_ANALYTIC_05312023.csv")
sn_brasil <- read.csv("Data2/ses/BR_L2_5_ANALYTIC_05312023.csv")
sn_chile <- read.csv("Data2/ses/CL_L3_ANALYTIC_05312023.csv")
sn_colombia <- read.csv("Data2/ses/CO_L3_ANALYTIC_05312023.csv")
sn_costarica <- read.csv("Data2/ses/CR_L2_5_ANALYTIC_05312023.csv")
sn_guatemala <- read.csv("Data2/ses/GT_L3_ANALYTIC_RES_05312023.csv")
sn_mexico <- read.csv("Data2/ses/MX_L3_ANALYTIC_05312023.csv")
sn_panama <- read.csv("Data2/ses/PA_L3_ANALYTIC_05312023.csv")
sn_peru <- read.csv("Data2/ses/PE_L3_ANALYTIC_05312023.csv")

bogota <- st_read("Data2/geom/colombia.gpkg") %>% 
  select("SALID3", 
         "CNSPOP05",
         "Shape_Area",
         "geom")
buenosaires <- st_read("Data2/geom/argentina.gpkg") %>% 
  select("SALID2_5", 
         "CNSPOP10",
         "Shape_Area",
         "geom")
guatemala <- st_read("Data2/geom/guatemala.gpkg") %>% 
  select("SALID3", 
         "CNSPOP02",
         "Shape_Area",
         "geom")
mexico <- st_read("Data2/geom/mexico.gpkg") %>% 
  select("salid3", 
         "CNSPOP10",
         "Shape_Area",
         "geom")
panama <- st_read("Data2/geom/panama.gpkg") %>% 
  select("salid3", 
         "CNSPOP10",
         "Shape_Area",
         "geom")
sanjose <- st_read("Data2/geom/costarica.gpkg") %>% 
  select("SALID2_5", 
         "CNSPOP11",
         "Shape_Area",
         "geom")
santiago <- st_read("Data2/geom/chile.gpkg") %>% 
  select("salid3", 
         "CNSPOP17",
         "Shape_Area",
         "geom")
saopaolo <- st_read("Data2/geom/brasil.gpkg") %>% 
  select("SALID2_5", 
         "CNSPOP10",
         "Shape_Area",
         "geom")
lima <- st_read("Data2/geom/peru.gpkg") %>% 
  select("SALID3", 
         "CNSPOP17",
         "Shape_Area",
         "geom")

geom_argentina <- merge(buenosaires, sn_argentina,
                   by.x="SALID2_5", by.y="SALID2_5", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_brasil <- merge(saopaolo, sn_brasil,
                by.x="SALID2_5", by.y="SALID2_5", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_chile <- merge(santiago, sn_chile,
               by.x="salid3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_colombia <- merge(bogota, sn_colombia,
                by.x="SALID3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )


geom_costarica <- merge(sanjose, sn_costarica,
                   by.x="SALID2_5", by.y="SALID2_5", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_guatemala <- merge(guatemala, sn_guatemala,
                   by.x="SALID3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_mexico <- merge(mexico, sn_mexico,
                by.x="salid3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_panama <- merge(panama, sn_panama,
                by.x="salid3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

geom_peru <- merge(lima, sn_peru,
                by.x="SALID3", by.y="SALID3", all.x=T) %>% 
  mutate(
    PM25 = (APSPM25MEAN2001L3  +
              APSPM25MEAN2002L3  +
              APSPM25MEAN2003L3  +
              APSPM25MEAN2004L3  +
              APSPM25MEAN2005L3  +
              APSPM25MEAN2006L3  +
              APSPM25MEAN2007L3  +
              APSPM25MEAN2008L3  +
              APSPM25MEAN2009L3  +
              APSPM25MEAN2010L3  +
              APSPM25MEAN2011L3  +
              APSPM25MEAN2012L3  +
              APSPM25MEAN2013L3  +
              APSPM25MEAN2014L3  +
              APSPM25MEAN2015L3  +
              APSPM25MEAN2016L3  +
              APSPM25MEAN2017L3  +
              APSPM25MEAN2018L3  +
              APSPM25MEAN2019L3  +
              APSPM25MEAN2020L3)/20
  )

ng_argentina <- geom_argentina %>% 
  st_drop_geometry()

ng_brasil <- geom_brasil %>% 
  st_drop_geometry()

ng_chile <- geom_chile %>% 
  st_drop_geometry()

ng_colombia <- geom_colombia %>% 
  st_drop_geometry()

ng_costarica <- geom_costarica %>% 
  st_drop_geometry()

ng_guatemala <- geom_guatemala %>% 
  st_drop_geometry()

ng_mexico <- geom_mexico %>% 
  st_drop_geometry()

ng_panama <- geom_panama %>% 
  st_drop_geometry()

ng_peru <- geom_peru %>% 
  st_drop_geometry()

#2. SII analysis ----------

edson <- function(x, pais = "peru"){
  s1 <- summary(x)
  df_coef <- as.data.frame(s1$coefficients)
  rank <- df_coef[["Estimate"]][2]
  confid <- tidy(x, conf.int = T, conf.level = 0.95)
  conf.low <- confid[["conf.low"]][2]
  conf.high <- confid[["conf.high"]][2]
  table_model <- data.frame(rank = rank,
                            conf.low = conf.low,
                            conf.high = conf.high,
                            country = pais)
  return(table_model)
}

##2.1 argentina ----
 ng_argentina <-  ng_argentina %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 1] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 2] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 3] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 4] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 5] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 6] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 7] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 8] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 9] <- 0
 ng_argentina$PM2.5_catq[ ng_argentina$PM2.5_catq == 10] <- 1

argentina_total <- sum(ng_argentina$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_argentina <- ng_argentina %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_attending <- lm(tasa_cases~rank, weights = pop,
         data = sii_argentina)

rank_lm_sii_argentina_attending <- edson(sii_argentina_attending, "argentina")

#primary school

sii_argentina <- ng_argentina %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_primary <- lm(tasa_cases~rank, weights = pop,
                              data = sii_argentina)

rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "argentina")

##2.2 brasil ----
ng_brasil <-  ng_brasil %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 1] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 2] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 3] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 4] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 5] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 6] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 7] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 8] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 9] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 10] <- 1

brasil_total <- sum(ng_brasil$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_brasil <- ng_brasil %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_brasil)

rank_lm_sii_brasil_attending <- edson(sii_brasil_attending, "brasil")

#primary school

sii_brasil <- ng_brasil %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_brasil)

rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "brasil")

##2.3 chile ----
ng_chile <-  ng_chile %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 1] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 2] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 3] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 4] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 5] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 6] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 7] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 8] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 9] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 10] <- 1

chile_total <- sum(ng_chile$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_chile <- ng_chile %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_chile)

rank_lm_sii_chile_attending <- edson(sii_chile_attending, "chile")

#primary school

sii_chile <- ng_chile %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_chile)

rank_lm_sii_chile_primary <- edson(sii_chile_primary, "chile")

##2.4 colombia ----
ng_colombia <-  ng_colombia %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 1] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 2] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 3] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 4] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 5] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 6] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 7] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 8] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 9] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 10] <- 1

colombia_total <- sum(ng_colombia$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_colombia <- ng_colombia %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_colombia)

rank_lm_sii_colombia_attending <- edson(sii_colombia_attending, "colombia")

#primary school

sii_colombia <- ng_colombia %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_colombia)

rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "colombia")

##2.5 costarica ----
ng_costarica <-  ng_costarica %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 1] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 2] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 3] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 4] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 5] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 6] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 7] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 8] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 9] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 10] <- 1

costarica_total <- sum(ng_costarica$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_costarica <- ng_costarica %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_costarica)

rank_lm_sii_costarica_attending <- edson(sii_costarica_attending, "costarica")

#primary school

sii_costarica <- ng_costarica %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_costarica)

rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "costarica")

##2.6 guatemala ----
ng_guatemala <-  ng_guatemala %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 1] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 2] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 3] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 4] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 5] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 6] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 7] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 8] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 9] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 10] <- 1

guatemala_total <- sum(ng_guatemala$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_guatemala <- ng_guatemala %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_guatemala)

rank_lm_sii_guatemala_attending <- edson(sii_guatemala_attending, "guatemala")

#primary school

sii_guatemala <- ng_guatemala %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_guatemala)

rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "guatemala")

##2.7 mexico ----
ng_mexico <-  ng_mexico %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 1] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 2] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 3] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 4] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 5] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 6] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 7] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 8] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 9] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 10] <- 1

mexico_total <- sum(ng_mexico$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_mexico <- ng_mexico %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_mexico)

rank_lm_sii_mexico_attending <- edson(sii_mexico_attending, "mexico")

#primary school

sii_mexico <- ng_mexico %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_mexico)

rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "mexico")

##2.8 panama ----
ng_panama <-  ng_panama %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 1] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 2] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 3] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 4] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 5] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 6] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 7] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 8] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 9] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 10] <- 1

panama_total <- sum(ng_panama$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_panama <- ng_panama %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_panama)

rank_lm_sii_panama_attending <- edson(sii_panama_attending, "panama")

#primary school

sii_panama <- ng_panama %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_panama)

rank_lm_sii_panama_primary <- edson(sii_panama_primary, "panama")

##2.9 peru ----
ng_peru <-  ng_peru %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 1] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 2] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 3] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 4] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 5] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 6] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 7] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 8] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 9] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 10] <- 1

peru_total <- sum(ng_peru$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_peru <- ng_peru %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_attending <- lm(tasa_cases~rank, weights = pop,
                           data = sii_peru)

rank_lm_sii_peru_attending <- edson(sii_peru_attending, "peru")

#primary school

sii_peru <- ng_peru %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_primary <- lm(tasa_cases~rank, weights = pop,
                         data = sii_peru)

rank_lm_sii_peru_primary <- edson(sii_peru_primary, "peru")

#3. SII Graphics----------------------------------

## Forest plot GG -------------------
rank_lm_sii_attending <- rbind(rank_lm_sii_all_countries_attending,
                               rank_lm_sii_argentina_attending,
                                               rank_lm_sii_brasil_attending,
                                               rank_lm_sii_chile_attending,
                                               rank_lm_sii_colombia_attending,
                                               rank_lm_sii_costarica_attending,
                                               rank_lm_sii_guatemala_attending,
                                               rank_lm_sii_mexico_attending,
                                               rank_lm_sii_panama_attending,
                                              rank_lm_sii_peru_attending) %>% 
  mutate(exposure = "School attendance (any school, 15 to 17 yo)")

rank_lm_sii_primary <- rbind(rank_lm_sii_all_countries_primary,
                             rank_lm_sii_argentina_primary,
                               rank_lm_sii_brasil_primary,
                               rank_lm_sii_chile_primary,
                               rank_lm_sii_colombia_primary,
                               rank_lm_sii_costarica_primary,
                               rank_lm_sii_guatemala_primary,
                               rank_lm_sii_mexico_primary,
                               rank_lm_sii_panama_primary,
                             rank_lm_sii_peru_primary) %>% 
  mutate(exposure = "Primary school attainment (>25 years old)")

rank_lm_sii <- rbind(rank_lm_sii_attending,
                     rank_lm_sii_primary) %>% 
  mutate(exposure_r = factor(exposure,levels = c(
    "School attendance (any school, 15 to 17 yo)",
    "Primary school attainment (>25 years old)")) ) %>% 
  mutate(country_r = fct_rev(country))

ggplot(rank_lm_sii, aes(x=rank, y=country_r, color=country_r)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (population weighted) of education by country", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure_r, ncol=2)

ggsave("rank_lm_sii_deciles_9c.png",
       last_plot(),
       dpi = 300,
       bg = "white",
       width = 9,
       height = 5)

ggsave("rank_lm_sii_deciles_9c.pdf",
       last_plot(),
       bg = "white",
       width = 9,
       height = 5)

#4. SII numerico ------
##4.1 argentina ----
ng_argentina <-  ng_argentina %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

argentina_total <- sum(ng_argentina$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_argentina <- ng_argentina %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_attending <- lm(out_cases~rank, weights = pop,
                              data = sii_argentina)

rank_lm_sii_argentina_attending <- edson(sii_argentina_attending, "argentina")

#primary school

sii_argentina <- ng_argentina %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_primary <- lm(out_cases~rank, weights = pop,
                            data = sii_argentina)

rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "argentina")

##4.2 brasil ----
ng_brasil <-  ng_brasil %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 1] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 2] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 3] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 4] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 5] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 6] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 7] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 8] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 9] <- 0
ng_brasil$PM2.5_catq[ ng_brasil$PM2.5_catq == 10] <- 1

brasil_total <- sum(ng_brasil$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_brasil <- ng_brasil %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_attending <- lm(out_cases~rank, weights = pop,
                           data = sii_brasil)

rank_lm_sii_brasil_attending <- edson(sii_brasil_attending, "brasil")

#primary school

sii_brasil <- ng_brasil %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_primary <- lm(out_cases~rank, weights = pop,
                         data = sii_brasil)

rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "brasil")

##4.3 chile ----
ng_chile <-  ng_chile %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 1] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 2] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 3] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 4] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 5] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 6] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 7] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 8] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 9] <- 0
ng_chile$PM2.5_catq[ ng_chile$PM2.5_catq == 10] <- 1

chile_total <- sum(ng_chile$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_chile <- ng_chile %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_attending <- lm(out_cases~rank, weights = pop,
                          data = sii_chile)

rank_lm_sii_chile_attending <- edson(sii_chile_attending, "chile")

#primary school

sii_chile <- ng_chile %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_primary <- lm(out_cases~rank, weights = pop,
                        data = sii_chile)

rank_lm_sii_chile_primary <- edson(sii_chile_primary, "chile")

##4.4 colombia ----
ng_colombia <-  ng_colombia %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 1] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 2] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 3] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 4] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 5] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 6] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 7] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 8] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 9] <- 0
ng_colombia$PM2.5_catq[ ng_colombia$PM2.5_catq == 10] <- 1

colombia_total <- sum(ng_colombia$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_colombia <- ng_colombia %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_attending <- lm(out_cases~rank, weights = pop,
                             data = sii_colombia)

rank_lm_sii_colombia_attending <- edson(sii_colombia_attending, "colombia")

#primary school

sii_colombia <- ng_colombia %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_primary <- lm(out_cases~rank, weights = pop,
                           data = sii_colombia)

rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "colombia")

##4.5 costarica ----
ng_costarica <-  ng_costarica %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 1] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 2] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 3] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 4] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 5] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 6] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 7] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 8] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 9] <- 0
ng_costarica$PM2.5_catq[ ng_costarica$PM2.5_catq == 10] <- 1

costarica_total <- sum(ng_costarica$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_costarica <- ng_costarica %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_attending <- lm(out_cases~rank, weights = pop,
                              data = sii_costarica)

rank_lm_sii_costarica_attending <- edson(sii_costarica_attending, "costarica")

#primary school

sii_costarica <- ng_costarica %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_primary <- lm(out_cases~rank, weights = pop,
                            data = sii_costarica)

rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "costarica")

##4.6 guatemala ----
ng_guatemala <-  ng_guatemala %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 1] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 2] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 3] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 4] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 5] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 6] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 7] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 8] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 9] <- 0
ng_guatemala$PM2.5_catq[ ng_guatemala$PM2.5_catq == 10] <- 1

guatemala_total <- sum(ng_guatemala$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_guatemala <- ng_guatemala %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_attending <- lm(out_cases~rank, weights = pop,
                              data = sii_guatemala)

rank_lm_sii_guatemala_attending <- edson(sii_guatemala_attending, "guatemala")

#primary school

sii_guatemala <- ng_guatemala %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_primary <- lm(out_cases~rank, weights = pop,
                            data = sii_guatemala)

rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "guatemala")

##4.7 mexico ----
ng_mexico <-  ng_mexico %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 1] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 2] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 3] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 4] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 5] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 6] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 7] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 8] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 9] <- 0
ng_mexico$PM2.5_catq[ ng_mexico$PM2.5_catq == 10] <- 1

mexico_total <- sum(ng_mexico$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_mexico <- ng_mexico %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_attending <- lm(out_cases~rank, weights = pop,
                           data = sii_mexico)

rank_lm_sii_mexico_attending <- edson(sii_mexico_attending, "mexico")

#primary school

sii_mexico <- ng_mexico %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_primary <- lm(out_cases~rank, weights = pop,
                         data = sii_mexico)

rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "mexico")

##4.8 panama ----
ng_panama <-  ng_panama %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 1] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 2] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 3] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 4] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 5] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 6] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 7] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 8] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 9] <- 0
ng_panama$PM2.5_catq[ ng_panama$PM2.5_catq == 10] <- 1

panama_total <- sum(ng_panama$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_panama <- ng_panama %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_attending <- lm(out_cases~rank, weights = pop,
                           data = sii_panama)

rank_lm_sii_panama_attending <- edson(sii_panama_attending, "panama")

#primary school

sii_panama <- ng_panama %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_primary <- lm(out_cases~rank, weights = pop,
                         data = sii_panama)

rank_lm_sii_panama_primary <- edson(sii_panama_primary, "panama")

##4.9 peru ----
ng_peru <-  ng_peru %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25, 10)) 

ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 1] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 2] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 3] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 4] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 5] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 6] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 7] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 8] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 9] <- 0
ng_peru$PM2.5_catq[ ng_peru$PM2.5_catq == 10] <- 1

peru_total <- sum(ng_peru$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_peru <- ng_peru %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_attending <- lm(out_cases~rank, weights = pop,
                         data = sii_peru)

rank_lm_sii_peru_attending <- edson(sii_peru_attending, "peru")

#primary school

sii_peru <- ng_peru %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_primary <- lm(out_cases~rank, weights = pop,
                       data = sii_peru)

rank_lm_sii_peru_primary <- edson(sii_peru_primary, "peru")

#5. SII Graphics----------------------------------
## Forest plot GG -------------------
rank_lm_sii_attending <- rbind(rank_lm_sii_argentina_attending,
                               rank_lm_sii_brasil_attending,
                               rank_lm_sii_chile_attending,
                               rank_lm_sii_colombia_attending,
                               rank_lm_sii_costarica_attending,
                               rank_lm_sii_guatemala_attending,
                               rank_lm_sii_mexico_attending,
                               rank_lm_sii_panama_attending,
                               rank_lm_sii_peru_attending) %>% 
  mutate(exposure = "School attendance (any school, 15 to 17 yo)")

rank_lm_sii_primary <- rbind(rank_lm_sii_argentina_primary,
                             rank_lm_sii_brasil_primary,
                             rank_lm_sii_chile_primary,
                             rank_lm_sii_colombia_primary,
                             rank_lm_sii_costarica_primary,
                             rank_lm_sii_guatemala_primary,
                             rank_lm_sii_mexico_primary,
                             rank_lm_sii_panama_primary,
                             rank_lm_sii_peru_primary) %>% 
  mutate(exposure = "Primary school attainment (>25 years old)")

rank_lm_sii <- rbind(rank_lm_sii_attending,
                     rank_lm_sii_primary) %>% 
  mutate(exposure_r = factor(exposure,levels = c(
    "School attendance (any school, 15 to 17 yo)",
    "Primary school attainment (>25 years old)")) ) %>% 
  mutate(country_r = fct_rev(country))

ggplot(rank_lm_sii, aes(x=rank, y=country_r, color=country_r)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (population weighted) of education by country", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure_r, ncol=2)

ggsave("rank_lm_sii_deciles_9c_cuanti.png",
       last_plot(),
       dpi = 300,
       bg = "white",
       width = 9,
       height = 5)

















#6. Merge all bases -------------
ng_argentina$ID <- ng_argentina$SALID2_5
sub_argentina <- ng_argentina %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_brasil$ID <- ng_brasil$SALID2_5
sub_brasil <- ng_brasil %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_chile$ID <- ng_chile$salid3
sub_chile <- ng_chile %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_colombia$ID <- ng_colombia$SALID3
sub_colombia <- ng_colombia %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_costarica$ID <- ng_costarica$SALID2_5
sub_costarica <- ng_costarica %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_guatemala$ID <- ng_guatemala$SALID3
sub_guatemala <- ng_guatemala %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_mexico$ID <- ng_mexico$salid3
sub_mexico <- ng_mexico %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_panama$ID <- ng_panama$salid3
sub_panama <- ng_panama %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

ng_peru$ID <- ng_peru$SALID3
sub_peru <- ng_peru %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25",
         "CNSPOPL3")

sub_all_countries <- rbind(sub_argentina,
                           sub_brasil,
                           sub_chile,
                           sub_colombia,
                           sub_costarica,
                           sub_guatemala,
                           sub_mexico,
                           sub_panama,
                           sub_peru)

all_countries_total <- sum(sub_all_countries$CNSPOPL3, na.rm = T) #Create total population value

#attending school
sii_all_countries <- sub_all_countries %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/all_countries_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_attending <- lm(tasa_cases~rank, weights = pop,
                         data = sii_all_countries)

rank_lm_sii_all_countries_attending <- edson(sii_all_countries_attending, "All Countries")

#primary school

sii_all_countries <- sub_all_countries %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/all_countries_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(CNSPOPL3) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_primary <- lm(tasa_cases~rank, weights = pop,
                       data = sii_all_countries)

rank_lm_sii_all_countries_primary <- edson(sii_all_countries_primary, "All Countries")

#Bar panel 

g1 <- sii_all_countries %>% 
  ggplot() +
  geom_col(
    aes(
      x=rank,
      y=tasa_cases
    ),
    fill= "#298A08"
  ) +
  xlim(0,1.01) +
  geom_smooth(
    aes(
      x=rank,
      y=tasa_cases
    ),
    method = lm, 
    formula = y ~ x,
    span = 1,
    fullrange = T,
    fill = "dark green",
    colour = "dark green"
  ) +
  theme_bw() +
  stat_regline_equation(
    aes(
      x=rank,
      y=tasa_cases
    ),
    label.x = 0.5, label.y = 0.1,
    formula = y ~ x
    )

g2 <- sii_all_countries %>% 
  ggplot() +
  xlim(0,1.01) +
  geom_smooth(
    aes(
      x=rank,
      y=out_cases
    ),
    method = lm, 
    formula = y ~ x,
    span = 1,
    fullrange = T,
    fill = "dark blue",
    colour = "dark blue"
  ) +
  theme_bw() +
  scale_y_continuous(position = "right") +
  stat_regline_equation(
    aes(
      x=rank,
      y=out_cases
    ),
    label.x = 0.5, label.y = 25,
    formula = y ~ x
  )

g3 <- g1 + g2
g3

ggsave("panelsSII_L3.pdf", plot = g3, width = 8, height = 4)

#L2 Analysis ---------------
##2.1 argentina ----
ng_L2_argentina <-  ng_argentina %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 1] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 2] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 3] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 4] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 5] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 6] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 7] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 8] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 9] <- 0
ng_L2_argentina$PM2.5_catq[ ng_L2_argentina$PM2.5_catq == 10] <- 1

argentina_total <- sum(ng_L2_argentina$pop_L2, na.rm = T) #Create total population value

#attending school
sii_argentina <- ng_L2_argentina %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_argentina)

rank_lm_sii_argentina_attending <- edson(sii_argentina_attending, "argentina")

#primary school

sii_argentina <- ng_L2_argentina %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/argentina_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_primary <- lm(tasa_cases~rank, weights = pop,
                              data = sii_argentina)

rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "argentina")














##2.2 brasil ----
ng_L2_brasil <-  ng_brasil %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 1] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 2] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 3] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 4] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 5] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 6] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 7] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 8] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 9] <- 0
ng_L2_brasil$PM2.5_catq[ ng_L2_brasil$PM2.5_catq == 10] <- 1

brasil_total <- sum(ng_L2_brasil$pop_L2, na.rm = T) #Create total population value

#attending school
sii_brasil <- ng_L2_brasil %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_brasil)

rank_lm_sii_brasil_attending <- edson(sii_brasil_attending, "brasil")

#primary school

sii_brasil <- ng_L2_brasil %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/brasil_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_brasil)

rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "brasil")














##2.3 chile ----
ng_L2_chile <-  ng_chile %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 1] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 2] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 3] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 4] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 5] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 6] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 7] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 8] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 9] <- 0
ng_L2_chile$PM2.5_catq[ ng_L2_chile$PM2.5_catq == 10] <- 1

chile_total <- sum(ng_L2_chile$pop_L2, na.rm = T) #Create total population value

#attending school
sii_chile <- ng_L2_chile %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_chile)

rank_lm_sii_chile_attending <- edson(sii_chile_attending, "chile")

#primary school

sii_chile <- ng_L2_chile %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/chile_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_chile)

rank_lm_sii_chile_primary <- edson(sii_chile_primary, "chile")














##2.4 colombia ----
ng_L2_colombia <-  ng_colombia %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 1] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 2] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 3] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 4] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 5] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 6] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 7] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 8] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 9] <- 0
ng_L2_colombia$PM2.5_catq[ ng_L2_colombia$PM2.5_catq == 10] <- 1

colombia_total <- sum(ng_L2_colombia$pop_L2, na.rm = T) #Create total population value

#attending school
sii_colombia <- ng_L2_colombia %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_colombia)

rank_lm_sii_colombia_attending <- edson(sii_colombia_attending, "colombia")

#primary school

sii_colombia <- ng_L2_colombia %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/colombia_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_colombia)

rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "colombia")














##2.5 costarica ----
ng_L2_costarica <-  ng_costarica %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 1] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 2] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 3] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 4] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 5] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 6] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 7] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 8] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 9] <- 0
ng_L2_costarica$PM2.5_catq[ ng_L2_costarica$PM2.5_catq == 10] <- 1

costarica_total <- sum(ng_L2_costarica$pop_L2, na.rm = T) #Create total population value

#attending school
sii_costarica <- ng_L2_costarica %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_costarica)

rank_lm_sii_costarica_attending <- edson(sii_costarica_attending, "costarica")

#primary school

sii_costarica <- ng_L2_costarica %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/costarica_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_costarica)

rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "costarica")
##2.6 guatemala ----
ng_L2_guatemala <-  ng_guatemala %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 1] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 2] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 3] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 4] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 5] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 6] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 7] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 8] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 9] <- 0
ng_L2_guatemala$PM2.5_catq[ ng_L2_guatemala$PM2.5_catq == 10] <- 1

guatemala_total <- sum(ng_L2_guatemala$pop_L2, na.rm = T) #Create total population value

#attending school
sii_guatemala <- ng_L2_guatemala %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_guatemala)

rank_lm_sii_guatemala_attending <- edson(sii_guatemala_attending, "guatemala")

#primary school

sii_guatemala <- ng_L2_guatemala %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/guatemala_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_guatemala)

rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "guatemala")
##2.7 mexico ----
ng_L2_mexico <-  ng_mexico %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 1] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 2] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 3] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 4] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 5] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 6] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 7] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 8] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 9] <- 0
ng_L2_mexico$PM2.5_catq[ ng_L2_mexico$PM2.5_catq == 10] <- 1

mexico_total <- sum(ng_L2_mexico$pop_L2, na.rm = T) #Create total population value

#attending school
sii_mexico <- ng_L2_mexico %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_mexico)

rank_lm_sii_mexico_attending <- edson(sii_mexico_attending, "mexico")

#primary school

sii_mexico <- ng_L2_mexico %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/mexico_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_mexico)

rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "mexico")
##2.8 panama ----
ng_L2_panama <-  ng_panama %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 1] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 2] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 3] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 4] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 5] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 6] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 7] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 8] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 9] <- 0
ng_L2_panama$PM2.5_catq[ ng_L2_panama$PM2.5_catq == 10] <- 1

panama_total <- sum(ng_L2_panama$pop_L2, na.rm = T) #Create total population value

#attending school
sii_panama <- ng_L2_panama %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_panama)

rank_lm_sii_panama_attending <- edson(sii_panama_attending, "panama")

#primary school

sii_panama <- ng_L2_panama %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/panama_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_panama)

rank_lm_sii_panama_primary <- edson(sii_panama_primary, "panama")
##2.9 peru ----
ng_L2_peru <-  ng_peru %>%
  group_by(SALID2) %>% 
  summarise(attending_L2 = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
            pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(attending_L2_prop = attending_L2/pop_L2,
         primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(attending_quantile_c = ntile(attending_L2_prop, 5)) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)) %>% 
  mutate(PM2.5_catq = ntile (PM25_L2_prop, 10)) 

ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 1] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 2] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 3] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 4] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 5] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 6] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 7] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 8] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 9] <- 0
ng_L2_peru$PM2.5_catq[ ng_L2_peru$PM2.5_catq == 10] <- 1

peru_total <- sum(ng_L2_peru$pop_L2, na.rm = T) #Create total population value

#attending school
sii_peru <- ng_L2_peru %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_attending <- lm(tasa_cases~rank, weights = pop,
                              data = sii_peru)

rank_lm_sii_peru_attending <- edson(sii_peru_attending, "peru")

#primary school

sii_peru <- ng_L2_peru %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/peru_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_primary <- lm(tasa_cases~rank, weights = pop,
                            data = sii_peru)

rank_lm_sii_peru_primary <- edson(sii_peru_primary, "peru")
#3. SII Graphics----------------------------------

## Forest plot GG -------------------
rank_lm_sii_attending <- rbind(rank_lm_sii_all_countries_attending,
                               rank_lm_sii_argentina_attending,
                               rank_lm_sii_brasil_attending,
                               rank_lm_sii_chile_attending,
                               rank_lm_sii_colombia_attending,
                               rank_lm_sii_costarica_attending,
                               rank_lm_sii_guatemala_attending,
                               rank_lm_sii_mexico_attending,
                               rank_lm_sii_panama_attending,
                               rank_lm_sii_peru_attending) %>% 
  mutate(exposure = "School attendance (any school, 15 to 17 yo)")

rank_lm_sii_primary <- rbind(rank_lm_sii_all_countries_primary,
                             rank_lm_sii_argentina_primary,
                             rank_lm_sii_brasil_primary,
                             rank_lm_sii_chile_primary,
                             rank_lm_sii_colombia_primary,
                             rank_lm_sii_costarica_primary,
                             rank_lm_sii_guatemala_primary,
                             rank_lm_sii_mexico_primary,
                             rank_lm_sii_panama_primary,
                             rank_lm_sii_peru_primary) %>% 
  mutate(exposure = "Primary school attainment (>25 years old)")

rank_lm_sii <- rbind(rank_lm_sii_attending,
                     rank_lm_sii_primary) %>% 
  mutate(exposure_r = factor(exposure,levels = c(
    "School attendance (any school, 15 to 17 yo)",
    "Primary school attainment (>25 years old)")) ) %>% 
  mutate(country_r = fct_rev(country))

ggplot(rank_lm_sii, aes(x=rank, y=country_r, color=country_r)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (population weighted) of education by country at District level", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure_r, ncol=2)

ggsave("rank_lm_sii_deciles_9c_L2.png",
       last_plot(),
       dpi = 300,
       bg = "white",
       width = 9,
       height = 5)

ggsave("rank_lm_sii_deciles_9c_L2.pdf",
       last_plot(),
       bg = "white",
       width = 9,
       height = 5)

##Merge L2 databases -------
ng_L2_argentina$ID <- ng_L2_argentina$SALID2
sub_argentina <- ng_L2_argentina %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_brasil$ID <- ng_L2_brasil$SALID2
sub_brasil <- ng_L2_brasil %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_chile$ID <- ng_L2_chile$SALID2
sub_chile <- ng_L2_chile %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_colombia$ID <- ng_L2_colombia$SALID2
sub_colombia <- ng_L2_colombia %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_costarica$ID <- ng_L2_costarica$SALID2
sub_costarica <- ng_L2_costarica %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_guatemala$ID <- ng_L2_guatemala$SALID2
sub_guatemala <- ng_L2_guatemala %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_mexico$ID <- ng_L2_mexico$SALID2
sub_mexico <- ng_L2_mexico %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_panama$ID <- ng_L2_panama$SALID2
sub_panama <- ng_L2_panama %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_peru$ID <- ng_L2_peru$SALID2
sub_peru <- ng_L2_peru %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM2.5_catq",
         "PM25_L2_prop",
         "pop_L2")

sub_all_countries_L2 <- rbind(sub_argentina,
                           sub_brasil,
                           sub_chile,
                           sub_colombia,
                           sub_costarica,
                           sub_guatemala,
                           sub_mexico,
                           sub_panama,
                           sub_peru)

all_countries_total_L2 <- sum(sub_all_countries_L2$pop_L2, na.rm = T) #Create total population value

#attending school
sii_all_countries <- sub_all_countries_L2 %>% 
  
  drop_na(attending_quantile_c) %>% 
  
  group_by(attending_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/all_countries_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop,
            out_cases = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases),
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_attending <- lm(tasa_cases~rank, weights = pop,
                                  data = sii_all_countries)

rank_lm_sii_all_countries_attending <- edson(sii_all_countries_attending, "All Countries")

#primary school

sii_all_countries <- sub_all_countries_L2 %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/all_countries_total, ### nota: mejorar nrow
            ncases = sum(as.numeric(pop_L2) * PM2.5_catq, na.rm = T),
            tasa_cases = ncases/pop) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         cum_cases = cumsum(ncases), 
         lt = cum_cases/sum(ncases),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_primary <- lm(tasa_cases~rank, weights = pop,
                                data = sii_all_countries)

rank_lm_sii_all_countries_primary <- edson(sii_all_countries_primary, "All Countries")

#Bar panel 

g1 <- sii_all_countries %>% 
  ggplot() +
  geom_col(
    aes(
      x=rank,
      y=tasa_cases
    ),
    fill= "#298A08"
  ) +
  xlim(-0.05,1) +
  coord_cartesian(ylim = c(0, 0.15)) +
  geom_smooth(
    aes(
      x=rank,
      y=tasa_cases
    ),
    method = lm, 
    formula = y ~ x,
    span = 1,
    fullrange = T,
    fill = "dark green",
    colour = "dark green"
  ) +
  theme_bw() +
  stat_regline_equation(
    aes(
      x=rank,
      y=tasa_cases
    ),
    label.x = 0.5, label.y = 0.1,
    formula = y ~ x
  )

g2 <- sii_all_countries %>% 
  ggplot() +
  xlim(0,1.01) +
  geom_smooth(
    aes(
      x=rank,
      y=out_cases
    ),
    method = lm, 
    formula = y ~ x,
    span = 1,
    fullrange = T,
    fill = "dark blue",
    colour = "dark blue"
  ) +
  theme_bw() +
  scale_y_continuous(position = "right") +
  stat_regline_equation(
    aes(
      x=rank,
      y=out_cases
    ),
    label.x = 0.5, label.y = 23,
    formula = y ~ x
  )

g3 <- g1 + g2
g3

ggsave("panelsSII_L2.pdf", plot = g3, width = 8, height = 4)
