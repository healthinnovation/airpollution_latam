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
library(scales)
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

#2. SII analysis L3 ----------

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
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

argentina_total <- sum(ng_argentina$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_argentina <- ng_argentina %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/argentina_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_argentina_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_argentina)
cf_argentina <- coef(sii_argentina_primary)

rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "Buenos Aires (AR)")

##2.2 brasil ----
ng_brasil <-  ng_brasil %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

brasil_total <- sum(ng_brasil$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_brasil <- ng_brasil %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/brasil_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_brasil_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_brasil)
cf_brasil <- coef(sii_brasil_primary)
rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "São Paulo (BR)")
##2.3 chile ----
ng_chile <-  ng_chile %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

chile_total <- sum(ng_chile$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_chile <- ng_chile %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/chile_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_chile_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_chile)
cf_chile <- coef(sii_chile_primary)
rank_lm_sii_chile_primary <- edson(sii_chile_primary, "Santiago (CL)")

##2.4 colombia ----
ng_colombia <-  ng_colombia %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

colombia_total <- sum(ng_colombia$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_colombia <- ng_colombia %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/colombia_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_colombia_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_colombia)
cf_colombia <- coef(sii_colombia_primary)
rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "Bogota (CO)")

##2.5 costarica ----
ng_costarica <-  ng_costarica %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

costarica_total <- sum(ng_costarica$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_costarica <- ng_costarica %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/costarica_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_costarica_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_costarica)
cf_costarica <- coef(sii_costarica_primary)
rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "San Jose (CR)")
##2.6 guatemala ----
ng_guatemala <-  ng_guatemala %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

guatemala_total <- sum(ng_guatemala$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_guatemala <- ng_guatemala %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/guatemala_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_guatemala_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_guatemala)
cf_guatemala <- coef(sii_guatemala_primary)
rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "Guatemala City (GT)")
##2.7 mexico ----
ng_mexico <-  ng_mexico %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

mexico_total <- sum(ng_mexico$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_mexico <- ng_mexico %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/mexico_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_mexico_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_mexico)
cf_mexico <- coef(sii_mexico_primary)
rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "Mexico City (MX)")
##2.8 panama ----
ng_panama <-  ng_panama %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

panama_total <- sum(ng_panama$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_panama <- ng_panama %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/panama_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_panama_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_panama)
cf_panama <- coef(sii_panama_primary)
rank_lm_sii_panama_primary <- edson(sii_panama_primary, "Panama City (PA)")
##2.9 peru ----
ng_peru <-  ng_peru %>%
  mutate(attending_quantile_c = ntile(CNSST1517L3, 5)) %>% 
  mutate(primary_quantile_c = ntile(CNSMINPR_L3, 5)/5)

peru_total <- sum(ng_peru$CNSPOPL3, na.rm = T) #Create total population value

#primary school
sii_peru <- ng_peru %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/peru_total,
            pm_mean = mean(PM25, na.rm = T)) %>% 
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )


sii_peru_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3, data = ng_peru)
cf_peru <- coef(sii_peru_primary)
rank_lm_sii_peru_primary <- edson(sii_peru_primary, "Lima (PE)")
#3. Merge all bases L3 -------------
ng_argentina$ID <- ng_argentina$SALID2_5
sub_argentina <- ng_argentina %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_brasil$ID <- ng_brasil$SALID2_5
sub_brasil <- ng_brasil %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_chile$ID <- ng_chile$salid3
sub_chile <- ng_chile %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_colombia$ID <- ng_colombia$SALID3
sub_colombia <- ng_colombia %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_costarica$ID <- ng_costarica$SALID2_5
sub_costarica <- ng_costarica %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_guatemala$ID <- ng_guatemala$SALID3
sub_guatemala <- ng_guatemala %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_mexico$ID <- ng_mexico$salid3
sub_mexico <- ng_mexico %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_panama$ID <- ng_panama$salid3
sub_panama <- ng_panama %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
         "PM25",
         "CNSPOPL3")

ng_peru$ID <- ng_peru$SALID3
sub_peru <- ng_peru %>% 
  select("ID",
         "attending_quantile_c",
         "primary_quantile_c",
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

#primary school

sii_all_countries <- sub_all_countries %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(CNSPOPL3), na.rm = T), 
            prop_p = pop/all_countries_total,
            pm_mean = mean(PM25, na.rm = T)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_primary <- lm(PM25~primary_quantile_c, weights = CNSPOPL3,
                                data = sub_all_countries)
cf_all_countries <- coef(sii_all_countries_primary)
rank_lm_sii_all_countries_primary <- edson(sii_all_countries_primary, "All Cities")

#4. Forest plot L3--------
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
  mutate(exposure = "Primary school attainment (>25 yo)")
rank_lm_sii_primary$country <- factor(rank_lm_sii_primary$country,
                                      levels = c(
                                        "Lima (PE)",
                                        "Panama City (PA)",
                                        "Mexico City (MX)",
                                        "Guatemala City (GT)",
                                        "San Jose (CR)",
                                        "Bogota (CO)",
                                        "Santiago (CL)",
                                        "São Paulo (BR)",
                                        "Buenos Aires (AR)",
                                        "All Cities"
                                      )
)

ggplot(rank_lm_sii_primary, aes(x=rank, y=country, color=country)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (Neighborhood level) of education by City", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure, ncol=2)

ggsave("figs/fig_04_pD_Usama.eps", height = 11, width = 16, units = "cm")

##4.1. Smooth plot L3 ----------
ggplot() +
  
  geom_point(data=sii_argentina, 
             aes(x=rank, y=pm_mean),
             color="#F69354",
             size=2.5) +
  geom_abline(intercept=cf_argentina[1],
              slope=cf_argentina[2],
              color="#F69354",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_brasil, 
             aes(x=rank, y=pm_mean),
             color="#FEBE82",
             size=2.5) +
  geom_abline(intercept=cf_brasil[1],
              slope=cf_brasil[2],
              color="#FEBE82",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_chile, 
             aes(x=rank, y=pm_mean),
             color="#DAE2BC",
             size=2.5) +
  geom_abline(intercept=cf_chile[1],
              slope=cf_chile[2],
              color="#DAE2BC",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_colombia, 
             aes(x=rank, y=pm_mean),
             color="#7DC9C1",
             size=2.5) +
  geom_abline(intercept=cf_colombia[1],
              slope=cf_colombia[2],
              color="#7DC9C1",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_costarica, 
             aes(x=rank, y=pm_mean),
             color="#39A69F",
             size=2.5) +
  geom_abline(intercept=cf_costarica[1],
              slope=cf_costarica[2],
              color="#39A69F",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_guatemala, 
             aes(x=rank, y=pm_mean),
             color="#2C9BB4",
             size=2.5) +
  geom_abline(intercept=cf_guatemala[1],
              slope=cf_guatemala[2],
              color="#2C9BB4",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_mexico, 
             aes(x=rank, y=pm_mean),
             color="#2E7998",
             size=2.5) +
  geom_abline(intercept=cf_mexico[1],
              slope=cf_mexico[2],
              color="#2E7998",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_panama, 
             aes(x=rank, y=pm_mean),
             color="#186179",
             size=2.5) +
  geom_abline(intercept=cf_panama[1],
              slope=cf_panama[2],
              color="#186179",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_peru, 
             aes(x=rank, y=pm_mean),
             color="#0B4E60",
             size=2.5) +
  geom_abline(intercept=cf_peru[1],
              slope=cf_peru[2],
              color="#0B4E60",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_all_countries, 
             aes(x=rank, y=pm_mean),
             color="#8C3A33",
             size=3) +
  geom_abline(intercept=cf_all_countries[1],
              slope=cf_all_countries[2],
              color="#8C3A33",
              size=1.8,
              alpha=0.98) +
  
  scale_x_continuous(
    breaks = c(0.2,0.4,0.6,0.8, 1),
    limits = c(0.05,1.05)
  ) +
  
  scale_y_continuous(
    breaks = c(15,20,25,30,35)
  ) +
  
  theme_bw()

ggsave("figs/fig_04_pB.pdf", height = 11, width = 16, units = "cm")

#5. SII analysis L2 ---------------
##5.1 argentina ----
ng_L2_argentina <-  ng_argentina %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

argentina_total <- sum(ng_L2_argentina$pop_L2, na.rm = T) #Create total population value

#primary school

sii_argentina <- ng_L2_argentina %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/argentina_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_argentina_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_argentina)
cf_argentina <- coef(sii_argentina_primary)
rank_lm_sii_argentina_primary <- edson(sii_argentina_primary, "Buenos Aires (AR)")

##5.2 brasil ----
ng_L2_brasil <-  ng_brasil %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

brasil_total <- sum(ng_L2_brasil$pop_L2, na.rm = T) #Create total population value

#primary school

sii_brasil <- ng_L2_brasil %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/brasil_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_brasil_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_brasil)
cf_brasil <- coef(sii_brasil_primary)
rank_lm_sii_brasil_primary <- edson(sii_brasil_primary, "São Paulo (BR)")

##5.3 chile ----
ng_L2_chile <-  ng_chile %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

chile_total <- sum(ng_L2_chile$pop_L2, na.rm = T) #Create total population value

#primary school

sii_chile <- ng_L2_chile %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/chile_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_chile_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_chile)
cf_chile <- coef(sii_chile_primary)
rank_lm_sii_chile_primary <- edson(sii_chile_primary, "Santiago (CL)")

##5.4 colombia ----
ng_L2_colombia <-  ng_colombia %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

colombia_total <- sum(ng_L2_colombia$pop_L2, na.rm = T) #Create total population value

#primary school

sii_colombia <- ng_L2_colombia %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/colombia_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_colombia_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_colombia)
cf_colombia <- coef(sii_colombia_primary)
rank_lm_sii_colombia_primary <- edson(sii_colombia_primary, "Bogota (CO)")

##5.5 costarica ----
ng_L2_costarica <-  ng_costarica %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

costarica_total <- sum(ng_L2_costarica$pop_L2, na.rm = T) #Create total population value

#primary school

sii_costarica <- ng_L2_costarica %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/costarica_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_costarica_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_costarica)
cf_costarica <- coef(sii_costarica_primary)
rank_lm_sii_costarica_primary <- edson(sii_costarica_primary, "San Jose (CR)")

##5.6 guatemala ----
ng_L2_guatemala <-  ng_guatemala %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

guatemala_total <- sum(ng_L2_guatemala$pop_L2, na.rm = T) #Create total population value

#primary school

sii_guatemala <- ng_L2_guatemala %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/guatemala_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_guatemala_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_guatemala)
cf_guatemala <- coef(sii_guatemala_primary)
rank_lm_sii_guatemala_primary <- edson(sii_guatemala_primary, "Guatemala City (GT)")

##5.7 mexico ----
ng_L2_mexico <-  ng_mexico %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

mexico_total <- sum(ng_L2_mexico$pop_L2, na.rm = T) #Create total population value

#primary school

sii_mexico <- ng_L2_mexico %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/mexico_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_mexico_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_mexico)
cf_mexico <- coef(sii_mexico_primary)
rank_lm_sii_mexico_primary <- edson(sii_mexico_primary, "Mexico City (MX)")

##5.8 panama ----
ng_L2_panama <-  ng_panama %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

panama_total <- sum(ng_L2_panama$pop_L2, na.rm = T) #Create total population value

#primary school

sii_panama <- ng_L2_panama %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/panama_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_panama_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_panama)
cf_panama <- coef(sii_panama_primary)
rank_lm_sii_panama_primary <- edson(sii_panama_primary, "Panama City (PA)")

##5.9 peru ----
ng_L2_peru <-  ng_peru %>%
  group_by(SALID2) %>% 
  summarise(pop_L2 = sum(CNSPOPL3, na.rm = T),
            primary_L2 = sum(CNSMINPR_L3*CNSPOPL3, na.rm = T),
            PM25_L2 = sum(PM25*CNSPOPL3, na.rm = T)) %>% 
  mutate(primary_L2_prop = primary_L2/pop_L2,
         PM25_L2_prop = PM25_L2/pop_L2) %>% 
  mutate(primary_quantile_c = ntile(primary_L2_prop, 5)/5)

peru_total <- sum(ng_L2_peru$pop_L2, na.rm = T) #Create total population value

#primary school

sii_peru <- ng_L2_peru %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/peru_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_peru_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = ng_L2_peru)
cf_peru <- coef(sii_peru_primary)
rank_lm_sii_peru_primary <- edson(sii_peru_primary, "Lima (PE)")

##6. Merge L2 databases -------
ng_L2_argentina$ID <- ng_L2_argentina$SALID2
sub_argentina <- ng_L2_argentina %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_brasil$ID <- ng_L2_brasil$SALID2
sub_brasil <- ng_L2_brasil %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_chile$ID <- ng_L2_chile$SALID2
sub_chile <- ng_L2_chile %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_colombia$ID <- ng_L2_colombia$SALID2
sub_colombia <- ng_L2_colombia %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_costarica$ID <- ng_L2_costarica$SALID2
sub_costarica <- ng_L2_costarica %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_guatemala$ID <- ng_L2_guatemala$SALID2
sub_guatemala <- ng_L2_guatemala %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_mexico$ID <- ng_L2_mexico$SALID2
sub_mexico <- ng_L2_mexico %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_panama$ID <- ng_L2_panama$SALID2
sub_panama <- ng_L2_panama %>% 
  select("ID",
         "primary_quantile_c",
         "PM25_L2_prop",
         "pop_L2")

ng_L2_peru$ID <- ng_L2_peru$SALID2
sub_peru <- ng_L2_peru %>% 
  select("ID",
         "primary_quantile_c",
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

#primary school

sii_all_countries <- sub_all_countries_L2 %>% 
  
  drop_na(primary_quantile_c) %>% 
  
  group_by(primary_quantile_c) %>% 
  
  summarize(pop = sum(as.numeric(pop_L2), na.rm = T), 
            prop_p = pop/all_countries_total,
            pm_mean = mean(PM25_L2_prop)) %>%  
  
  mutate(cum_p = cumsum(pop),
         pt = cum_p/sum(pop),
         rank = (pt + lag(pt, default = 0))/2
  )

sii_all_countries_primary <- lm(PM25_L2_prop~primary_quantile_c, weights = pop_L2, data = sub_all_countries_L2)
cf_all_countries <- coef(sii_all_countries_primary)
rank_lm_sii_all_countries_primary <- edson(sii_all_countries_primary, "All Cities")

##7. Forest plot L2 -------------------
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
  mutate(exposure = "Primary school attainment (>25 yo)")

rank_lm_sii_primary$country <- factor(rank_lm_sii_primary$country,
                                      levels = c(
                                        "Lima (PE)",
                                        "Panama City (PA)",
                                        "Mexico City (MX)",
                                        "Guatemala City (GT)",
                                        "San Jose (CR)",
                                        "Bogota (CO)",
                                        "Santiago (CL)",
                                        "São Paulo (BR)",
                                        "Buenos Aires (AR)",
                                        "All Cities"
                                      )
)

ggplot(rank_lm_sii_primary, aes(x=rank, y=country, color=country)) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.5) +
  geom_point(size = 2.5) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title="SII (Subcity level) of education by City", x="Slope Index of Inequality", y="") +
  theme_bw() +
  scale_color_innova("jama") +
  theme(legend.position = "none") +
  facet_wrap(.~exposure, ncol=2)

ggsave("figs/fig_04_pC_usama.eps", height = 11, width = 16, units = "cm")
##7.1. Smooth plot L2 ----------
ggplot() +
  
  geom_point(data=sii_argentina, 
             aes(x=rank, y=pm_mean),
             color="#F69354",
             size=2.5) +
  geom_abline(intercept=cf_argentina[1],
              slope=cf_argentina[2],
              color="#F69354",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_brasil, 
             aes(x=rank, y=pm_mean),
             color="#FEBE82",
             size=2.5) +
  geom_abline(intercept=cf_brasil[1],
              slope=cf_brasil[2],
              color="#FEBE82",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_chile, 
             aes(x=rank, y=pm_mean),
             color="#DAE2BC",
             size=2.5) +
  geom_abline(intercept=cf_chile[1],
              slope=cf_chile[2],
              color="#DAE2BC",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_colombia, 
             aes(x=rank, y=pm_mean),
             color="#7DC9C1",
             size=2.5) +
  geom_abline(intercept=cf_colombia[1],
              slope=cf_colombia[2],
              color="#7DC9C1",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_costarica, 
             aes(x=rank, y=pm_mean),
             color="#39A69F",
             size=2.5) +
  geom_abline(intercept=cf_costarica[1],
              slope=cf_costarica[2],
              color="#39A69F",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_guatemala, 
             aes(x=rank, y=pm_mean),
             color="#2C9BB4",
             size=2.5) +
  geom_abline(intercept=cf_guatemala[1],
              slope=cf_guatemala[2],
              color="#2C9BB4",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_mexico, 
             aes(x=rank, y=pm_mean),
             color="#2E7998",
             size=2.5) +
  geom_abline(intercept=cf_mexico[1],
              slope=cf_mexico[2],
              color="#2E7998",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_panama, 
             aes(x=rank, y=pm_mean),
             color="#186179",
             size=2.5) +
  geom_abline(intercept=cf_panama[1],
              slope=cf_panama[2],
              color="#186179",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_peru, 
             aes(x=rank, y=pm_mean),
             color="#0B4E60",
             size=2.5) +
  geom_abline(intercept=cf_peru[1],
              slope=cf_peru[2],
              color="#0B4E60",
              size=1.2,
              alpha=0.55) +
  
  geom_point(data=sii_all_countries, 
             aes(x=rank, y=pm_mean),
             color="#8C3A33",
             size=3) +
  geom_abline(intercept=cf_all_countries[1],
              slope=cf_all_countries[2],
              color="#8C3A33",
              size=1.8,
              alpha=0.98) +
  
  scale_x_continuous(
    breaks = c(0.2,0.4,0.6,0.8, 1),
    limits = c(0,1.05)
  ) +
  
  scale_y_continuous(
    breaks = c(15,20,25,30,35)
  ) +
  
  theme_bw()

ggsave("figs/fig_04_pA.pdf", height = 11, width = 16, units = "cm")
