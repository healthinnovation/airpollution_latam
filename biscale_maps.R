#MAp Biscale plots -----------
# A. gg_barplot -----------------------------------------------------------
gg_barplot <- function(x,nrow = 1){
  g0 <- x %>% 
    ggplot(
      aes(
        x = bi_class,
        y = total,
        fill = bi_class
      )
    ) + 
    geom_bar(stat = 'identity') + 
    theme_minimal() + 
    theme(legend.position = "none") +
    labs(x = "", y = "")
  
  if(nrow == 1){
    g1 <- g0 + 
      scale_fill_manual(values = c("#AE384C","#BA7A8F","#CADECF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  else if(nrow == 2){ 
    g1 <- g0 + 
      scale_fill_manual(values = c("#75304A","#7F688A","#87A1C7")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  } else {
    g1 <- g0 + 
      scale_fill_manual(values = c("#3D2847","#425785","#4785BF")) + 
      theme(
        plot.margin=margin(t=-0.5,unit="cm"),
        axis.text = element_text(size = 4)
      ) + 
      theme(
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,1.5,-0.5,1.5),"lines")
      )
  }
  return(g1)
} 

# B. Biscale color palette - customized -----------------------------------

bi_pal2 <- function (pal, dim = 3, preview = TRUE) 
{
  if (missing(pal) == TRUE) {
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (pal %in% c("Brown", "DkBlue", "DkCyan", "DkViolet", "GrPink") == 
      FALSE) {
    stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'Brown', 'DkBlue', 'DkCyan', 'DkViolet', and 'GrPink'.")
  }
  if (is.numeric(dim) == FALSE) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (dim != 2 & dim != 3) {
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  if (is.logical(preview) == FALSE) {
    stop("A logical scalar must be supplied for 'preview'. Please provide either 'TRUE' or 'FALSE'.")
  }
  if (preview == TRUE) {
    out <- bi_legend(pal = pal, dim = dim, size = 16)
  }
  else if (preview == FALSE) {
    if (pal == "DkViolet") {
      out <- pal_dkviolet2(n = dim)
    }
    else if (pal == "GrPink") {
      out <- pal_grpink(n = dim)
    }
    else if (pal == "DkBlue") {
      out <- pal_dkblue(n = dim)
    }
    else if (pal == "DkCyan") {
      out <- pal_dkcyan(n = dim)
    }
    else if (pal == "Brown") {
      out <- pal_brown(n = dim)
    }
  }
  return(out)
}

pal_dkviolet <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#3F2949", # high x, high y
      "1-2" = "#4885C1", # low x, high y
      "2-1" = "#AE3A4E", # high x, low y
      "1-1" = "#CABED0" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#CADECF", # high x, high y
      "2-3" = "#BA7A8F",
      "1-3" = "#AE384C", # low x, high y
      "3-2" = "#87A1C7",
      "2-2" = "#7F688A", # medium x, medium y
      "1-2" = "#75304A",
      "3-1" = "#4785BF", # high x, low y
      "2-1" = "#425785",
      "1-1" = "#3D2847" # low x, low y
    )
    
  }
  return(out)
}

# gray pink palette
pal_grpink <- function(n){
  
  # construct palette
  if (n == 2){
    
    out <- c(
      "2-2" = "#574249", # high x, high y
      "1-2" = "#64ACBE", # low x, high y
      "2-1" = "#C85A5A", # high x, low y
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  } else if (n == 3){
    
    out <- c(
      "3-3" = "#574249", # high x, high y
      "2-3" = "#627F8C",
      "1-3" = "#64ACBE", # low x, high y
      "3-2" = "#985356",
      "2-2" = "#AD9EA5", # medium x, medium y
      "1-2" = "#B0D5DF",
      "3-1" = "#C85A5A", # high x, low y
      "2-1" = "#E4ACAC",
      "1-1" = "#E8E8E8" # low x, low y
    )
    
  }
  
  # return output
  return(out)
  
}


bi_legend <- function(pal, dim = 3, xlab, ylab, size = 10, flip_axes = FALSE, rotate_pal = FALSE, pad_width = NA, pad_color = '#ffffff'){
  
  # global binding
  bi_class = bi_fill = x = y = NULL
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (missing(xlab) == TRUE){
    xlab <- "x var "
  }
  
  if (is.character(xlab) == FALSE){
    stop("The 'xlab' argument must be a character string.")
  }
  
  if (missing(ylab) == TRUE){
    ylab <- "y var "
  }
  
  if (is.character(ylab) == FALSE){
    stop("The 'ylab' argument must be a character string.")
  }
  
  if (is.numeric(size) == FALSE){
    stop("The 'size' argument must be a numeric value.")
  }
  
  # nse
  xQN <- rlang::quo_name(rlang::enquo(xlab))
  yQN <- rlang::quo_name(rlang::enquo(ylab))
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # create tibble for plotting
  x <- dplyr::tibble(
    bi_class = names(x),
    bi_fill = x
  )
  
  # reformat
  leg <- tidyr::separate(x, bi_class, into = c("x", "y"), sep = "-")
  leg <- dplyr::mutate(leg, x = as.integer(x), y = as.integer(y))
  
  # create ggplot2 legend object
  legend <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = leg, mapping = ggplot2::aes(x = x, y = y, fill = bi_fill), lwd = pad_width, col = pad_color) +
    ggplot2::scale_fill_identity() +
    ggplot2::labs(x = substitute(paste(xQN, ""%->%"")), y = substitute(paste(yQN, ""%->%""))) +
    bi_theme() +
    ggplot2::theme(axis.title = ggplot2::element_text(size = size)) +
    ggplot2::coord_fixed()
  
  # return output
  return(legend)
  
}
bi_theme <- function(base_family = "sans", base_size = 24, bg_color = "#ffffff", font_color = "#000000", ...) {
  
  ggplot2::theme_minimal(base_family = base_family, base_size = base_size) +
    ggplot2::theme(
      
      # text defaults
      text = ggplot2::element_text(color = font_color),
      
      # remove all axes
      axis.line = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      
      # add a grid that blends into plot background
      panel.grid.major = ggplot2::element_line(color = bg_color, size = 0.2),
      panel.grid.minor = ggplot2::element_blank(),
      
      # background colors
      plot.background = ggplot2::element_rect(fill = bg_color, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_color, color = NA),
      legend.background = ggplot2::element_rect(fill = bg_color, color = NA),
      
      # borders and margins
      plot.margin = ggplot2::unit(c(.5, .5, .2, .5), "cm"),
      panel.border = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(c(-.1, 0.2, .2, 0.2), "cm"),
      
      # titles
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.25), hjust = 0.5, color = font_color, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = font_color,
                                            margin = ggplot2::margin(b = -0.1, t = -0.1, l = 2, unit = "cm"),
                                            face = "bold", debug = FALSE),
      legend.title = ggplot2::element_text(color = font_color),
      legend.text = ggplot2::element_text(hjust = 0, color = font_color),
      
      # captions
      plot.caption = ggplot2::element_text(size = ggplot2::rel(.6), hjust = .5,
                                           margin = ggplot2::margin(t = 0.2, b = 0, unit = "cm"),
                                           color = font_color),
      ...
    )
  
}

bi_scale_fill2 <- function(pal, dim = 3, flip_axes = FALSE, rotate_pal = FALSE, ...){
  
  # check parameters
  if (missing(pal) == TRUE){
    stop("A palette must be specified for the 'pal' argument. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis' or supply a custom palette created with 'bi_pal_custom()'.")
  }
  
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    if (dim == 2 & length(pal) != 4){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    } else if (dim == 3 & length(pal) != 9){
      stop("There is a mismatch between the length of your custom palette object and the given dimensions.")
    }
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    if (pal %in% c("BlGold", "BlOrange", "BlYellow", "Brown", "Diverging", "DkBlue", "DkCyan", "DkViolet", "Fire", "GnPink", "GnPurple", "GrPink", "OrgPurple", "Reds", "Viridis") == FALSE){
      stop("The given palette is not one of the allowed options for bivariate mapping. Please choose one of: 'BlGold', 'BlOrange', 'BlYellow', 'Brown', 'Diverging', 'DkBlue', 'DkCyan', 'DkViolet', 'Fire', 'GnPink', 'GnPurple', 'GrPink', 'OrgPurple', 'Reds' or 'Viridis'.")
    }
    
  }
  
  if (is.numeric(dim) == FALSE){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  if (dim != 2 & dim != 3){
    stop("The 'dim' argument only accepts the numeric values '2' or '3'.")
  }
  
  # obtain palette
  if ("bi_pal_custom" %in% class(pal) == TRUE) {
    
    x <- pal
    
  } else if ("bi_pal_custom" %in% class(pal) == FALSE){
    
    x <- switch(pal,
                "DkViolet" = pal_dkviolet(n = dim),
                "GrPink" = pal_grpink(n = dim),
                "DkBlue" = pal_dkblue(n = dim),
                "DkCyan" = pal_dkcyan(n = dim),
                "Brown" = pal_brown(n = dim),
                "BlGold" = pal_blgold(n = dim),
                "BlOrange" = pal_blorange(n = dim),
                "BlYellow" = pal_blyellow(n = dim),
                "Viridis" = pal_viridis(n = dim),
                "Diverging" = pal_diverging(n = dim),
                "GnPink" = pal_gnpink(n = dim),
                "GnPurple" = pal_gnpurp(n = dim),
                "OrgPurple" = pal_orgpurp(n = dim),
                "Fire" = pal_fire(n = dim),
                "Reds" = pal_reds(n = dim)
    )
    
    if(flip_axes){
      x <- bi_pal_flip(x)
    }
    
    if(rotate_pal){
      x <- bi_pal_rotate(x)
    }
    
  }
  
  # apply to ggplot object
  ggplot2::scale_fill_manual(values = x, ...)
  
}

# C. Maps biscale ---------------------------------------------------------
gg_bimap <- function(data, xlim, ylim){
  g0 <- data %>% 
    ggplot() + 
    geom_sf(
      data = dep,
      lwd = 0.5,
      fill = "#d9d9d9",
      show.legend = FALSE,
      color = "white"
    ) +
    geom_sf(
      data = data,
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
    bi_scale_fill2(pal = "DkViolet", dim = 3) +
    coord_sf(
      xlim = xlim,
      ylim = ylim,
      expand = FALSE
    ) +
    theme_bw()  +
    theme(
      axis.text =  element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    ) + 
    facet_grid(.~title) + 
    theme(
      strip.background = element_rect(
        colour = "black",
        fill = "white")
    )
}

#==========================================================================

# 3. Bogota attending -----------------------------------------------
library(janitor)
data1 %>% 
  tabyl(bi_class)

data1 <- bi_class(
  bogota,
  x = CNSST1517L3,
  y = APSPM25MEAN2018L3,
  style = "quantile",
  dim = 3
) %>% 
  drop_na(CNSST1517L3)

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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2002L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2011L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2017L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
  y = APSPM25MEAN2010L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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
  x = CNSST1517L3,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
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

# 11. Peru attending -----------------------------------------------
geom_peru$pm
data9 <- bi_class(
  geom_peru,
  x = CNSST1517L3,
  y = PM25,
  style = "quantile",
  dim = 3
)

newdata <- data9 %>%
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of Neighborhoods",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school (15-17 yo) →",
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

#L2 Biscale maps ===========================================================
# 3. Bogota attending -----------------------------------------------
library(janitor)

geom_L2_colombia <- geom_colombia %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25)
    ) %>% 
  mutate(
    school = school_tot/pop
  )

data1 <- bi_class(
  geom_L2_colombia,
  x = school,
  y = pm25,
  style = "quantile",
  dim = 3
)

newdata <- data1 %>%
  st_set_geometry(NULL) %>% 
  group_by(bi_class) %>% 
  summarise(total = n())

## Bar plot by categories:
##> categories: 1-3 | 2-3 | 3-3 
cat1 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-3",1:3))
cat1[nrow(cat1) + 1,1] = c("3-3")
cat1[is.na(cat1)] <- 0
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
cat2[nrow(cat2) + 1,1] = c("1-2")
cat2[is.na(cat2)] <- 0
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
cat3[nrow(cat3) + 1,1] = c("2-1")
cat3[is.na(cat3)] <- 0
b3 <- gg_barplot(x = cat3,nrow = 3)

##> Customization of bar plot
plot <- plot_grid(b1, b2, b3, ncol = 1) 
barras <- plot + 
  draw_plot_label(
    label = "N° of Districts",
    vjust = 0.1,
    x = 0.08,
    y = -0.25,
    angle = 90,
    size = 7
  ) +
  draw_plot_label(
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_colombia,
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
geom_L2_argentina <- geom_argentina %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25)
  ) %>% 
  mutate(
    school = school_tot/pop
  )

data2 <- bi_class(
  geom_L2_argentina,
  x = school,
  y = pm25,
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
cat1[nrow(cat1) + 1,1] = c("1-3")
cat1[is.na(cat1)] <- 0
b1 <- gg_barplot(x = cat1,nrow = 1)

##> categories: 1-2 | 2-2 | 3-2 
cat2 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-2",1:3))
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_argentina,
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
geom_L2_guatemala <- geom_guatemala %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data3 <- bi_class(
  geom_L2_guatemala,
  x = school,
  y = pm25,
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
cat2[nrow(cat2) + 1,1] = c("2-2")
cat2[is.na(cat2)] <- 0
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_guatemala,
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
geom_L2_panama <- geom_panama %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data4 <- bi_class(
  geom_L2_panama,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_panama,
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
geom_L2_costarica <- geom_costarica %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data4 <- bi_class(
  geom_L2_costarica,
  x = school,
  y = pm25,
  style = "quantile",
  dim = 3
)

data5 <- bi_class(
  geom_L2_costarica,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_costarica,
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
geom_L2_chile <- geom_chile %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data6 <- bi_class(
  geom_L2_chile,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_chile,
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
geom_L2_brasil <- geom_brasil %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data7 <- bi_class(
  geom_L2_brasil,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_brasil,
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
geom_L2_mexico <- geom_mexico %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data8 <- bi_class(
  geom_L2_mexico,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_mexico,
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

# 11. Peru attending -----------------------------------------------
geom_L2_peru <- geom_peru %>% 
  group_by(SALID2) %>% 
  summarise(
    school_tot = sum(CNSST1517L3*CNSPOPL3, na.rm = T),
    pop = sum(CNSPOPL3, na.rm = T),
    pm25 = mean(PM25, na.rm = T)
  ) %>% 
  mutate(
    school = school_tot/pop
  ) %>% 
  drop_na()

data8 <- bi_class(
  geom_L2_peru,
  x = school,
  y = pm25,
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
b2 <- gg_barplot(x = cat2,nrow = 2)

##> categories: 1-1 | 2-1 | 3-1 
cat3 <- newdata %>% 
  filter(bi_class %in% sprintf("%s-1",1:3))
b3 <- gg_barplot(x = cat3,nrow = 3)

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
    label = "% of attending school (15-17 yo) →",
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
    x = "% of attending school (15-17 yo) → ",
    y = "PM2.5 →"
  )

##> Cooking maps
basemap <- ggplot() + 
  geom_sf(
    data = geom_L2_peru,
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

end9 <- ggdraw() +
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

gg2 <- ggarrange(
  end1, end2, end3, end4,
  end5, end6, end7, end8, end9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala city (Guatemala)", "D. Panama city (Panama)", 
    "E. San Jose de Costa Rica (Costa Rica)", "F. Santiago de Chile (Chile)",
    "G. Sao Paolo (Brasil)", "H. Mexico city (Mexico", "I. Lima (Peru)"),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "city_map_attending.png",
  plot = gg2,
  width = 20.5,
  height = 16,
  bg = "white",
  dpi = 400  
)

ggsave(
  filename = "city_map_attending.pdf",
  plot = gg2,
  width = 20.5,
  height = 16,
  bg = "white"  
)

#map==================================
map_colombia <- merge(geom_L2_colombia, ng_regression_rank,
                      by.x = "SALID2", by.y = "SALID2")

m1 <- merge(geom_L2_colombia, ng_regression_percentage,
      by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m2 <- merge(geom_L2_argentina, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m3 <- merge(geom_L2_guatemala, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m4 <- merge(geom_L2_panama, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m5 <- merge(geom_L2_costarica, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m6 <- merge(geom_L2_chile, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m7 <- merge(geom_L2_brasil, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m8 <- merge(geom_L2_mexico, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

m9 <- merge(geom_L2_peru, ng_regression_percentage,
            by.x = "SALID2", by.y = "SALID2") %>% 
  ggplot() +
  geom_sf(aes(fill = categoria),
          show.legend = T,
          lwd = 0.5) +
  theme_bw() +
  theme(
    axis.text =  element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_innova("npr", reverse = T)

gg3 <- ggarrange(
  m1, m2, m3, m4, m5, m6, m7, m8, m9,
  labels = c(
    "A. Bogota (Colombia)", "B. Buenos Aires (Argentina)", 
    "C. Guatemala City (Guatemala)", "D. Panama City (Panama)", 
    "E. San Jose (Costa Rica)", "F. Santiago (Chile)",
    "G. São Paulo (Brazil)", "H. Mexico City (Mexico)", "I. Lima (Peru)"),
  nrow = 3,
  ncol = 3
)

ggsave(
  filename = "figs/city_map_L2_usama.pdf",
  plot = gg3,
  width = 20.5,
  height = 18,
  bg = "white",
  dpi = 400  
)
