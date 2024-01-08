require(lubridate)


# make two plots on top of eachother with the legend sandwiched between
# this layout works well for wide barcharts
# @effect make a file with name filename
# @return a ggplot2 object
sandwich_barplot <- function(p1, p2, filename, width=8, height=9){
  legend <- cowplot::get_legend(p1)
  p1 <- p1 + ggplot2::theme(legend.position = "none")
  p2 <- p2 + ggplot2::theme(legend.position = "none")
  p12 <- cowplot::plot_grid(p1, legend, p2, ncol=1, rel_heights=c(1,0.2,1), labels=NULL) 
  ggplot2::ggsave(plot = p12, filename = filename, width = width, height = height, dpi = 300)
  p12
}

date_to_quarter <- function(datey){
  year_str = sub("20", "", year(datey %m+% months(3)))
  quarter_str = (((month(datey) + 2) %% 12)) %/% 3 + 1
  paste0(year_str, "Q", quarter_str)
}

increment_quarter <- function(quarter){
  y = as.integer(sub("Q.*", "", quarter))
  q = as.integer(sub(".*Q", "", quarter))
  if(all(q == 4)){
    y <- y + 1
    q <- 1
  } else {
    q <- q + 1
  }
  paste0(y, "Q", q)
}

labelFromQuarters <- function(quarters){
  quarters <- sort(quarters)
  if(length(quarters) == 1){
    quarters
  } else {
    paste0("[", quarters[1], " to ", rev(quarters)[1], "]") 
  }
}

orderConstellations <- function(d){
  clvl <- levels(d$Constellation)
  d$Constellation <- factor(d$Constellation, levels=c(
    sort(clvl[!grepl("V", clvl)]),
    sort(clvl[ grepl("V", clvl)])
  ))
  d
}

# ===== prepConstellationData
prepGConstData <- function(data){
  cdata <- data %>%
    dplyr::mutate( # create a HAtype and NAtype column
      H = dplyr::case_when(!is.na(H1) ~ H1,
                           !is.na(H3) ~ H3),
      N = dplyr::case_when(!is.na(N1) ~ N1,
                           !is.na(N2) ~ N2)
    )

  # Get counts
  # ===== Get the counts
  (tots <- nrow(cdata))

  hdata <- cdata %>%
    dplyr::group_by(Subtype, H, N, Constellation) %>%
    dplyr::summarise(
      n = (dplyr::n()/tots*100) %>% round(., digits=1),
      nn = n
    ) %>%
    dplyr::select(Constellation, Subtype, H, N, n, nn) %>%
    dplyr::ungroup(.)

  totdata <- dplyr::group_by(cdata, Constellation) %>%
    dplyr::summarise(
      Subtype="total",
      H="total",
      N="total",
      n = (dplyr::n()/tots*100) %>% round(., digits=1),
      nn = n/3,
      nn = dplyr::case_when(nn<0.5 ~ 0.5,
                            1==1 ~ nn),
      labels="total"
    ) %>%
    dplyr::select(Constellation, Subtype, H, N, n, nn) %>%
    dplyr::ungroup()

  subtype_order <- c("total", "H1N1", "H1N2", "H3N1", "H3N2", "mixed")

  rbind(hdata, totdata) %>%
    dplyr::mutate(
      labels=dplyr::case_when(H=="total" ~ "total",
                              1==1 ~ paste(H, N, sep=".")),
      Subtype=factor(Subtype, subtype_order),
      Constellation=factor(Constellation)
    ) %>% orderConstellations
}

make_hhdata <- function(x, quarters){
  subset(x, Collection_Q %in% quarters) %>%
  subset(Subtype != "mixed") %>%            # drop mixed isolates
  subset(!is.na(Subtype)) %>%               # drop anything missing a subtype
  subset(!(is.na(H1) & is.na(H3))) %>%      # must have an H1 or H3 classified
  subset(!(is.na(N1) & is.na(N2))) %>%      # must have an N1 or N2 classified
  subset(grepl("^[A-Z][A-Z]$", State)) %>%  # drop isolates without state information
  subset(!is.na(Constellation)) %>%         # drop missing constellation isolates
  subset(!grepl("-", Constellation))        # drop missing constellation isolates
}


# ===== Function: hhdata ---> gene constellation plot
plotConstellation <- function(data){

  # Get counts
  # ===== Get the counts
  (tots <- nrow(data))

  df <- prepGConstData(data)

  time_period <- octoflushow::dates_to_str(data$Date)
  xlabel <- "HA and NA Phylogenetic Clade Pairs"
  ylabel <- "Gene constellations"
  title <- paste("Gene Constellations for ", time_period, " (n=", tots, ")", sep = "")

  df$n <- ifelse(df$n == 0, "", df$n)

  p <- ggplot2::ggplot(df, ggplot2::aes(x = labels, y = Constellation, fill = log(nn))) +
    ggplot2::geom_tile(color = "black") +
    ggplot2::scale_fill_gradient2(
      low = "white", mid = "#43a2ca",
      high = "#0868ac", na.value = "white",
      midpoint = mean(log(df$nn))
    ) +
    ggplot2::geom_text(ggplot2::aes(label = n), size=2.5) +
    ggplot2::labs(
      title = title,
      x = xlabel,
      y = ylabel
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      plot.margin = grid::unit(rep(0, 4), "lines"),
      axis.text.y = ggplot2::element_text(family = "mono")
    ) +
    ggplot2::facet_grid(. ~ Subtype, space = "free", scales = "free")
  # Add background lines
  for (yint in c(-1:nlevels(df$Constellation)+1)) { # number of unique constellations
    p <- p + ggplot2::geom_hline(yintercept = yint + 0.5, size = 0.25, color = "gray")
  }
  for (xint in c(-1:length(unique(df$labels))+1)) { # number of unique ha/na pairings
    p <- p + ggplot2::geom_vline(xintercept = xint + 0.5, size = 0.25, color = "gray")
  }
  return(p)
}

### heatmaps

# ===== Heatmap plot function
heatmap_HANA <- function(df, dates, text=TRUE, totals=FALSE, font_size=3) {
  df <- dplyr::select(df, H_Type, variable, percent)
  if(totals){
    x_totals <- dplyr::group_by(df, H_Type) %>% dplyr::summarize(variable = "Total", percent = sum(percent, na.rm=TRUE)) %>% dplyr::ungroup()
    y_totals <- dplyr::group_by(df, variable) %>% dplyr::summarize(H_Type = "Total", percent = sum(percent, na.rm=TRUE)) %>% dplyr::ungroup()
    df <- rbind(df, x_totals, y_totals)
    df$variable <- droplevels(df$variable)
    df$H_Type <- droplevels(df$H_Type)
    df$H_Type <- factor(df$H_Type, levels=rev(levels(df$H_Type)))
    nX = nlevels(df$variable)
    nY = nlevels(df$H_Type)
  }

  mid.value <- (min(df$percent) + max(df$percent)) / 2
  title <- paste("Percentage of HA and NA combinations -", octoflushow::dates_to_str(dates), sep = " ")
  df$label <- ifelse(df$percent == 0, "", round(df$percent, 1)) 

  p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = H_Type)) +
    ggplot2::geom_tile(ggplot2::aes(fill = percent), colour = "black") +
    ggplot2::scale_fill_gradient2(low = "white", mid = "#43a2ca", high = "#0868ac",
                                  space = "Lab", midpoint = mid.value, guide = "colorbar") +
    ggplot2::theme_bw() +
    ggplot2::labs(y="HA clade", x="NA clade", title=title) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, size = 8, hjust = 1, vjust = 0.5))

  if(totals){
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = nX - 0.5, xend = nX - 0.5, y = 0.5, yend = nY + 0.5), color="black", size=1) +
      ggplot2::geom_segment(ggplot2::aes(x = 0.5, xend = nX + 0.5, y = 1.5, yend = 1.5), color="black", size=1)
  }

  if(text==TRUE){
    p <- p + ggplot2::geom_text(ggplot2::aes(label = label), size = font_size)
  }
  return(p)
}

hana_counts <- function(df){
  df %>%
    dplyr::select(Barcode, H_Type, N_Type) %>% # Barcode, H1.clade, N1.clade (or H3, and N2)
    reshape2::dcast(H_Type ~ N_Type, fun.aggregate = length, value.var = "Barcode") %>% # Count number of HA and NA pairs
    reshape2::melt(id = "H_Type") %>% # Prep for ggplot format
    dplyr::mutate(percent = round(value * 100 / sum(value), digits = 1)) %>% # get 2 digit percentage counts
    dplyr::arrange(desc(percent)) %>%
    dplyr::mutate(
      H_Type = factor(H_Type, levels = ha_order),
      variable = factor(variable, levels = na_order)
      )
}

# Order the labels
ha_order <- c(
  "H1.alpha",
  "H1.beta",
  "H1.gamma",
  "H1.gamma2",
  "H1.gamma2-beta-like",
  "H1.pandemic",
  "H1.delta1",
  "H1.delta1a",
  "H1.delta1b",
  "H1.delta2",
  "H3.I",
  "H3.II",
  "H3.III",
  "H3.IV",
  "H3.IV-A",
  "H3.IV-B",
  "H3.IV-C",
  "H3.IV-D",
  "H3.IV-E",
  "H3.IV-F",
  "H3.IV-G",
  "H3.IV-H",
  "H3.IV-I",
  "H3.IV-J",
  "H3.IV-K",
  "H3.2010.1",
  "H3.2010.2",
  "H3.other-human"
)

na_order <- c(
  "N1.Classical",
  "N1.Pandemic",
  "N1.MN99",
  "N2.1998",
  "N2.2002",
  "N2.2016",
  "N2.TX98",
  "N2.Human-like"
)
