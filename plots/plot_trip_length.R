# plot maximum depths


source("setup.R")

# set output directory
outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import dive data
xls <- paste0(input_data, "/cardona/DadesFiguraDuradaviarge.xlsx")
data <- read_excel(xls, sheet = "Full1")


# plot 
p <- ggplot(data = data) +
  # scatterplot
  geom_point(aes(x = luzhoras, y = viajedias), color="skyblue3") +
  # histogram axis
  scale_x_continuous(breaks = seq(4, 16, by = 2),
                     limits = c(4,16),
                     #expand = c(0, 0),
                     labels = seq(4, 16, by = 2)) +
  scale_y_continuous(breaks = pretty(data$viajedias, n=10),
                     limits = c(0, 40)#,
                     #expand = expansion(mult = c(0.01, 0))
                     ) +
  xlab("Day length (h)") + ylab("Trip length (days)") +
  # theme settings
  theme_article(base_size=14) +
  theme(
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
  )

  
# export plot
p_png <- paste0(outdir,"/tripLength.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)


