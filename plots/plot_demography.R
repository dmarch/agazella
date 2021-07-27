# plot demographic results


source("setup.R")

# set output directory
outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import dive data
demo_xls <- paste0(input_data, "/cardona/DadesDemografia.xlsx")
data <- read_excel(demo_xls)

# order factor levels
data$class <- factor(data$class, levels = c("juvenile", "subadult", "adult"))

# plot
p <- ggplot(data, aes(x=location, y=percentage_avg, fill=class)) + 
  # add error bar
  geom_errorbar(aes(ymin=percentage_avg-percentage_sd,
                    ymax=percentage_avg+percentage_sd),
                width=.2, position=position_dodge(.9)) +
  # add bar
  geom_bar(stat="identity", position=position_dodge()) +
  # define colors
  scale_fill_brewer(palette="Blues") +
  # y axis
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  # add labels
  xlab("Haul-out location") + ylab("Percentage (%)") +
  # theme
  theme_article(base_size=14) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.92),
        panel.grid.major.y = element_line( size=.1, color="grey50"),
        axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)))
  

# export plot
p_png <- paste0(outdir,"/demography.png")
ggsave(p_png, p, width=16, height=14, units="cm", dpi=300)

  
