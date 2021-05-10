# plot trip timings


source("setup.R")

# set output directory
outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# import dive data
xls <- paste0(input_data, "/cardona/DadesFiguraIniciFinalViatge.xlsx")
data <- read_excel(xls)

# prepare data
data <- data %>%
  # extract hour of the day from Excel dates
  mutate(Departure = hour(inicio),
         Arrival = hour(final),
         date = as.Date(UTC_Date)) %>%
  # select variables
  dplyr::select(Tag_ID, date, Departure, Arrival)




# transform from wide to long format
data_long <- melt(data, id.vars=c("Tag_ID", "date"))



p <- ggplot(data = data_long) +
  # histogram
  geom_histogram(aes(value, fill=variable),
                 binwidth = 1,
                 color="white") +
  # histogram color
  #scale_fill_manual(name = "Trip",
  #                  values = brewer.pal(4, 'Set1')) +
  # histogram axis
  scale_x_continuous(breaks = seq(0, 23, by = 2),
                     expand = c(0,0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
   xlab("Hour of the day") + ylab("Number of trips") +
  # facet grid
  facet_wrap(variable~., ncol=1) +
  # theme settings
  theme_article(base_size=14) +
  theme(
    legend.position = "none",
    strip.text.y.right = element_blank(),
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    axis.title.x = element_text(margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0))
  )

  
# export plot
p_png <- paste0(outdir,"/tripTiming.png")
ggsave(p_png, p, width=14, height=12, units="cm", dpi=300)



## plot per date

p <- ggplot(data = data_long) +
  # histogram
  geom_point(aes(x = date, y = value, colour=variable)) +
  scale_y_continuous(limits=c(0,23),
    breaks = seq(0, 23, by = 2),
                     expand = c(0.02,0)) +
  # theme settings
  theme_article(base_size=14) +
  theme(legend.position = "none")




