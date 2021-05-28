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
  mutate(
    date = as.Date(UTC_Date),
    Departure = hour(inicio),
         Arrival = hour(final)) %>%
  # select variables
  dplyr::select(Tag_ID, date, Departure, Arrival)




# transform from wide to long format
data_long <- melt(data, id.vars=c("Tag_ID", "date"))

# prepare data
data_long <- data_long %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(time = paste(date, value) %>% parse_date_time("Ymd H"))


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


all_days <- seq.POSIXt(from = min(data_long$time), to = max(data_long$time), by = "days")
crds <- matrix(c(-60, -65), nrow=1)

sunrise <- sunriset(crds, all_days, direction=c("sunrise"), POSIXct.out=TRUE)
sunset <- sunriset(crds, all_days, direction=c("sunset"), POSIXct.out=TRUE)

ephemeris <- getSunlightTimes(date = as.Date(all_days), lat = -65, lon = -60)
ephemeris$sunrise2 <- as.numeric(format(ephemeris$sunrise, "%H%M"))
ephemeris$sunset2 = as.numeric(format(ephemeris$sunset, "%H%M"))


data_long$time2 <- as.numeric(format(data_long$time, "%H%M"))

# for graph #1 y-axis
time_format <- function(hrmn) substr(sprintf("%04d", hrmn),1,2)


p <- ggplot(data = data_long, aes(x = date)) +
  geom_ribbon(data = ephemeris,
              aes(x = date, ymin=sunrise2, ymax=sunset2),
              fill="#ffeda0") +

  # histogram
  geom_point(aes(y = time2, colour=variable)) +
  scale_y_continuous(labels=time_format, limits=c(0,2400), breaks=seq(0, 2400, 200), expand=c(0,0))

  # scale_y_continuous(limits=c(0,23),
  #   breaks = seq(0, 23, by = 2),
  #                    expand = c(0.02,0)) +
  # theme settings
  theme_article(base_size=14) +
  theme(legend.position = "none")




