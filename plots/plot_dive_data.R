# plot maximum depths


source("setup.R")

# import data
depth_xls <- paste0(input_data, "/cardona/DadesFiguraProfundidadmaximadiaria.xlsx")
data <- read_excel(depth_xls, sheet = "dive")

# import seal icon
library(magick)

img <- image_read(paste0(input_data, "/img/shutterstock_1802091355.png")) %>%
  image_transparent(color="white", fuzz = 0) %>%
  image_colorize(opacity=90, color="blue") %>%
  image_scale("100")

print(img)

r <- as.raster(img)
plot(r)

# reorder categories for plots
data$month <- factor(data$month, levels = c("March", "April", "May"))
data$tagID <- factor(data$tagID, levels = c("64555", "64537", "64528", "64538"))

library(RColorBrewer)
# plot 
ggplot(data = data) +
  geom_histogram(aes(maxdepth, fill=tagID),
                 binwidth = 10,
                 color="white") +
  scale_fill_manual(name = "Tag",
                    values = brewer.pal(4, 'Set1')) +
  coord_flip() +
  scale_x_continuous(trans = "reverse",
                     minor_breaks = seq(0, 200, by = 10),
                     breaks = seq(0, 200, by = 50),
                     guide = guide_prism_minor()) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                     breaks = seq(0, 12, by = 2)) +
  facet_grid(tagID~month) +
  theme_article(base_size=14) +
  xlab("Maximum dive depth (m)") + ylab("Number of dives") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    strip.text.y.right = element_blank()
    #strip.text.y.right = element_text(angle = 0)
  )


grid.raster(r, x=0.9, y=0.8, width=0.05)

