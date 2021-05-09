# plot maximum depths


source("setup.R")

outdir <- paste(output_data, "fig", sep="/")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)


# import data
depth_xls <- paste0(input_data, "/cardona/DadesFiguraProfundidadmaximadiaria.xlsx")
data <- read_excel(depth_xls, sheet = "dive")

# reorder categories for plots
data$month <- factor(data$month, levels = c("March", "April", "May"))
data$tagID <- factor(data$tagID, levels = c("64555", "64537", "64528", "64538"))

library(RColorBrewer)
library(ggimage)

# create data.frame for labels
sealUrl <- "http://phylopic.org/assets/images/submissions/7ce9ff63-7eec-4fc3-8f37-5f66f9b924a9.original.png"

labels <- data.frame(month = "May",
                     tagID = unique(data$tagID),
                     label = paste0("#", unique(data$tagID)),
                     image = sealUrl)
labels$month <- factor(labels$month, levels = c("March", "April", "May"))


# plot 
p <- ggplot(data = data) +
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
  geom_text(data=labels, aes(x=175, y=10.5, label=label, colour = tagID)) +
  geom_image(data=labels, aes(x=140, y=11, image=image, colour = tagID), size=0.3, asp=1.2) +
  scale_colour_brewer(palette = "Set1") +
  theme_article(base_size=14) +
  xlab("Maximum dive depth (m)") + ylab("Number of days") +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_line( size=.1, color="grey50"),
    strip.text.y.right = element_blank()
    #strip.text.y.right = element_text(angle = 0)
  )


# export plot
p_png <- paste0(outdir,"/maxDiveDepth.png")
ggsave(p_png, p, width=14, height=14, units="cm", dpi=300)



# import seal icon
# library(magick)
# img <- image_read(paste0(input_data, "/img/shutterstock_1802091355.png")) %>%
#   image_transparent(color="white", fuzz = 0) %>%
#   image_colorize(opacity=90, color=brewer.pal(4, 'Set1')[1]) %>%
#   image_scale("100")

data %>%
  dplyr::group_by(tagID, month) %>%
  dplyr::summarize(n = n())
