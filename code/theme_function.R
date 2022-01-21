# Function for customising ggplot theme for R project
# Hannah Buckland
# 21/01/2022


theme_LPS <- function() {
  theme_bw() +
    theme(
      aspect.ratio = 0.7,
      legend.position = "right",
      axis.text = element_text(colour = "black", size = 12),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}

