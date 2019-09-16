multipanel_theme <- function(my_ggplot) {
  theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      strip.text.x = element_text(size = 7, face = "bold"),
      strip.text.y = element_text(size = 7, face = "bold"),
      strip.background = element_rect(colour = "gray80", fill = NA)
    )
}

singlepanel_theme <- function(my_ggplot) {
  theme_minimal(base_size = 10) +
    theme(
      plot.subtitle = element_text(color = "#666666"),
      plot.caption = element_text(color = "#AAAAAA", size = 10)
    )
}
