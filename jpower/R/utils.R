jmvTheme <- function() {
    ggplot2::theme(
        text = ggplot2::element_text(size=16, colour='#333333'),
        plot.background = ggplot2::element_rect(fill='transparent', color=NA),
        panel.background = ggplot2::element_rect(fill='#E8E8E8'),
        plot.margin = ggplot2::margin(15, 15, 15, 15),
        axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, 0, 0, 0)),
        axis.text.y = ggplot2::element_text(margin=ggplot2::margin(0, 5, 0, 0)),
        axis.title.x = ggplot2::element_text(margin=ggplot2::margin(10, 0, 0, 0)),
        axis.title.y = ggplot2::element_text(margin=ggplot2::margin(0, 10, 0, 0)),
        plot.title = ggplot2::element_text(margin=ggplot2::margin(0, 0, 15, 0)),
        legend.background = ggplot2::element_rect("transparent"),
        legend.key = ggplot2::element_rect(fill='#E8E8E8'))
}


striped.segments = function(col1 = "black", col2 = "white", ...){
  segments(..., col = col1, lty = 1)
  segments(..., col = col2, lty = 2)
}

striped.lines = function(col1 = "black", col2 = "white", ...){
  lines(..., col = col1, lty = 1)
  lines(..., col = col2, lty = 2)
}

striped.Arrows = function(col1 = "black", col2 = "white", ...){
  segments(..., col = col1, lty = 1)
  shape::Arrows(..., col = col2, lty = 2)
}



