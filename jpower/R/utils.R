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


pwr.t2n.ratio = function(n_ratio = 1, d, sig.level, power, alternative){
  fn = Vectorize(function(n1){
    effN = n1 * n_ratio /  (1 + n_ratio)
    df = n1 * (1 + n_ratio) - 2
    ncp = sqrt(effN) * d
    if(alternative == "two.sided"){
      critt = qt(sig.level/2, df)
      pow = pt(critt, df, ncp) + 1 - pt(-critt, df, ncp)
    }else if(alternative == "less"){
      critt = qt(sig.level, df)
      pow = pt(critt, df, ncp) 
    }else if(alternative == "greater"){
      critt = qt(1 - sig.level, df)
      pow = 1 - pt(critt, df, ncp) 
    }else{
      stop("Invalid alternative.")
    }
    return(log(pow) - log(power))
  }, "n1")
  rt = uniroot(fn, c(ceiling( 3 / (1 + n_ratio) ), 1e+09))$root
  return(ceiling(rt))
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



