# https://stackoverflow.com/questions/71686257/how-do-i-make-ggrepel-move-some-labels-outside-us-map-boundaries

college_layers <- function(d, label, alLeft = TRUE) {

  xlimz <- if (all(d$x > 0)) {c(2.5e6, NA)} else {c(NA, -2e6)}



  geom_label_repel(
    data = d,
    aes(x, y, label = label),
    xlim = xlimz,
    ylim = c(-Inf, Inf),
    size = 3,
    force = 20,
    box.padding = 0.3,
    max.overlaps = 30,
    point.padding = NA,
    alpha = 1.0,
    min.segment.length = 0.1,
    segment.color = "black",
    segment.size = 1,
    seed = 1000,
    hjust=if(alLeft){0}else{1}
  )
}
