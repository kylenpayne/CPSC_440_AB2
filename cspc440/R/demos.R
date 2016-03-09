# demos.R
plot_reject_region <- function(){
  Z <- rnorm(100000)
  dt <- data.table::data.table(Z)
  #  generate kdf
  gg <- dt[,list(x=density(Z)$x, y=density(Z)$y)]
  #  calculate quantiles
  q1 <- qnorm(0.025)
  q2 <- qnorm(0.975)
  ggplot2::ggplot(dt) + ggplot2::stat_density(ggplot2::aes(x=Z))  +
    ggplot2::geom_ribbon(data=subset(gg, x<q1),
    ggplot2::aes(x=x,ymax=y),ymin=0,fill="red", alpha=0.5) +
    ggplot2::geom_ribbon(data=subset(gg, x > q2),
                ggplot2::aes(x=x,ymax=y),ymin=0,fill="red", alpha=0.5) +
    ggplot2::annotate("text", x = -2.5, y = 0.075, label = c("Rejection"), parse=TRUE) +
    ggplot2::annotate("text", x = 2.5, y = 0.075, label = c("Rejection"), parse=TRUE) +
    ggplot2::annotate("text", x = 2.5, y = .2, label = c("Fail to Reject Region")) +
    ggplot2::geom_segment(ggplot2::aes(x = -2.5, y = .065, xend = -2.5, yend = .03), arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
    ggplot2::geom_segment(ggplot2::aes(x = 2.5, y = .065, xend = 2.5, yend = .03), arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
    ggplot2::geom_segment(ggplot2::aes(x=q1, y=0.025, xend=q2, yend=0.025), color="white") +
    ggplot2::geom_segment(ggplot2::aes(x=q1, y=0.025, xend=-5, yend=0.025), arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
    ggplot2::geom_segment(ggplot2::aes(x=q2, y=0.025, xend=5, yend=0.025), arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
    ggplot2::geom_segment(ggplot2::aes(x = 2.5, y = .18, xend = 0, yend = .025), arrow = ggplot2::arrow(length = ggplot2::unit(0.5, "cm"))) +
    ggplot2::theme_bw()
    print("This takes a minute to output a plot, so here's a joke: It is proven that the celebration of birthdays is healthy. Statistics show that those people who celebrate the most birthdays become the oldest.")
}
