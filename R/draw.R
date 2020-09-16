# =============================================================================
#      Filename:  draw.R
#        Author:  D Craig Jones <jones.dCraig@gmail.com>
#    Maintainer:  D Craig Jones <jones.dCraig@gmail.com>
#
#   Description:  [Describe the program's goal, IN DETAIL.]
#
# -----------------------------------------------------------------------------
#     Constants:
#
#    Parameters:
#
# -----------------------------------------------------------------------------
#
#  Deficiencies:
#
#         To-Do:
#                 • Add "srt" parameter to text call to rotate text.
#                 • Add user-select isoflow line.
#                 • Add label to Manning Equation Line "DM"; Lanfear-Coll "LC"; and Shu.. "SS"
#                 • Add Function for "SS", currently in sandbox
# =============================================================================


#https://www.adsenv.com/sites/default/files/whitepapers/sg%2009%20paper%202010-06-11.pdf
#Enfinger, K.L. PE and Mitchell, P.S. PE (2010) "Scattergraph Principles and Practice: Evaluating Self-Cleansing in Existing Sewers Using the Tractive Force Method". ADS Environmental Services.

draw_self_cleaning <- function(d_in, n, p_mm=1, linecolor="grey50", linetype=4, y_label=d_in*0.75) {
  y <- seq(0, d_in, length.out=1000)

  tau <- 0.0181*p_mm^0.277
  gamma <- 62.3 #lb/ft³

  v_fps <- 1.486/n*(part_full_hyd_radius(y,d_in)/12)^(1/6)*sqrt(tau/gamma)

  graphics::lines(v_fps, y, col=linecolor, lty=linetype, lwd=2)

  v_label <- 1.486/n*(part_full_hyd_radius(y_label,d_in)/12)^(1/6)*sqrt(tau/gamma)
  graphics::points(v_label, y_label, cex=3, col="white", pch=15)
  graphics::text(v_label, y_label, expression(tau[c]), col = linecolor, pos = 1, font = 2, cex = 1, offset=-0.25)
}

draw_scattergraph <- function(D=8, max.v=5, step.v=1, max.d=16, step.d=2) {
  SCATTERGRAPH_PARAMS <<- NULL

  padding.v <- 0.15

  # Draw Scattergraph Template
  plot(0,0
       , xlim=c(-max.v/5,max.v*(1+padding.v))
       , ylim=c(0,max.d)
       , type="n"
       , axes=FALSE
       , xaxs="i"
       , yaxs="i"
       , xlab = "flow velocity (ft/sec)"
       , ylab = "flow depth (in)"
  )

  # Draw Scattergraph Gridlines
  for (i in seq(0,max.d,step.d)) {graphics::lines(c(0,max.v), c(i,i), col="grey75")}
  for (i in seq(0,max.v,step.v)) {graphics::lines(c(i,i), c(0,max.d), col="grey75")}


  # Calculate Pipe Cut-Away Section
  d <- D/2
  r <- d*D/3

  k <- d/2
  y <- seq(0,d, length.out=100)

  h <- -sqrt(r^2-d^2/4)
  cutaway.x <- sqrt(r^2-(y-k)^2)+h

  # Draw Pipe Cut-Away Sections
  graphics::lines(cutaway.x,y+d, lwd=2)
  graphics::lines(-cutaway.x,y+d, lwd=2)
  graphics::lines(-cutaway.x,y, lwd=2)
  graphics::lines(cutaway.x+max.v,y, lwd=2)
  graphics::lines(-cutaway.x+max.v,y, lwd=2)
  graphics::lines(cutaway.x+max.v,y+d, lwd=2)

  # Calculate Pipe Cross-section
  h <- 0
  k <- D/2
  b <- D/2
  a <- -max.v/5
  y <- seq(0,D,length.out=100)

  circle.x <- sqrt((1-(y-k)^2/b^2)*a^2)+h

  # Draw Pipe Cross-section
  graphics::lines(-circle.x,y, lwd=2)

  # Draw Pipeline

  graphics::lines(c(0,max.v), c(D,D), lwd=2)
  graphics::lines(c(0,max.v), c(0,0), lwd=2)

  # Label Axes
  graphics::axis(2, at=seq(0,max.d, step.d))
  graphics::axis(1, at=seq(0,max.v,step.v))
}


# Enfinger, K.L. PE and Stevens, P.L. PE (2006)
#"Scattergraph Principles and Practice: Practical Application of the Froude Number to Flow Monitor Data".
# ADS Environmental Services.
# https://www.adsenv.com/sites/default/files/whitepapers/sg%2005%20paper%202008-07-31.pdf


draw_isofroude <- function(d_in=8, Fr=1, max.v=5, linecolor="grey50", linetype=6, v_label=max.v*.85) {
  d_ft <- d_in/12
  y_ft <- seq(0,d_ft,length.out=1000)

  theta <- 2*acos(1-2*y_ft/d_ft)
  area <- d_ft^2/8*(theta-sin(theta))
  width <- sin(theta/2)*d_ft

  v_fps <- Fr*sqrt(32.2*area/width)

  tmp <- data.frame(v_fps=v_fps, y_in=y_ft*12)
  tmp <- tmp[tmp$v_fps<=max.v,]

  graphics::lines(tmp$v_fps, tmp$y_in, col=linecolor, lty=linetype, lwd=2)

  text_label <- paste0("Fr=", Fr)
  y_label <- stats::approx(tmp$v_fps, tmp$y_in, xout=v_label)$y
  graphics::points(v_label, y_label, cex=5, col="white", pch=15)
  graphics::text(v_label, y_label, text_label, col = linecolor, pos = 1, font = 3, cex = 0.75, offset=-0.25)

}

draw_isoflow <- function(d=8,q_gpm, max.v=5, max.d=16, linecolor="grey25", linetype=1) {
  y <- seq(0,max.d,length.out=1000)

  area <- rep(3.1416*d^2/4, length(y))

  for (i in 1:length(y)) {
    if (y[i]<=d) {
      theta <- 2*acos(1-2*y[i]/d)
      area[i] <- d^2/8*(theta-sin(theta))
    }
  }

  v_fps <- (q_gpm/448.8)/(area/144)

  v_fps[v_fps>max.v]=NA

  graphics::lines(v_fps, y, col=linecolor, lty=linetype, lwd=1)

  text_label <- paste0(prettyNum(q_gpm, big.mark=","), " gpm")
  graphics::text(0.408*q_gpm/d^2, d+1, text_label, pos=4, cex=0.80, col=linecolor)
}

draw_manning <- function(d=8, n=0.013, s=0.4/100, linecolor="black", linetype=2) {
  y <- seq(0,d,length.out=100)

  theta <- 2*acos(1-2*y/d)
  area <- d^2/8*(theta-sin(theta))
  perimeter <- 0.5*theta*d
  r <- area/perimeter

  q_gpm <- 448.8*1.49/n*(area/12^2)*(r/12)^(2/3)*sqrt(s)
  v_fps <- (q_gpm/448.8)/(area/144)

  graphics::lines(v_fps, y, col=linecolor, lty=linetype, lwd=2)
}

draw_scattergraph(24, 5, 1, 30, 6)
draw_manning(24, 0.01,0.08/100)
draw_isofroude(24, 0.7)
draw_self_cleaning(24, 0.01)
draw_isoflow(24,2000,5,30)
