# =============================================================================
#      Filename:  part_full.R
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
#  Deficiencies:  [If you know of any problems with the code, provide
#                 details here, otherwise clearly state that you know
#                 of no unsatisfied requirements and no logic errors.]
# =============================================================================


part_full_theta <- function(y, d) { 2*acos(1-2*y/d) }

part_full_area <- function(y,d) {
  theta <- part_full_theta(y, d)

  (d^2)/8*(theta-sin(theta))
}

part_full_perimeter <- function(y,d) { 0.5*part_full_theta(y,d)*d }

part_full_hyd_radius <- function(y,d) { part_full_area(y,d)/part_full_perimeter(y,d) }

part_full_flow <- function(y_in=8, d_in=8, n=0.013, s=0.4/100) {
  a <- part_full_area(y_in, d_in)
  r <- part_full_hyd_radius(y_in, d_in)

  448.8*1.486/n*(a/12^2)*(r/12)^(2/3)*sqrt(s)

}

part_full_velocity <- function(y_in=8, d_in=8, n=0.013, s=0.4/100) {
  a <- part_full_area(y_in, d_in)
  q <- part_full_flow(y_in, d_in, n, s)

  (q/448.8)/(a/144)

}



# Enfinger, K.L. PE and Schutzbach, J.S. (2020)
# "Scattergraph Principles and Practice: Camp’s Varying Roughness Coefficient Applied to the Manning Equation".
# ADS Environmental Services.
# https://www.adsenv.com/sites/default/files/whitepapers/SG-03-Paper-2020-07-22.pdf

part_full_variable_n <- function(y, d) {1.04 + 2.30*(y/d) - 6.86*(y/d)^2 + 7.79*(y/d)^3 - 3.27*(y/d)^4
}







