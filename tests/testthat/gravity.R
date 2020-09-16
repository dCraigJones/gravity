context("part_full")

# https://www.engineersedge.com/fluid_flow/partially_full_pipe_flow_calculation/partiallyfullpipeflow_calculation.htm

test_that("dimensional parameters are correct" , {
  expect_equal(round(part_full_area(15,36)/144,2), 2.79)
  expect_equal(round(part_full_perimeter(15,36)/12,1), 4.2)
  expect_equal(round(part_full_hyd_radius(15,36)/12,2), 0.66)
  expect_equal(round(part_full_area(2,8)/144,2), 0.07)
  expect_equal(round(part_full_perimeter(2,8)/12,1), 0.7)
  expect_equal(round(part_full_hyd_radius(2,8)/12,2), 0.10)
  expect_equal(round(part_full_area(20,24)/144,2), 2.8)
  expect_equal(round(part_full_perimeter(20,24)/12,1), 4.6)
  expect_equal(round(part_full_hyd_radius(20,24)/12,2), 0.61)
})
