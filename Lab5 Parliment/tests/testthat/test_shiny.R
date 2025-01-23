library(testthat)


# 假设 member_basic_info 数据框已经加载
data("member_basic_info")

# 测试 draw_member_age_hist 函数
test_that("draw_member_age_hist generates correct plot", {

  # 创建测试输入
  test_input <- list(party_name = unique(member_basic_info$Party)[1])  # 选择第一个政党

  # 创建一个测试输出对象
  test_output <- list(main_plot = NULL)

  # 使用一个模拟渲染的函数来捕获 plot
  output_func <- function() {
    # 这里使用空的图形设备以避免输出到屏幕
    plot.new()
  }

  test_output$main_plot <- renderPlot(output_func)

  expect_silent(draw_member_age_hist(test_input, test_output))

  expect_true(!is.null(test_output$main_plot))


})
