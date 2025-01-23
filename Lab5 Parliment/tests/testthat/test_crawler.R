library(testthat)
resource <-"https://www.riksdagen.se/en/members-and-parties/members/"
member_info_df <- tibble(
  Name = character(),
  Party = character(),
  Constituency = character(),
  Birthyear = character(),
  Address = character(),
  Email = character(),
  Telephone = character(),
  Title = character(),
  Age = numeric()
)

test_that("check the basic info of party member", {
  url <- "https://www.riksdagen.se/en/members-and-parties/member/alexandra-anstrell_8310e472-996b-4c8a-bce5-4d09fb20a66a/"
  info_df <- get_member_detail_info(url, member_info_df)
  expect_equal(info_df$Birthyear, "1974")
  expect_equal(info_df$Age, 50)
  expect_equal(info_df$Party, "The Moderate Party")
  expect_equal(info_df$Title, "Civilekonom.")
  expect_equal(info_df$Name, "Alexandra Anstrell  (Mod)")
  expect_true(all(c("Name","Party","Birthyear","Title","Telephone","Age","Constituency") %in% colnames(info_df)))

})


test_that("get_parties returns correct party names and abbreviations", {
  expected_parties <- list(
    'The Social Democratic Party'='socdem',
    'The Sweden Democrats'='swedem',
    'The Moderate Party'='mod',
    'The Left Party'='lft',
    'The Centre Party'='cen',
    'The Christian Democrats'='chrdem',
    'The Green Party'='grn',
    'The Liberal Party'='lib'
  )
  expect_equal(get_parties(), expected_parties)
})

test_that("get_name_return_detail_url_by_party extracts URLs correctly", {
  # 使用一个测试 HTML 文档（需要根据实际情况修改）
  test_html <- read_html("https://www.riksdagen.se/en/members-and-parties/members/?parti=socdem")
  result <- get_name_return_detail_url_by_party(test_html, "socdem")
  expect_type(class(result[[1]]), "character")
  expect_true(length(result[[1]]) > 0)
  expect_error(get_name_return_detail_url_by_party(test_html, "invalid_party"))
})



