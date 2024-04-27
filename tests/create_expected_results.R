# # `file_size`
# df_test_file_size <- NULL
# for (units in c('bytes', 'kb', 'mb', 'gb')) {
#   df_test_file_size <- rbind(df_test_file_size, file_size(
#     system.file('extdata', package = 'dataR') %>%
#       list.files(recursive = T, full.names = T), units) %>%
#       mutate(filename = stringr::str_split_i(filename, '/inst/', -1L),
#              unit = units)
#   )
# }
# usethis::use_data(df_test_file_size, internal = T, overwrite = T) asdasdasdasdasda


devtools::load_all()
devtools::test()
devtools::check()

devtools::document()



