library(tidyverse)
library(robotoolbox)

###
kobo_setup(url = Sys.getenv("KOBOTOOLBOX_PROD_URL"),
           token = Sys.getenv("KOBOTOOLBOX_PROD_TOKEN"))

###
kobo_settings()

###
# asset_list1 <- kobo_asset_list()

###
uid1 <- "aEwTYNcU76UvLgiY89rPWm"
asset1 <- kobo_asset(uid1)
asset1

###
uid2 <- "aYuTZn9vegi3Z49MXwKjep"
asset2 <- kobo_asset(uid2)
asset2

###
uid3 <- "aANhxwX9S6BCsiYMgQj9kV"
asset3 <- kobo_asset(uid3)
asset3

###
data1 <- kobo_data(asset1)

###
data2_en <- kobo_data(asset2, lang = "English (en)")
data2_fr <- kobo_data(asset2, lang = "Francais (fr)")
data2_ar <- kobo_data(asset2, lang = "Arabic (ar)")
data2_default <- kobo_data(asset2)

###
data3 <- kobo_data(asset3)

###
usethis::use_data(asset1, overwrite = TRUE)
usethis::use_data(asset2, overwrite = TRUE)
usethis::use_data(asset3, overwrite = TRUE)
#usethis::use_data(asset_list1, overwrite = TRUE)
usethis::use_data(data1, overwrite = TRUE)
usethis::use_data(data2_en, overwrite = TRUE)
usethis::use_data(data2_fr, overwrite = TRUE)
usethis::use_data(data2_ar, overwrite = TRUE)
usethis::use_data(data2_default, overwrite = TRUE)
usethis::use_data(data3, overwrite = TRUE)
