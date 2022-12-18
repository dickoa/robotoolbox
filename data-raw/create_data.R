library(tidyverse)
library(robotoolbox)

###
kobo_setup(url = Sys.getenv("KOBOTOOLBOX_PROD_URL"),
           token = Sys.getenv("KOBOTOOLBOX_PROD_TOKEN"))

###
kobo_settings()

###
# asset_list <- kobo_asset_list()

###
## uid1 <- "a7XzRuPFn9j5WkT2mR6wbg"
## asset1 <- kobo_asset(uid1)
## asset1

###
uid_ml <- "aYuTZn9vegi3Z49MXwKjep"
asset_ml <- kobo_asset(uid_ml)
asset_ml

###
uid_rg <- "aANhxwX9S6BCsiYMgQj9kV"
asset_rg <- kobo_asset(uid_rg)
asset_rg

###
data_ml_en <- kobo_data(asset_ml, lang = "English (en)")
data_ml_fr <- kobo_data(asset_ml, lang = "Francais (fr)")
data_ml_ar <- kobo_data(asset_ml, lang = "Arabic (ar)")
data_ml_default <- kobo_data(asset_ml)

###
data_rg <- kobo_data(asset_rg)

###
#usethis::use_data(asset1, overwrite = TRUE)
usethis::use_data(asset_ml, overwrite = TRUE)
usethis::use_data(asset_rg, overwrite = TRUE)
#usethis::use_data(asset_list, overwrite = TRUE)
#usethis::use_data(data1, overwrite = TRUE)
usethis::use_data(data_ml_en, overwrite = TRUE)
usethis::use_data(data_ml_fr, overwrite = TRUE)
usethis::use_data(data_ml_ar, overwrite = TRUE)
usethis::use_data(data_ml_default, overwrite = TRUE)
usethis::use_data(data_rg, overwrite = TRUE)
