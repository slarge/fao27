# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

stock_list_raw <- jsonlite::fromJSON("http://sd.ices.dk/services/odata3/StockListDWs3",
                                     simplifyDataFrame = TRUE)$value
devtools::use_data(stock_list_raw, overwrite = TRUE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Assessment Graphs Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#
# sag_summary_raw <- icesSAG::getSAG(stock = NULL,
#                                    year = 2014:2017,
#                                    data = "summary",
#                                    combine = TRUE)
# devtools::use_data(sag_summary_raw, overwrite = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: ICES Stock Assessment Graphs Database #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

sag_refpts_raw <- icesSAG::getSAG(stock = NULL,
                                  year = 2014:2017,
                                  data = "refpts",
                                  combine = TRUE)
devtools::use_data(sag_refpts_raw, overwrite = TRUE)


sag_keys_raw <- do.call("rbind", lapply(2014:2017,
                                        function(x) icesSAG::findAssessmentKey(stock = NULL,
                                                                               year = x,
                                                                               full = TRUE)[, c("AssessmentYear",
                                                                                                "AssessmentKey",
                                                                                                "StockKeyLabel")]))
devtools::use_data(sag_keys_raw, overwrite = TRUE)

get_stock_status <- function(assessmentKey) {
  dat <- icesSAG::getStockStatusValues(assessmentKey)[[1]]
  if(is.null(dat)) stop(paste0("NULL value returned for assessmentKey = ", assessmentKey))
  dat
}

sag_stock_status_raw <- sag_keys_raw %>%
  mutate(stock_status = purrr::map(.x = AssessmentKey, purrr::possibly(get_stock_status, otherwise = NA_real_))) %>%
  filter(!is.na(stock_status)) %>%
  tidyr::unnest(stock_status) %>%
  select(-AssessmentKey1)

devtools::use_data(sag_stock_status_raw, overwrite = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# DATA SOURCE: FAO species names and labels #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

spURL <- "http://www.fao.org/fishery/static/ASFIS/ASFIS_sp.zip"
tmpFileSp <- tempfile(fileext = ".zip")
download.file(spURL, destfile = tmpFileSp, mode = "wb", quiet = TRUE)
species_list_raw <- read.delim(unzip(tmpFileSp, "ASFIS_sp_Feb_2017.txt"),
                               fill = TRUE,
                               stringsAsFactors = FALSE,
                               header = TRUE,
                               na.strings = "")
devtools::use_data(species_list_raw, overwrite = TRUE)
