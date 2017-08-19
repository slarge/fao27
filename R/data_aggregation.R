rm(list = ls())
library(RcppRoll)
library(dplyr)
library(icesSAG)

reference_vals <- function(assessmentKey){
  ref_tab <- icesSAG::getFishStockReferencePoints(assessmentKey = assessmentKey)[[1]] %>%
    select(AssessmentYear, Fpa, Bpa, FLim, Blim, FMSY,
           MSYBtrigger)
  return(ref_tab)
}

summary_vals <- function(assessmentKey){
  sum_tab <- icesSAG::getSummaryTable(assessmentKey = assessmentKey)[[1]] %>%
    select(Year, low_SSB, high_SSB, SSB, catches, landings, discards, low_F, F, high_F,
           fishstock, AssessmentYear)
  return(sum_tab)
}


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Establish the year of the assessment and tidy up some ICES issues ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

data("stock_list_raw")
data("species_list_raw")

stock_list <- stock_list_raw %>%
  filter(ActiveYear == 2017,
         !is.na(YearOfLastAssessment),
         !grepl("\\.21\\.", StockKeyLabel)) %>%
  # ICES adopted new stock codes in 2017. Multiyear MSY/MP stocks are linked to old codes until new assessment
  mutate(StockCode = case_when(is.na(YearOfLastAssessment) &
                          is.na(YearOfNextAssessment) ~ StockKeyLabel,
                          active_year >= 2017 &
                          YearOfLastAssessment < 2017 &
                          AdviceCategory %in% c("MSY", "MP", "MSY/PA") ~ PreviousStockKeyLabel,
                          TRUE ~ StockKeyLabel),
    ID2016 = toupper(gsub( "-.*$", "", StockCode)),
    ID2017 = toupper(gsub("\\..*", "", StockCode)),
    SpeciesID = case_when(active_year < 2017 ~ ID2016,
                          YearOfLastAssessment < 2017 &
                            AdviceCategory %in% c("MSY", "MP", "MSY/PA") ~ ID2016,
                          grepl("*ebastes", SpeciesScientificName) ~ "RED",
                          TRUE ~ ID2017),
    YearOfLastAssessment = ifelse(StockCode %in% c("tur.27.4", "nop.27.3a4"),
                                  2016,
                                  YearOfLastAssessment)) %>%
  distinct(.keep_all = TRUE)

## ~~~~~~~~~~~~~~~~ ##
## Read in Table D2 ##
## ~~~~~~~~~~~~~~~~ ##
d2 <- readxl::read_xlsx("data-raw/FAO27-Table.xlsx", sheet = "TableFinal", range = "A2:Y39")
colnames(d2)[1:4] <- c("ISSCAAP", "English_name", "Scientific_name", "countries")

target_species <- d2 %>%
  tidyr::gather(key = YEAR, value = landings, -English_name, -Scientific_name, -countries, -ISSCAAP) %>%
  mutate(ISSCAAP = ISSCAAP)

species_list_raw$ISSCAAP <- as.character(species_list_raw$ISSCAAP)

## ~~~~~~~~~~~~~~~~~ ##
## Link FAO and ICES ##
## ~~~~~~~~~~~~~~~~~ ##

ices_data <- target_species %>%
  filter(!grepl("-", English_name),
         !grepl("*ther", English_name)) %>%
  select(1:3) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(species_list_raw, by = c("English_name", "Scientific_name", "ISSCAAP")) %>%
  left_join(stock_list, by = c("X3A_CODE" = "SpeciesID" ))

other_species <- d2 %>%
  filter(grepl("Total", ISSCAAP)) %>%
  mutate(ISSCAAP = as.numeric(gsub(" Total", "", ISSCAAP))) %>%
  tidyr::gather(key = YEAR, value = landings, -English_name, -Scientific_name, -countries, -ISSCAAP) %>%
  select(1) %>%
  distinct(.keep_all = TRUE)

other_data <- species_list_raw %>%
  filter(ISSCAAP %in% other_species$ISSCAAP) %>%
  left_join(stock_list, by = c("X3A_CODE" = "SpeciesID")) %>%
  filter(!is.na(StockKeyLabel),
         !StockKeyLabel %in% ices_data$StockKeyLabel) %>%
  mutate(ISSCAAP = paste0(ISSCAAP, " Total"))

## ~~~~~~~~~~~~~~~~~~~~ ##
## Start the assessment ##
## ~~~~~~~~~~~~~~~~~~~~ ##

all_data <- bind_rows(ices_data,
                      other_data) %>%
  mutate(DataCategory = floor(as.numeric(DataCategory)),
         StockAssessmentType = case_when(DataCategory == 2 ~ 1, #Surplus Production;
                                         DataCategory == 1 ~ 2, #Age/size-structured;
                                         DataCategory == 3 &
                                           grepl("Age|age", AssessmentType) ~ 2, #empirically-based for trends;
                                         DataCategory == 3 &
                                           !grepl("Age|age", AssessmentType) ~ 3, #empirically-based;
                                         DataCategory == 4~ 4, #others;
                                         DataCategory > 4 ~ 5, #Not Available
                                         TRUE ~ 5)
  ) %>%
  select(ISSCAAP,
         English_name,
         Scientific_name,
         StockKeyLabel,
         PreviousStockKeyLabel,
         StockKeyDescription,
         DataCategory,
         ActiveYear,
         YearOfLastAssessment,
         YearOfNextAssessment,
         StockAssessmentType,
         AssessmentType,
         AdviceCategory,
         AssessmentKey)

# These stocks don't have assessment keys - get them from earlier advice
odds <- all_data %>%
  filter(is.na(AssessmentKey),
         !is.na(StockKeyLabel),
         !StockKeyLabel %in% c("sal.27.32"))

odds_key <- sag_keys_raw %>%
  filter(StockKeyLabel %in% odds$PreviousStockKeyLabel) %>%
  group_by(StockKeyLabel) %>%
  filter(AssessmentYear == max(AssessmentYear),
         !is.na(StockKeyLabel))

all_data$AssessmentKey[all_data$PreviousStockKeyLabel %in% odds_key$StockKeyLabel] <- odds_key$AssessmentKey
all_data$YearOfLastAssessment[all_data$PreviousStockKeyLabel %in% odds_key$StockKeyLabel] <- odds_key$AssessmentYear

## ~~~~~~~~~~~~~~~~~~~~ ##
## Get reference points ##
## ~~~~~~~~~~~~~~~~~~~~ ##

ices_refpts <- bind_rows(all_data %>%
                           filter(is.na(AssessmentKey)),
                         all_data %>%
                           filter(!is.na(AssessmentKey)) %>%
                           mutate(reference_list = purrr::map(AssessmentKey,
                                                  reference_vals)) %>%
                           tidyr::unnest(reference_list)
)

## ~~~~~~~~~~~~~~~~~~~ ##
## Get stock summaries ##
## ~~~~~~~~~~~~~~~~~~~ ##

ices_status <- bind_rows(all_data %>%
                           filter(is.na(AssessmentKey)),
                         all_data %>%
                           filter(!is.na(AssessmentKey)) %>%
                           mutate(summary_list = purrr::map(AssessmentKey, summary_vals)) %>%
                           tidyr::unnest(summary_list)
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Map ICES reference points to FAO ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

ssb_status <- ices_status %>%
  mutate(SSB = SSB/1000,
         high_SSB = high_SSB/1000,
         low_SSB = low_SSB/1000) %>%
  group_by(StockKeyLabel) %>%
  arrange(Year) %>%
  do(tail(., n = 5)) %>%
  mutate(SSBavg = mean(SSB, na.rm = TRUE)) %>%
  filter(Year == YearOfLastAssessment) %>%
  left_join(sag_refpts_raw, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear")) %>%
  mutate(Blim = Blim/1000,
         Bpa = Bpa/1000,
         SSBrefdesc = "ICES Precautionary Approach",
         SSBstatus = case_when(SSB > Blim ~ "F", # Fully exploited
                               SSB < Bpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         SSBratioH = case_when(high_SSB > Blim ~ "F", # Fully exploited
                               high_SSB < Bpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         SSBratioL = case_when(low_SSB > Blim ~ "F", # Fully exploited
                               low_SSB < Bpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         SSBuncertainty = case_when(is.na(SSBratioL) & is.na(SSBratioH) ~ NA_character_,
                                    SSBratioH == SSBratioL ~ "H",
                                    SSBratioH != SSBratioL ~ "L",
                                    TRUE ~ NA_character_),
         SSBrefpt = case_when(SSBstatus == "F" ~ Blim,
                              SSBstatus == "O" ~ Bpa,
                              TRUE ~ NA_real_),
         SSBratio = SSB/SSBrefpt) %>%
  select(StockKeyLabel,
         SSBval = SSB,
         SSBavg,
         SSBrefpt,
         SSBrefdesc,
         SSBratio,
         SSBuncertainty,
         # SSBratioH,
         # SSBratioL,
         SSBstatus,
         SSByear = Year)


F_status <- ices_status %>%
  group_by(StockKeyLabel) %>%
  filter(Year >= YearOfLastAssessment - 6,
         Year <= YearOfLastAssessment - 1) %>%
  arrange(Year) %>%
  do(tail(., n = 5)) %>%
  mutate(Favg = ifelse(all(is.na(F)),
                       NA,
                       mean(F, na.rm = TRUE))) %>%
  filter(Year == YearOfLastAssessment - 1) %>%
  left_join(sag_refpts_raw, by = c("StockKeyLabel", "AssessmentKey", "AssessmentYear")) %>%
  mutate(Frefdesc = "ICES Precautionary Approach",
         Fstatus = case_when(F < FLim ~ "F", # Fully exploited
                               F > Fpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         FratioH = case_when(high_F < FLim ~ "F", # Fully exploited
                               high_F > Fpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         FratioL = case_when(low_F < FLim ~ "F", # Fully exploited
                               low_F > Fpa ~ "O", # Over exploited
                               TRUE ~ NA_character_),
         Funcertainty = case_when(is.na(FratioL) & is.na(FratioH) ~ NA_character_,
                                    FratioH == FratioL ~ "H",
                                  FratioH != FratioL ~ "L",
                                    TRUE ~ NA_character_),
         Frefpt = case_when(is.na(Fstatus) ~ NA_real_,
                            Fstatus == "F" ~ FLim,
                            Fstatus == "O" ~ Fpa,
                            TRUE ~ NA_real_),
         Fratio = F/Frefpt) %>%
  select(StockKeyLabel,
         Fval = F,
         Favg,
         Frefpt,
         Frefdesc,
         Fratio,
         Funcertainty,
         FratioH,
         FratioL,
         Fstatus,
         Fyear = Year)


E_status <- all_data %>%
  mutate(eT0 = NA,
         eTm = NA,
         eTf = NA,
         Eunit = NA,
         Equal = NA) %>%
  select(StockKeyLabel,
         eT0,
         eTm,
         eTf,
         Eunit,
         Equal)

C_status <- ices_status %>%
  mutate(landings = landings/1000,
         catches = catches/1000) %>%
  group_by(StockKeyLabel) %>%
  arrange(Year) %>%
  mutate(Clen = length(catches[!is.na(catches)]),
         Llen = length(landings[!is.na(landings)]),
         Cavg = case_when(Clen > 5 ~ roll_mean(catches, n = 5, align = "right", fill = NA, na.rm = TRUE),
                          Clen <= 5 &
                            Llen > 5 ~ roll_mean(landings, n = 5, align = "right", fill = NA, na.rm = TRUE),
                          Clen <= 5 &
                            Llen == 0 ~ as.numeric(catches),
                          Clen == 0 &
                            Llen <= 5 ~ as.numeric(landings),
                          # Clen == 0 & Llen == 0 ~ NA_real_,
                          TRUE ~ NA_real_),
         Cmax = case_when(!is.na(Cavg) ~ max(Cavg, na.rm = TRUE),
                          TRUE ~ NA_real_)) %>%
  filter(Year >= YearOfLastAssessment - 6,
         Year <= YearOfLastAssessment - 1) %>%
  arrange(Year) %>%
  do(tail(., n = 5)) %>%
  mutate(Cavg = case_when(!all(is.na(catches)) == TRUE ~ mean(catches, na.rm = TRUE),
                          all(is.na(catches)) == TRUE ~ mean(landings, na.rm = TRUE),
                          TRUE ~ NA_real_)) %>%
  filter(Year == YearOfLastAssessment - 1) %>%
  mutate(Cval = case_when(!is.na(catches) ~ catches,
                          is.na(catches) ~ landings,
                          TRUE ~ NA_integer_),
         Cratio = Cval/Cmax,
         Cstatus = case_when(Cratio <= 0.5 ~ "O",
                             Cratio > 0.5 ~ "F",
                             TRUE ~ NA_character_)) %>%
  select(StockKeyLabel,
         Cval,
         Cavg,
         Cmax,
         Cratio,
         Cstatus,
         Cyear = Year)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Create the nasty spreadsheet ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

group_data <- target_species %>%
  filter(YEAR == max(YEAR)) %>%
  mutate(`SOFIA ID` = NA,
         `FAO Area` = 27,
         `Management Agency/Advisory board` = "ICES",
         `Percentage of landings "assessed"` = NA,
         `Weighted Status (U, F, O)` = NA,
         `Weighting Method` = NA,
         `Rationale for combined assessment` = NA) %>%
  select(`SOFIA ID`,
         `FAO Area`,
         ISSCAAP,
         English_name,
         Scientific_name,
         `Countries` = countries,
         `Management Agency/Advisory board`,
         `Total Landings 1000s tons` = landings,
         `Total Landings Year` = YEAR,
         `Percentage of landings "assessed"`,
         `Percentage of landings "assessed"`,
         `Weighted Status (U, F, O)`,
         `Weighting Method`,
         `Rationale for combined assessment`) #%>%

all_status <- all_data %>%
  mutate(`Individual stock` = NA,
         `Proportion of Total Landings` = NA,
         `Stock Assessment Type` = NA,
         `Stock Assessment method/software` = NA,
         `Overfished (Y/N)` = NA, # SSB relative to SSBref
         `Overfishing (Y/N)` = NA) %>% # relative to Fref
  select(`Individual stock` = StockKeyDescription,
         `Proportion of Total Landings`,
         `Stock Assessment Type` = StockAssessmentType,
         `Stock Assessment method/software` = AssessmentType,
         `Overfished (Y/N)`, # SSB relative to SSBref
         `Overfishing (Y/N)`,
         ISSCAAP,
         StockKeyLabel,
         English_name,
         Scientific_name) %>%
  left_join(ssb_status, by = "StockKeyLabel") %>%
  left_join(F_status, by = "StockKeyLabel") %>%
  left_join(E_status, by = "StockKeyLabel") %>%
  left_join(C_status, by = "StockKeyLabel") %>%
  mutate(`Overfished (Y/N)` = case_when(SSBratio < 1 ~ "Y",
                                        SSBratio >= 1 ~ "N",
                                        TRUE ~ NA_character_),
         `Overfishing (Y/N)` = case_when(Fratio > 1 ~ "Y",
                                         Fratio <= 1 ~ "N",
                                         TRUE ~ NA_character_))


final_table <- group_data %>%
  full_join(all_status, by = c("ISSCAAP", "English_name", "Scientific_name")) %>%
  rename(`ISSCAAP group` = ISSCAAP,
         `Stock of species group` = English_name,
         `Scientific name` = Scientific_name)  %>%
  mutate(group = "A") %>%
  tidyr::spread(`SOFIA ID`, value = group) %>%
  select(-StockKeyLabel) %>%
  arrange(`ISSCAAP group`)

final_table[ncol(final_table)] <- NULL

final_table <- t(final_table)

write.csv(final_table, file = paste0("analysis/data/derived_data/", Sys.Date(), "_FAO-sofia-output.csv"), row.names = TRUE)
