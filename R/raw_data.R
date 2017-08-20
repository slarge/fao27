#' ICES Stock database
#'
#' Data describing ICES Stocks. Accessed 5 July 2017
#'
#' \itemize{
#'  \item{StockDatabaseID}{Add text}
#'  \item{StockKey}{Add text}
#'  \item{StockKeyLabel}{Add text}
#'  \item{StockKeyDescription}{Add text}
#'  \item{PreviousStockKey}{Add text}
#'  \item{PreviousStockKeyLabel}{Add text}
#'  \item{ActiveYear}{Add text}
#'  \item{SpeciesScientificName}{Add text}
#'  \item{SpeciesCommonName}{Add text}
#'  \item{EcoRegion}{Add text}
#'  \item{ExpertGroup}{Add text}
#'  \item{ExpertGroupDescription}{Add text}
#'  \item{AdviceDraftingGroup}{Add text}
#'  \item{AdviceDraftingGroupDescription}{Add text}
#'  \item{DataCategory}{Add text}
#'  \item{YearOfLastAssessment}{Add text}
#'  \item{AssessmentFrequency}{Add text}
#'  \item{YearOfNextAssessment}{Add text}
#'  \item{AssessmentType}{Add text}
#'  \item{AdviceReleaseDate}{Add text}
#'  \item{AdviceCategory}{Add text}
#'  \item{AdviceType}{Add text}
#'  \item{UseOfDiscardsInAdvice}{Add text}
#'  \item{PABufferApplied}{Add text}
#'  \item{TrophicGuild}{Add text}
#'  \item{FisheriesGuild}{Add text}
#'  \item{SizeGuild}{Add text}
#'  \item{Published}{Add text}
#'  \item{GeneratedOn}{Add text}
#'  \item{SectionNumber}{Add text}
#'  \item{AssessmentKey}{Add text}
#' }
#'
#' @format A data frame with 1297 rows and 31 variables.
#' @source \url{https://sd.ices.dk/}
"stock_list_raw"

#' ICES Stock Assessment Graphs database - summary information from assessment output
#'
#' Data from published ICES advice from 2014-2017. Accessed Accessed 5 July 2017. ICES Stock Assessment Database, 2017/July ICES, Copenhagen
#'
#' \itemize{
#'	\item{Year}{Add text}
#'	\item{recruitment}{Add text}
#'	\item{high_recruitment}{Add text}
#'	\item{low_recruitment}{Add text}
#'	\item{low_SSB}{Add text}
#'	\item{SSB}{Add text}
#'	\item{high_SSB}{Add text}
#'	\item{catches}{Add text}
#'	\item{landings}{Add text}
#'	\item{discards}{Add text}
#'	\item{low_F}{Add text}
#'	\item{F}{Add text}
#'	\item{high_F}{Add text}
#'	\item{StockPublishNote}{Add text}
#'	\item{Fage}{Add text}
#'	\item{fishstock}{Add text}
#'	\item{recruitment_age}{Add text}
#'	\item{AssessmentYear}{Add text}
#'	\item{units}{Add text}
#'	\item{stockSizeDescription}{Add text}
#'	\item{stockSizeUnits}{Add text}
#'	\item{fishingPressureDescription}{Add text}
#'	\item{fishingPressureUnits}{Add text}
#' }
#'
#' @format A data frame with 20204 rows and 23 variables:
#' @source \url{https://standardgraphs.ices.dk/}
"sag_summary_raw"

#' ICES Stock Assessment Graphs database - reference points
#'
#' Data from published ICES advice from 2014-2017. Accessed Accessed 5 July 2017. “ICES Stock Assessment Database, 2017/July ICES, Copenhagen”
#'
#' \itemize{
#'	\item{AssessmentKey}{Add text}
#'	\item{StockKeyLabel}{Add text}
#'	\item{StockDatabaseID}{Add text}
#'	\item{StockKey}{Add text}
#'	\item{AssessmentYear}{Add text}
#'	\item{FLim}{Add text}
#'	\item{Fpa}{Add text}
#'	\item{Bpa}{Add text}
#'	\item{Blim}{Add text}
#'	\item{FMSY}{Add text}
#'	\item{MSYBtrigger}{Add text}
#'	\item{Fmanagement}{Add text}
#'	\item{Bmanagement}{Add text}
#'	\item{RecruitmentAge}{Add text}
#'	\item{RecruitmentLength}{Add text}
#' }
#'
#' @format A data frame with 545 rows and 15 variables:
#' @source \url{https://standardgraphs.ices.dk/}
"sag_refpts_raw"

#' ICES Stock Assessment Graphs database - keys
#'
#' Data from published ICES advice from 2014-2017. Accessed Accessed 5 July 2017. “ICES Stock Assessment Database, 2017/July ICES, Copenhagen”
#'
#' \itemize{
#'	\item{AssessmentYear}{Add text}
#'	\item{AssessmentKey}{Add text}
#'	\item{StockKeyLabel}{Add text}
#' }
#'
#' @format A data frame with 545 rows and 3 variables:
#' @source \url{https://standardgraphs.ices.dk/}
"sag_keys_raw"

#' ICES Stock Assessment Graphs database - stock status output
#'
#' Data from published ICES advice from 2014-2017. Accessed Accessed 5 July 2017. “ICES Stock Assessment Database, 2017/July. ICES, Copenhagen”
#'
#' \itemize{
#'	\item{AssessmentYear}{Add text}
#'	\item{AssessmentKey}{Add text}
#'	\item{StockKeyLabel}{Add text}
#'	\item{year}{Add text}
#'	\item{status}{Add text}
#'	\item{statusicon}{Add text}
#'	\item{type}{Add text}
#'	\item{lineNumber}{Add text}
#'	\item{lineDescription}{Add text}
#'	\item{fishingPressure}{Add text}
#'	\item{stockSize}{Add text}
#'	\item{stockSizeStatus}{Add text}
#'	\item{fishingPressureStatus}{Add text}
#' }
#'
#' @format A data frame with 6163 rows and 13 variables:
#' @source \url{https://standardgraphs.ices.dk/}
"sag_stock_status_raw"

#' ASFIS list of species
#'
#' ASFIS list of species includes 12 700 species items selected according
#' to their interest or relation to fisheries and aquaculture.
#' For each species item stored in a record, codes (ISSCAAP group, taxonomic and 3-alpha)
#' and taxonomic information (scientific name, author(s), family, and higher taxonomic classification) are provided.
#'
#' Version 2-2017
#'
#' \itemize{
#'	\item{ISSCAAP}{Add text}
#'	\item{TAXOCODE}{Add text}
#'	\item{X3A_CODE}{Add text}
#'	\item{Scientific_name}{Add text}
#'	\item{English_name}{Add text}
#'	\item{French_name}{Add text}
#'	\item{Spanish_name}{Add text}
#'	\item{Author}{Add text}
#'	\item{Family}{Add text}
#'	\item{Order}{Add text}
#'	\item{Stats_data}{Add text}
#' }
#'
#' @format A data frame with 12700 rows and 11 variables:
#' @source \url{http://www.fao.org/fishery/collection/asfis/en}
"species_list_raw"
