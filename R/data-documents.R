#' Compiled contraceptive use data
#'
#' A dataset containing contraceptive use aggregates compiled by the UNPD:
#' * Prevalence estimates from national surveys | _World Contraceptive Use by Marital Status and Age, 2017_ [@un_desa_pd_WCUMA_2017].
#' * Number of women by marital status and age  | _Estimates and Projections of Women of Reproductive Age Who Are Married or in a Union: 2018 Revision_ [@undesa_pd2018_estim_projec_women_reprod_age].
#' * Country region information, areas and regions| _Standard Country or Area Codes for Statistical Use (M49)_ [@UN_M49]; sexual activity among unmarried women: [@kantorova18_contr].
#'
#' @format A data frame with 31 variables:
#' \describe{
#'   \item{division_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} country code (\cite{United Nations, 2019}).}
#'   \item{start_date}{\emph{\sQuote{Numeric}} Year, as a decimal (e.g., 1999.24...), marking the start of the reference period.}
#'   \item{end_date}{\emph{\sQuote{Numeric}} Year, as a decimal (e.g., 2006.44...), marking the end of the reference period.}
#'   \item{is_in_union}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Rows with \dQuote{Y} are observations for married/in-union women; rows with \dQuote{N} are observations for unmarried/not-in-union women. See also the section \dQuote{Notes on cohabiting partnerships} below.}
#'   \item{age_range}{\emph{\sQuote{Character}} age range, e.g., \dQuote{15-49}, \dQuote{15-19}, etc. This is the age range for which the observation can be used to produce estiamtes for, not the age range surveyed.}
#'   \item{data_series_type}{\emph{\sQuote{Character}} taking values \dQuote{DHS}, \dQuote{MICS}, \dQuote{National survey}, \dQuote{PMA}, or \dQuote{Other}. Indicates the source survey.}
#'   \item{group_type_relative_to_baseline}{\emph{\sQuote{Character}} indicating any difference relative to the baseline group, namely married/in-union or unmarried/not-in-union women (according to \code{is_in_union}). This field is used to assign \emph{mislcassification biases} (see appendices to (\cite{Alkema, et al., 2013; Wheldon et al., 2018}). When \code{is_in_union == "Y"}, takes values in: \describe{
#'     \item{AL}{(\dQuote{all women}) All women, regardless of marital/union status, included in the survey.}
#'     \item{BS}{(\dQuote{both sexes}) Men and women included in the survey.}
#'     \item{EM}{(\dQuote{ever married}) Ever-married women included in the survey.}
#'     \item{HW}{(\dQuote{husbands/wives}) Husbands and wives included in the survey.}
#'     \item{MW}{(\dQuote{married women}) Survey population was married women, i.e., no difference relative to baseline group.}
#'     \item{SA}{(\dQuote{sexually active}) All sexually active women, regardless of marital status, included in the survey.}
#'   }
#'   When \code{is_in_union == "N"}, takes values in: \describe{
#'     \item{FM}{Steriliztion was the only contraceptive method recorded.}
#'     \item{PW}{(\dQuote{partnered women}) Only women with a partner were included in the survey. See also the section \dQuote{Notes on cohabiting partnerships} below.}
#'     \item{UW}{(\dQuote{unmarried women}) Survey population was unmarried women, i.e., no difference relative to baseline group.}
#'   }}
#'   \item{contraceptive_use_modern}{\emph{\sQuote{Numeric}} Prevalence of modern contraceptive use.}
#'   \item{contraceptive_use_traditional}{\emph{\sQuote{Numeric}} Prevalence of traditional contraceptive use.}
#'   \item{contraceptive_use_any}{\emph{\sQuote{Numeric}} Prevalence of contraceptive use, any method.}
#'   \item{unmet_need_modern}{\emph{\sQuote{Numeric}} Unmet need for modern methods of contraception.}
#'   \item{unmet_need_any}{\emph{\sQuote{Numeric}} Unmet need for contraception, any method.}
#'   \item{is_pertaining_to_methods_used_since_last_pregnancy}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation pertains to contraceptive methods used since last pregnancy.}
#'   \item{pertaining_to_methods_used_since_last_pregnancy_reason}{\emph{\sQuote{Character}} If \code{is_pertaining_to_methods_used_since_last_pregnancy == "Y"}, provides an explanation, otherwise blank.}
#'   \item{has_geographical_region_bias}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation is likely biased due to geographical region covered.}
#'   \item{geographical_region_bias_reason}{\emph{\sQuote{Character}} If \code{has_geographical_region_bias == "Y"}, provides an explanation, otherwise blank.}
#'   \item{has_non_pregnant_and_other_positive_biases}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation likely subject to positive biases due to reasons other than pregnancy, geography, or age group.}
#'   \item{non_pregnant_and_other_positive_biases_reason}{\emph{\sQuote{Character}} If \code{has_non_pregnant_and_other_positive_biases == \dQuote{Y}}, provides an explanation, otherwise blank.}
#'   \item{age_group_bias}{\emph{\sQuote{Character}} One of \dQuote{+}, \dQuote{-}, \dQuote{?}, or \dQuote{None}. Indicates the direction of likely bias due the survey targeting an age group other than 15-49. \dQuote{+} and \dQuote{-} indicate positive and negative bias, respectively. \dQuote{?} indicates a bias of unknown direction, and \dQuote{None} indicates no bias.}
#'   \item{modern_method_bias}{\emph{\sQuote{Character}} One of \dQuote{+}, \dQuote{-}, or \dQuote{None}. Indicates observations likely to be biased due to the inclusion (\dQuote{+}) or exclusion (\dQuote{-}) of sterilization from modern method use.}
#'   \item{has_traditional_method_bias}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Indicates observations likely to be biased due to the inclusion of folk methods in traditional method use.}
#'   \item{traditional_method_bias_reason}{This field is not used.}
#'   \item{has_absence_of_probing_questions_bias}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Indicates observations likely to be biased due to the absense of probing questions in survey about knowledge of specific contraceptive methods.}
#'   \item{se_modern}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{contraceptive_use_modern}.}
#'   \item{se_traditional}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{contraceptive_use_traditional}.}
#'   \item{se_unmet_need}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{unmet_need_for_any_method}.}
#'   \item{se_log_r_modern_no_use}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     contraceptive_use_modern / (1 - contraceptive_use_any_method)}}
#'   \item{se_log_r_traditional_no_use}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     contraceptive_use_traditional / (1 - contraceptive_use_any_method)}}
#'   \item{se_log_r_unmet_no_need}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     unmet_need_for_any_method / (1 - contraceptive_use_any_method)}}
#'   \item{source_id}{\emph{\sQuote{Numeric}} Unique source (i.e., survey) identifer.}
#'   \item{record_id}{\emph{\sQuote{Numeric}} Unique record identifier.}
#' }
#'
#' @section Notes on cohabiting partnerships:
#' \itemize{
#' \item The classification into \dQuote{married/in-union} and
#' \dQuote{unmarried/not-in-union} encoded in the \code{is_in_union}
#' field treats cohabiting partnerships in the first category. That
#' is, women with a cohabiting partner are included in observations with \code{is_in_union == "Y"}. Observations
#' with \code{is_in_union == "N"} include only women with a
#' non-cohabiting partner (in addition to women who are unmarried or
#' not in-union).
#' \item Observations with \code{is_in_union == "N"} and
#' \code{group_type_relative_to_baseline == "PW"} are from surveys that excluded single
#' women.
#'}
#'
#' @references
#'
#' Alkema, L., Kantorova, V., Menozzi, C., Biddlecom,
#' A. (2013). National, regional, and global rates and trends in
#' contraceptive prevalence and unmet need for family planning between
#' 1990 and 2015: A systematic and comprehensive analysis. \emph{The Lancet}, 381(9878), 1642-1652.
#'
#' United Nations (2019). \emph{Standard Country or Area Codes for
#' Statistical Use (M49)}. United Nations, Department of Economic and
#' Social Affairs, Statistics
#' Division. \url{https://unstats.un.org/unsd/methodology/m49/}
#' (visited on 2019-10-21).
#'
#' Wheldon, M. C., Kantorova, V., Ueffing, P., Dasgupta,
#' A. N. Z. (2018). Methods for estimating and projecting key family
#' planning indicators among all women of reproductive age. Technical
#' Paper No. 2018/2, United Nations, Department of Economic and Social Affairs, Population Division.
#' \url{https://www.un.org/en/development/desa/population/publications/pdf/technical/TP2018-2.pdf} (visited on 2019-10-21).
#'
#' @source UN 2018,	2018.0.1
"contraceptive_use"


#' Divisions, unit data
#'
#' A dataset containing units compiled by the UNPD
#'
#' @format A data frame with 13 variables:
#' \describe{
#'   \item{division_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} country code (\cite{United Nations, 2019}).}
#'   \item{name_country}{\emph{\sQuote{Character}}, country name.}
#'   \item{name_region}{\emph{\sQuote{Character}}, region name (e.g., \dQuote{Africa}).}
#'   \item{name_sub_region}{\emph{\sQuote{Character}}, subregion name (e.g., \dQuote{Eastern Africa}).}
#'   \item{region_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} region code (\cite{United Nations, 2019}).}
#'   \item{sub_region_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} subregion code (\cite{United Nations, 2019}).}
#'   \item{is_developed_region}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries classified as belonging to developed regions (\code{is_developed_region == "Y"}).}
#'   \item{is_less_developed_region}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries classified as belonging to less developed regions (\code{is_less_developed_region == "Y"}).}
#'   \item{is_least_developed_country}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries classified as least developed (\code{is_least_developed_country == "Y"}).}
#'   \item{is_in_sub_saharan_africa}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries in sub-Saharan Africa (\code{is_in_sub_saharan_africa == "Y"}).}
#'   \item{is_unmarried_sexual_activity}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries classified as having low sexual activity among unmarried women (\code{is_unmarried_sexual_activity == "Y"}; see \cite{Wheldon et al., 2018}).}
#'   \item{is_low_population}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries with population below 90,000 (\code{is_low_population == "Y"}).}
#'   \item{is_fp2020}{\emph{\sQuote{Character}} taking values \dQuote{Y} and \dQuote{N}. Indicates countries belonging to the group of \emph{FP2020} focus countries (\code{is_fp2020 == "Y"}; \cite{FP2020, 2019}).}
#' }
#'
#' @references
#'
#' FP2020 (2019). \url{http://www.familyplanning2020.org/} (visited on 2019-10-21).
#'
#' United Nations (2019). \emph{Standard Country or Area Codes for
#' Statistical Use (M49)}. United Nations, Department of Economic and
#' Social Affairs, Statistics
#' Division. \url{https://unstats.un.org/unsd/methodology/m49/}
#' (visited on 2019-10-21).
#'
#' Wheldon, M. C., Kantorova, V., Ueffing, P., Dasgupta,
#' A. N. Z. (2018). Methods for estimating and projecting key family
#' planning indicators among all women of reproductive age. Technical
#' Paper No. 2018/2, United Nations, Department of Economic and Social Affairs, Population Division.
#' \url{https://www.un.org/en/development/desa/population/publications/pdf/technical/TP2018-2.pdf} (visited on 2019-10-21).
#'
#' @source UN 2018,	2018.0.1
"divisions"


#' Population count data
#'
#' A dataset containing population counts compiled by the UNPD
#'
#' @format A data frame with 5 variables:
#' \describe{
#'   \item{division_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} country code (\cite{United Nations, 2019}).}
#'   \item{is_in_union}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Rows with \dQuote{Y} are observations for married/in-union women; rows with \dQuote{N} are observations for unmarried/not-in-union women.}
#'   \item{age_range}{\emph{\sQuote{Character}} age range, e.g., \dQuote{15-49}, \dQuote{15-19}, etc.}
#'   \item{mid_year}{\emph{\sQuote{Numeric}} Year of \code{population_count}. The values in this field are whole years (e.g., \dQuote{1970}) but the population counts are estimates for the middle of the given year (e.g., \dQuote{1970.5}).}
#'   \item{population_count}{\emph{\sQuote{Numeric}} Population count.}
#' }
#'
#' @references
#'
#' United Nations (2019). \emph{Standard Country or Area Codes for
#' Statistical Use (M49)}. United Nations, Department of Economic and
#' Social Affairs, Statistics Division. \url{https://unstats.un.org/unsd/methodology/m49/}
#' (visited on 2019-10-21).
#'
#' @source UN 2018,	2018.0.1
"population_counts"

#' Service statistics
#'
#' Service stat data must contain the following variables. Names must be as documented (eg. all lowercase). `fpemdata::service_stats` returns a vector of column names. Data not publicly available. 
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{"division_numeric_code"}{country code}
#'   \item{"name"}{country name}
#'   \item{"year"}{mid year of observation period}
#'   \item{"emu"}
#'   \item{"ss_type"}
#'   ...
#' }
#' @source 
"service_stats"

#' Contraceptive use track 20
#'
#' Track20 customizes the database prepared by the United Nations, editing, adding, and deleting surveys for use in the FP2020 countries. 
#'
#' In general, differences stem from:
#' * Timelines to finalize datasets (UNPD finalizes data in February, while Track20 adds new surveys through April)
#' * Use of microdata versus published reports
#' * Hierarchy of method effectiveness
#' * Coding of individual methods as modern or traditional
#' * Survey specific classification of Lactational Amenorrhea Method
#' * Changes between versions of reports
#' * Differences in PMA2020 data
#' * Exclusions/Inclusion of surveys
#' 
#' @format A data frame with 31 variables:
#' \describe{
#'   \item{division_numeric_code}{\emph{\sQuote{Numeric}} 3-digit \emph{M49} country code (\cite{United Nations, 2019}).}
#'   \item{start_date}{\emph{\sQuote{Numeric}} Year, as a decimal (e.g., 1999.24...), marking the start of the reference period.}
#'   \item{end_date}{\emph{\sQuote{Numeric}} Year, as a decimal (e.g., 2006.44...), marking the end of the reference period.}
#'   \item{is_in_union}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Rows with \dQuote{Y} are observations for married/in-union women; rows with \dQuote{N} are observations for unmarried/not-in-union women. See also the section \dQuote{Notes on cohabiting partnerships} below.}
#'   \item{age_range}{\emph{\sQuote{Character}} age range, e.g., \dQuote{15-49}, \dQuote{15-19}, etc. This is the age range for which the observation can be used to produce estiamtes for, not the age range surveyed.}
#'   \item{data_series_type}{\emph{\sQuote{Character}} taking values \dQuote{DHS}, \dQuote{MICS}, \dQuote{National survey}, \dQuote{PMA}, or \dQuote{Other}. Indicates the source survey.}
#'   \item{group_type_relative_to_baseline}{\emph{\sQuote{Character}} indicating any difference relative to the baseline group, namely married/in-union or unmarried/not-in-union women (according to \code{is_in_union}). This field is used to assign \emph{mislcassification biases} (see appendices to (\cite{Alkema, et al., 2013; Wheldon et al., 2018}). When \code{is_in_union == "Y"}, takes values in: \describe{
#'     \item{AL}{(\dQuote{all women}) All women, regardless of marital/union status, included in the survey.}
#'     \item{BS}{(\dQuote{both sexes}) Men and women included in the survey.}
#'     \item{EM}{(\dQuote{ever married}) Ever-married women included in the survey.}
#'     \item{HW}{(\dQuote{husbands/wives}) Husbands and wives included in the survey.}
#'     \item{MW}{(\dQuote{married women}) Survey population was married women, i.e., no difference relative to baseline group.}
#'     \item{SA}{(\dQuote{sexually active}) All sexually active women, regardless of marital status, included in the survey.}
#'   }
#'   When \code{is_in_union == "N"}, takes values in: \describe{
#'     \item{FM}{Steriliztion was the only contraceptive method recorded.}
#'     \item{PW}{(\dQuote{partnered women}) Only women with a partner were included in the survey. See also the section \dQuote{Notes on cohabiting partnerships} below.}
#'     \item{UW}{(\dQuote{unmarried women}) Survey population was unmarried women, i.e., no difference relative to baseline group.}
#'   }}
#'   \item{contraceptive_use_modern}{\emph{\sQuote{Numeric}} Prevalence of modern contraceptive use.}
#'   \item{contraceptive_use_traditional}{\emph{\sQuote{Numeric}} Prevalence of traditional contraceptive use.}
#'   \item{contraceptive_use_any}{\emph{\sQuote{Numeric}} Prevalence of contraceptive use, any method.}
#'   \item{unmet_need_modern}{\emph{\sQuote{Numeric}} Unmet need for modern methods of contraception.}
#'   \item{unmet_need_any}{\emph{\sQuote{Numeric}} Unmet need for contraception, any method.}
#'   \item{is_pertaining_to_methods_used_since_last_pregnancy}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation pertains to contraceptive methods used since last pregnancy.}
#'   \item{pertaining_to_methods_used_since_last_pregnancy_reason}{\emph{\sQuote{Character}} If \code{is_pertaining_to_methods_used_since_last_pregnancy == "Y"}, provides an explanation, otherwise blank.}
#'   \item{has_geographical_region_bias}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation is likely biased due to geographical region covered.}
#'   \item{geographical_region_bias_reason}{\emph{\sQuote{Character}} If \code{has_geographical_region_bias == "Y"}, provides an explanation, otherwise blank.}
#'   \item{has_non_pregnant_and_other_positive_biases}{\emph{\sQuote{Character}} Takes values \dQuote{N} and \dQuote{Y}. If \dQuote{Y}, observation likely subject to positive biases due to reasons other than pregnancy, geography, or age group.}
#'   \item{non_pregnant_and_other_positive_biases_reason}{\emph{\sQuote{Character}} If \code{has_non_pregnant_and_other_positive_biases == \dQuote{Y}}, provides an explanation, otherwise blank.}
#'   \item{age_group_bias}{\emph{\sQuote{Character}} One of \dQuote{+}, \dQuote{-}, \dQuote{?}, or \dQuote{None}. Indicates the direction of likely bias due the survey targeting an age group other than 15-49. \dQuote{+} and \dQuote{-} indicate positive and negative bias, respectively. \dQuote{?} indicates a bias of unknown direction, and \dQuote{None} indicates no bias.}
#'   \item{modern_method_bias}{\emph{\sQuote{Character}} One of \dQuote{+}, \dQuote{-}, or \dQuote{None}. Indicates observations likely to be biased due to the inclusion (\dQuote{+}) or exclusion (\dQuote{-}) of sterilization from modern method use.}
#'   \item{has_traditional_method_bias}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Indicates observations likely to be biased due to the inclusion of folk methods in traditional method use.}
#'   \item{traditional_method_bias_reason}{This field is not used.}
#'   \item{has_absence_of_probing_questions_bias}{\emph{\sQuote{Character}} taking values \dQuote{Y} or \dQuote{N}. Indicates observations likely to be biased due to the absense of probing questions in survey about knowledge of specific contraceptive methods.}
#'   \item{se_modern}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{contraceptive_use_modern}.}
#'   \item{se_traditional}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{contraceptive_use_traditional}.}
#'   \item{se_unmet_need}{\emph{\sQuote{Numeric}} Estimated design-based standard error of \code{unmet_need_for_any_method}.}
#'   \item{se_log_r_modern_no_use}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     contraceptive_use_modern / (1 - contraceptive_use_any_method)}}
#'   \item{se_log_r_traditional_no_use}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     contraceptive_use_traditional / (1 - contraceptive_use_any_method)}}
#'   \item{se_log_r_unmet_no_need}{\emph{\sQuote{Numeric}} Estimated design-based standard error of the quantity \preformatted{
#'     unmet_need_for_any_method / (1 - contraceptive_use_any_method)}}
#'   \item{source_id}{\emph{\sQuote{Numeric}} Unique source (i.e., survey) identifer.}
#'   \item{record_id}{\emph{\sQuote{Numeric}} Unique record identifier.}
#' }
#'
"contraceptive_use_track20"

