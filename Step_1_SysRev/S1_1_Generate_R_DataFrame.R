# Author: Kunru Song
# Run from the project root. Import the checked workbook without changing it.
library(dplyr)
library(stringr)
library(readxl)
input_dir <- 'Res_1_Data_for_analyses'

# Explicit column types and reviewed nominal categories.
schema <- list(Data = c(paper_id = "character", reference_raw = "character", 
year = "integer", research_class = "factor", size = "integer", 
S1 = "factor", mean_age = "double", sd_age = "double", male_pct = "double", 
age_group = "factor", age_range = "character", number_of_factors = "integer", 
continent = "factor", geographic_region = "factor", nation_region = "factor", 
language = "factor", sample_type = "factor", sampling = "factor", 
recruitment = "factor", analysis_type = "factor", validation = "factor", 
S2 = "factor", extraction = "factor", rotation = "factor", retention = "factor", 
scoring_points = "factor", tve_prop = "double", factor_naming = "character", 
factor_structure = "character", item_dropping = "character"), 
    Stages = c(paper_id = "character", reference_raw = "character", 
    year = "integer", stage_order_within_paper = "integer", stage = "factor", 
    stage_role = "factor", model_type = "factor", model_scope = "character", 
    stage_n = "integer", S1_stage = "factor", instrument = "factor", 
    p = "integer", item_scoring_stage = "factor", input_matrix = "factor", 
    input_treatment = "factor", M1 = "factor", extraction = "factor", 
    estimator = "factor", M2 = "factor", record_retention = "factor", 
    k = "integer", retention = "factor", M3 = "factor", rotation = "factor", 
    M4 = "factor", R1 = "factor", R2 = "factor", R3 = "factor", 
    R4 = "factor", R5 = "factor", R6 = "factor", R7 = "factor", 
    R8 = "factor", R9 = "factor", mean_age = "double", sd_age = "double", 
    age_group = "factor", age_range = "character", male_pct = "double", 
    nation_region = "factor", continent = "factor", geographic_region = "factor", 
    language = "factor", sample_type = "factor", factor_structure = "character", 
    factor_naming = "character", item_dropping = "character", 
    record_sampling = "factor", record_recruitment = "factor"
    ))
vocabulary <- list(Data = list(paper_id = NULL, reference_raw = NULL, year = NULL, 
    research_class = c("I", "II", "III"), size = NULL, S1 = c("0", 
    "1", "2"), mean_age = NULL, sd_age = NULL, male_pct = NULL, 
    age_group = c("mixed", "children and adolescents", "adult", 
    "adolescent"), age_range = NULL, number_of_factors = NULL, 
    continent = c("Europe", "Asia", "Americas", "Africa"), geographic_region = c("Northern Europe", 
    "Eastern Asia", "Western Europe", "Northern America", "Southern Europe", 
    "Western Asia", "Southern Asia", "South-Eastern Asia", "Eastern Europe", 
    "Southern Africa", "Eastern Africa", "South America", "Northern Africa", 
    "Central America"), nation_region = c("UK", "Hong Kong, China", 
    "Switzerland", "Finland", "Germany", "United States", "Italy", 
    "Lebanon", "South Korea", "Canada", "Bangladesh", "Portugal", 
    "Singapore", "Greece", "Jordan", "India", "Spain", "Malaysia", 
    "Poland", "France", "Japan", "Iran", "Namibia", "Uganda", 
    "Mainland China", "Turkey", "North Macedonia", "Thailand", 
    "Pakistan", "Croatia", "South Africa", "Romania", "Israel", 
    "Peru", "Colombia", "Indonesia", "Saudi Arabia", "Egypt", 
    "Ethiopia", "Costa Rica", "Brazil"), language = c("English", 
    "Chinese", "French", "Finnish", "German", "Italian", "Arabic", 
    "Korean", "Bangla", "Portuguese", "Greek", "Spanish", "Malay", 
    "Polish", "Japanese", "Persian", "Turkish", "Macedonian", 
    "Thai", "Croatian", "Urdu", "Romanian", "Hebrew", "Indonesian", 
    "Amharic", "Hindi"), sample_type = c("mixed", "primary and secondary school students", 
    "university students", "community", "general adults", "secondary school students", 
    "mixed students", "clinical"), sampling = c("convenience", 
    "multi-stage stratified", "cluster", "cluster random", "simple random"
    ), recruitment = c("online", "offline"), analysis_type = c("EFA only", 
    "EFA+CFA", "CFA only", "bifactor EFA+CFA", "bifactor CFA", 
    "ESEM", "CFA", "bifactor ESEM"), validation = c("single sample", 
    "split-half", "independent samples"), S2 = c("0", "1", "2"
    ), extraction = c("PCA", "ML", "PAF", "GLS", "WLSMV", "MLR", 
    "MRFA", "DWLS", "RDWLS"), rotation = c("varimax", "promax", 
    "oblimin", "none", "oblique primary factors; Schmid-Leiman transformation", 
    "geomin", "quartimin", "weighted oblimin", "oblique (unspecified)", 
    "target"), retention = NULL, scoring_points = c("5-point", 
    "6-point"), tve_prop = NULL, factor_naming = NULL, factor_structure = NULL, 
    item_dropping = NULL), Stages = list(paper_id = NULL, reference_raw = NULL, 
    year = NULL, stage_order_within_paper = NULL, stage = c("EFA", 
    "CFA", "bifactor EFA", "bifactor CFA", "ESEM", "bifactor ESEM"
    ), stage_role = c("Exploration", "Validation"), model_type = c("Ordinary", 
    "Higher-order", "Bifactor", "Unresolved hierarchy", "Joint scales"
    ), model_scope = NULL, stage_n = NULL, S1_stage = c("0", 
    "1", "2"), instrument = c("Original IAT", "Parent-Child IAT", 
    "IAT-A", "Dhir adolescent wording", "WoW IAT merged items", 
    "Poker IAT merged items", "Greek adolescent wording", "IAT-R", 
    "IAT-SNS", "Joint IAT/SAS", "Wei revised wording"), p = NULL, 
    item_scoring_stage = c("5-point", "6-point"), input_matrix = c("Covariance", 
    "Polychoric"), input_treatment = c("Continuous explicit", 
    "Ordinal explicit"), M1 = c("0", "1", "2"), extraction = c("PCA", 
    "Latent model", "Common factor"), estimator = c("PCA", "ML", 
    "MLM", "PAF", "LS (unspecified)", "ML-SB", "MLR", "GLS", 
    "WLSMV", "ULS", "MRFA", "RDWLS"), M2 = c("0", "1", "2"), 
    record_retention = NULL, k = NULL, 
    retention = c("Heuristic only", "PA/MAP/Hull reported", "Model-fit search"
    ), M3 = c("0", "1", "2"), rotation = c("Orthogonal", "Oblique", 
    "Conflicting direction", "Oblique to Schmid-Leiman", "Target; direction unclear", 
    "Oblimin", "Others or unspecified"), M4 = c("0", "1", "2"
    ), R1 = c("0", "1", "2"), R2 = c("0", "1", "2"), R3 = c("0", 
    "1", "2"), R4 = c("0", "1", "2"), R5 = c("0", "1", "2"), 
    R6 = c("0", "1", "2"), R7 = c("0", "1", "2"), R8 = c("0", 
    "1", "2"), R9 = c("0", "1", "2"), mean_age = NULL, sd_age = NULL, 
    age_group = c("mixed", "children and adolescents", "adult", 
    "adolescent"), age_range = NULL, male_pct = NULL, nation_region = c("UK", 
    "Hong Kong, China", "Switzerland", "Finland", "Germany", 
    "United States", "Italy", "Lebanon", "South Korea", "Canada", 
    "Bangladesh", "Portugal", "Singapore", "Greece", "Jordan", 
    "India", "Spain", "Malaysia", "Poland", "France", "Japan", 
    "Iran", "Namibia", "Uganda", "Mainland China", "Turkey", 
    "North Macedonia", "Thailand", "Pakistan", "Croatia", "South Africa", 
    "Romania", "Israel", "Peru", "Colombia", "Indonesia", "Saudi Arabia", 
    "Egypt", "Ethiopia", "Costa Rica", "Brazil"), continent = c("Europe", 
    "Asia", "Americas", "Africa"), geographic_region = c("Northern Europe", 
    "Eastern Asia", "Western Europe", "Northern America", "Southern Europe", 
    "Western Asia", "Southern Asia", "South-Eastern Asia", "Eastern Europe", 
    "Southern Africa", "Eastern Africa", "South America", "Northern Africa", 
    "Central America"), language = c("English", "Chinese", "French", 
    "Finnish", "German", "Italian", "Arabic", "Korean", "Bangla", 
    "Portuguese", "Greek", "Spanish", "Malay", "Polish", "Japanese", 
    "Persian", "Turkish", "Macedonian", "Thai", "Croatian", "Urdu", 
    "Romanian", "Hebrew", "Indonesian", "Amharic", "Hindi"), 
    sample_type = c("mixed", "primary and secondary school students", 
    "university students", "community", "general adults", "secondary school students", 
    "mixed students", "clinical"), factor_structure = NULL, factor_naming = NULL, 
    item_dropping = NULL, record_sampling = c("convenience", 
    "multi-stage stratified", "cluster", "cluster random", "simple random"
    ), record_recruitment = c("online", "offline")))

score_fields <- c('S1','S2','S1_stage','M1','M2','M3','M4',paste0('R',1:9))
retention_aliases <- c('pa'='parallel analysis','parallel analysis'='parallel analysis',
  'map'='MAP','minimum average partial'='MAP','hull method'='Hull method',
  'model fit indices'='fit indices','fit indices'='fit indices','scree plot'='scree plot',
  'factor loadings'='factor loadings','interpretability'='interpretability',
  'model comparison'='model comparison','model modification'='model modification',
  'modification indices'='modification indices','fixed maximum three factors'='fixed maximum three factors',
  'parameter estimates'='parameter estimates','inter-factor correlations'='inter-factor correlations')
records <- list()
for (sheet in names(schema)) {
  dat <- read_excel(file.path(input_dir,
    'Coding_Results_Harmonized_Prepared_for_analysis_Double-Checked.xlsx'),
    sheet=sheet, col_types='text', na=character(), trim_ws=FALSE, .name_repair='minimal') %>%
    select(-matches('(^|[^a-z])notes?($|[^a-z])', ignore.case=TRUE)) %>% as.data.frame()
  names(dat)[names(dat)=='_geographic_region'] <- 'geographic_region'
  names(dat) <- make.names(names(dat))
  if (!identical(names(dat), names(schema[[sheet]]))) stop('Workbook columns changed: ', sheet)
  for (field in names(dat)) {
    value <- dat[[field]] %>% str_replace_all('[\t\r\n\u00a0 ]+', ' ') %>% str_trim()
    key <- tolower(value)
    value[is.na(key) | key==''] <- 'Missing (blank)'
    value[key %in% c('nr','n.r.','not reported','not report','not specified','not stated')] <- 'Not Reported'
    value[key %in% c('n/a','not applicable','not apply')] <- 'Not Applicable'
    value[key %in% 'na'] <- if (field %in% score_fields) 'Not Applicable' else 'NA (unspecified)'
    value[key %in% c('unclear','unknown','not clear')] <- 'Unclear'
    value[key %in% c('stage-specific; see stages','stage specific; see stages')] <- 'Stage-specific; see Stages'
    unavailable <- c('Not Reported','Not Applicable','Missing (blank)','NA (unspecified)',
      'Unclear','Stage-specific; see Stages')
    type <- schema[[sheet]][[field]]
    if (type %in% c('integer','double')) {
      value[value %in% unavailable] <- NA_character_
      number <- suppressWarnings(as.numeric(value))
      if (any(!is.na(value) & is.na(number))) stop('Unparsed number: ', sheet, '/', field)
      if (type=='integer') {
        if (any(number!=trunc(number), na.rm=TRUE)) stop('Non-integer: ', field)
        number <- as.integer(number)
      }
      dat[[field]] <- number
      next
    }
    if (field=='age_group') value[tolower(value)=='children and adolescent'] <- 'children and adolescents'
    if (sheet=='Data' && field=='rotation') value[tolower(value)=='oblique'] <- 'oblique (unspecified)'
    if (type=='factor') {
      multiple <- field %in% c('continent','geographic_region','nation_region','language','sample_type','record_retention') ||
        (sheet=='Data' && field %in% c('retention','extraction','analysis_type'))
      retention <- field=='record_retention' || (sheet=='Data' && field=='retention')
      for (i in seq_along(value)) {
        if (value[i] %in% unavailable) next
        tokens <- if (multiple) strsplit(value[i], ';', fixed=TRUE)[[1]] else value[i]
        tokens <- trimws(tokens)
        if (retention) {
          keys <- tolower(tokens)
          kaiser <- keys=='kaiser criterion (eigenvalue > 1)' |
            grepl('^(kaiser( criterion| rule)?|eigen[ -]?values?\\s*>\\s*1|eigen[ -]?values? (greater than|above) (one|1))$', keys)
          tokens <- unname(retention_aliases[keys])
          tokens[kaiser] <- 'Kaiser criterion (eigenvalue > 1)'
        } else {
          allowed <- vocabulary[[sheet]][[field]]
          tokens <- allowed[match(tolower(tokens), tolower(allowed))]
        }
        if (anyNA(tokens)) stop('Unreviewed category: ', sheet, '/', field, ': ', value[i])
        if (multiple) tokens <- sort(unique(tokens), method='radix')
        value[i] <- paste(tokens, collapse='; ')
      }
      value <- factor(value, levels=sort(unique(value), method='radix'))
    }
    dat[[field]] <- value
  }
  records[[sheet]] <- dat
}
# Spelling and case-only harmonization; factor meanings remain unchanged.
naming <- unique(c(records$Data$factor_naming, records$Stages$factor_naming))
corrected <- naming %>% str_replace_all('modificaiton','modification') %>% str_replace_all('Overal PUI','Overall PUI')
canonical <- corrected[match(tolower(corrected), tolower(corrected))]
for (sheet in names(records)) {
  records[[sheet]]$factor_naming <- canonical[match(records[[sheet]]$factor_naming, naming)]
}
# Downstream modules use the record data and linked stages; keep unlinked stages in memory.
saveRDS(records$Data, file.path(input_dir, 'Dat_SysRev_Data_Records.rds'))
# Link stages to records by exact identity, with the four approved split-record merges.
data_keys <- with(records$Data, paste(paper_id, reference_raw, year, sep='|'))
stage_keys <- with(records$Stages, paste(paper_id, reference_raw, year, sep='|'))
merge_from <- c('PDF57|kaya et al. #1|2016','PDF57|kaya et al. #2|2016',
  'PDF49|samaha et al. #1|2018','PDF49|samaha et al. #2|2018',
  'PDF17|lu et al. #1|2022','PDF17|lu et al #2|2022',
  'PDF30|wei et al. #1|2025','PDF30|wei et al. #2|2025')
merge_to <- rep(c('PDF57|kaya et al.|2016','PDF49|samaha et al.|2018',
  'PDF17|lu et al|2022','PDF30|wei et al.|2025'), each=2)
linked_keys <- stage_keys
merged <- match(stage_keys, merge_from)
linked_keys[!is.na(merged)] <- merge_to[merged[!is.na(merged)]]
data_rows <- match(linked_keys, data_keys)
if (anyNA(data_rows)) stop('A stage requires an explicit record linkage.')
stages <- records$Stages %>% mutate(data_row=data_rows,
  record_id=sprintf('D%03d', data_row), stage_row=row_number(),
  stage_id=paste(stage_keys, paste0('stage=',stage_order_within_paper), sep='|')) %>%
  relocate(data_row, record_id, stage_row, stage_id)
saveRDS(stages, file.path(input_dir, 'Dat_SysRev_Linked_Stages.rds'))
