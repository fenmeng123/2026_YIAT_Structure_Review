# Author: Kunru Song
# Study-level descriptive statistics, Table 1 and Figure 3 data.
# Figure 3: Geographical and language coverage, reported factor numbers, and quality assessment.
library(dplyr)
library(tidyr)
library(openxlsx)
input_dir <- 'Res_1_Data_for_analyses'
out <- 'Res_2_Results/S1_2_StudyCharacteristicsDescriptiveVisualization'
dir.create(out, recursive=TRUE, showWarnings=FALSE)
d <- readRDS(file.path(input_dir, 'Dat_SysRev_Data_Records.rds')) %>%
  mutate(across(where(is.factor), as.character), record_id=sprintf('Data_%03d', row_number()),
    country_raw=nation_region)

# Membership summaries retain multilingual and multiregion records.
memberships <- counts <- list()
fields <- c(language='language', region='geographic_region', research='research_class', factors='number_of_factors')
for (name in names(fields)) {
  m <- d %>% transmute(record_id, category=as.character(.data[[fields[[name]]]])) %>%
    separate_longer_delim(category, delim=';') %>% mutate(category=trimws(category),
      category=if_else(is.na(category) | tolower(category) %in% c('','nr','na','not reported'), 'Not reported', category)) %>%
    distinct(record_id, category)
  memberships[[name]] <- m
  counts[[name]] <- m %>% count(category, name='n') %>% arrange(desc(n), category) %>%
    mutate(denominator=sum(n), percent=100*n/denominator, label=sprintf('%s  %d (%.1f%%)', category,n,percent))
}
counts$research <- counts$research %>% arrange(match(category,c('I','II','III','Not reported')))
counts$factors <- counts$factors %>% arrange(as.integer(category))
language_mapping <- counts$language %>% filter(category!='Not reported') %>%
  select(category,n) %>% mutate(display_category=if_else(n==1L,'Other Languages',category))
language_display <- memberships$language %>% filter(category!='Not reported') %>%
  left_join(language_mapping, by='category') %>% transmute(record_id, category=display_category)
display_counts <- counts
display_counts$language <- language_display %>% count(category,name='n') %>%
  arrange(category=='Other Languages',desc(n),category) %>%
  mutate(denominator=sum(n),percent=100*n/denominator,label=sprintf('%s  %d (%.1f%%)',category,n,percent))
map_records <- d %>% transmute(record_id,country_raw, size,
  map_unit=recode(country_raw, 'Mainland China'='China','Hong Kong, China'='China',
    'Macao, China'='China','Taiwan, China'='China','UK'='United Kingdom'),
  map_eligible=!grepl(';',country_raw,fixed=TRUE) & country_raw!='Not Reported' & !is.na(size))
country_totals <- map_records %>% filter(!grepl(';',country_raw,fixed=TRUE),country_raw!='Not Reported') %>%
  group_by(map_unit) %>% summarise(records=n(),known_size_records=sum(!is.na(size)),
    missing_size_records=sum(is.na(size)),aggregated_size=sum(size,na.rm=TRUE),.groups='drop')
scatter <- d %>% arrange(year,number_of_factors,record_id) %>% group_by(year,number_of_factors) %>%
  mutate(year_display=year+if(n()==1L)0 else seq(-.28,.28,length.out=n())) %>% ungroup()

# Paper-cluster bootstrap of the descriptive year trend (the original seed and 2,000 draws).
fit <- lm(number_of_factors~year,data=d)
prediction <- data.frame(year=seq(min(d$year),max(d$year),length.out=150L))
prediction$fit <- as.numeric(predict(fit,prediction))
papers <- unique(d$paper_id)
paper_rows <- split(seq_len(nrow(d)), d$paper_id)
boot <- matrix(NA_real_,2,2000)
set.seed(20260926)
for (b in seq_len(ncol(boot))) {
  selected <- sample(papers,length(papers),replace=TRUE)
  sampled_rows <- unlist(paper_rows[selected], use.names=FALSE)
  boot[,b] <- coef(lm(number_of_factors~year, data=d[sampled_rows, ]))
}
predicted <- outer(prediction$year,boot[2,],'*')+matrix(boot[1,],nrow(prediction),ncol(boot),byrow=TRUE)
prediction$lower <- apply(predicted,1,quantile,.025,names=FALSE)
prediction$upper <- apply(predicted,1,quantile,.975,names=FALSE)
trend <- list(predictions=prediction,coefficients=coef(fit),slope_per_decade=unname(coef(fit)[2]*10),
  slope_ci_per_decade=unname(quantile(boot[2,]*10,c(.025,.975))),r_squared=summary(fit)$r.squared,
  records=nrow(d),papers=length(papers),bootstrap_draws=2000L,seed=20260926L,bootstrap_coefficients=boot)

# Table 1 follows the author's compact categories, including explicit pooled groups.
tab <- data.frame(Characteristic=c('Sample size, median [Q1–Q3]','Aggregated participants, n',
  'Sample mean age (years), median [Q1–Q3]','Male participants (%), median [Q1–Q3]',
  'Number of factors, median [Q1–Q3]'),Value='')
numeric_fields <- c('size','mean_age','male_pct','number_of_factors')
for (j in seq_along(numeric_fields)) {
  q <- quantile(d[[numeric_fields[j]]],c(.5,.25,.75),na.rm=TRUE,type=7,names=FALSE)
  z <- formatC(q,format='f',digits=c(0,1,1,0)[j])
  tab$Value[c(1,3,4,5)[j]] <- paste0(z[1],' [',z[2],'–',z[3],']')
}
tab$Value[2] <- format(sum(d$size,na.rm=TRUE),big.mark=',',trim=TRUE)
groups <- list(
  age_group=list('above 18 years'='adult','below 18 years'=c('adolescent','children and adolescents'),'mixed'='mixed'),
  sample_type=list('university students'='university students',
    'primary and/or secondary school students'=c('secondary school students','primary and secondary school students'),
    'general adults'='general adults','mixed'=c('mixed','mixed students','community','community; university students','clinical')),
  sampling=list('convenience'='convenience','cluster'=c('cluster','cluster random'),'multi-stage stratified'='multi-stage stratified'),
  recruitment=list('offline'='offline','online'='online'),
  research_class=list('Exploration and validation'='III','Exploration only'='I','Validation only'='II'),
  validation=list('single sample'='single sample','split-half'='split-half','independent samples'='independent samples'),
  scoring_points=list('5-point'='5-point','6-point'='6-point'))
titles <- c('Sample age group, n (%)','Participant type, n (%)','Sampling method, n (%)',
  'Recruitment channel, n (%)','Study design, n (%)','Factorial validation design','Y-IAT item scoring')
for (j in seq_along(groups)) {
  tab <- bind_rows(tab,data.frame(Characteristic=titles[j],Value=''))
  for (label in names(groups[[j]])) {
    n <- sum(d[[names(groups)[j]]] %in% groups[[j]][[label]])
    tab <- bind_rows(tab,data.frame(Characteristic=label,Value=sprintf('%d (%.1f%%)',n,100*n/nrow(d))))
  }
}
# Material for Step 3 contains data and statistical summaries, never plotting functions.
figure_data <- list(Data=d,scatter=scatter,memberships=memberships,counts=counts,display_counts=display_counts,
  language_mapping=language_mapping,map_records=map_records,country_totals=country_totals,
  map_exceptions=filter(map_records,!map_eligible),trend=trend,
  totals=list(records=nrow(d),papers=n_distinct(d$paper_id),language_mentions=nrow(memberships$language),
    known_languages=sum(counts$language$category!='Not reported'),plotted_language_mentions=nrow(language_display),
    excluded_language_mentions=sum(memberships$language$category=='Not reported'),
    singleton_languages=sum(language_mapping$n==1L),
    singleton_language_records=n_distinct(language_display$record_id[language_display$category=='Other Languages']),
    region_mentions=nrow(memberships$region),known_regions=sum(counts$region$category!='Not reported'),
    known_size=sum(!is.na(d$size)),mapped_size=sum(map_records$size[map_records$map_eligible]),
    unallocated_multicountry_size=sum(map_records$size[!map_records$map_eligible])))
saveRDS(figure_data,file.path(out,'Figure_3_StudyCharacteristics.rds'))

# Export column names and table values with default worksheet formatting.
wb <- openxlsx::createWorkbook(creator='Kunru Song')
openxlsx::addWorksheet(wb,'Table 1')
openxlsx::writeData(wb,'Table 1',tab,headerStyle=NULL)
# Remove empty drawing references created by openxlsx 4.2.8.1.
wb$worksheets[[1]]$drawing <- character()
wb$worksheets[[1]]$legacyDrawing <- character()
wb$worksheets_rels[[1]] <- wb$worksheets_rels[[1]][!grepl('/(drawing|vmlDrawing)"',wb$worksheets_rels[[1]])]
openxlsx::saveWorkbook(wb,file.path(out,'Table_1_StudyCharacteristics.xlsx'),overwrite=TRUE)

# Official map source layers are handed to Step 3 without plotting here.
map_sources <- list()
paths <- list.files(file.path(input_dir,'MapSources'),pattern='^MNR_countries_[0-9]+_[0-9]+[.]json$',full.names=TRUE)
countries <- list()
for(p in paths) countries <- c(countries,jsonlite::read_json(p,simplifyVector=FALSE)$recordsets[[1]]$features)
map_sources$countries <- countries
for(layer in c('islands','coastlines','borders','undetermined','region_boundaries','ceasefire'))
  map_sources[[layer]] <- jsonlite::read_json(file.path(input_dir,'MapSources',paste0('MNR_',layer,'.json')),simplifyVector=FALSE)$recordsets[[1]]$features
saveRDS(map_sources,file.path(out,'Figure_3_Map.rds'))
