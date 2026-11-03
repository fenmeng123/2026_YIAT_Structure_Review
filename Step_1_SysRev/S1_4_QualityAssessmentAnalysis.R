# Author: Kunru Song
# Analysis-level quality distributions, compact Table 2 and Figure 3 panel data.
# Figure 3: Geographical and language coverage, reported factor numbers, and quality assessment.
library(dplyr)
library(tidyr)
library(openxlsx)
input_dir <- 'Res_1_Data_for_analyses'
out <- 'Res_2_Results/S1_4_QualityAssessmentAnalysis'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
records <- readRDS(file.path(input_dir,'Dat_SysRev_Data_Records.rds'))
stages <- readRDS(file.path(input_dir,'Dat_SysRev_Linked_Stages.rds')) %>%
  mutate(across(where(is.factor),as.character))

# Original item scores remain separate; no total quality score is calculated.
item_labels <- c(M1='Input treatment',M2='Factor extraction',
  M3='Factor retention',M4='Rotation method',R1='Analysis sample and item set',
  R2='Scale and analytical input',R3='Method implementation',R4='Data preparation and rationale',
  R5='Selection / prespecification',R6='Rotation reporting',R7='Loadings and factor relations',
  R8='Fit evaluation',R9='Deletions and modifications')
quality_long <- stages %>% select(stage_id,paper_id,stage_role,all_of(names(item_labels))) %>%
  pivot_longer(all_of(names(item_labels)),names_to='criterion',values_to='score')
quality_summary <- quality_long %>% group_by(stage_role,criterion) %>%
  summarise(total=n(),applicable=sum(score!='Not Applicable'),n0=sum(score=='0'),
    n1=sum(score=='1'),n2=sum(score=='2'),n_na=sum(score=='Not Applicable'),papers=n_distinct(paper_id),.groups='drop')
saveRDS(list(summary=quality_summary,labels=item_labels,stages=stages),file.path(out,'Figure_3_QualityAssessment.rds'))

# Table 2: one row per exploration stage; linked-record retention may include CFA criteria.
d <- stages %>% filter(stage_role=='Exploration') %>%
  mutate(record_rotation=as.character(records$rotation[data_row]),
    estimator_detail=case_when(estimator %in% c('PCA','PAF','MRFA') ~ 'Algorithm only',TRUE ~ estimator),
    extraction_detail=case_when(estimator %in% c('ML','MLM','ML-SB','MLR') ~ 'ML',
      estimator %in% c('WLSMV','RDWLS','DWLS') ~ 'WLS',TRUE ~ estimator),
    rotation_detail=case_when(rotation=='Oblimin' ~ 'oblimin',
      rotation=='Others or unspecified' ~ 'Others or unspecified',
      rotation=='Oblique to Schmid-Leiman' ~ 'Schmid-Leiman',
      rotation %in% c('Orthogonal','Oblique') ~ record_rotation,TRUE ~ rotation))
criteria <- c('Kaiser criterion (eigenvalue > 1)','scree plot','parallel analysis','MAP','Hull method',
  'fit indices','model comparison','factor loadings','interpretability','fixed maximum three factors')
tokens <- strsplit(d$record_retention,';',fixed=TRUE)
membership <- matrix(FALSE,nrow(d),length(criteria),dimnames=list(d$stage_id,criteria))
for (i in seq_along(tokens)) membership[i,] <- criteria %in% trimws(tokens[[i]])
d$retention_count <- ifelse(d$record_retention %in% c('Not Reported','Not Applicable'),
  'Undetermined',as.character(rowSums(membership)))

sections <- list(
  'Analytical data type'=list(field='input_treatment',groups=list('Ordinal'='Ordinal explicit',
    'Continuous'='Continuous explicit','Not reported'='Unclear')),
  'Estimator'=list(field='estimator_detail',groups=list(
    'Extraction algorithm only (PCA / PAF / MRFA)'='Algorithm only','Maximum likelihood (ML)'='ML',
    'Robust maximum likelihood and variants'=c('ML-SB','MLM','MLR'),
    'WLSMV and variants'=c('WLSMV','RDWLS'),'Generalized least squares (GLS)'='GLS','Not reported'='Not Reported')),
  'Extraction'=list(field='extraction_detail',groups=list(
    'Principal component analysis (PCA)'='PCA','Principal axis factoring (PAF)'='PAF',
    'Maximum likelihood factoring'='ML','Generalized least squares factoring'='GLS',
    'Weighted least squares factoring'='WLS','Minimum rank factor analysis (MRFA)'='MRFA','Not reported'='Not Reported')),
  'Number of applied factor retention approaches'=list(field='retention_count',groups=list(
    'One'='1','Two'='2','Three'='3','Four'='4','Five'='5','Not reported'='Undetermined')),
  'Retention'=list(field='record_retention',groups=list(
    'Kaiser criterion (eigenvalue > 1)'='Kaiser criterion (eigenvalue > 1)','Scree plot'='scree plot',
    'parallel analysis'='parallel analysis','minimum average partial'='MAP',
    'goodness-of-fit indices'=c('fit indices','Hull method'),
    'comparison of dimensionality models'=c('model comparison','fixed maximum three factors'),
    'factor loadings and interpretability'=c('factor loadings','interpretability'))),
  'Rotation'=list(field='rotation_detail',groups=list('Orthogonal: Varimax'='varimax',
    'Oblique'=c('oblimin','weighted oblimin','promax','quartimin','geomin','oblique (unspecified)','Schmid-Leiman'),
    'Oblimin'=c('oblimin','weighted oblimin'),'Promax'='promax','Quartimin'='quartimin','Geomin'='geomin',
    'Unspecified oblique rotation'=c('oblique (unspecified)','Schmid-Leiman'),
    'Others or unspecified'='Others or unspecified','Not reported'='Not Reported','Not Applicable'='Not Applicable')))
rows <- list()
for (section in names(sections)) {
  rows[[length(rows)+1L]] <- c(section,'','')
  specification <- sections[[section]]
  for (label in names(specification$groups)) {
    categories <- specification$groups[[label]]
    hit <- if (section=='Retention') rowSums(membership[,categories,drop=FALSE])>0 else d[[specification$field]] %in% categories
    n <- sum(hit)
    rows[[length(rows)+1L]] <- c('',label,sprintf('%d (%.1f%%)',n,100*n/nrow(d)))
  }
}
tab <- as.data.frame(do.call(rbind,rows),stringsAsFactors=FALSE)
names(tab) <- c('Methodological aspect','Method / criterion',sprintf('Exploration, n = %d',nrow(d)))

# Export column names and table values with default worksheet formatting.
wb <- openxlsx::createWorkbook(creator='Kunru Song')
openxlsx::addWorksheet(wb,'Table 2')
openxlsx::writeData(wb,'Table 2',tab,headerStyle=NULL)
# Remove empty drawing references created by openxlsx 4.2.8.1.
wb$worksheets[[1]]$drawing <- character()
wb$worksheets[[1]]$legacyDrawing <- character()
wb$worksheets_rels[[1]] <- wb$worksheets_rels[[1]][!grepl('/(drawing|vmlDrawing)"',wb$worksheets_rels[[1]])]
openxlsx::saveWorkbook(wb,file.path(out,'Table_2_MethodologicalIssues.xlsx'),overwrite=TRUE)
