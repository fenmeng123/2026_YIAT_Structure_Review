# Author: Kunru Song
# Exploratory adjacent-category ordinal regression and multiple linear regression.
library(dplyr)
library(openxlsx)
.libPaths(c('Res_2_Results/S1_6_ExploratoryRegression/Runtime/R_library',.libPaths()))
library(brglm2)
library(clubSandwich)
input_dir <- 'Res_1_Data_for_analyses'
out <- 'Res_2_Results/S1_6_ExploratoryRegression'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
d <- readRDS(file.path(input_dir,'Dat_SysRev_Data_Records.rds'))
s <- readRDS(file.path(input_dir,'Dat_SysRev_Linked_Stages.rds'))
model <- d %>% transmute(record_id=sprintf('D%03d',row_number()),paper_id,
  k=as.integer(number_of_factors),sample_size_1000=size/1000,
  male_group=factor(case_when(is.na(male_pct) ~ 'Missing',male_pct<40 ~ '<40%',male_pct>60 ~ '>60%',
    TRUE ~ '40%-60%'),levels=c('40%-60%','<40%','>60%','Missing')))
maps <- list(study_design=c(III='Exploration and validation',I='Exploration only',II='Validation only'),
  validation_design=c('single sample'='single sample','split-half'='split-half','independent samples'='independent samples'),
  participant_type=c('university students'='university students',
    'secondary school students'='primary and/or secondary school students',
    'primary and secondary school students'='primary and/or secondary school students','general adults'='general adults',
    mixed='mixed','mixed students'='mixed',community='mixed','community; university students'='mixed',clinical='mixed'),
  sampling_method=c(convenience='convenience',cluster='cluster','cluster random'='cluster','multi-stage stratified'='multi-stage stratified'),
  item_scoring=c('5-point'='0','6-point'='1'),
  rotation=c(varimax='Orthogonal',promax='Oblique',oblimin='Oblique',quartimin='Oblique',geomin='Oblique',
    'weighted oblimin'='Oblique','oblique (unspecified)'='Oblique',
    'oblique primary factors; Schmid-Leiman transformation'='Oblique',target='Missing'))
fields <- c(study_design='research_class',validation_design='validation',participant_type='sample_type',
  sampling_method='sampling',item_scoring='scoring_points',rotation='rotation')
missing_labels <- c('Not Reported','Not Applicable','Missing (blank)','NA (unspecified)','Unclear','none','')
for (variable in names(maps)) {
  raw <- as.character(d[[fields[[variable]]]])
  value <- unname(maps[[variable]][raw])
  value[is.na(raw) | raw %in% missing_labels] <- 'Missing'
  if (anyNA(value)) stop('Unmapped regression category: ',variable)
  model[[variable]] <- factor(value,levels=unique(c(unname(maps[[variable]]),'Missing')))
}
extraction_map <- c(PCA='PCA',PAF='PAF',ML='ML',MLR='ML',MLM='ML',`ML-SB`='ML',GLS='Others',
  WLSMV='WLS',DWLS='WLS',RDWLS='WLS',WLS='WLS',MRFA='Others',`Not Reported`='Missing')
groups <- list(retention_kaiser='Kaiser criterion (eigenvalue > 1)',retention_scree='scree plot',
  retention_parallel='parallel analysis',retention_map='MAP',retention_fit=c('fit indices','Hull method'),
  retention_models=c('model comparison','fixed maximum three factors'),
  retention_interpret=c('factor loadings','interpretability'))
extraction <- rep('Missing',nrow(d)); retention <- vector('list',nrow(d))
for (i in seq_len(nrow(d))) {
  z <- s %>% filter(data_row==i)
  e <- z %>% filter(stage_role=='Exploration')
  raw <- as.character(d$extraction[i]); tokens <- unique(trimws(strsplit(raw,';',fixed=TRUE)[[1]]))
  if (!is.na(raw) && !raw %in% missing_labels) {
    if (length(tokens)>1L) {
      candidates <- unique(unname(extraction_map[as.character(e$estimator)])) %>% setdiff('Missing')
      if (length(candidates)==1L) extraction[i] <- candidates
    } else extraction[i] <- unname(extraction_map[tokens])
  }
  ret <- as.character(d$retention[i])
  if (ret=='Stage-specific; see Stages') ret <- unique(as.character(z$record_retention))
  retention[[i]] <- if (anyNA(ret) || all(ret %in% missing_labels)) 'Missing' else
    sort(unique(trimws(unlist(strsplit(ret[!ret %in% missing_labels],';',fixed=TRUE)))))
}
model$extraction <- factor(extraction,levels=c('PCA','PAF','ML','WLS','Others','Missing'))
for (variable in names(groups)) {
  value <- rep('No',nrow(d))
  for (i in seq_len(nrow(d))) if (any(retention[[i]] %in% groups[[variable]])) value[i] <- 'Yes'
  model[[variable]] <- factor(value,levels=c('No','Yes'))
}
predictors <- c('sample_size_1000','male_group','study_design','validation_design','participant_type',
  'sampling_method','item_scoring','extraction',names(groups),'rotation')
# Author-requested post hoc exclusion applies only here. Unreported retention is seven zeros.
analysis <- model %>% filter(!is.na(sample_size_1000),k!=7L)
observed <- droplevels(analysis)
observed$k_ordered <- ordered(observed$k,levels=sort(unique(observed$k)))
linear_formula <- reformulate(predictors,response='k')
ordinal_formula <- reformulate(predictors,response='k_ordered')

# Multiple linear regression: adjusted mean factor-count differences, clustered by paper.
linear <- lm(linear_formula,data=observed,x=TRUE,y=TRUE,na.action=na.fail)
V_CR2 <- vcovCR(linear,cluster=observed$paper_id,type='CR2')
linear_test <- coef_test(linear,vcov=V_CR2,test='Satterthwaite') %>% as.data.frame()
linear_ci <- conf_int(linear,vcov=V_CR2,test='Satterthwaite',level=.95) %>% as.data.frame()

# Mean bias reduction supplies finite adjacent-category estimates under separation.
# Zero weights retain outcome levels when a paper is omitted from the jackknife.
papers <- unique(observed$paper_id)
leaveout <- NULL
for (i in 0:length(papers)) {
  observed$jack_weight <- if (i==0L) rep(1,nrow(observed)) else as.numeric(observed$paper_id!=papers[i])
  ordinal <- bracl(ordinal_formula,data=observed,weights=jack_weight,parallel=TRUE,
    type='AS_mean',maxit=1500,slowit=.5,epsilon=1e-8,na.action=na.fail)
  if (!isTRUE(ordinal$converged)) ordinal <- bracl(ordinal_formula,data=observed,weights=jack_weight,
    parallel=TRUE,type='AS_mean',maxit=3000,slowit=.2,epsilon=1e-8,na.action=na.fail)
  if (!isTRUE(ordinal$converged)) stop('Ordinal fit did not converge for jackknife index ',i)
  if (i==0L) {
    ordinal_full <- ordinal
    leaveout <- matrix(NA_real_,length(papers),length(coef(ordinal)),dimnames=list(papers,names(coef(ordinal))))
  } else leaveout[i,] <- coef(ordinal)
}
centered <- sweep(leaveout,2,colMeans(leaveout))
V_JK <- (length(papers)-1)/length(papers)*crossprod(centered)
# bracl uses lower/higher odds: negate coefficients to report higher/lower odds.
beta <- -coef(ordinal_full); se <- sqrt(diag(V_JK)); df <- length(papers)-1L
ordinal_result <- data.frame(term=names(beta),B=as.numeric(beta),SE=se,
  OR=exp(beta),OR_low=exp(beta-qt(.975,df)*se),OR_high=exp(beta+qt(.975,df)*se),
  p=2*pt(-abs(beta/se),df),row.names=NULL)

# One coefficient row per requested category, including unused Missing levels.
labels <- c(sample_size_1000='Sample size (per 1,000 participants)',male_group='Male participants (%)',
  study_design='Study design',validation_design='Validation design',participant_type='Participant type',
  sampling_method='Sampling method',item_scoring='Y-IAT scoring',extraction='Extraction',
  retention_kaiser='Retention: Kaiser criterion',retention_scree='Retention: scree plot',
  retention_parallel='Retention: parallel analysis',retention_map='Retention: minimum average partial',
  retention_fit='Retention: goodness-of-fit indices',retention_models='Retention: dimensionality model comparison',
  retention_interpret='Retention: loadings and interpretability',rotation='Rotation')
rowinfo <- data.frame(Predictor='Intercept',Contrast='All reference categories; sample size = 0',
  n=nrow(analysis),term='(Intercept)',reference=FALSE)
for (variable in predictors) {
  levs <- if (is.factor(analysis[[variable]])) levels(analysis[[variable]]) else 'Continuous'
  for (j in seq_along(levs)) {
    level <- levs[j]; label <- level; categorical <- is.factor(analysis[[variable]])
    if (variable=='item_scoring') label <- recode(level,'0'='5-point (0)','1'='6-point (1)')
    if (variable=='extraction') label <- recode(level,PCA='Principal component analysis (PCA)',
      PAF='Principal axis factoring (PAF)',ML='Maximum likelihood factoring',
      WLS='Weighted least squares factoring',Others='Others (GLS / MRFA)')
    rowinfo <- bind_rows(rowinfo,data.frame(Predictor=labels[[variable]],Contrast=label,
      n=if(categorical)sum(analysis[[variable]]==level) else nrow(analysis),
      term=if(categorical)paste0(variable,level) else variable,reference=categorical && j==1L))
  }
}
linear_table <- rowinfo %>% select(Predictor,Contrast,n) %>% mutate(n=as.character(n),
  `B (factors)`='NE',`SE (CR2)`='NE',`95% CI`='NE',`df (Satt.)`='NE',p='NE',Status='No observations')
for (i in seq_len(nrow(rowinfo))) {
  if (rowinfo$reference[i]) {
    linear_table[i,4:8] <- list('Ref.','—','—','—','—'); linear_table$Status[i] <- 'Reference'; next
  }
  j <- match(rowinfo$term[i],linear_test$Coef)
  if (is.na(j)) next
  linear_table[i,4:8] <- list(sprintf('%.3f',linear_test$beta[j]),sprintf('%.3f',linear_test$SE[j]),
    sprintf('[%.3f, %.3f]',linear_ci$CI_L[j],linear_ci$CI_U[j]),sprintf('%.2f',linear_test$df_Satt[j]),
    if(linear_test$p_Satt[j]<.001)'<0.001' else sprintf('%.3f',linear_test$p_Satt[j]))
  linear_table$Status[i] <- 'Estimated'
}
ordinal_rows <- bind_rows(data.frame(Predictor='Adjacent intercept',Contrast=paste0(2:6,' versus ',1:5),
  n=nrow(analysis),term=paste0(1:5,':(Intercept)'),reference=FALSE),filter(rowinfo,term!='(Intercept)'))
ordinal_table <- ordinal_rows %>% select(Predictor,Contrast,n) %>% mutate(n=as.character(n),
  `B (log OR)`='NE',`SE (paper JK)`='NE',`OR (higher k)`='NE',`95% CI (OR)`='NE',p='NE',Status='No observations')
for (i in seq_len(nrow(ordinal_rows))) {
  if (ordinal_rows$reference[i]) {
    ordinal_table[i,4:8] <- list('Ref.','—','1.000','—','—'); ordinal_table$Status[i] <- 'Reference'; next
  }
  j <- match(ordinal_rows$term[i],ordinal_result$term)
  if (is.na(j)) next
  z <- ordinal_result[j,]; values <- c(z$OR,z$OR_low,z$OR_high); formatted <- character(3)
  for (h in 1:3) formatted[h] <- if(values[h]>=1e4 || values[h]<1e-3) formatC(values[h],format='e',digits=2) else sprintf('%.3f',values[h])
  ordinal_table[i,4:8] <- list(sprintf('%.3f',z$B),sprintf('%.3f',z$SE),formatted[1],
    paste0('[',formatted[2],', ',formatted[3],']'),if(z$p<.001)'<0.001' else sprintf('%.3f',z$p))
  ordinal_table$Status[i] <- 'Bias-reduced estimate'
}
tables <- list('1_Ordinal_bias_reduced'=ordinal_table,'2_Multiple_linear'=linear_table)

# Export existing statistical sheets with default worksheet formatting.
wb <- createWorkbook(creator='Kunru Song')
for (sheet in names(tables)) {
  addWorksheet(wb,sheet)
  writeData(wb,sheet,tables[[sheet]],headerStyle=NULL)
}
# Remove empty drawing references created by openxlsx 4.2.8.1.
for (i in seq_along(wb$worksheets)) {
  wb$worksheets[[i]]$drawing <- character()
  wb$worksheets[[i]]$legacyDrawing <- character()
  wb$worksheets_rels[[i]] <- wb$worksheets_rels[[i]][!grepl('/(drawing|vmlDrawing)"',wb$worksheets_rels[[i]])]
}
saveWorkbook(wb,file.path(out,'Table_S1_8_01_ExploratoryRegression.xlsx'),overwrite=TRUE)
