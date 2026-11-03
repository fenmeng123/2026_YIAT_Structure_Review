# Author: Kunru Song
# Fixed-condition agreement across all 380 item-by-construct cosine cells.
library(dplyr)
library(openxlsx)
input_dir <- 'Res_1_Data_for_analyses'; out <- 'Res_2_Results/S2_5_CorrelationOfCosineSimilarity'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
spec <- data.frame(key=c('main','S1','S2','S3','S4'),figure=c('Figure 4',paste('Figure',paste0('S',1:4))),
  model=c('personality','mpnet','harmony','personality','multilingual_mpnet'),
  version=c(rep('MAK_YOUNG_2020',3),'ORIGINAL','CHINESE'),
  comparison_type=c('Reference','Encoder','Encoder','Wording','Language, wording and encoder'))
inputs <- readRDS(file.path(input_dir,'Semantic_Inputs.rds')); criteria <- inputs$domains
matrices <- list()
for (i in seq_len(nrow(spec))) {
  e <- readRDS(file.path(input_dir,paste0('embeddings_formal_',spec$model[i],'_',spec$version[i],'_definition.rds')))
  X <- do.call(rbind,e$items$vector); X <- X/sqrt(rowSums(X^2)); rownames(X) <- e$items$item_id
  D <- do.call(rbind,e$domains$vector); D <- D/sqrt(rowSums(D^2)); rownames(D) <- e$domains$domain_id
  matrices[[spec$key[i]]] <- (X %*% t(D))[paste0('IAT_',1:20),criteria$domain_id]
}
scores <- expand.grid(item_id=paste0('IAT_',1:20),construct_id=criteria$domain_id,KEEP.OUT.ATTRS=FALSE,stringsAsFactors=FALSE) %>%
  mutate(framework=criteria$framework[match(construct_id,criteria$domain_id)],
    construct_label=criteria$Standardized_Label[match(construct_id,criteria$domain_id)])
for (key in spec$key) scores[[key]] <- as.vector(matrices[[key]])
RNGkind('Mersenne-Twister','Inversion','Rejection'); set.seed(20261016)
plan <- matrix(NA_integer_,99999,20)
for (b in seq_len(nrow(plan))) plan[b,] <- sample.int(20)
comparisons <- details <- list()
x <- matrices$main
for (i in 1:4) {
  key <- paste0('S',i); y <- matrices[[key]]; pair <- cbind(as.vector(x),as.vector(y))
  grand <- mean(pair); target <- rowMeans(pair); condition <- colMeans(pair)
  residual <- sweep(sweep(pair,1,target),2,condition)+grand
  MS_target <- 2*sum((target-grand)^2)/(nrow(pair)-1)
  MS_error <- sum(residual^2)/(nrow(pair)-1)
  icc <- (MS_target-MS_error)/(MS_target+MS_error)
  rho <- cor.test(pair[,1],pair[,2],method='spearman',alternative='two.sided',exact=FALSE,continuity=FALSE)
  # Permute complete item profiles, preserving all 19 construct columns together.
  xc <- x-mean(x); yc <- y-mean(y)
  xr <- matrix(rank(as.vector(x),ties.method='average'),20); xr <- xr-mean(xr)
  yr <- matrix(rank(as.vector(y),ties.method='average'),20); yr <- yr-mean(yr)
  raw_cross <- tcrossprod(xc,yc); rank_cross <- tcrossprod(xr,yr)
  indices <- cbind(rep(1:20,each=nrow(plan)),as.vector(plan))
  raw_sum <- rowSums(matrix(raw_cross[indices],nrow=nrow(plan)))
  rank_sum <- rowSums(matrix(rank_cross[indices],nrow=nrow(plan)))
  null_icc <- 2*raw_sum/(sum(xc^2)+sum(yc^2))
  null_rho <- rank_sum/sqrt(sum(xr^2)*sum(yr^2))
  upper_icc <- sum(null_icc>=icc-1e-12)
  upper_rho <- sum(null_rho>=unname(rho$estimate)-1e-12)
  lower_rho <- sum(null_rho<=unname(rho$estimate)+1e-12)
  comparisons[[key]] <- data.frame(comparison=paste('Figure 4 vs Figure',key),n_cells=nrow(pair),n_conditions=2L,
    ICC_C1=icc,ICC_p_F_nominal=pf(MS_target/MS_error,379,379,lower.tail=FALSE),
    ICC_p_item_permutation=(upper_icc+1)/(nrow(plan)+1),Spearman_rho=unname(rho$estimate),
    Spearman_p_asymptotic_nominal=rho$p.value,
    Spearman_p_item_permutation=min(1,2*min(upper_rho+1,lower_rho+1)/(nrow(plan)+1)),
    comparison_type=spec$comparison_type[i+1L])
  details[[key]] <- data.frame(comparison=comparisons[[key]]$comparison,ICC_F=MS_target/MS_error,
    ICC_df1=379L,ICC_df2=379L,Spearman_t=unname(rho$estimate)*sqrt(378/(1-unname(rho$estimate)^2)),
    Spearman_df=378L,permutations=nrow(plan),seed=20261016L,ICC_perm_upper_count=upper_icc,
    Spearman_perm_upper_count=upper_rho,Spearman_perm_lower_count=lower_rho,
    ICC_perm_null_mean=mean(null_icc),Spearman_perm_null_mean=mean(null_rho))
}
tables <- list(Comparisons=bind_rows(comparisons),Cosine_scores=scores,Conditions=spec,
  Benchmark_dictionary=criteria,Inference_details=bind_rows(details))

# Export existing statistical sheets with default worksheet formatting.
wb <- createWorkbook(creator='Kunru Song')
for (sheet in names(tables)) {
  addWorksheet(wb,sheet)
  writeData(wb,sheet,tables[[sheet]],headerStyle=NULL)
}
# Keep the existing Notes tab empty to preserve the workbook structure.
addWorksheet(wb,'Notes')
# Remove empty drawing references created by openxlsx 4.2.8.1.
for (i in seq_along(wb$worksheets)) {
  wb$worksheets[[i]]$drawing <- character()
  wb$worksheets[[i]]$legacyDrawing <- character()
  wb$worksheets_rels[[i]] <- wb$worksheets_rels[[i]][!grepl('/(drawing|vmlDrawing)"',wb$worksheets_rels[[i]])]
}
saveWorkbook(wb,file.path(out,'Table_S2_05_CorrelationOfCosineSimilarity.xlsx'),overwrite=TRUE)
