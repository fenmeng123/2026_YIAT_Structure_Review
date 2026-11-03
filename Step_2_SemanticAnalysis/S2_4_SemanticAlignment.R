# Author: Kunru Song
# Semantic alignment and correspondence with published item coassignment.
library(dplyr)
library(tidyr)
library(openxlsx)
input_dir <- 'Res_1_Data_for_analyses'; out <- 'Res_2_Results/S2_4_SemanticAlignment'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
inputs <- readRDS(file.path(input_dir,'Semantic_Inputs.rds'))
literature <- readRDS(file.path(input_dir,'Dat_Literature_Cooccurrence.rds'))
spec <- data.frame(key=c('main','S1','S2','S3','S4'),
  model=c('personality','mpnet','harmony','personality','multilingual_mpnet'),
  version=c(rep('MAK_YOUNG_2020',3),'ORIGINAL','CHINESE'))
mats <- literature$results$main$matrices; P <- mats$paper_proportion; rates <- mats$paper_rates
domains <- inputs$domains; results <- list(); table_rows <- list()
table_names <- c('Item_criterion_cosines','Item_pair_cosines','Concordance','Corpus_intervals','Item_rankings',
  'Relative_assignment','Literature_sensitivity','Item_influence','Paper_influence','Items')
for (name in table_names) table_rows[[name]] <- list()
for (branch in seq_len(nrow(spec))) {
  key <- spec$key[branch]; model <- spec$model[branch]; version <- spec$version[branch]
  e <- readRDS(file.path(input_dir,paste0('embeddings_formal_',model,'_',version,'_definition.rds')))
  X <- do.call(rbind,e$items$vector); X <- X/sqrt(rowSums(X^2)); rownames(X) <- e$items$item_id
  D <- do.call(rbind,e$domains$vector); D <- D/sqrt(rowSums(D^2)); rownames(D) <- e$domains$domain_id
  A <- X %*% t(D); S <- tcrossprod(X); items <- inputs$versions[[version]]
  rankings <- coverage <- list()
  for (framework in unique(domains$framework)) {
    jj <- which(domains$framework==framework); scores <- A[,jj,drop=FALSE]
    ties <- sweep(scores,1,apply(scores,1,max),'-')>=-1e-8
    for (i in seq_len(nrow(scores))) {
      ord <- order(-scores[i,],colnames(scores)); winners <- which(ties[i,])
      top2 <- which(scores[i,]>=scores[i,ord[2]]-1e-8)
      second <- which(abs(scores[i,]-scores[i,ord[2]])<=1e-8)
      rankings[[length(rankings)+1L]] <- data.frame(item_id=items$item_id[i],framework,
        highest_alignment=scores[i,ord[1]],second_alignment=scores[i,ord[2]],
        top1_top2_margin=scores[i,ord[1]]-scores[i,ord[2]],
        top1_criteria=paste(colnames(scores)[winners],collapse='; '),top1_ties=length(winners),
        top2_criteria=paste(sort(colnames(scores)[top2]),collapse='; '),top2_set_size=length(top2),
        second_criterion=paste(sort(colnames(scores)[second]),collapse='; '))
    }
    for (j in seq_along(jj)) coverage[[length(coverage)+1L]] <- data.frame(framework,
      domain_id=domains$domain_id[jj[j]],domain_label=domains$domain_label[jj[j]],criterion_type=domains$criterion_type[jj[j]],
      relative_top1_count=sum(ties[,j]/rowSums(ties)),relative_top1_fraction=sum(ties[,j]/rowSums(ties))/nrow(scores),
      top1_including_ties=sum(ties[,j]),mean_alignment=mean(scores[,j]),median_alignment=median(scores[,j]),
      max_alignment=max(scores[,j]),highest_alignment_items=paste(items$item_id[max(scores[,j])-scores[,j]<=1e-8],collapse='; '))
  }
  # Whole-item QAP: the same permutation acts on both axes of the literature matrix.
  upper <- upper.tri(S); rho <- cor(S[upper],P[upper],method='spearman')
  pearson <- cor(S[upper],P[upper]); null <- numeric(4999)
  set.seed(20261013)
  for (b in seq_along(null)) {
    permutation <- sample.int(nrow(P)); permuted <- P[permutation,permutation]
    null[b] <- cor(S[upper],permuted[upper],method='spearman')
  }
  qap_p <- (1+sum(abs(null)>=abs(rho)))/(length(null)+1L)
  # Paper-cluster resampling retains all correlated records and item pairs within a paper.
  set.seed(20261014)
  sampled_indices <- matrix(NA_integer_,1000,nrow(rates))
  draws <- matrix(NA_real_,1000,2,dimnames=list(NULL,c('spearman','pearson_descriptive')))
  bootstrap_coefficients <- matrix(NA_real_,1000,2)
  x <- unname(P[upper]); y <- unname(S[upper])
  coefficients <- lm.fit(cbind(`(Intercept)`=1,coassignment=x),y)$coefficients
  for (b in seq_len(nrow(draws))) {
    sampled <- sample.int(nrow(rates),nrow(rates),replace=TRUE); sampled_indices[b,] <- sampled
    sampled_rates <- rates[sampled,,drop=FALSE]
    Pb <- matrix(colSums(sampled_rates,na.rm=TRUE)/colSums(is.finite(sampled_rates)),20,20)
    draws[b,1] <- cor(S[upper],Pb[upper],method='spearman'); draws[b,2] <- cor(S[upper],Pb[upper])
    bootstrap_coefficients[b,] <- lm.fit(cbind(`(Intercept)`=1,coassignment=unname(Pb[upper])),y)$coefficients
  }
  grid_x <- seq(min(x),max(x),length.out=200)
  prediction <- outer(bootstrap_coefficients[,1],rep(1,length(grid_x)))+outer(bootstrap_coefficients[,2],grid_x)
  interval <- apply(prediction,2,quantile,probs=c(.025,.975),names=FALSE,type=7)
  trend <- data.frame(x=grid_x,fit=unname(coefficients[1]+coefficients[2]*grid_x),lower=interval[1,],upper=interval[2,])
  intervals <- list()
  for (j in 1:2) {
    q <- quantile(draws[,j],c(.025,.975),names=FALSE)
    intervals[[j]] <- data.frame(metric=colnames(draws)[j],attempted_replicates=1000L,finite_replicates=sum(is.finite(draws[,j])),
      percentile_lower=q[1],percentile_upper=q[2],interpretation='Paper-cluster bootstrap corpus interval; semantic embeddings held fixed')
  }
  item_influence <- paper_influence <- list()
  for (i in 1:20) {
    Si <- S[-i,-i]; Pi <- P[-i,-i]; u <- upper.tri(Si)
    ri <- cor(Si[u],Pi[u],method='spearman')
    item_influence[[i]] <- data.frame(omitted_item=rownames(S)[i],n_remaining_items=19L,spearman=ri,
      change_from_full=ri-rho,pearson_descriptive=cor(Si[u],Pi[u]))
  }
  for (i in seq_len(nrow(rates))) {
    other <- rates[-i,,drop=FALSE]; Pi <- matrix(colSums(other,na.rm=TRUE)/colSums(is.finite(other)),20,20)
    ri <- cor(S[upper],Pi[upper],method='spearman')
    paper_influence[[i]] <- data.frame(omitted_paper=rownames(rates)[i],n_remaining_papers=nrow(rates)-1L,
      spearman=ri,change_from_full=ri-rho,pearson_descriptive=cor(S[upper],Pi[upper]))
  }
  variants <- list(All_records_paper_equal=P,All_records_record_equal=mats$record_proportion,
    Exclude_one_group_paper_equal=literature$results$multifactor$matrices$paper_proportion,
    Retained_only_legacy_denominator=literature$results$Retained_only_legacy_denominator$matrices$paper_proportion)
  sensitivity <- list()
  for (name in names(variants)) {
    v <- variants[[name]]; ok <- upper & is.finite(v)
    sensitivity[[name]] <- data.frame(scenario=name,n_item_pairs=sum(ok),spearman=cor(S[ok],v[ok],method='spearman'),
      pearson_descriptive=cor(S[ok],v[ok]),status='Descriptive denominator/weighting sensitivity; not used for model selection')
  }
  ij <- which(upper,arr.ind=TRUE)
  pairs <- data.frame(item_i=rownames(S)[ij[,1]],item_j=rownames(S)[ij[,2]],semantic_cosine=S[ij],
    literature_coassignment_paper_equal=P[ij],literature_coassignment_record_equal=mats$record_proportion[ij],
    same_factor_records=mats$raw_count[ij],available_records=mats$record_denominator[ij],available_papers=mats$paper_denominator[ij]) %>%
    mutate(semantic_rank=rank(-semantic_cosine,ties.method='average'),
      coassignment_rank=rank(-literature_coassignment_paper_equal,ties.method='average'),
      signed_rank_difference=semantic_rank-coassignment_rank)
  alignment_cells <- expand.grid(item_id=rownames(A),domain_id=colnames(A),stringsAsFactors=FALSE) %>%
    mutate(framework=domains$framework[match(domain_id,domains$domain_id)],
      Standardized_Label=domains$Standardized_Label[match(domain_id,domains$domain_id)],cosine=as.vector(A))
  concordance <- data.frame(spearman=rho,pearson_descriptive=pearson,n_item_pairs=sum(upper),qap_two_sided_p=qap_p,
    attempted_permutations=length(null),finite_permutations=sum(is.finite(null)),n_records=mats$n_records,n_papers=mats$n_papers)
  branch_tables <- list(Item_criterion_cosines=alignment_cells,Item_pair_cosines=pairs,Concordance=concordance,
    Corpus_intervals=bind_rows(intervals),Item_rankings=bind_rows(rankings),Relative_assignment=bind_rows(coverage),
    Literature_sensitivity=bind_rows(sensitivity),Item_influence=bind_rows(item_influence),
    Paper_influence=bind_rows(paper_influence),Items=items)
  for (name in names(branch_tables)) table_rows[[name]][[key]] <-
    bind_cols(data.frame(figure=key,model=model,item_version=version),branch_tables[[name]])
  results[[key]] <- list(paper_cooccurrence=P,semantic=S,alignment=A,items=items,criteria=domains,
    rho=rho,qap_p=qap_p,model_name=model,version=version,trend=trend,
    bootstrap=list(draws=draws,sampled_indices=sampled_indices,coefficients=bootstrap_coefficients),
    qap_null=null,benchmark_redundancy=tcrossprod(D))
}
tables <- list()
for (name in names(table_rows)) tables[[name]] <- bind_rows(table_rows[[name]])
tables$Benchmark_dictionary <- domains

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
saveWorkbook(wb,file.path(out,'Table_S2_04_SemanticAlignment.xlsx'),overwrite=TRUE)
# Raw matrices, labels and estimated trends are the only handoff to Step 3.
display_translations <- read.csv(file.path(input_dir,'chinese_display_translations.csv'),stringsAsFactors=FALSE,fileEncoding='UTF-8')
saveRDS(list(spec=spec,conditions=results,display_translations=display_translations),file.path(out,'Semantic_Figures.rds'))
