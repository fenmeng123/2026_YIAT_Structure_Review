# Author: Kunru Song
# Agreement among three fixed encoders using identical revised-English wording.
library(dplyr)
library(tidyr)
library(openxlsx)
input_dir <- 'Res_1_Data_for_analyses'; out <- 'Res_2_Results/S2_3_EncoderAgreement'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
inputs <- readRDS(file.path(input_dir,'Semantic_Inputs.rds'))
items <- inputs$versions$MAK_YOUNG_2020; domains <- inputs$domains
models <- c('mpnet','personality','harmony')
similarity <- item_similarity <- criterion_similarity <- list()
for (model in models) {
  e <- readRDS(file.path(input_dir,paste0('embeddings_formal_',model,'_MAK_YOUNG_2020_definition.rds')))
  X <- do.call(rbind,e$items$vector); X <- X/sqrt(rowSums(X^2)); rownames(X) <- e$items$item_id
  D <- do.call(rbind,e$domains$vector); D <- D/sqrt(rowSums(D^2)); rownames(D) <- e$domains$domain_id
  similarity[[model]] <- X %*% t(D)
  item_similarity[[model]] <- tcrossprod(X); criterion_similarity[[model]] <- tcrossprod(D)
}
item_criterion <- expand.grid(item_id=items$item_id,domain_id=domains$domain_id,stringsAsFactors=FALSE) %>%
  mutate(framework=domains$framework[match(domain_id,domains$domain_id)],
    criterion_type=domains$criterion_type[match(domain_id,domains$domain_id)])
item_indices <- which(upper.tri(item_similarity[[1]]),arr.ind=TRUE)
criterion_indices <- which(upper.tri(criterion_similarity[[1]]),arr.ind=TRUE)
item_pairs <- data.frame(id_a=items$item_id[item_indices[,1]],id_b=items$item_id[item_indices[,2]])
criterion_pairs <- data.frame(id_a=domains$domain_id[criterion_indices[,1]],id_b=domains$domain_id[criterion_indices[,2]])
for (model in models) {
  item_criterion[[model]] <- as.vector(similarity[[model]])
  item_pairs[[model]] <- item_similarity[[model]][item_indices]
  criterion_pairs[[model]] <- criterion_similarity[[model]][criterion_indices]
}
datasets <- list(Item_criterion_all=item_criterion,Item_item=item_pairs,Criterion_criterion_supplementary=criterion_pairs)
for (framework in unique(domains$framework)) datasets[[paste0('Item_criterion_',framework)]] <-
  item_criterion %>% filter(.data$framework==.env$framework)
icc <- summaries <- list(); comparisons <- list(); pair_rows <- list()
combos <- combn(models,2,simplify=FALSE)
for (dataset in names(datasets)) {
  scores <- as.matrix(datasets[[dataset]][models]); n <- nrow(scores); k <- ncol(scores)
  grand <- mean(scores); target <- rowMeans(scores); encoder <- colMeans(scores)
  residual <- sweep(sweep(scores,1,target),2,encoder)+grand
  MS_target <- k*sum((target-grand)^2)/(n-1); MS_error <- sum(residual^2)/((n-1)*(k-1))
  icc[[dataset]] <- data.frame(dataset,n_score_cells=n,n_fixed_encoders=k,
    ICC_C1=(MS_target-MS_error)/(MS_target+(k-1)*MS_error),MS_target,MS_error)
  for (pair in combos) {
    comparisons[[length(comparisons)+1L]] <- list(x=scores[,pair[1]],y=scores[,pair[2]],
      identity=data.frame(dataset,model_a=pair[1],model_b=pair[2]),type='dataset')
  }
  for (model in models) summaries[[length(summaries)+1L]] <- data.frame(dataset,model,
    n_score_cells=n,mean_cosine=mean(scores[,model]),sd_cosine=sd(scores[,model]),
    min_cosine=min(scores[,model]),median_cosine=median(scores[,model]),max_cosine=max(scores[,model]))
}
# Within-framework ranking sets retain all ties within 1e-8.
rankings <- top_pairwise <- disagreement <- list()
for (framework in unique(domains$framework)) {
  jj <- which(domains$framework==framework); ids <- domains$domain_id[jj]
  for (i in seq_len(nrow(items))) {
    tops <- list(); score_matrix <- matrix(NA_real_,length(jj),length(models))
    for (m in seq_along(models)) {
      model <- models[m]; scores <- similarity[[model]][i,jj]; score_matrix[,m] <- scores
      ord <- order(-scores,ids)
      tops[[model]] <- list(ids=sort(ids[max(scores)-scores<=1e-8]),
        top2_ids=sort(ids[scores[ord[2]]-scores<=1e-8]),margin=scores[ord[1]]-scores[ord[2]])
      rankings[[length(rankings)+1L]] <- data.frame(item_id=items$item_id[i],framework,model,
        top1_criteria=paste(tops[[model]]$ids,collapse=';'),top2_criteria=paste(tops[[model]]$top2_ids,collapse=';'),
        top2_set_size=length(tops[[model]]$top2_ids),top1_ties=length(tops[[model]]$ids),
        highest_cosine=scores[ord[1]],second_cosine=scores[ord[2]],top1_top2_margin=tops[[model]]$margin)
    }
    for (pair in combos) {
      first <- tops[[pair[1]]]$ids; second <- tops[[pair[2]]]$ids
      first2 <- tops[[pair[1]]]$top2_ids; second2 <- tops[[pair[2]]]$top2_ids
      top_pairwise[[length(top_pairwise)+1L]] <- data.frame(item_id=items$item_id[i],framework,
        model_a=pair[1],model_b=pair[2],exact_set_agreement=identical(first,second),
        any_top1_overlap=length(intersect(first,second))>0,top1_jaccard=length(intersect(first,second))/length(union(first,second)),
        top2_exact_set_agreement=identical(first2,second2),any_top2_overlap=length(intersect(first2,second2))>0,
        top2_jaccard=length(intersect(first2,second2))/length(union(first2,second2)),
        model_a_top1=paste(first,collapse=';'),model_b_top1=paste(second,collapse=';'),
        model_a_top2=paste(first2,collapse=';'),model_b_top2=paste(second2,collapse=';'))
      comparisons[[length(comparisons)+1L]] <- list(x=similarity[[pair[1]]][i,jj],y=similarity[[pair[2]]][i,jj],
        identity=data.frame(item_id=items$item_id[i],framework,model_a=pair[1],model_b=pair[2]),type='profile')
    }
    first_sets <- lapply(tops,'[[','ids'); second_sets <- lapply(tops,'[[','top2_ids')
    ranges <- apply(score_matrix,1,max)-apply(score_matrix,1,min)
    equal1 <- equal2 <- TRUE
    for (model in models) {
      equal1 <- equal1 && identical(tops[[model]]$ids,tops[[1]]$ids)
      equal2 <- equal2 && identical(tops[[model]]$top2_ids,tops[[1]]$top2_ids)
    }
    disagreement[[length(disagreement)+1L]] <- data.frame(item_id=items$item_id[i],framework,
      all_encoder_top1_sets_equal=equal1,any_all_encoder_top1_overlap=length(Reduce(intersect,first_sets))>0,
      all_encoder_top2_sets_equal=equal2,any_all_encoder_top2_overlap=length(Reduce(intersect,second_sets))>0,
      min_top1_top2_margin=min(unlist(lapply(tops,'[[','margin'))),max_cosine_range=max(ranges),
      largest_disagreement_criterion=ids[which.max(ranges)],mean_cosine_range=mean(ranges))
  }
}
profiles <- list()
for (comparison in comparisons) {
  x <- comparison$x; y <- comparison$y
  vx <- mean((x-mean(x))^2); vy <- mean((y-mean(y))^2); cross <- mean((x-mean(x))*(y-mean(y)))
  metrics <- data.frame(n_score_cells=length(x),pearson_r=cor(x,y),spearman_rho=cor(x,y,method='spearman'),
    mean_model_a=mean(x),mean_model_b=mean(y),mean_bias_b_minus_a=mean(y-x),MAE=mean(abs(y-x)),
    RMSE=sqrt(mean((y-x)^2)),CCC=2*cross/(vx+vy+(mean(x)-mean(y))^2))
  row <- bind_cols(comparison$identity,metrics)
  if(comparison$type=='dataset')pair_rows[[length(pair_rows)+1L]] <- row else profiles[[length(profiles)+1L]] <- row
}
top_pairwise <- bind_rows(top_pairwise); disagreement <- bind_rows(disagreement)
top_summary <- top_pairwise %>% group_by(framework,model_a,model_b) %>%
  summarise(exact_set_agreement_fraction=mean(exact_set_agreement),any_top1_overlap_fraction=mean(any_top1_overlap),
    mean_top1_jaccard=mean(top1_jaccard),top2_exact_set_agreement_fraction=mean(top2_exact_set_agreement),
    any_top2_overlap_fraction=mean(any_top2_overlap),mean_top2_jaccard=mean(top2_jaccard),n_items=n(),.groups='drop')
all_summary <- disagreement %>% group_by(framework) %>% summarise(across(c(all_encoder_top1_sets_equal,
  any_all_encoder_top1_overlap,all_encoder_top2_sets_equal,any_all_encoder_top2_overlap),sum),n_items=n(),.groups='drop')
tables <- list(Pairwise_agreement=bind_rows(pair_rows),ICC_fixed_encoders=bind_rows(icc),
  Score_distributions=bind_rows(summaries),Top_sets_pairwise_summary=top_summary,Top_sets_all_model_summary=all_summary,
  Item_criterion_scores=item_criterion,Item_pair_scores=item_pairs,Criterion_pairs_supplement=criterion_pairs,
  Item_rankings=bind_rows(rankings),Top_sets_item_agreement=top_pairwise,Item_profile_agreement=bind_rows(profiles),
  Disagreement_by_item=disagreement,Items=items,Benchmark_dictionary=domains)

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
saveWorkbook(wb,file.path(out,'Table_S2_03_EncoderAgreement.xlsx'),overwrite=TRUE)
