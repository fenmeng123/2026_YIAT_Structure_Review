# Author: Kunru Song
# Published item coassignment, paper-balanced synthesis and exploratory stability.
library(dplyr)
library(tidyr)
library(cluster)
input_dir <- 'Res_1_Data_for_analyses'
out <- 'Res_2_Results/S1_5_FactorCooccurenceAnalysis'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
x <- readRDS(file.path(input_dir,'Dat_Factor_Allocations.rds'))
stages <- readRDS(file.path(input_dir,'Dat_SysRev_Linked_Stages.rds'))
records <- x$records; allocation <- x$coassignment_allocation
cells <- x$cells
pos <- cbind(match(cells$record_id,rownames(allocation)),cells$item)
allocation[pos[cells$status=='Not retained',,drop=FALSE]] <- 0L
# Zero means explicitly dropped. Unknown, merged and non-factor status remain NA.
observed_k <- integer(nrow(allocation))
for (i in seq_len(nrow(allocation))) observed_k[i] <- length(unique(allocation[i,!is.na(allocation[i,]) & allocation[i,]>0]))
for (field in c('extraction','input_matrix','rotation','estimator','retention')) {
  context <- rep('No exploration stage',nrow(records))
  for (i in seq_len(nrow(records))) {
    z <- stages %>% filter(record_id==records$record_id[i],stage_role=='Exploration')
    if (nrow(z)) context[i] <- paste(sort(unique(as.character(z[[field]]))),collapse='; ')
  }
  records[[paste0(field,'_context')]] <- context
}
strata <- list(
  Common_factor_context=grepl('PAF|ML|GLS|WLS|MRFA|common',records$extraction_context,ignore.case=TRUE),
  PCA_context=grepl('PCA|principal component',records$extraction_context,ignore.case=TRUE),
  No_exploration_context=grepl('^No exploration stage$',records$extraction_context),
  Oblique_context=grepl('oblimin|promax|oblique|geomin',records$rotation_context,ignore.case=TRUE),
  Orthogonal_context=grepl('varimax|orthogonal|equamax|quartimax',records$rotation_context,ignore.case=TRUE),
  PA_MAP_Hull_context=grepl('parallel|MAP|Hull|(^|[; ,])PA([; ,]|$)',records$retention_context,ignore.case=TRUE),
  Heuristic_retention_context=grepl('Heuristic only|eigen|scree|Kaiser',records$retention_context,ignore.case=TRUE) &
    !grepl('parallel|MAP|Hull|(^|[; ,])PA([; ,]|$)',records$retention_context,ignore.case=TRUE),
  Polychoric_context=grepl('polychoric',records$input_matrix_context,ignore.case=TRUE),
  ML_context=grepl('(^|[; ,])ML(R|M|-SB)?([; ,]|$)|maximum likelihood',records$estimator_context,ignore.case=TRUE),
  PAF_context=grepl('PAF|principal axis',records$estimator_context,ignore.case=TRUE) |
    grepl('PAF|principal axis',records$extraction_context,ignore.case=TRUE),
  WLSMV_context=grepl('WLSMV',records$estimator_context,ignore.case=TRUE))
all_records <- rep(TRUE,nrow(records))
scenarios <- c(list(main=all_records,multifactor=observed_k>1 & records$number_of_factors>1,
  Equal_record_weighting=all_records,Complete_linkage=all_records,Retained_only_legacy_denominator=all_records),strata)
results <- list()
for (scenario in names(scenarios)) {
  take <- scenarios[[scenario]]
  r <- records[take,,drop=FALSE]; a <- allocation[take,,drop=FALSE]
  if (nrow(r)<2L || n_distinct(r$paper_id)<2L) next
  if (scenario=='Retained_only_legacy_denominator') a[!is.na(a) & a==0L] <- NA_integer_
  # Form binary record-pair outcomes; diagonals are availability bookkeeping only.
  num <- den <- matrix(0,nrow(a),400)
  for (i in seq_len(nrow(a))) {
    v <- a[i,]; available <- !is.na(v)
    denominator <- outer(available,available,'&')
    numerator <- outer(v,v,'==') & outer(v>0,v>0,'&')
    numerator[is.na(numerator)] <- FALSE; diag(numerator) <- available
    den[i,] <- as.vector(denominator); num[i,] <- as.vector(numerator & denominator)
  }
  paper_rows <- split(seq_len(nrow(r)),as.character(r$paper_id))
  paper_num <- paper_den <- matrix(0,length(paper_rows),400,dimnames=list(names(paper_rows),NULL))
  for (i in seq_along(paper_rows)) {
    paper_num[i,] <- colSums(num[paper_rows[[i]],,drop=FALSE])
    paper_den[i,] <- colSums(den[paper_rows[[i]],,drop=FALSE])
  }
  paper_rates <- paper_num/paper_den; paper_rates[!is.finite(paper_rates)] <- NA_real_
  available_papers <- colSums(!is.na(paper_rates))
  paper_mean <- colSums(paper_rates,na.rm=TRUE)/available_papers
  record_mean <- colSums(num)/colSums(den)
  paper_mean[available_papers==0] <- NA_real_; record_mean[colSums(den)==0] <- NA_real_
  mats <- list(raw_count=colSums(num),record_denominator=colSums(den),record_proportion=record_mean,
    paper_proportion=paper_mean,paper_denominator=available_papers)
  for (name in names(mats)) mats[[name]] <- matrix(mats[[name]],20,20,dimnames=list(colnames(a),colnames(a)))
  mats$paper_rates <- paper_rates; mats$record_outcomes <- ifelse(den==1,num,NA_real_)
  mats$n_records <- nrow(r); mats$n_papers <- length(paper_rows)
  mats$record_ids <- r$record_id; mats$paper_ids <- as.character(r$paper_id)
  P <- if(scenario=='Equal_record_weighting')mats$record_proportion else mats$paper_proportion
  clusters <- NULL
  if (!anyNA(P)) {
    linkage <- if(scenario=='Complete_linkage')'complete' else 'average'
    distance <- as.dist(1-P); tree <- hclust(distance,method=linkage)
    cuts <- memberships <- cluster_rows <- list(); summaries <- list()
    for (k in 2:7) {
      g <- cutree(tree,k=k)
      # Number clusters by their first original item, independent of source F labels.
      g <- setNames(match(g,unique(g)),names(g)); cuts[[as.character(k)]] <- g
      memberships[[as.character(k)]] <- data.frame(k=k,item=names(g),item_number=1:20,cluster=as.integer(g))
      same <- outer(g,g,'=='); lower <- lower.tri(P)
      summaries[[as.character(k)]] <- data.frame(k=k,mean_silhouette=mean(silhouette(g,distance)[,'sil_width']),
        mean_within_coassignment=mean(P[lower & same]),mean_between_coassignment=mean(P[lower & !same]),
        min_cluster_size=min(table(g)),singleton_clusters=sum(table(g)==1L))
      for (j in sort(unique(g))) cluster_rows[[length(cluster_rows)+1L]] <- data.frame(k=k,cluster=j,
        n_items=sum(g==j),items=paste(which(g==j),collapse=';'))
    }
    membership <- bind_rows(memberships); cluster_summary <- bind_rows(cluster_rows); summary <- bind_rows(summaries)
    B <- if(scenario %in% c('Equal_record_weighting','Complete_linkage','Retained_only_legacy_denominator'))0L else 1000L
    seed <- if(scenario=='main')20261013L else if(scenario=='multifactor')20261014L else
      if(scenario %in% names(strata))20261113L+match(scenario,names(strata)) else 20261006L
    boot_P <- matrix(NA_real_,B,400); boot_ari <- matrix(NA_real_,B,6)
    boot_jaccard <- matrix(NA_real_,B,nrow(cluster_summary)); consensus <- list()
    for (k in 2:7) consensus[[as.character(k)]] <- matrix(0,20,20)
    valid <- 0L
    set.seed(seed)
    for (b in seq_len(B)) {
      sampled <- sample.int(nrow(paper_rates),nrow(paper_rates),replace=TRUE)
      rates <- paper_rates[sampled,,drop=FALSE]
      pp <- colSums(rates,na.rm=TRUE)/colSums(!is.na(rates)); pp[!is.finite(pp)] <- NA_real_
      boot_P[b,] <- pp
      if (anyNA(pp)) next
      boot_distance <- 1-matrix(pp,20,20); diag(boot_distance) <- 0
      boot_tree <- hclust(as.dist(boot_distance),method='average'); valid <- valid+1L
      for (k in 2:7) {
        reference <- cuts[[as.character(k)]]; bc <- cutree(boot_tree,k=k); bc <- match(bc,unique(bc))
        cross <- table(reference,bc); n2 <- sum(cross)*(sum(cross)-1)/2
        left <- sum(rowSums(cross)*(rowSums(cross)-1)/2)
        right <- sum(colSums(cross)*(colSums(cross)-1)/2); expected <- left*right/n2
        denominator <- (left+right)/2-expected
        boot_ari[b,k-1L] <- if(abs(denominator)<1e-12)as.numeric(all(outer(reference,reference,'==')==outer(bc,bc,'=='))) else
          (sum(cross*(cross-1)/2)-expected)/denominator
        for (j in which(cluster_summary$k==k)) {
          ids <- which(reference==cluster_summary$cluster[j]); overlaps <- numeric(k)
          for (h in seq_len(k)) overlaps[h] <- length(intersect(ids,which(bc==h)))/length(union(ids,which(bc==h)))
          boot_jaccard[b,j] <- max(overlaps)
        }
        consensus[[as.character(k)]] <- consensus[[as.character(k)]]+outer(bc,bc,'==')
      }
    }
    pair_lower <- pair_upper <- P*NA_real_
    if (B>0L) {
      pair_lower[] <- apply(boot_P,2,quantile,.025,na.rm=TRUE,names=FALSE)
      pair_upper[] <- apply(boot_P,2,quantile,.975,na.rm=TRUE,names=FALSE)
      summary$bootstrap_valid <- colSums(is.finite(boot_ari))
      summary$bootstrap_ari_median <- apply(boot_ari,2,quantile,.5,na.rm=TRUE,names=FALSE)
      summary$bootstrap_ari_p025 <- apply(boot_ari,2,quantile,.025,na.rm=TRUE,names=FALSE)
      summary$bootstrap_ari_p975 <- apply(boot_ari,2,quantile,.975,na.rm=TRUE,names=FALSE)
      cluster_summary$bootstrap_jaccard_mean <- colMeans(boot_jaccard,na.rm=TRUE)
      cluster_summary$bootstrap_jaccard_p025 <- apply(boot_jaccard,2,quantile,.025,na.rm=TRUE,names=FALSE)
      cluster_summary$bootstrap_jaccard_p975 <- apply(boot_jaccard,2,quantile,.975,na.rm=TRUE,names=FALSE)
      for (k in names(consensus)) consensus[[k]] <- consensus[[k]]/valid
    }
    clusters <- list(tree=tree,memberships=membership,diagnostics=summary,cluster_summary=cluster_summary,
      pair_lower=pair_lower,pair_upper=pair_upper,bootstrap_ari=boot_ari,bootstrap_jaccard=boot_jaccard,
      bootstrap_consensus=consensus,bootstrap_reps=B,seed=seed)
  }
  indices <- which(upper.tri(P),arr.ind=TRUE)
  pairs <- data.frame(item_i=rownames(P)[indices[,1]],item_j=colnames(P)[indices[,2]],
    paper_proportion=mats$paper_proportion[indices],record_proportion=mats$record_proportion[indices],
    same_factor_records=mats$raw_count[indices],available_records=mats$record_denominator[indices],
    available_papers=mats$paper_denominator[indices])
  pairs$bootstrap_p025 <- if(is.null(clusters))NA_real_ else clusters$pair_lower[indices]
  pairs$bootstrap_p975 <- if(is.null(clusters))NA_real_ else clusters$pair_upper[indices]
  for (i in seq_len(nrow(pairs))) {
    others <- setdiff(1:20,indices[i,])
    best <- max(P[indices[i,1],others],P[indices[i,2],others],na.rm=TRUE)
    pairs$pair_margin_over_other_partners[i] <- P[indices[i,,drop=FALSE]]-best
  }
  results[[scenario]] <- list(matrices=mats,analysis_matrix=P,clusters=clusters,pairs=pairs)
}
# Leave-one-paper influence on the primary paper-balanced matrix.
rates <- results$main$matrices$paper_rates; P <- results$main$matrices$paper_proportion
influence <- list()
for (i in seq_len(nrow(rates))) {
  other <- rates[-i,,drop=FALSE]
  Pi <- matrix(colSums(other,na.rm=TRUE)/colSums(is.finite(other)),20,20)
  delta <- (Pi-P)[upper.tri(P)]
  influence[[i]] <- data.frame(omitted_paper=rownames(rates)[i],
    matrix_spearman=cor(P[upper.tri(P)],Pi[upper.tri(P)],method='spearman',use='complete.obs'),
    mean_absolute_pair_change=mean(abs(delta),na.rm=TRUE),max_absolute_pair_change=max(abs(delta),na.rm=TRUE))
}
result <- list(results=results,paper_influence=bind_rows(influence),records=records,allocation=allocation,
  interpretation='Exploratory synthesis of published assignments, including single-factor records; no latent-factor validity claim.')
saveRDS(result,file.path(input_dir,'Dat_Literature_Cooccurrence.rds'))
saveRDS(result,file.path(out,'Literature_Cooccurrence.rds'))
