# Author: Kunru Song
# Source factor memberships and analysis metadata. Display alignment belongs to Step 3.
# Figure 2: Overview of factor structures and analysis methods across the included studies.
library(dplyr)
library(tidyr)
input_dir <- 'Res_1_Data_for_analyses'
out <- 'Res_2_Results/S1_3_FactorStructureAndAnalysisStages'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
d <- readRDS(file.path(input_dir,'Dat_SysRev_Data_Records.rds')) %>%
  mutate(record_id=sprintf('D%03d',row_number()),data_row=row_number(),excel_row=data_row+1L,
    display_k=number_of_factors,display_note='',numbering='Original Y-IAT coordinates',n_status_category_items=0L)
stages <- readRDS(file.path(input_dir,'Dat_SysRev_Linked_Stages.rds'))
sets <- source_sets <- vector('list',nrow(d))
factors <- categories <- cells <- list()
raw <- eligible <- matrix(NA_integer_,nrow(d),20,dimnames=list(d$record_id,paste0('IAT_',1:20)))
status_mask <- matrix(FALSE,nrow(d),20,dimnames=dimnames(raw))
non_factor_name <- 'No significant loadings and no group factor assignment'
# Chen & Nath (2016), Table 2: source item order mapped to original IAT coordinates.
chen <- c(3L,5L,8L,9L,10L,11L,17L,18L,19L,1L,2L,6L,7L,16L,4L,12L,13L,14L,15L,20L)
for (i in seq_len(nrow(d))) {
  z <- d[i,]; text <- as.character(z$factor_structure)
  s <- list()
  if (!is.na(text) && !tolower(trimws(text)) %in% c('not applicable','not reported','')) {
    pieces <- trimws(strsplit(text,';',fixed=TRUE)[[1]])
    for (piece in pieces) {
      if (!grepl('^F[0-9]+\\s*:\\s*[0-9]+(\\s*,\\s*[0-9]+)*$',piece,perl=TRUE)) stop('Unparsed allocation: ',piece)
      id <- trimws(sub(':.*','',piece))
      s[[id]] <- sort(as.integer(trimws(strsplit(sub('^[^:]+:','',piece),',',fixed=TRUE)[[1]])))
    }
  }
  naming <- trimws(strsplit(as.character(z$factor_naming),';',fixed=TRUE)[[1]])
  naming <- naming[grepl('^F[0-9]+\\s*:',naming,perl=TRUE)]
  source_names <- character()
  for (id in unique(trimws(sub(':.*','',naming)))) {
    labels <- unique(trimws(sub('^[^:]+:','',naming[trimws(sub(':.*','',naming))==id])))
    source_names[id] <- if (length(labels)==1L) labels else NA_character_
  }
  drop_text <- as.character(z$item_dropping)
  drops <- if (tolower(drop_text)=='none') integer() else as.integer(trimws(strsplit(drop_text,',',fixed=TRUE)[[1]]))
  reconstructed <- !length(s)
  if (reconstructed) {
    if (z$number_of_factors!=1L) stop('Missing multifactor allocation: ',z$record_id)
    s <- list(F1=setdiff(1:20,drops))
  }
  if (z$paper_id=='PDF13') {
    for (j in seq_along(s)) s[[j]] <- sort(chen[s[[j]]])
    drops <- sort(chen[drops]); d$numbering[i] <- 'Chen Table2 content crosswalk; adapted wording'
  }
  source_sets[[i]] <- s
  status_ids <- names(source_names)[!is.na(source_names) & tolower(source_names)==tolower(non_factor_name)]
  category_items <- if (length(status_ids)) unlist(s[status_ids],use.names=FALSE) else integer()
  s <- s[!names(s) %in% status_ids]
  if (length(s)!=z$number_of_factors) stop('Factor count disagrees with allocation: ',z$record_id)
  status_mask[i,category_items] <- TRUE
  d$n_status_category_items[i] <- length(category_items)
  # Merged indicators are shown descriptively but cannot support individual-item pairs.
  merged <- if (z$paper_id=='PDF50') c(3L,6L,8L,19L) else integer()
  conflict <- intersect(unlist(s),drops)
  sets[[i]] <- s
  for (j in seq_along(s)) raw[i,s[[j]]] <- j
  eligible[i,] <- raw[i,]
  eligible[i,unique(c(conflict,merged))] <- NA_integer_
  status <- rep('Unassigned',20)
  status[drops] <- 'Not retained'; status[!is.na(raw[i,])] <- 'Assigned'
  status[category_items] <- 'No group assignment'; status[merged] <- 'Merged indicator'
  status[conflict] <- 'Allocation/drop conflict'
  for (j in seq_along(s)) {
    factors[[length(factors)+1L]] <- data.frame(record_id=z$record_id,data_row=i,paper_id=as.character(z$paper_id),
      reference=as.character(z$reference_raw),year=z$year,k=length(s),original_factor=names(s)[j],
      original_factor_name=unname(source_names[names(s)[j]]),local_factor=j,
      items=paste(sort(s[[j]]),collapse=','),source_allocation=text,
      source_factor_naming=as.character(z$factor_naming),reconstructed=reconstructed)
  }
  source_id <- source_name <- rep(NA_character_,20)
  for (id in names(source_sets[[i]])) {
    ids <- source_sets[[i]][[id]]; is_status <- id %in% status_ids
    source_id[ids] <- id; source_name[ids] <- unname(source_names[id])
    categories[[length(categories)+1L]] <- data.frame(record_id=z$record_id,data_row=i,
      paper_id=as.character(z$paper_id),reference=as.character(z$reference_raw),source_factor=id,
      source_factor_name=unname(source_names[id]),category_type=if(is_status)'Status category' else 'Factor',
      local_factor=if(is_status)NA_integer_ else match(id,names(s)),items=paste(sort(ids),collapse=','),
      source_allocation=text,source_factor_naming=as.character(z$factor_naming))
  }
  cells[[i]] <- data.frame(record_id=z$record_id,data_row=i,item=1:20,local_factor=raw[i,],status=status,
    source_factor=source_id,source_factor_name=source_name,
    category_type=ifelse(status_mask[i,],'Status category',ifelse(!is.na(raw[i,]),'Factor',NA_character_)),
    coassignment_eligible=!is.na(eligible[i,]))
}
source_categories <- bind_rows(categories)
allocations <- list(records=d,sets=sets,source_sets=source_sets,factors=bind_rows(factors),
  source_categories=source_categories,category_annotations=filter(source_categories,category_type=='Status category'),
  cells=bind_rows(cells),raw_allocation=raw,coassignment_allocation=eligible,chen_crosswalk=chen)
# Derived input for subsequent analyses; Step 3 receives the same source material in Res_2_Results.
saveRDS(allocations,file.path(input_dir,'Dat_Factor_Allocations.rds'))
saveRDS(list(allocations=allocations,stages=stages),file.path(out,'Figure_2_FactorStructureAndAnalysisStages.rds'))
