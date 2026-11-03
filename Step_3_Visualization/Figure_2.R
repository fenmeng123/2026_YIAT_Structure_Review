# Author: Kunru Song
# Figure 2. Overview of factor structures and analysis methods across the included studies.
# Source partitions, descriptive display alignment and all 115 analyses.
library(dplyr)
library(tidyr)
library(grid)
material <- readRDS('Res_2_Results/S1_3_FactorStructureAndAnalysisStages/Figure_2_FactorStructureAndAnalysisStages.rds')
x <- material$allocations
stages <- material$stages
s1_5_palette <- c(F1='#6B9DBA',F2='#D7A17F',F3='#92AF91',F4='#AA93B6',F5='#C68197',F6='#C0AF71',F7='#65AFA7')

# Display F slots maximize item-set overlap, never redefine a reported local factor.
anchors <- list(F1=c(10,12,13,15,20),F2=c(1,2,14,16,17),F3=c(2,6,8),F4=c(3,19),F5=c(9,18),F6=c(7,11),F7=4L)
f <- x$factors
a <- matrix(0,7,20,dimnames=list(names(anchors),NULL))
z <- matrix(0,nrow(f),20)
for(j in 1:7)a[j,anchors[[j]]] <- 1
for(i in seq_len(nrow(f)))z[i,as.integer(strsplit(f$items[i],',',fixed=TRUE)[[1]])] <- 1
weights <- 1/as.numeric(table(x$records$paper_id)[as.character(f$paper_id)])
multifactor <- which(f$k > 1L)
by_record <- split(seq_len(nrow(f)), f$record_id)
permutations <- list(matrix(1:7,ncol=1))
for(k in 2:7) {
  previous <- permutations[[k-1]]; rows <- list()
  for(i in seq_len(nrow(previous)))for(v in setdiff(1:7,previous[i,]))rows[[length(rows)+1L]] <- c(previous[i,],v)
  permutations[[k]] <- do.call(rbind,rows)
}
proto <- a
previous <- NULL
for(iter in 1:30) {
  score <- matrix(0,nrow(z),7)
  for(j in 1:7)for(i in seq_len(nrow(z)))score[i,j] <- sum(pmin(z[i,],proto[j,]))/sum(pmax(z[i,],proto[j,]))
  slot <- integer(nrow(f))
  for(ii in by_record) {
    if(length(ii)==1L) {slot[ii] <- 1L;next}
    pp <- permutations[[length(ii)]]
    objective <- rowSums(matrix(score[ii,,drop=FALSE][cbind(rep(seq_along(ii),each=nrow(pp)),as.vector(pp))],nrow=nrow(pp)))
    slot[ii] <- pp[which.max(objective),]
  }
  if(!is.null(previous) && identical(slot,previous))break
  previous <- slot
  for(j in 1:7) {
    ii <- intersect(which(slot==j),multifactor)
    if(length(ii))proto[j,] <- .35*a[j,]+.65*colSums(z[ii,,drop=FALSE]*weights[ii])/sum(weights[ii])
  }
}
f$display_factor <- paste0('F',slot)
x$cells <- x$cells %>% left_join(select(f,record_id,local_factor,display_factor),by=c('record_id','local_factor'))
d <- x$records %>% arrange(display_k,year,reference_raw,data_row) %>% mutate(plot_row=row_number())
raw_reference <- trimws(as.character(d$reference_raw))
sample_label <- ifelse(grepl('#[0-9]+',raw_reference),sub('.*?(#[0-9]+).*','\\1',raw_reference,perl=TRUE),'')
author_label <- sub('\\s*#.*$','',raw_reference) %>% sub('\\s+Hong Kong school sample$','',.,ignore.case=TRUE) %>% tools::toTitleCase()
author_label <- gsub('et al[.,]?','et al.',author_label,ignore.case=TRUE)
d$label <- trimws(paste0(author_label,' (',d$year,') ',sample_label))
d$sample_size_label <- format(d$size,big.mark=',',scientific=FALSE,trim=TRUE)
d$validation_label <- recode(as.character(d$validation),'single sample'='Single sample','split-half'='Split-half','independent samples'='Independent')
cc <- x$cells %>% mutate(plot_row=match(record_id,d$record_id))
display_methods <- stages %>% arrange(stage_order_within_paper,stage_row) %>%
  distinct(record_id,stage_role,stage,.keep_all=TRUE) %>% group_by(record_id,stage_role) %>%
  summarise(n_methods=n(),display_label=paste(sub('^bifactor ','BF-',stage),collapse='/'),.groups='drop')
method_cells <- expand_grid(plot_row=seq_len(nrow(d)),role_column=1:2) %>%
  mutate(record_id=d$record_id[plot_row],stage_role=c('Exploration','Validation')[role_column]) %>%
  left_join(display_methods,by=c('record_id','stage_role')) %>%
  mutate(n_methods=replace_na(n_methods,0L),display_label=replace_na(display_label,'Not conducted')) %>%
  arrange(role_column,plot_row)
item_drops <- cc %>% group_by(item) %>% summarise(n=sum(status=='Not retained'),.groups='drop')
stage_totals <- data.frame(stage=c('EFA','CFA','bifactor EFA','bifactor CFA','ESEM','bifactor ESEM'),
  label=c('EFA','CFA','BF-EFA','BF-CFA','ESEM','BF-ESEM'),role_column=c(1,2,1,2,2,2)) %>%
  left_join(count(stages,stage,name='n'),by='stage') %>% mutate(fill=if_else(role_column==1,'#87CEEB','#F8766D'))
quality <- list()
for(criterion in c('S1','S2')) {
  n <- as.integer(table(factor(as.character(d[[criterion]]),levels=0:2)))
  labels <- if(criterion=='S1')c('N < 100','N 100–139','N ≥ 140') else c('Single sample','Split-half','Independent')
  quality[[criterion]] <- data.frame(criterion,score=0:2,label=labels,n,denominator=nrow(d),
    fill=c('#E4B5B0','#E6D4AF','#527DA0'),display_label=sprintf('%s\n%d (%.1f%%)',labels,n,100*n/nrow(d)))
}
dir.create('Res_3_Report',showWarnings=FALSE)
cairo_pdf('Res_3_Report/Figure_2_FactorStructureAndAnalysisStages.pdf',width=195/25.4,height=270/25.4,family='Arial',bg='white')
grid.newpage()
method_width <- convertWidth(grobWidth(textGrob('CFA/ESEM/BF-ESEM',gp=gpar(fontfamily='Arial',fontsize=7))),'mm',valueOnly=TRUE)+2
fig <- list(records=d,cells=cc,stages=stages,method_cells=method_cells,item_drops=item_drops,stage_totals=stage_totals,
  record_quality=bind_rows(quality),width_mm=195,height_mm=270,label_left=5,sample_size_center=48,validation_center=66,
  validation_column_right=78,k_center=80.5,matrix_left=83,matrix_width=56,method_left=139,
  method_width=2*method_width,method_column_width=method_width,body_bottom=43,body_height=201.6,
  row_height=2.8,cell_width=2.8,tile_side=2.66,bar_height=30.7,summary_bottom=7.5,
  quality_width=73,quality_height=6,quality_centers=c(S1=35.7,S2=18.2),
  not_conducted_colour='#727B84',unassigned_colour='#B8BDC5',bar_colour='#A5ADB3')

# Draw all rows and summaries at the accepted physical dimensions.
grid::pushViewport(grid::viewport(width = grid::unit(fig$width_mm, "mm"), height = grid::unit(fig$height_mm, "mm")))
  top <- fig$body_bottom+fig$body_height
  # A single factor legend remains; methods are labelled directly in the cells.
  right_edge<-fig$method_left+fig$method_width
legend_title_y <- top + 22
legend_y <- top + 16
header_y <- top + 4
grid::grid.text("Factor Assignment Within Each Study", x = grid::unit(fig$label_left, "mm"), y = grid::unit(legend_title_y, 
    "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 8.5, fontface = "bold", 
    col = "#161616"))
key_size <- 1.8
key_gap <- 0.6
between <- 1
legend_size <- 7
  factor_labels<-c(names(s1_5_palette),'Unassigned','Dropped')
  factor_widths <- numeric(length(factor_labels))
  for(j in seq_along(factor_labels)) factor_widths[j] <- grid::convertWidth(grid::grobWidth(grid::textGrob(factor_labels[j],gp=grid::gpar(fontfamily='Arial',fontsize=legend_size))),'mm',valueOnly=TRUE)
  factor_total<-sum(factor_widths+key_size+key_gap)+(length(factor_labels)-1)*between
  cursor<-fig$label_left
for (j in seq_along(factor_labels)) {
    xx <- cursor + key_size/2
    fill <- if (j <= 7) 
        s1_5_palette[j]
    else if (j == 8) 
        fig$unassigned_colour
    else "white"
    grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(legend_y, "mm"), width = grid::unit(key_size, "mm"), 
        height = grid::unit(key_size, "mm"), gp = grid::gpar(fill = fill, col = if (j == 9) 
            "#D1D5D8"
        else NA, lwd = 0.25))
    if (j == 9) {
        grid::pushViewport(grid::viewport(x = grid::unit(xx, "mm"), y = grid::unit(legend_y, "mm"), width = grid::unit(key_size, 
            "mm"), height = grid::unit(key_size, "mm"), clip = "on"))
        for (center in key_size * c(0.25, 0.5, 0.75)) grid::grid.lines(x = grid::unit(center + c(-key_size/2, key_size/2), 
            "mm"), y = grid::unit(c(0, key_size), "mm"), gp = grid::gpar(col = "#8B949B", lwd = 0.42))
        grid::popViewport()
    }
    grid::grid.text(factor_labels[j], x = grid::unit(cursor + key_size + key_gap, "mm"), y = grid::unit(legend_y, 
        "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = legend_size, fontface = "plain", col = "#161616"))
    cursor <- cursor + key_size + key_gap + factor_widths[j] + between
}
grid::grid.text("Data record", x = grid::unit(fig$label_left, "mm"), y = grid::unit(header_y, "mm"), just = "left", 
    gp = grid::gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold", col = "#161616"))
grid::grid.text("Sample\nsize", x = grid::unit(fig$sample_size_center, "mm"), y = grid::unit(header_y + 1, "mm"), 
    just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 7, fontface = "bold", 
        col = "#161616"))
grid::grid.text("Validation\ndesign", x = grid::unit(fig$validation_center, "mm"), y = grid::unit(header_y + 1, 
    "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 7, fontface = "bold", 
    col = "#161616"))
grid::grid.text("k", x = grid::unit(fig$k_center, "mm"), y = grid::unit(header_y, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", 
    fontsize = 8, fontface = "bold", col = "#161616"))
grid::grid.text("Y-IAT item", x = grid::unit(fig$matrix_left + fig$matrix_width/2, "mm"), y = grid::unit(top + 
    9, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold", 
    col = "#161616"))
grid::grid.text("Analysis Methods", x = grid::unit(fig$method_left + fig$method_width/2, "mm"), y = grid::unit(top + 
    9, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold", 
    col = "#161616"))
for (j in 1:20) grid::grid.text(j, x = grid::unit(fig$matrix_left + (j - 0.5) * fig$cell_width, "mm"), y = grid::unit(header_y, 
    "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, fontface = "plain", 
    col = "#161616"))
grid::grid.text("Exploration", x = grid::unit(fig$method_left + fig$method_column_width * 0.5, "mm"), y = grid::unit(header_y, 
    "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 7.5, fontface = "plain", 
    col = "#161616"))
grid::grid.text("Validation", x = grid::unit(fig$method_left + fig$method_column_width * 1.5, "mm"), y = grid::unit(header_y, 
    "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 7.5, fontface = "plain", 
    col = "#161616"))
  # Shared row coordinates are used for both matrices and labels.
  ys <- top-(seq_len(72)-.5)*fig$row_height
for (r in seq_len(72)) {
    if (r%%2 == 0) 
        grid::grid.rect(x = grid::unit((fig$label_left + fig$method_left + fig$method_width)/2, "mm"), y = grid::unit(ys[r], 
            "mm"), width = grid::unit(fig$method_left + fig$method_width - fig$label_left, "mm"), height = grid::unit(fig$row_height, 
            "mm"), gp = grid::gpar(fill = "#F7F8F9", col = NA, lwd = 0.25))
    grid::grid.text(fig$records$label[r], x = grid::unit(fig$label_left, "mm"), y = grid::unit(ys[r] + 0.35, "mm"), 
        just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 6.8, fontface = "plain", col = "#161616"))
    grid::grid.text(fig$records$sample_size_label[r], x = grid::unit(fig$sample_size_center, "mm"), y = grid::unit(ys[r] + 
        0.35, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 6.8, fontface = "plain", col = "#161616"))
    grid::grid.text(fig$records$validation_label[r], x = grid::unit(fig$validation_center, "mm"), y = grid::unit(ys[r] + 
        0.35, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 6.8, fontface = "plain", col = "#161616"))
    grid::grid.text(fig$records$display_k[r], x = grid::unit(fig$k_center, "mm"), y = grid::unit(ys[r] + 0.35, 
        "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", fontsize = 7, fontface = "plain", col = "#161616"))
}
  cc<-fig$cells
for (i in seq_len(nrow(cc))) {
    xx <- fig$matrix_left + (cc$item[i] - 0.5) * fig$cell_width
    yy <- ys[cc$plot_row[i]]
    fill <- if (is.na(cc$display_factor[i])) 
        if (cc$status[i] == "No group assignment") 
            fig$unassigned_colour
        else "white"
    else s1_5_palette[cc$display_factor[i]]
    grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(yy, "mm"), width = grid::unit(fig$tile_side, "mm"), 
        height = grid::unit(fig$tile_side, "mm"), gp = grid::gpar(fill = fill, col = NA, lwd = 0.25))
    if (cc$status[i] == "Not retained") {
        grid::pushViewport(grid::viewport(x = grid::unit(xx, "mm"), y = grid::unit(yy, "mm"), width = grid::unit(fig$tile_side, 
            "mm"), height = grid::unit(fig$tile_side, "mm"), clip = "on"))
        for (center in fig$tile_side * c(0.25, 0.5, 0.75)) grid::grid.lines(x = grid::unit(center + c(-fig$tile_side/2, 
            fig$tile_side/2), "mm"), y = grid::unit(c(0, fig$tile_side), "mm"), gp = grid::gpar(col = "#8B949B", 
            lwd = 0.42))
        grid::popViewport()
    }
}
for (i in seq_len(nrow(fig$method_cells))) {
    mc <- fig$method_cells[i, ]
    xx <- fig$method_left + (mc$role_column - 0.5) * fig$method_column_width
    grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(ys[mc$plot_row], "mm"), width = grid::unit(fig$method_column_width - 
        0.14, "mm"), height = grid::unit(fig$tile_side, "mm"), gp = grid::gpar(fill = if (mc$n_methods == 0L) 
        fig$not_conducted_colour
    else "white", col = NA, lwd = 0.25))
    grid::grid.text(mc$display_label, x = grid::unit(xx, "mm"), y = grid::unit(ys[mc$plot_row] + 0.35, "mm"), just = "centre", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 7, fontface = "plain", col = if (mc$n_methods == 0L) 
            "white"
        else "#161616"))
}
  # Thin horizontal separators locate k blocks without breaking shared row alignment.
  breaks<-c(0,which(diff(fig$records$display_k)!=0),72)
for (b in breaks) grid::grid.lines(x = grid::unit(c(fig$label_left, fig$method_left + fig$method_width), "mm"), 
    y = grid::unit(c(top - b * fig$row_height, top - b * fig$row_height), "mm"), gp = grid::gpar(col = "#9AA4AC", 
        lwd = 0.45))
for (xx in fig$method_left + (0:2) * fig$method_column_width) grid::grid.lines(x = grid::unit(c(xx, xx), "mm"), 
    y = grid::unit(c(fig$body_bottom, top), "mm"), gp = grid::gpar(col = "#DEE2E5", lwd = 0.35))
  # Downward summaries begin exactly at the last matrix row's lower boundary.
  # Counts use all source records/stages, not the deduplicated cell labels.
  base <- fig$body_bottom
  drop_max <- max(1L,fig$item_drops$n)
for (j in 1:20) {
    xx <- fig$matrix_left + (j - 0.5) * fig$cell_width
    h <- fig$bar_height * fig$item_drops$n[j]/drop_max
    if (h > 0) 
        grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(base - h/2, "mm"), width = grid::unit(fig$tile_side, 
            "mm"), height = grid::unit(h, "mm"), gp = grid::gpar(fill = fig$bar_colour, col = NA, lwd = 0.25))
    grid::grid.text(fig$item_drops$n[j], x = grid::unit(xx, "mm"), y = grid::unit(base - h - 2.2, "mm"), just = "centre", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, fontface = "plain", col = "#161616"))
}
grid::grid.text("Times dropped", x = grid::unit(fig$k_center, "mm"), y = grid::unit(base - fig$bar_height/2, "mm"), 
    rot = 90, gp = grid::gpar(fontfamily = "Arial", fontsize = 7, col = "#161616"))
for (role in 1:2) {
    ss <- fig$stage_totals[fig$stage_totals$role_column == role, ]
    pitch <- fig$method_column_width/nrow(ss)
    for (j in seq_len(nrow(ss))) {
        xx <- fig$method_left + (role - 1) * fig$method_column_width + (j - 0.5) * pitch
        h <- fig$bar_height * ss$n[j]/max(fig$stage_totals$n)
        if (h > 0) 
            grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(base - h/2, "mm"), width = grid::unit(min(3.2, 
                pitch * 0.72), "mm"), height = grid::unit(h, "mm"), gp = grid::gpar(fill = ss$fill[j], col = NA, 
                lwd = 0.25))
        grid::grid.text(ss$n[j], x = grid::unit(xx, "mm"), y = grid::unit(base - h - 2.2, "mm"), just = "centre", 
            gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, fontface = "plain", col = "#161616"))
        label_grob <- grid::textGrob(ss$label[j], rot = 90, gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, 
            col = "#161616"))
        label_height <- grid::convertHeight(grid::grobHeight(label_grob), "mm", valueOnly = TRUE)
        label_y <- if (ss$label[j] %in% c("EFA", "CFA")) 
            base - h/2
        else base - h - 3.8 - label_height/2
        grid::grid.text(ss$label[j], x = grid::unit(xx, "mm"), y = grid::unit(label_y, "mm"), rot = 90, gp = grid::gpar(fontfamily = "Arial", 
            fontsize = 6.5, col = "#161616"))
    }
}
  # D-064: two 100% bars summarize the existing S1/S2 ratings once per Data
  # record. All widths remain proportional to n/72. The researcher approved
  # adjacent external callouts when rare categories cannot contain text.
quality_left <- fig$label_left
quality_width <- fig$quality_width
quality_height <- fig$quality_height
for (criterion in c("S1", "S2")) {
    # Match Figure 3: Inadequate, Partially adequate, Very good from left to right.
    q <- fig$record_quality %>% filter(.data$criterion == .env$criterion) %>% arrange(score)
    # Keep the existing annotation text; place the two small-category callouts
    # beneath their segments and the largest-category annotation inside its bar.
    inside_score <- if (criterion == "S1") 2L else 0L
    callout_positions <- if (criterion == "S1") c(0.07, 0.27, NA) else c(NA, 0.72, 0.9)
    callout_offsets <- if (criterion == "S1") c(1.1, 0.6, NA) else c(NA, 0.6, 1.1)
    yy <- fig$quality_centers[criterion]
    grid::grid.text(if (criterion == "S1") 
        "Sample Size"
    else "Validation Design", x = grid::unit(quality_left, "mm"), y = grid::unit(yy + 5.5, "mm"), just = "left", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, fontface = "bold", col = "#161616"))
    cursor <- quality_left
    for (j in seq_len(nrow(q))) {
        w <- quality_width * q$n[j]/q$denominator[j]
        xx <- cursor + w/2
        grid::grid.rect(x = grid::unit(xx, "mm"), y = grid::unit(yy, "mm"), width = grid::unit(w, "mm"), height = grid::unit(quality_height, 
            "mm"), gp = grid::gpar(fill = q$fill[j], col = "white", lwd = 0.3))
        if (q$score[j] == inside_score) {
            grid::grid.text(q$display_label[j], x = grid::unit(xx, "mm"), y = grid::unit(yy, "mm"), gp = grid::gpar(fontfamily = "Arial", 
                fontsize = 6, lineheight = 1.03, col = if (q$score[j] == 2) 
                  "white"
                else "#161616"))
        }
        else {
            label_x <- quality_left + quality_width * callout_positions[j]
            elbow_y <- yy - quality_height/2 - callout_offsets[j]
            grid::grid.lines(x = grid::unit(c(xx, xx), "mm"), y = grid::unit(c(yy - quality_height/2, elbow_y), 
                "mm"), gp = grid::gpar(col = "#67757F", lwd = 0.35))
            grid::grid.lines(x = grid::unit(c(xx, label_x), "mm"), y = grid::unit(c(elbow_y, elbow_y), "mm"), gp = grid::gpar(col = "#67757F", 
                lwd = 0.35))
            grid::grid.lines(x = grid::unit(c(label_x, label_x), "mm"), y = grid::unit(c(elbow_y, yy - 4.3), "mm"), 
                gp = grid::gpar(col = "#67757F", lwd = 0.35))
            grid::grid.text(q$display_label[j], x = grid::unit(label_x, "mm"), y = grid::unit(yy - 6.9, "mm"), 
                gp = grid::gpar(fontfamily = "Arial", fontsize = 6, lineheight = 1.03, col = "#161616"))
        }
        cursor <- cursor + w
    }
}
grid::popViewport()
dev.off()
