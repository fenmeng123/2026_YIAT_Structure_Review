# Author: Kunru Song
# Semantic figures: all geometry and display transformations are confined to Step 3.
library(dplyr)
library(grid)
library(cluster)
material <- readRDS('Res_2_Results/S2_4_SemanticAlignment/Semantic_Figures.rds')
palette_groups <- c('#6B9DBA','#D7A17F','#92AF91','#AA93B6','#C68197','#C0AF71','#65AFA7')
conditions <- material$conditions
profile_trees <- list()
upper_limit <- -Inf
for(key in names(conditions)) {
  p <- conditions[[key]]; weights <- 1/sqrt(3*as.numeric(table(p$criteria$framework)[p$criteria$framework]))
  profile_trees[[key]] <- hclust(dist(sweep(p$alignment,2,weights,'*')),method='average')
  upper_limit <- max(upper_limit,p$alignment)
}
profile_trees$S1 <- profile_trees$S2 <- profile_trees$main
filenames <- c(main='Figure_4_Personality_RevisedEnglish.pdf',S1='Figure_S1_MPNet_RevisedEnglish.pdf',
  S2='Figure_S2_Harmony_RevisedEnglish.pdf',S3='Figure_S3_Personality_OriginalEnglish.pdf',S4='Figure_S4_MultilingualMPNet_Chinese.pdf')
requested_k <- c(main=3L,S1=3L,S2=4L,S3=6L,S4=4L)
# Author-approved compact page and typography, matching the edited Figure 4.
page_height <- 191
panel_title_size <- 10
axis_title_size <- 8
annotation_size <- 6.6
dir.create('Res_3_Report',showWarnings=FALSE)
figure_keys <- 'main'
for (key in figure_keys) {
    payload <- conditions[[key]]
    supplementary <- key != "main"
    bilingual <- key == "S4"
    matrix_y <- 5.375
    matrix_size <- 70
    top <- 84.5
    cell_h <- 4.35
    if (!supplementary) {
        bx <- 122.06
        cx <- 218.52
        cw <- 66.406
    } else {
        bx <- 20
        cx <- 139
        cw <- 153
    }
    cairo_pdf(file.path("Res_3_Report", filenames[[key]]), width = 297/25.4, height = page_height/25.4, family = "Arial",
        bg = "white")
    grid.newpage()
    {
        grid::grid.rect(grid::unit(0, "mm"), grid::unit(page_height - 0, "mm"), grid::unit(297, "mm"), grid::unit(page_height,
            "mm"), just = c("left", "top"), gp = grid::gpar(fill = "#FFFFFF", col = NA, lwd = 0.3))
    }
    roles <- if (supplementary)
        "semantic"
    else c("paper_cooccurrence", "semantic")
    for (role in roles) {
        mat <- payload[[role]]
        distance <- as.dist(1 - mat)
        hc <- hclust(distance, method = "average")
        if (role == "semantic") {
            k <- requested_k[[key]]
            x <- bx
            panel <- if (supplementary)
                "A"
            else "B"
            title <- "Item–Item Semantic Similarity Matrix"
            palette <- c("#FFFFFF", "#E9CCAE", "#C3885F", "#8E4335")
        }
        else {
            silhouette_scores <- numeric(5)
            for (k0 in 2:6) silhouette_scores[k0 - 1] <- mean(silhouette(cutree(hc, k0), distance)[, "sil_width"])
            k <- (2:6)[which.max(silhouette_scores)]
            x <- 20
            panel <- "A"
            title <- "Item–Item Co-occurrence Matrix"
            palette <- c("#FFFFFF", "#BCD2E0", "#6D99B5", "#284F70")
        }
        original <- cutree(hc, k)
        groups <- match(original, unique(original[hc$order]))
        names(groups) <- names(original)
        cut_height <- mean(hc$height[c(20 - k, 21 - k)])
        limits <- range(mat[upper.tri(mat)])
        mm <- mat[hc$order, hc$order]
        y <- matrix_y
        cell <- matrix_size/20
        {
            grid::grid.text(paste0(panel, ".  ", title), x = grid::unit(if (!supplementary && role == "semantic") 106 else x - 15, "mm"), y = grid::unit(page_height - 2.5,
                "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = panel_title_size, fontface = "bold",
                col = "#263238", lineheight = 0.85), name = paste0("panel-title-", panel))
        }
        ramp <- colorRampPalette(palette)(256)
        for (i in 1:20) for (j in 1:20) if (i != j) {
            fill <- ramp[1 + round((mm[i, j] - limits[1])/diff(limits) * 255)]
            {
                grid::grid.rect(grid::unit(x + (j - 1) * cell, "mm"), grid::unit(page_height - (y + (i - 1) * cell), "mm"),
                  grid::unit(cell, "mm"), grid::unit(cell, "mm"), just = c("left", "top"), gp = grid::gpar(fill = fill,
                    col = "#FFFFFF", lwd = 0.22), name = paste0("pair-cell-", role, "-", i, "-", j))
            }
        }
        positions <- match(1:20, hc$order)
        centers <- numeric(19)
        members <- vector("list", 19)
        segments <- list()
        segment_colours <- character()
        for (j in 1:19) {
            children <- hc$merge[j, ]
            coords <- matrix(0, 2, 2)
            child_members <- list()
            for (h in 1:2) {
                child <- children[h]
                coords[h, ] <- if (child < 0)
                  c(positions[-child], 0)
                else c(centers[child], hc$height[child])
                child_members[[h]] <- if (child < 0)
                  -child
                else members[[child]]
            }
            centers[j] <- mean(coords[, 1])
            members[[j]] <- unlist(child_members)
            for (h in 1:2) {
                pos <- coords[h, ]
                cut <- min(hc$height[j], cut_height)
                g <- unique(groups[child_members[[h]]])
                if (pos[2] < cut) {
                  segments[[length(segments) + 1L]] <- c(pos, pos[1], cut)
                  segment_colours <- c(segment_colours, if (length(g) == 1) palette_groups[g] else "#586168")
                }
                if (hc$height[j] > max(pos[2], cut)) {
                  segments[[length(segments) + 1L]] <- c(pos[1], max(pos[2], cut), pos[1], hc$height[j])
                  segment_colours <- c(segment_colours, "#586168")
                }
            }
            g <- unique(groups[members[[j]]])
            segments[[length(segments) + 1L]] <- c(coords[1, 1], hc$height[j], coords[2, 1], hc$height[j])
            segment_colours <- c(segment_colours, if (hc$height[j] <= cut_height && length(g) == 1) palette_groups[g] else "#586168")
        }
        ss <- do.call(rbind, segments)
        scale <- 9/max(hc$height)
        for (j in seq_len(nrow(ss))) {
            grid::grid.lines(grid::unit(x - 6 - ss[j, c(2, 4)] * scale, "mm"), grid::unit(page_height - (y + (ss[j, c(1,
                3)] - 0.5) * cell), "mm"), gp = grid::gpar(col = segment_colours[j], lwd = 0.9, lineend = "butt",
                linejoin = "mitre"))
        }
        for (g in 1:k) {
            positions <- which(groups[hc$order] == g)
            first <- min(positions)
            size <- length(positions)
            {
                grid::grid.rect(grid::unit(x + (first - 1) * cell, "mm"), grid::unit(page_height - (y + (first - 1) * cell),
                  "mm"), grid::unit(size * cell, "mm"), grid::unit(size * cell, "mm"), just = c("left", "top"),
                  gp = grid::gpar(fill = NA, col = "#000000", lwd = 1.2))
            }
        }
        labs <- sub("^IAT_", "", rownames(mm))
        for (i in 1:20) {
            {
                grid::grid.text(labs[i], x = grid::unit(x - 1.7, "mm"), y = grid::unit(page_height - (y + (i - 0.5) * cell),
                  "mm"), just = "right", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 6.2, fontface = "plain",
                  col = "#263238", lineheight = 0.85))
            }
            {
                grid::grid.text(labs[i], x = grid::unit(x + (i - 0.5) * cell, "mm"), y = grid::unit(page_height - (y +
                  matrix_size + 2), "mm"), just = "centre", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 6.2,
                  fontface = "plain", col = "#263238", lineheight = 0.85))
            }
        }
        bar_x <- x + matrix_size + 2
        for (i in 1:256) {
            grid::grid.rect(grid::unit(bar_x, "mm"), grid::unit(page_height - (y + (i - 1) * matrix_size/256), "mm"), grid::unit(3,
                "mm"), grid::unit(matrix_size/256 + 0.001, "mm"), just = c("left", "top"), gp = grid::gpar(fill = ramp[257 -
                i], col = NA, lwd = 0.3))
        }
        {
            grid::grid.rect(grid::unit(bar_x, "mm"), grid::unit(page_height - y, "mm"), grid::unit(3, "mm"), grid::unit(matrix_size,
                "mm"), just = c("left", "top"), gp = grid::gpar(fill = NA, col = "#586168", lwd = 0.35))
        }
        ticks <- seq(limits[1], limits[2], length.out = 5)
        yy <- y + matrix_size - (ticks - limits[1])/diff(limits) * matrix_size
        for (i in 1:5) {
            {
                grid::grid.lines(grid::unit(c(bar_x + 3, bar_x + 4), "mm"), grid::unit(page_height - rep(yy[i], 2), "mm"),
                  gp = grid::gpar(col = "#263238", lwd = 0.45, lineend = "butt", linejoin = "mitre"))
            }
            {
                grid::grid.text(sprintf("%.2f", ticks[i]), x = grid::unit(bar_x + 4.7, "mm"), y = grid::unit(page_height -
                  yy[i], "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = 6.2, fontface = "plain",
                  col = "#263238", lineheight = 0.85))
            }
        }
    }
    use <- upper.tri(payload$semantic)
    xx <- payload$paper_cooccurrence[use]
    yy <- payload$semantic[use]
    trend <- payload$trend
    x <- cx
    y <- matrix_y
    width <- cw
    height <- 66.406
    xlim <- range(c(xx, trend$x))
    xlim <- xlim + c(-1, 1) * 0.04 * diff(xlim)
    ylim <- range(c(yy, trend$fit, trend$lower, trend$upper))
    ylim <- ylim + c(-1, 1) * 0.04 * diff(ylim)
    {
        grid::grid.text(paste0(if (supplementary)
            "B"
        else "C", ".  Correlation between Co-occurrence and Semantic"), x = grid::unit(if (supplementary) x - 13 else 205.8, "mm"), y = grid::unit(page_height -
            2.5, "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = panel_title_size, fontface = "bold",
            col = "#263238", lineheight = 0.85), name = "scatter-title")
    }
    band_x <- x + (c(trend$x, rev(trend$x)) - xlim[1])/diff(xlim) * width
    band_y <- y + height - (c(trend$lower, rev(trend$upper)) - ylim[1])/diff(ylim) * height
    grid.polygon(unit(band_x, "mm"), unit(page_height - band_y, "mm"), gp = gpar(fill = "#CBD8E588", col = NA))
    {
        grid::grid.lines(grid::unit(x + (trend$x - xlim[1])/diff(xlim) * width, "mm"), grid::unit(page_height - (y + height -
            (trend$fit - ylim[1])/diff(ylim) * height), "mm"), gp = grid::gpar(col = "#345F83", lwd = 1, lineend = "butt",
            linejoin = "mitre"))
    }
    point_x <- x + (xx - xlim[1])/diff(xlim) * width
    point_y <- y + height - (yy - ylim[1])/diff(ylim) * height
    grid.points(unit(point_x, "mm"), unit(page_height - point_y, "mm"), pch = 21, size = unit(1.05, "mm"), gp = gpar(col = "#8F5440",
        fill = "#C98F7088", lwd = 0.35))
    {
        grid::grid.lines(grid::unit(c(x, x, x + width), "mm"), grid::unit(page_height - c(y, y + height, y + height), "mm"),
            gp = grid::gpar(col = "#263238", lwd = 0.6, lineend = "butt", linejoin = "mitre"), name = "scatter-axes")
    }
    for (v in point_x) {
        grid::grid.lines(grid::unit(rep(v, 2), "mm"), grid::unit(page_height - c(y + height, y + height - 1.3), "mm"),
            gp = grid::gpar(col = "#284F70", lwd = 0.4, lineend = "butt", linejoin = "mitre"))
    }
    for (v in point_y) {
        grid::grid.lines(grid::unit(c(x, x + 1.3), "mm"), grid::unit(page_height - rep(v, 2), "mm"), gp = grid::gpar(col = "#8E4335",
            lwd = 0.4, lineend = "butt", linejoin = "mitre"))
    }
    xt <- pretty(xlim, n = 5)
    xt <- xt[xt >= xlim[1] - 1e-10 & xt <= xlim[2] + 1e-10]
    yt <- pretty(ylim, n = 5)
    yt <- yt[yt >= ylim[1] - 1e-10 & yt <= ylim[2] + 1e-10]
    for (v in xt) {
        position <- x + (v - xlim[1])/diff(xlim) * width
        {
            grid::grid.lines(grid::unit(rep(position, 2), "mm"), grid::unit(page_height - c(y + height, y + height + 1.25),
                "mm"), gp = grid::gpar(col = "#263238", lwd = 0.5, lineend = "butt", linejoin = "mitre"))
        }
        {
            grid::grid.text(format(v, trim = TRUE, scientific = FALSE, digits = 4), x = grid::unit(position, "mm"),
                y = grid::unit(page_height - (y + height + 3), "mm"), just = "centre", rot = 0, gp = grid::gpar(fontfamily = "Arial",
                  fontsize = 6.2, fontface = "plain", col = "#263238", lineheight = 0.85))
        }
    }
    for (v in yt) {
        position <- y + height - (v - ylim[1])/diff(ylim) * height
        {
            grid::grid.lines(grid::unit(c(x - 1.25, x), "mm"), grid::unit(page_height - rep(position, 2), "mm"), gp = grid::gpar(col = "#263238",
                lwd = 0.5, lineend = "butt", linejoin = "mitre"))
        }
        {
            grid::grid.text(format(v, trim = TRUE, scientific = FALSE, digits = 4), x = grid::unit(x - 2, "mm"),
                y = grid::unit(page_height - position, "mm"), just = "right", rot = 0, gp = grid::gpar(fontfamily = "Arial",
                  fontsize = 6.2, fontface = "plain", col = "#263238", lineheight = 0.85))
        }
    }
    {
        grid::grid.text("Factor co-occurrence", x = grid::unit(x + width/2, "mm"), y = grid::unit(page_height - (y + height +
            6.4), "mm"), just = "centre", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = axis_title_size, fontface = "plain",
            col = "#263238", lineheight = 0.85))
    }
    {
        grid::grid.text("Item–item cosine similarity", x = grid::unit(x - 7, "mm"), y = grid::unit(page_height - (y +
            height/2), "mm"), just = "centre", rot = 90, gp = grid::gpar(fontfamily = "Arial", fontsize = axis_title_size, fontface = "plain",
            col = "#263238", lineheight = 0.85))
    }
    annotation <- c(sprintf("Spearman’s rho = %.3f  ", payload$rho), "p", sprintf(" = %.4f", payload$qap_p))
    faces <- c("plain", "italic", "plain")
    widths <- numeric(3)
    for (j in 1:3) {
        pushViewport(viewport(gp = gpar(fontfamily = "Arial", fontsize = annotation_size, fontface = faces[j])))
        widths[j] <- convertWidth(stringWidth(annotation[j]), "mm", valueOnly = TRUE)
        popViewport()
        {
            grid::grid.text(annotation[j], x = grid::unit(x + 2.85 + sum(widths[seq_len(j - 1)]), "mm"), y = grid::unit(page_height -
                (y + if (key == "S3") 5.0 else 1.85), "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = annotation_size, fontface = faces[j],
                col = "#263238", lineheight = 0.85))
        }
    }
    hc <- profile_trees[[key]]
    ids <- hc$labels[hc$order]
    mm <- payload$alignment[ids, , drop = FALSE]
    criteria <- payload$criteria
    items <- payload$items[match(ids, payload$items$item_id), ]
    starts <- c(145, 193, 263.5)
    widths <- c(6, 9, 4)
    cell_w <- 7.5
    bottom <- top + 20 * cell_h
    word_width <- 120
    word_size <- 6.4
    mixed <- vector("list", 20)
    if (!bilingual) {
        repeat {
            pushViewport(viewport(gp = gpar(fontfamily = "Arial", fontsize = word_size)))
            measured <- convertWidth(stringWidth(items$display_wording), "mm", valueOnly = TRUE)
            popViewport()
            if (max(measured) <= word_width || word_size <= 6.01)
                break
            word_size <- round(word_size - 0.1, 1)
        }
    }
    else {
        translations <- material$display_translations[match(ids, material$display_translations$item_id), ]
        for (i in 1:20) {
            chars <- strsplit(enc2utf8(items$display_wording[i]), "", fixed = TRUE)[[1]]
            words <- strsplit(paste0("[", translations$translated_english[i], "]"), " +")[[1]]
            tokens <- data.frame(text = c(chars, paste0(" ", words)), family = c(ifelse(grepl("^[ -~]$", chars),
                "Arial", "Microsoft YaHei"), rep("Arial", length(words))), line = 1L, x_mm = 0)
            line <- 1L
            offset <- 0
            for (j in seq_len(nrow(tokens))) {
                pushViewport(viewport(gp = gpar(fontfamily = tokens$family[j], fontsize = word_size)))
                w <- convertWidth(stringWidth(tokens$text[j]), "mm", valueOnly = TRUE)
                if (offset + w > word_width && offset > 0) {
                  line <- line + 1L
                  offset <- 0
                  tokens$text[j] <- trimws(tokens$text[j], which = "left")
                  w <- convertWidth(stringWidth(tokens$text[j]), "mm", valueOnly = TRUE)
                }
                popViewport()
                tokens$line[j] <- line
                tokens$x_mm[j] <- offset
                offset <- offset + w
            }
            if (line > 2L)
                stop("Bilingual wording requires more than two lines: ", ids[i])
            mixed[[i]] <- tokens
        }
    }
    row_heights <- rep(cell_h, 20)
    if (bilingual) {
        multiline <- logical(20)
        for (i in 1:20) multiline[i] <- max(mixed[[i]]$line) > 1L
        row_heights[multiline] <- 7
        row_heights[!multiline] <- (20 * cell_h - sum(row_heights[multiline]))/sum(!multiline)
    }
    row_edges <- top + c(0, cumsum(row_heights))
    row_centers <- head(row_edges, -1) + row_heights/2
    leaf_y <- row_centers[match(seq_len(20), hc$order)]
    centers <- numeric(19)
    for (j in 1:19) {
        coords <- matrix(0, 2, 2)
        for (h in 1:2) {
            child <- hc$merge[j, h]
            coords[h, ] <- if (child < 0)
                c(leaf_y[-child], 0)
            else c(centers[child], hc$height[child])
        }
        centers[j] <- mean(coords[, 1])
        scale <- 9/max(hc$height)
        for (h in 1:2) {
            grid::grid.lines(grid::unit(14 - c(coords[h, 2], hc$height[j]) * scale, "mm"), grid::unit(page_height - rep(coords[h,
                1], 2), "mm"), gp = grid::gpar(col = "#586168", lwd = 0.7, lineend = "butt", linejoin = "mitre"))
        }
        {
            grid::grid.lines(grid::unit(rep(14 - hc$height[j] * scale, 2), "mm"), grid::unit(page_height - coords[, 1],
                "mm"), gp = grid::gpar(col = "#586168", lwd = 0.7, lineend = "butt", linejoin = "mitre"))
        }
    }
    {
        grid::grid.rect(grid::unit(15, "mm"), grid::unit(page_height - top, "mm"), grid::unit(130, "mm"), grid::unit(20 *
            cell_h, "mm"), just = c("left", "top"), gp = grid::gpar(fill = "#FFFFFF", col = "#A6AFB4", lwd = 0.4))
    }
    {
        grid::grid.lines(grid::unit(rep(21, 2), "mm"), grid::unit(page_height - c(top, bottom), "mm"), gp = grid::gpar(col = "#A6AFB4",
            lwd = 0.35, lineend = "butt", linejoin = "mitre"))
    }
    for (i in 1:19) {
        grid::grid.lines(grid::unit(c(15, 145), "mm"), grid::unit(page_height - rep(row_edges[i + 1], 2), "mm"), gp = grid::gpar(col = "#CED3D6",
            lwd = 0.3, lineend = "butt", linejoin = "mitre"))
    }
    for (i in 1:20) {
        {
            grid::grid.text(sub("IAT_", "", ids[i]), x = grid::unit(18, "mm"), y = grid::unit(page_height - row_centers[i],
                "mm"), just = "centre", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = word_size, fontface = "plain",
                col = "#263238", lineheight = 0.85))
        }
        if (bilingual) {
            tokens <- mixed[[i]]
            lines <- max(tokens$line)
            yy <- row_centers[i] + (seq_len(lines) - (lines + 1)/2) * 2.9
            for (j in seq_len(nrow(tokens))) {
                grid::grid.text(tokens$text[j], x = grid::unit(23 + tokens$x_mm[j], "mm"), y = grid::unit(page_height -
                  yy[tokens$line[j]], "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = tokens$family[j],
                  fontsize = word_size, fontface = "plain", col = "#263238", lineheight = 0.85))
            }
        }
        else {
            grid::grid.text(items$display_wording[i], x = grid::unit(23, "mm"), y = grid::unit(page_height - row_centers[i],
                "mm"), just = "left", rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = word_size, fontface = "plain",
                col = "#263238", lineheight = 0.85))
        }
    }
    wording_header <- if (key %in% c("main", "S1", "S2"))
        "Y-IAT item wording (Mak & Young, 2020)" else if (key == "S3")
        "Y-IAT item wording (Original English)" else
        "Y-IAT item wording (Chinese; English translation)"
    grid.text("No.", x = unit(18, "mm"), y = unit(page_height - (top - 2.8), "mm"),
        gp = gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold", col = "#263238"),
        name = "item-number-header")
    grid.text(wording_header, x = unit(23, "mm"), y = unit(page_height - (top - 2.8), "mm"),
        just = "left", gp = gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold", col = "#263238"),
        name = "wording-header")
    if (!bilingual) {
        pushViewport(viewport(gp = gpar(fontfamily = "Arial", fontsize = 8, fontface = "bold")))
        header_width <- convertWidth(stringWidth(wording_header), "mm", valueOnly = TRUE)
        popViewport()
        grid.text("How often do you...", x = unit(23 + header_width + 2.5, "mm"),
            y = unit(page_height - (top - 2.8), "mm"), just = "left",
            gp = gpar(fontfamily = "Arial", fontsize = 8, fontface = "italic", col = "#263238"),
            name = "shared-question-stem")
    }
    titles <- paste0(if (supplementary)
        c("C", "D", "E")
    else c("D", "E", "F"), ".  ", c("Six-component model", "DSM-5-TR", "ICD-11"))
    ramp <- colorRampPalette(c("#DECFCD", "#BD805F", "#793E37"))(256)
    offset <- 0L
    for (g in 1:3) {
        x <- starts[g]
        {
            grid::grid.text(titles[g], x = grid::unit(x, "mm"), y = grid::unit(page_height - (top - 2.8), "mm"), just = "left",
                rot = 0, gp = grid::gpar(fontfamily = "Arial", fontsize = panel_title_size, fontface = "bold", col = "#263238",
                  lineheight = 0.85), name = paste0("alignment-title-", g))
        }
        jj_all <- offset + seq_len(widths[g])
        for (jj in seq_len(widths[g])) {
            j <- offset + jj
            {
                grid::grid.text(criteria$Standardized_Label[j], x = grid::unit(x + (jj - 0.5) * cell_w, "mm"),
                  y = grid::unit(page_height - (bottom + 2), "mm"), just = c("right", "centre"), rot = 30, gp = grid::gpar(fontfamily = "Arial",
                    fontsize = 6.2, fontface = "plain", col = "#263238", lineheight = 0.85))
            }
            for (i in 1:20) {
                value <- mm[i, j]
                below <- value < 0.1
                fill <- if (below)
                  "#D3D3D3"
                else ramp[1 + round((value - 0.1)/(upper_limit - 0.1) * 255)]
                ink <- if (below)
                  "#FFFFFF"
                else "#000000"
                {
                  grid::grid.rect(grid::unit(x + (jj - 1) * cell_w, "mm"), grid::unit(page_height - row_edges[i], "mm"),
                    grid::unit(cell_w, "mm"), grid::unit(row_heights[i], "mm"), just = c("left", "top"), gp = grid::gpar(fill = fill,
                      col = "#FFFFFF", lwd = 0.3), name = paste0("alignment-cell-", g, "-", i, "-", jj))
                }
                {
                  grid::grid.text(sub("^(-?)0\\.", "\\1.", sprintf("%.2f", value)), x = grid::unit(x + (jj - 0.5) *
                    cell_w, "mm"), y = grid::unit(page_height - row_centers[i], "mm"), just = "centre", rot = 0, gp = grid::gpar(fontfamily = "Arial",
                    fontsize = 6.3, fontface = "plain", col = ink, lineheight = 0.85))
                }
            }
        }
        for (jj in seq_len(widths[g])) for (i in 1:20) {
            j <- offset + jj
            if (!ids[i] %in% c("IAT_4", "IAT_7") && mm[i, j] == max(mm[i, jj_all])) {
                grid::grid.rect(grid::unit(x + (jj - 1) * cell_w, "mm"), grid::unit(page_height - row_edges[i], "mm"),
                  grid::unit(cell_w, "mm"), grid::unit(row_heights[i], "mm"), just = c("left", "top"), gp = grid::gpar(fill = NA,
                    col = "#000000", lwd = 1))
            }
        }
        offset <- offset + widths[g]
    }
    dev.off()
}
