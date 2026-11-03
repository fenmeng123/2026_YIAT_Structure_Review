# Author: Kunru Song
# Figure 3. Geographical and language coverage, reported factor numbers, and quality assessment.
# Study characteristics and exploration/validation quality profiles.
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
study <- readRDS('Res_2_Results/S1_2_StudyCharacteristicsDescriptiveVisualization/Figure_3_StudyCharacteristics.rds')
map_sources <- readRDS('Res_2_Results/S1_2_StudyCharacteristicsDescriptiveVisualization/Figure_3_Map.rds')
quality <- readRDS('Res_2_Results/S1_4_QualityAssessmentAnalysis/Figure_3_QualityAssessment.rds')
world <- list()
for(layer in names(map_sources)) {
  features <- map_sources[[layer]]; rows <- list()
  for(i in seq_along(features)) {
    feature <- features[[i]]; geometry <- feature$geometry
    xy <- cbind(x=unlist(lapply(geometry$points,'[[','x')),y=unlist(lapply(geometry$points,'[[','y')))
    parts <- unlist(geometry$parts); if(!length(parts))parts <- nrow(xy)
    country <- if('FCNAME' %in% unlist(feature$fieldNames))unlist(feature$fieldValues)[match('FCNAME',unlist(feature$fieldNames))] else ''
    rows[[i]] <- data.frame(x=xy[,1],y=xy[,2],feature=paste(layer,feature$ID,sep='_'),
      ring=paste(layer,feature$ID,rep(seq_along(parts),parts),sep='_'),country_cn=country)
  }
  world[[layer]] <- bind_rows(rows)
}
dir.create('Res_3_Report',showWarnings=FALSE)
cairo_pdf('Res_3_Report/Figure_3_StudyCharacteristicsQualityEvaluation.pdf',width=260/25.4,height=326/25.4,family='Arial',bg='white')
grid.newpage()

# Build the upper band from its own record-level statistics.
figure <- {
    a <- {
        study$display_counts$language$start <- c(0, head(cumsum(study$display_counts$language$n)/sum(study$display_counts$language$n) * 
            2 * pi, -1))
        study$display_counts$language$end <- cumsum(study$display_counts$language$n)/sum(study$display_counts$language$n) * 
            2 * pi
        palette <- setNames(grDevices::hcl(h = (seq_len(nrow(study$display_counts$language)) * 137.508 + 205)%%360, 
            c = 42, l = 64), study$display_counts$language$category)
        palette["Other Languages"] <- "#B8BDC5"
        {
            slice_rows <- list()
            for (i in seq_len(nrow(study$display_counts$language))) {
                theta <- seq(study$display_counts$language$start[i], study$display_counts$language$end[i], length.out = 100)
                slice_rows[[i]] <- data.frame(category = study$display_counts$language$category[i], x = c(25, 25 + 
                  11 * cos(theta), 25), y = c(72, 72 - 11 * sin(theta), 72))
            }
            slices <- dplyr::bind_rows(slice_rows)
        }
        study$display_counts$language$y <- 55 - (seq_len(nrow(study$display_counts$language)) - 1) * 3.35
        study$display_counts$language$display <- ifelse(study$display_counts$language$category == "Other Languages", 
            sprintf("Other %d languages", study$totals$singleton_languages), study$display_counts$language$category)
        study$display_counts$language$count <- sprintf("%d (%.1f%%)", study$display_counts$language$n, study$display_counts$language$percent)
        ggplot2::ggplot(slices, ggplot2::aes(x, y, group = category, fill = category)) + ggplot2::geom_polygon(colour = "white", 
            linewidth = 0.18, show.legend = FALSE) + ggplot2::geom_point(data = study$display_counts$language, 
            ggplot2::aes(x = 1.6, y = y, fill = category), shape = 22, size = 1.6, stroke = 0, inherit.aes = FALSE, 
            show.legend = FALSE) + ggplot2::geom_text(data = study$display_counts$language, ggplot2::aes(x = 4, 
            y = y, label = display), hjust = 0, size = 7.2/ggplot2::.pt, family = "Arial", inherit.aes = FALSE) + 
            ggplot2::geom_text(data = study$display_counts$language, ggplot2::aes(x = 49, y = y, label = count), 
                hjust = 1, size = 7.2/ggplot2::.pt, family = "Arial", inherit.aes = FALSE) + ggplot2::annotate("text", 
            x = 25, y = 87, label = "Y-IAT language versions", size = 8.5/ggplot2::.pt, fontface = "bold", family = "Arial") + 
            ggplot2::annotate("text", x = 49, y = 59.5, label = "n (%)", hjust = 1, size = 7.2/ggplot2::.pt, family = "Arial") + 
            ggplot2::scale_fill_manual(values = palette) + ggplot2::coord_fixed(xlim = c(0, 50), ylim = c(0, 91), 
            expand = FALSE) + ggplot2::theme_void(base_family = "Arial") + ggplot2::theme(plot.margin = ggplot2::margin(0, 
            0, 0, 0))
    }
    c <- {
        study$display_counts$region$start <- c(0, head(cumsum(study$display_counts$region$n)/sum(study$display_counts$region$n) * 
            2 * pi, -1))
        study$display_counts$region$end <- cumsum(study$display_counts$region$n)/sum(study$display_counts$region$n) * 
            2 * pi
        palette <- setNames(grDevices::hcl(h = (seq_len(nrow(study$display_counts$region)) * 137.508 + 205)%%360, 
            c = 42, l = 64), study$display_counts$region$category)
        palette["Other Languages"] <- "#B8BDC5"
        {
            slice_rows <- list()
            for (i in seq_len(nrow(study$display_counts$region))) {
                theta <- seq(study$display_counts$region$start[i], study$display_counts$region$end[i], length.out = 100)
                slice_rows[[i]] <- data.frame(category = study$display_counts$region$category[i], x = c(25, 25 + 
                  11 * cos(theta), 25), y = c(72, 72 - 11 * sin(theta), 72))
            }
            slices <- dplyr::bind_rows(slice_rows)
        }
        study$display_counts$region$y <- 55 - (seq_len(nrow(study$display_counts$region)) - 1) * 3.35
        study$display_counts$region$display <- ifelse(study$display_counts$region$category == "Other Languages", 
            sprintf("Other %d languages", 0L), study$display_counts$region$category)
        study$display_counts$region$count <- sprintf("%d (%.1f%%)", study$display_counts$region$n, study$display_counts$region$percent)
        ggplot2::ggplot(slices, ggplot2::aes(x, y, group = category, fill = category)) + ggplot2::geom_polygon(colour = "white", 
            linewidth = 0.18, show.legend = FALSE) + ggplot2::geom_point(data = study$display_counts$region, ggplot2::aes(x = 1.6, 
            y = y, fill = category), shape = 22, size = 1.6, stroke = 0, inherit.aes = FALSE, show.legend = FALSE) + 
            ggplot2::geom_text(data = study$display_counts$region, ggplot2::aes(x = 4, y = y, label = display), 
                hjust = 0, size = 7.2/ggplot2::.pt, family = "Arial", inherit.aes = FALSE) + ggplot2::geom_text(data = study$display_counts$region, 
            ggplot2::aes(x = 49, y = y, label = count), hjust = 1, size = 7.2/ggplot2::.pt, family = "Arial", inherit.aes = FALSE) + 
            ggplot2::annotate("text", x = 25, y = 87, label = "Geographic regions", size = 8.5/ggplot2::.pt, fontface = "bold", 
                family = "Arial") + ggplot2::annotate("text", x = 49, y = 59.5, label = "n (%)", hjust = 1, size = 7.2/ggplot2::.pt, 
            family = "Arial") + ggplot2::scale_fill_manual(values = palette) + ggplot2::coord_fixed(xlim = c(0, 
            50), ylim = c(0, 91), expand = FALSE) + ggplot2::theme_void(base_family = "Arial") + ggplot2::theme(plot.margin = ggplot2::margin(0, 
            0, 0, 0))
    }
    b <- {
        antarctic_coasts <- paste0("coastlines_", 1:32)
        stopifnot("南极洲" %in% world$countries$country_cn, all(antarctic_coasts %in% world$coastlines$feature))
        world$countries <- world$countries[world$countries$country_cn != "南极洲", ]
        world$coastlines <- world$coastlines[!world$coastlines$feature %in% antarctic_coasts, ]
        key <- {
            data.frame(map_unit = c("Bangladesh", "Brazil", "Canada", "Colombia", "Costa Rica", "Croatia", "Ethiopia", 
                "Finland", "France", "Germany", "Greece", "China", "India", "Indonesia", "Iran", "Israel", "Italy", 
                "Japan", "Jordan", "Lebanon", "Malaysia", "North Macedonia", "Pakistan", "Peru", "Poland", "Portugal", 
                "Romania", "Singapore", "South Africa", "South Korea", "Spain", "Switzerland", "Thailand", "Turkey", 
                "United Kingdom", "United States"), country_cn = c("孟加拉国", "巴西", "加拿大", "哥伦比亚", 
                "哥斯达黎加", "克罗地亚", "埃塞俄比亚", "芬兰", "法国", "德国", "希腊", "中华人民共和国", 
                "印度", "印度尼西亚", "伊朗", "以色列/巴勒斯坦", "意大利", "日本", "约旦", 
                "黎巴嫩", "马来西亚", "北马其顿", "巴基斯坦", "秘鲁", "波兰", "葡萄牙", "罗马尼亚", 
                "新加坡", "南非", "韩国", "西班牙", "瑞士", "泰国", "土耳其", "英国", "美国"))
        }
        stopifnot(all(study$country_totals$map_unit %in% key$map_unit))
        totals <- dplyr::left_join(study$country_totals, key, by = "map_unit")
        stopifnot(all(totals$country_cn %in% world$countries$country_cn))
        world$countries <- dplyr::left_join(world$countries, totals, by = "country_cn")
        world$countries$aggregated_size[is.na(world$countries$aggregated_size)] <- 0
        world$countries$reported_size <- ifelse(world$countries$aggregated_size > 0, world$countries$aggregated_size, 
            NA_real_)
        positive_limits <- range(totals$aggregated_size[is.finite(totals$aggregated_size) & totals$aggregated_size > 
            0])
        scale_breaks <- pretty(positive_limits, n = 3)
        scale_breaks <- sort(unique(c(positive_limits, scale_breaks[scale_breaks > positive_limits[1] + 0.12 * 
            diff(positive_limits) & scale_breaks < positive_limits[2] - 0.12 * diff(positive_limits)])))
        multi <- study$map_exceptions[grepl(";", study$map_exceptions$country_raw, fixed = TRUE), ]
        multi_note <- paste(sprintf("%s, %s", gsub(";", " /", multi$country_raw, fixed = TRUE), format(multi$size, 
            big.mark = ",", scientific = FALSE, trim = TRUE)), collapse = "; ")
        incomplete <- totals$map_unit[totals$missing_size_records > 0]
        missing_note <- if (length(incomplete)) 
            paste("Incomplete N:", paste(incomplete, collapse = ", "))
        else "No missing N among single-country records"
        p <- ggplot2::ggplot() + ggplot2::geom_polygon(data = world$countries, ggplot2::aes(x, y, group = feature, 
            subgroup = ring, fill = reported_size), rule = "evenodd", colour = NA) + ggplot2::geom_path(data = world$coastlines, 
            ggplot2::aes(x, y, group = ring), linewidth = 0.13, colour = "#333333") + ggplot2::geom_path(data = world$borders, 
            ggplot2::aes(x, y, group = ring), linewidth = 0.13, colour = "#333333") + ggplot2::geom_path(data = world$undetermined, 
            ggplot2::aes(x, y, group = ring), linewidth = 0.13, colour = "#333333", linetype = "dashed") + ggplot2::geom_path(data = world$region_boundaries, 
            ggplot2::aes(x, y, group = ring), linewidth = 0.13, colour = "#333333", linetype = "dotted") + ggplot2::geom_path(data = world$ceasefire, 
            ggplot2::aes(x, y, group = ring), linewidth = 0.13, colour = "#333333", linetype = "dashed") + ggplot2::geom_point(data = world$islands, 
            ggplot2::aes(x, y), size = 0.12, colour = "#333333") + ggplot2::scale_fill_gradient(low = "#87CEEB", 
            high = "#08306B", name = "Number of participants (N)", labels = scales::label_comma(), limits = positive_limits, 
            breaks = scale_breaks, na.value = "#E6E6E6", guide = ggplot2::guide_colourbar(barwidth = grid::unit(45, 
                "mm"), barheight = grid::unit(2.6, "mm"), title.position = "top", display = "rectangles")) + ggplot2::coord_fixed(xlim = c(-1.66, 
            1.66), ylim = c(-0.9, 1.05), expand = FALSE) + ggplot2::labs(title = "Geographic coverage by sample size", 
            subtitle = sprintf("%s reported participants across %d mapped units", format(study$totals$mapped_size, 
                big.mark = ",", scientific = FALSE), nrow(totals)), caption = paste0("Light gray: no allocatable reported N. Sky blue to dark blue: increasing reported N (minimum N = ", 
                positive_limits[1], ").\nUnallocated N: ", multi_note, ".\nChina includes mainland and Hong Kong; ", 
                missing_note, ". MNR joint Israel / Palestine unit uses Israel records.")) + {
            ggplot2::theme_classic(base_size = 8, base_family = "Arial") + ggplot2::theme(plot.margin = ggplot2::margin(2, 
                2, 2, 2), plot.background = ggplot2::element_rect(fill = "white", colour = NA), panel.background = ggplot2::element_rect(fill = "white", 
                colour = NA), axis.line = ggplot2::element_line(linewidth = 0.25), axis.ticks = ggplot2::element_line(linewidth = 0.25), 
                plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5), legend.text = ggplot2::element_text(size = 7.5), 
                legend.title = ggplot2::element_text(size = 8))
        } + ggplot2::theme(axis.title = ggplot2::element_blank(), axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), 
            axis.line = ggplot2::element_blank(), legend.position = "bottom", legend.background = ggplot2::element_blank())
        list(plot = p, totals = totals)
    }$plot + ggplot2::labs(title = NULL, subtitle = NULL, caption = NULL, fill = "Number of participants (N)") + 
        ggplot2::theme(legend.title = ggplot2::element_text(size = 8), legend.text = ggplot2::element_text(size = 7.5), 
            legend.margin = ggplot2::margin(0, 0, 0, 0), plot.margin = ggplot2::margin(0, 0, 0, 0))
    scatter <- ggplot2::ggplotGrob({
        d <- study$scatter
        known <- !is.na(d$size)
        t <- study$trend$predictions
        ggplot2::ggplot() + ggplot2::geom_ribbon(data = t, ggplot2::aes(year, ymin = lower, ymax = upper), fill = "#CBD8E5", 
            alpha = 0.55) + ggplot2::geom_line(data = t, ggplot2::aes(year, fit), colour = "#345F83", linewidth = 0.65) + 
            ggplot2::geom_point(data = d[known, ], ggplot2::aes(year_display, number_of_factors, size = size, shape = research_class), 
                fill = "white", colour = "black", stroke = 0.4) + ggplot2::geom_point(data = d[!known, ], ggplot2::aes(year_display, 
            number_of_factors, shape = research_class), size = 2, fill = "#D0D0D0", colour = "black", stroke = 0.4, 
            show.legend = FALSE) + ggplot2::scale_shape_manual(values = c(I = 23, II = 22, III = 21), limits = c("I", 
            "II", "III"), name = "Study design", labels = c("Exploration only", "Validation only", "Exploration and validation")) + 
            ggplot2::scale_size_area(max_size = 5.5, breaks = c(100, 500, 2000), labels = c("100", "500", "2,000"), 
                name = "Sample size (N)") + ggplot2::scale_y_continuous(breaks = seq_len(max(study$Data$number_of_factors, 
            na.rm = TRUE)), limits = c(0.5, max(study$Data$number_of_factors, na.rm = TRUE) + 0.5), expand = c(0, 
            0)) + ggplot2::scale_x_continuous(breaks = seq(2005, max(d$year), 5), expand = ggplot2::expansion(add = 0.7)) + 
            ggplot2::labs(x = "Publication year", y = "The best number of factors") + {
            ggplot2::theme_classic(base_size = 8, base_family = "Arial") + ggplot2::theme(plot.margin = ggplot2::margin(2, 
                2, 2, 2), plot.background = ggplot2::element_rect(fill = "white", colour = NA), panel.background = ggplot2::element_rect(fill = "white", 
                colour = NA), axis.line = ggplot2::element_line(linewidth = 0.25), axis.ticks = ggplot2::element_line(linewidth = 0.25), 
                plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5), legend.text = ggplot2::element_text(size = 7.5), 
                legend.title = ggplot2::element_text(size = 8))
        } + ggplot2::guides(shape = ggplot2::guide_legend(order = 1, override.aes = list(size = 2)), size = ggplot2::guide_legend(order = 2, 
            override.aes = list(shape = 21))) + ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.08, 
            0.98), legend.justification.inside = c(0, 1), legend.box = "horizontal", legend.key.height = grid::unit(3.2, 
            "mm"), legend.key.width = grid::unit(3, "mm"), legend.text = ggplot2::element_text(size = 7), legend.title = ggplot2::element_text(size = 7.5), 
            legend.margin = ggplot2::margin(1, 2, 1, 2), legend.spacing = grid::unit(2, "mm"), legend.background = ggplot2::element_blank(), 
            legend.box.background = ggplot2::element_blank(), panel.border = ggplot2::element_rect(fill = NA, colour = "#555555", 
                linewidth = 0.25))
    })
    bars <- ggplot2::ggplotGrob({
        tab <- study$counts$factors
        tab <- tab[tab$category != "Not reported", ]
        tab$k <- as.integer(tab$category)
        tab$label <- sprintf("%d (%.1f%%)", tab$n, 100 * tab$n/nrow(study$Data))
        ggplot2::ggplot(tab, ggplot2::aes(n, k)) + ggplot2::geom_col(orientation = "y", width = 0.6, fill = "#527DA0") + 
            ggplot2::geom_text(ggplot2::aes(label = label), hjust = 0, nudge_x = 0.45, size = 7.5/ggplot2::.pt, 
                family = "Arial") + ggplot2::scale_y_continuous(breaks = seq_len(max(study$Data$number_of_factors, 
            na.rm = TRUE)), limits = c(0.5, max(study$Data$number_of_factors, na.rm = TRUE) + 0.5), expand = c(0, 
            0)) + ggplot2::scale_x_continuous(limits = c(0, max(tab$n) * 1.35), breaks = c(0, 10, 20), expand = c(0, 
            0)) + ggplot2::labs(x = "Number of studies", y = NULL) + {
            ggplot2::theme_classic(base_size = 8, base_family = "Arial") + ggplot2::theme(plot.margin = ggplot2::margin(2, 
                2, 2, 2), plot.background = ggplot2::element_rect(fill = "white", colour = NA), panel.background = ggplot2::element_rect(fill = "white", 
                colour = NA), axis.line = ggplot2::element_line(linewidth = 0.25), axis.ticks = ggplot2::element_line(linewidth = 0.25), 
                plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5), legend.text = ggplot2::element_text(size = 7.5), 
                legend.title = ggplot2::element_text(size = 8))
        } + ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
    })
    heights <- grid::unit.pmax(scatter$heights, bars$heights)
    scatter$heights <- bars$heights <- heights
    panel <- bars$layout[bars$layout$name == "panel", ]
    inset <- grid::grobTree(ggplot2::ggplotGrob({
        study$counts$research$start <- c(0, head(cumsum(study$counts$research$n)/sum(study$counts$research$n) * 
            2 * pi, -1))
        study$counts$research$end <- cumsum(study$counts$research$n)/sum(study$counts$research$n) * 2 * pi
        study$counts$research$mid <- (study$counts$research$start + study$counts$research$end)/2
        {
            polygon_rows <- list()
            for (i in seq_len(nrow(study$counts$research))) {
                angle <- seq(study$counts$research$start[i], study$counts$research$end[i], length.out = 80)
                polygon_rows[[i]] <- data.frame(category = study$counts$research$category[i], x = c(11, 11 + 10 * 
                  cos(angle), 11), y = c(13, 13 - 10 * sin(angle), 13))
            }
            poly <- dplyr::bind_rows(polygon_rows)
        }
        study$counts$research$x <- 25
        study$counts$research$y <- c(22, 13, 4)
        ggplot2::ggplot(poly, ggplot2::aes(x, y, group = category, fill = category)) + ggplot2::geom_polygon(colour = "white", 
            linewidth = 0.2) + ggplot2::geom_point(data = study$counts$research, ggplot2::aes(x = x, y = y, shape = category, 
            fill = category), colour = "#111111", size = 1.7, stroke = 0.35, inherit.aes = FALSE) + ggplot2::geom_text(data = study$counts$research, 
            ggplot2::aes(x = x + 3, y = y, label = sprintf("%d (%.1f%%)", n, percent)), hjust = 0, size = 7/ggplot2::.pt, 
            family = "Arial", inherit.aes = FALSE) + ggplot2::scale_shape_manual(values = c(I = 23, II = 22, III = 21)) + 
            ggplot2::scale_fill_manual(values = c(I = "#527DA0", II = "#C89A70", III = "#89A99D")) + ggplot2::coord_fixed(xlim = c(0, 
            51), ylim = c(0, 27), expand = FALSE) + ggplot2::labs(title = "Study design") + ggplot2::theme_void(base_family = "Arial") + 
            ggplot2::theme(legend.position = "none", plot.title = ggplot2::element_text(size = 7.5, hjust = 0.5), 
                plot.margin = ggplot2::margin(0, 0, 0, 0))
    }), vp = grid::viewport(x = 0.67, y = 0.8, width = 0.65, height = 0.46))
    bars <- gtable::gtable_add_grob(bars, inset, t = panel$t, l = panel$l, b = panel$b, r = panel$r, name = "study-design-inset", 
        clip = "off")
    parts <- list(language = ggplot2::ggplotGrob(a), map = ggplot2::ggplotGrob(b), region = ggplot2::ggplotGrob(c), 
        scatter = scatter, bars = bars)
    positions <- list(language = c(206, 72, 51, 91), map = c(55, 74, 150, 89), region = c(3, 72, 51, 91), scatter = c(3, 
        7, 184, 61), bars = c(189, 7, 68, 61))
    list(parts = parts, positions = positions, width_mm = 260, height_mm = 176)
}
pushViewport(viewport(x=unit(0,'mm'),y=unit(150,'mm'),width=unit(260,'mm'),height=unit(176,'mm'),just=c('left','bottom')))
grid.text('A. Language versions, participants and geographic coverage',x=unit(3,'mm'),y=unit(171,'mm'),
  just=c('left','centre'),gp=gpar(fontfamily='Arial',fontsize=11,fontface='bold'))
grid.text('B. Reported Y-IAT factor counts and publication year',x=unit(3,'mm'),y=unit(70,'mm'),
  just=c('left','centre'),gp=gpar(fontfamily='Arial',fontsize=11,fontface='bold'))
for(name in names(figure$parts)) {
  p <- figure$positions[[name]]
  pushViewport(viewport(x=unit(p[1],'mm'),y=unit(p[2],'mm'),width=unit(p[3],'mm'),height=unit(p[4],'mm'),just=c('left','bottom')))
  grid.draw(figure$parts[[name]]);popViewport()
}
popViewport()

# Lower quality panels: applicable-stage denominators; five whole-bar NA placeholders.
criterion_labels <- quality$labels
criterion_labels[c("M2", "R2", "R5")] <- c(
  "Factor extraction",
  "Response scale and input type", "Rationale of final solution"
)
items <- names(criterion_labels)
# Compact the title and legend gaps while retaining 8-mm row spacing.
row_y <- c(seq(137, 113, by = -8), seq(94, 30, by = -8))
colours <- c('0'='#E4B5B0','1'='#E6D4AF','2'='#527DA0')
pushViewport(viewport(x=0,y=0,width=unit(260,'mm'),height=unit(159,'mm'),just=c('left','bottom')))
grid::grid.text("Methodological support", x = grid::unit(7, "mm"), y = grid::unit(144, "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", 
    fontsize = 8.5, fontface = "bold", col = "#20272B"))
grid::grid.text("Reporting completeness", x = grid::unit(7, "mm"), y = grid::unit(102, "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", 
    fontsize = 8.5, fontface = "bold", col = "#20272B"))
for (j in seq_along(items)) {
    grid::grid.text(items[j], x = grid::unit(7, "mm"), y = grid::unit(row_y[j], "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", 
        fontsize = 7.5, fontface = "bold", col = "#20272B"))
    grid::grid.text(criterion_labels[j], x = grid::unit(18, "mm"), y = grid::unit(row_y[j], "mm"), just = "left", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 7.3, fontface = "plain", col = "#20272B"))
}
for (k in 1:2) {
    role <- c("Exploration", "Validation")[k]
    left <- c(62, 159)[k]
    w <- 66
    grid::grid.text(paste0(c("C. ", "D. ")[k], role), x = grid::unit(left, "mm"), y = grid::unit(150, "mm"), just = "left", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 10, fontface = "bold", col = "#20272B"))
    grid::grid.text("Counts (N)", x = grid::unit(left + w + 3, "mm"), y = grid::unit(144, "mm"), just = "left", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, fontface = "bold", col = "#20272B"))
    for (j in seq_along(items)) {
        yy <- row_y[j]
        item <- items[j]
        sm <- quality$summary %>% filter(stage_role == role, criterion == item)
        whole_na <- sm$applicable == 0 || (role == "Validation" && item %in% c("M2", "M4", "R6")) || (role == 
            "Exploration" && item == "R8")
        if (whole_na) {
            grid.rect(x = unit(left + w/2, "mm"), y = unit(yy, "mm"), width = unit(w, "mm"), height = unit(4.7, 
                "mm"), gp = gpar(fill = NA, col = "#C7CDD1", lwd = 0.55))
            grid::grid.text("Not Applicable", x = grid::unit(left + w/2, "mm"), y = grid::unit(yy, "mm"), just = "centre", 
                gp = grid::gpar(fontfamily = "Arial", fontsize = 7.5, fontface = "plain", col = "#59656D"))
            next
        }
        n <- unlist(sm[c("n0", "n1", "n2")], use.names = FALSE)
        proportion <- n/sum(n)
        segments <- data.frame(score = 0:2, n, proportion, xmin = c(0, head(cumsum(proportion), -1)), xmax = cumsum(proportion)) %>% 
            filter(n > 0) %>% mutate(label = sprintf("%.1f%%", 100 * proportion), center = left + (xmin + xmax)/2 * 
            w)
        widths <- numeric(nrow(segments))
        for (i in seq_len(nrow(segments))) widths[i] <- convertWidth(grobWidth(textGrob(segments$label[i], gp = gpar(fontfamily = "Arial", 
            fontsize = 6.8))), "mm", valueOnly = TRUE)
        segments$inside <- widths + 1.2 <= segments$proportion * w
        segments$label_x <- segments$center
        external <- which(!segments$inside)
        if (length(external)) {
            widths <- numeric(length(external))
            for (i in seq_along(external)) widths[i] <- convertWidth(grobWidth(textGrob(segments$label[external[i]], 
                gp = gpar(fontfamily = "Arial", fontsize = 6.3))), "mm", valueOnly = TRUE)
            positions <- pmax(left + widths/2, pmin(left + w - widths/2, segments$center[external]))
            if (length(external) > 1L) {
                for (i in 2:length(external)) positions[i] <- max(positions[i], positions[i - 1] + (widths[i - 
                  1] + widths[i])/2 + 0.8)
                if (tail(positions + widths/2, 1) > left + w) {
                  positions[length(external)] <- left + w - tail(widths, 1)/2
                  for (i in (length(external) - 1L):1L) positions[i] <- min(positions[i], positions[i + 1] - (widths[i] + 
                    widths[i + 1])/2 - 0.8)
                }
            }
            segments$label_x[external] <- positions
        }
        for (i in seq_len(nrow(segments))) {
            d <- segments[i, ]
            xx <- d$center
            grid.rect(x = unit(xx, "mm"), y = unit(yy, "mm"), width = unit(d$proportion * w, "mm"), height = unit(4.7, 
                "mm"), gp = gpar(fill = colours[as.character(d$score)], col = "white", lwd = 0.35))
            if (d$inside) 
                grid::grid.text(d$label, x = grid::unit(xx, "mm"), y = grid::unit(yy, "mm"), just = "centre", gp = grid::gpar(fontfamily = "Arial", 
                  fontsize = 6.8, fontface = "plain", col = if (d$score == 2) 
                    "white"
                  else "#20272B"))
            else {
                grid::grid.lines(x = grid::unit(c(xx, d$label_x), "mm"), y = grid::unit(c(yy + 2.35, yy + 2.9), 
                  "mm"), gp = grid::gpar(col = "#59656D", lwd = 0.4))
                grid::grid.text(d$label, x = grid::unit(d$label_x, "mm"), y = grid::unit(yy + 4.25, "mm"), just = "centre", 
                  gp = grid::gpar(fontfamily = "Arial", fontsize = 6.3, fontface = "plain", col = "#20272B"))
            }
        }
        grid::grid.text(sprintf("%d / %d / %d (%d)", sm$n0, sm$n1, sm$n2, sm$applicable), x = grid::unit(left + 
            w + 3, "mm"), y = grid::unit(yy, "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", fontsize = 6.5, 
            fontface = "plain", col = "#20272B"))
    }
    grid::grid.lines(x = grid::unit(c(left, left + w), "mm"), y = grid::unit(c(22, 22), "mm"), gp = grid::gpar(col = "#4E5960", 
        lwd = 0.5))
    for (p in seq(0, 1, by = 0.25)) {
        xx <- left + w * p
        grid::grid.lines(x = grid::unit(c(xx, xx), "mm"), y = grid::unit(c(22, 20.8), "mm"), gp = grid::gpar(col = "#4E5960", 
            lwd = 0.5))
        grid::grid.text(paste0(round(p * 100), "%"), x = grid::unit(xx, "mm"), y = grid::unit(17.6, "mm"), just = "centre", 
            gp = grid::gpar(fontfamily = "Arial", fontsize = 7, fontface = "plain", col = "#20272B"))
    }
}
grid::grid.text("Criterion rating", x = grid::unit(62, "mm"), y = grid::unit(11.5, "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", 
    fontsize = 7.2, fontface = "bold", col = "#20272B"))
rating_labels <- c("Inadequate", "Partially adequate", "Very good")
for (j in 0:2) {
    xx <- c(94, 122, 160)[j + 1L]
    grid.rect(x = unit(xx, "mm"), y = unit(11.5, "mm"), width = unit(4, "mm"), height = unit(3, "mm"), gp = gpar(fill = colours[as.character(j)], 
        col = NA))
    grid::grid.text(rating_labels[j + 1L], x = grid::unit(xx + 3.5, "mm"), y = grid::unit(11.5, "mm"), just = "left", 
        gp = grid::gpar(fontfamily = "Arial", fontsize = 7.2, fontface = "plain", col = "#20272B"))
}
grid.rect(x=unit(186,'mm'),y=unit(11.5,'mm'),width=unit(4,'mm'),height=unit(3,'mm'),gp=gpar(fill=NA,col='#C7CDD1',lwd=.55))
grid::grid.text("Not Applicable", x = grid::unit(189.5, "mm"), y = grid::unit(11.5, "mm"), just = "left", gp = grid::gpar(fontfamily = "Arial", 
    fontsize = 7.2, fontface = "plain", col = "#20272B"))
grid::grid.text("N = analyses in each bar", x = grid::unit(216, "mm"), y = grid::unit(11.5, "mm"), just = "left", 
    gp = grid::gpar(fontfamily = "Arial", fontsize = 7.2, fontface = "plain", col = "#20272B"))
popViewport()
dev.off()
