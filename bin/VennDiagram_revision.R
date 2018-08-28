######################### two set #########################
draw.pairwise.venn <- function (area1, area2, cross.area, category = rep("", 2), euler.d = TRUE, 
    scaled = TRUE, inverted = FALSE, ext.text = TRUE, ext.percent = rep(0.05, 
        3), lwd = rep(2, 2), lty = rep("solid", 2), col = rep("black", 
        2), fill = NULL, alpha = rep(0.5, 2), label.col = rep("black", 
        3), cex = rep(1, 3), fontface = rep("plain", 3), fontfamily = rep("serif", 
        3), cat.pos = c(-50, 50), cat.dist = rep(0.025, 2), cat.cex = rep(1, 
        2), cat.col = rep("black", 2), cat.fontface = rep("plain", 
        2), cat.fontfamily = rep("serif", 2), cat.just = rep(list(c(0.5, 
        0.5)), 2), cat.default.pos = "outer", cat.prompts = FALSE, 
    ext.pos = rep(0, 2), ext.dist = rep(0, 2), ext.line.lty = "solid", 
    ext.length = rep(0.95, 2), ext.line.lwd = 1, rotation.degree = 0, 
    rotation.centre = c(0.5, 0.5), ind = TRUE, sep.dist = 0.05, 
    offset = 0, cex.prop = NULL, print.mode = "raw", sigdigs = 3, 
    ...) 
{
    if (length(category) == 1) {
        category <- rep(category, 2)
    }
    else if (length(category) != 2) {
        flog.error("Unexpected parameter length for 'category'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'category'")
    }
    if (length(ext.percent) == 1) {
        ext.percent <- rep(ext.percent, 3)
    }
    else if (length(ext.percent) != 3) {
        flog.error("Unexpected parameter length for 'ext.percent'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'ext.percent'")
    }
    if (length(ext.pos) == 1) {
        ext.pos <- rep(ext.pos, 2)
    }
    else if (length(ext.pos) != 2) {
        flog.error("Unexpected parameter length for 'ext.pos'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'ext.pos'")
    }
    if (length(ext.dist) == 1) {
        ext.dist <- rep(ext.dist, 2)
    }
    else if (length(ext.dist) != 2) {
        flog.error("Unexpected parameter length for 'ext.dist'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'ext.dist'")
    }
    if (length(ext.length) == 1) {
        ext.length <- rep(ext.length, 2)
    }
    else if (length(ext.length) != 2) {
        flog.error("Unexpected parameter length for 'ext.length'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'ext.length'")
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, 2)
    }
    else if (length(lwd) != 2) {
        flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lwd'")
    }
    if (length(lty) == 1) {
        lty <- rep(lty, 2)
    }
    else if (length(lty) != 2) {
        flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lty'")
    }
    if (length(col) == 1) {
        col <- rep(col, 2)
    }
    else if (length(col) != 2) {
        flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'col'")
    }
    if (length(label.col) == 1) {
        label.col <- rep(label.col, 3)
    }
    else if (length(label.col) != 3) {
        flog.error("Unexpected parameter length for 'label.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'label.col'")
    }
    if (length(cex) == 1) {
        cex <- rep(cex, 3)
    }
    else if (length(cex) != 3) {
        flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cex'")
    }
    if (length(fontface) == 1) {
        fontface <- rep(fontface, 3)
    }
    else if (length(fontface) != 3) {
        flog.error("Unexpected parameter length for 'fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontface'")
    }
    if (length(fontfamily) == 1) {
        fontfamily <- rep(fontfamily, 3)
    }
    else if (length(fontfamily) != 3) {
        flog.error("Unexpected parameter length for 'fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontfamily'")
    }
    if (length(fill) == 1) {
        fill <- rep(fill, 2)
    }
    else if (length(fill) != 2 & length(fill) != 0) {
        flog.error("Unexpected parameter length for 'fill'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fill'")
    }
    if (length(alpha) == 1) {
        alpha <- rep(alpha, 2)
    }
    else if (length(alpha) != 2 & length(alpha) != 0) {
        flog.error("Unexpected parameter length for 'alpha'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'alpha'")
    }
    if (length(ext.line.lwd) != 1) {
        flog.error("Unexpected parameter length for 'ext.line.lwd'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'ext.line.lwd'")
    }
    if (length(cat.pos) == 1) {
        cat.pos <- rep(cat.pos, 2)
    }
    else if (length(cat.pos) != 2) {
        flog.error("Unexpected parameter length for 'cat.pos'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.pos'")
    }
    if (length(cat.dist) == 1) {
        cat.dist <- rep(cat.dist, 2)
    }
    else if (length(cat.dist) != 2) {
        flog.error("Unexpected parameter length for 'cat.dist'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.dist'")
    }
    if (length(cat.col) == 1) {
        cat.col <- rep(cat.col, 2)
    }
    else if (length(cat.col) != 2) {
        flog.error("Unexpected parameter length for 'cat.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.col'")
    }
    if (length(cat.cex) == 1) {
        cat.cex <- rep(cat.cex, 2)
    }
    else if (length(cat.cex) != 2) {
        flog.error("Unexpected parameter length for 'cat.cex'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.cex'")
    }
    if (length(cat.fontface) == 1) {
        cat.fontface <- rep(cat.fontface, 2)
    }
    else if (length(cat.fontface) != 2) {
        flog.error("Unexpected parameter length for 'cat.fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontface'")
    }
    if (length(cat.fontfamily) == 1) {
        cat.fontfamily <- rep(cat.fontfamily, 2)
    }
    else if (length(cat.fontfamily) != 2) {
        flog.error("Unexpected parameter length for 'cat.fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontfamily'")
    }
    if (length(offset) != 1) {
        flog.error("Unexpected parameter length for 'offset'. Try using 'rotation.degree' to achieve non-vertical offsets", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'offset'. Try using 'rotation.degree' to achieve non-vertical offsets")
    }
    if (!(class(cat.just) == "list" & length(cat.just) == 2 & 
        length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2)) {
        flog.error("Unexpected parameter format for 'cat.just'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter format for 'cat.just'")
    }
    if (!euler.d & scaled) {
        flog.error("Uninterpretable parameter combination\nPlease set both euler.d = FALSE and scaled = FALSE to force Venn diagrams.", 
            name = "VennDiagramLogger")
        stop("Uninterpretable parameter combination\nPlease set both euler.d = FALSE and scaled = FALSE to force Venn diagrams.")
    }
    if (offset > 1 | offset < 0) {
        flog.error("'Offset' must be between 0 and 1.  Try using 'rotation.degree = 180' to achieve offsets in the opposite direction.", 
            name = "VennDiagramLogger")
        stop("'Offset' must be between 0 and 1.  Try using 'rotation.degree = 180' to achieve offsets in the opposite direction.")
    }
    if (cross.area > area1 | cross.area > area2) {
        flog.error("Impossible: cross section area too large.", 
            name = "VennDiagramLogger")
        stop("Impossible: cross section area too large.")
    }
    cat.pos <- cat.pos + rotation.degree
    if (((cat.default.pos != "outer") & (cat.default.pos != "text")) & 
        cat.prompts) {
        flog.info("No default location recognized.  Automatically changing to 'outer'", 
            name = "VennDiagramLogger")
        cat.default.pos <- "outer"
    }
    if ((cat.default.pos == "outer") & cat.prompts) {
        flog.info("Placing category labels at default outer locations.  Use 'cat.pos' and 'cat.dist' to modify location.", 
            name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.pos':", cat.pos[1], "degrees,", 
            cat.pos[2], "degrees"), name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.dist':", cat.dist[1], ",", 
            cat.dist[2]), name = "VennDiagramLogger")
    }
    if ((cat.default.pos == "text") & cat.prompts) {
        flog.info("Placing category labels at default text locations.  Use 'cat.pos' and 'cat.dist' to modify location.", 
            name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.pos':", cat.pos[1], "degrees,", 
            cat.pos[2], "degrees"), name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.dist':", cat.dist[1], ",", 
            cat.dist[2]), name = "VennDiagramLogger")
    }
    max.circle.size = 0.2
    special.coincidental <- FALSE
    special.inclusion <- FALSE
    special.exclusion <- FALSE
    list.switch <- FALSE
    grob.list <- gList()
    if (!inverted) {
        tmp1 <- max(area1, area2)
        tmp2 <- min(area1, area2)
        if (tmp1 != area1) {
            list.switch <- TRUE
        }
        area1 <- tmp1
        area2 <- tmp2
        r1 <- sqrt(area1/pi)
        r2 <- sqrt(area2/pi)
        if (r2 == 0) {
            r2 <- 0.5 * r1
        }
        shrink.factor <- max.circle.size/r1
    }
    else {
        tmp1 <- max(area1, area2)
        tmp2 <- min(area1, area2)
        if (tmp1 != area1) {
            list.switch <- TRUE
        }
        area1 <- tmp1
        area2 <- tmp2
        r1 <- sqrt(area1/pi)
        r2 <- sqrt(area2/pi)
        if (r1 == 0) {
            r1 <- 0.5 * r2
        }
        shrink.factor <- max.circle.size/r2
    }
    if (xor(list.switch, inverted)) {
        category <- rev(category)
        lwd <- rev(lwd)
        lty <- rev(lty)
        col <- rev(col)
        fill <- rev(fill)
        alpha <- rev(alpha)
        label.col <- rev(label.col)
        cex <- rev(cex)
        fontface <- rev(fontface)
        fontfamily <- rev(fontfamily)
        cat.pos <- rev(cat.pos)
        cat.dist <- rev(cat.dist)
        cat.col <- rev(cat.col)
        cat.cex <- rev(cat.cex)
        cat.fontface <- rev(cat.fontface)
        cat.fontfamily <- rev(cat.fontfamily)
        cat.just <- rev(cat.just)
        ext.pos <- rev(ext.pos)
        ext.length <- rev(ext.length)
    }
    r1 <- r1 * shrink.factor
    r2 <- r2 * shrink.factor
    if (area1 == area2 & area2 == cross.area) {
        special.coincidental <- TRUE
    }
    if (cross.area != 0 & (cross.area == area2 | cross.area == 
        area1)) {
        special.inclusion <- TRUE
    }
    if (0 == cross.area) {
        special.exclusion <- TRUE
    }
    denom <- area1 + area2 - cross.area
    wrapLab <- function(num) {
        stri = ""
        if (print.mode[1] == "percent") {
            stri <- paste(signif(num * 100/denom, digits = sigdigs), 
                "%", sep = "")
            if (isTRUE(print.mode[2] == "raw")) {
                stri <- paste(stri, "\n(", num, ")", sep = "")
            }
        }
        if (print.mode[1] == "raw") {
            stri <- num
            if (isTRUE(print.mode[2] == "percent")) {
                stri <- paste(stri, "\n(", paste(signif(num * 
                  100/denom, digits = sigdigs), "%)", sep = ""), 
                  sep = "")
            }
        }
        return(stri)
    }
    if (scaled & !special.inclusion & !special.exclusion & !special.coincidental) {
        d <- find.dist(area1, area2, cross.area, inverted = inverted)
        d <- d * shrink.factor
        x.centre.1 <- (1 + r1 - r2 - d)/2
        x.centre.2 <- x.centre.1 + d
        tmp <- VennDiagram::ellipse(x = x.centre.1, y = 0.5, 
            a = ifelse(!inverted, r1, r2), b = ifelse(!inverted, 
                r1, r2), gp = gpar(lty = 0, fill = fill[1], alpha = alpha[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = x.centre.2, y = 0.5, 
            a = ifelse(inverted, r1, r2), b = ifelse(inverted, 
                r1, r2), gp = gpar(lty = 0, fill = fill[2], alpha = alpha[2]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = x.centre.1, y = 0.5, 
            a = ifelse(!inverted, r1, r2), b = ifelse(!inverted, 
                r1, r2), gp = gpar(lwd = lwd[1], lty = lty[1], 
                col = col[1], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = x.centre.2, y = 0.5, 
            a = ifelse(inverted, r1, r2), b = ifelse(inverted, 
                r1, r2), gp = gpar(lwd = lwd[2], lty = lty[2], 
                col = col[2], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)


        ### BEGIN WWK
        areas <- c(area1-cross.area, area2-cross.area, cross.area)

        if(length(cex.prop) == 1){
            maxArea = max(areas)
            if(cex.prop == "lin"){
                for(i in 1:length(areas)){
                    cex[i] = cex[i] * areas[i] / maxArea
                }
            }
            else if(cex.prop == "log10"){
                for(i in 1:length(areas)){

                    if(areas[i] != 0){
                        cex[i] = as.integer(cex[i]) * log10(areas[i]) / log10(maxArea)
                    }
                    else{
                        warn(paste("Error in log10 rescaling of areas: area ",i," is zero", sep=""))
                    }
                }        
            }
            else {
                stop(paste("Unknown value passed to cex.prop:", cex.prop))
            }
        }
        ### END WWK

        if (ext.text) {
            area.1.pos <- x.centre.1 + ifelse(!inverted, -r1 + 
                ((2 * r1 - (r1 + r2 - d))/2), -r2 + ((2 * r2 - 
                (r2 + r1 - d))/2))
            area.2.pos <- x.centre.2 + ifelse(!inverted, r2 - 
                ((2 * r2 - (r1 + r2 - d))/2), r1 - ((2 * r1 - 
                (r2 + r1 - d))/2))
            if ((area1 - cross.area)/area1 > ext.percent[1] & 
                (area1 - cross.area)/area2 > ext.percent[1]) {
                tmp <- textGrob(label = wrapLab(ifelse(!inverted, 
                  area1, area2) - cross.area), x = area.1.pos, 
                  y = 0.5, gp = gpar(col = label.col[1], cex = cex[1], 
                    fontface = fontface[1], fontfamily = fontfamily[1]))
                grob.list <- gList(grob.list, tmp)
            }
            else {
                label.pos <- find.cat.pos(area.1.pos, 0.5, ext.pos[1], 
                  ext.dist[1], r1)
                area.1.xpos <- label.pos$x
                area.1.ypos <- label.pos$y
                tmp <- textGrob(label = wrapLab(ifelse(!inverted, 
                  area1, area2) - cross.area), x = area.1.xpos, 
                  y = area.1.ypos, gp = gpar(col = label.col[1], 
                    cex = cex[1], fontface = fontface[1], fontfamily = fontfamily[1]))
                grob.list <- gList(grob.list, tmp)
                tmp <- linesGrob(x = c(area.1.pos + ext.length[1] * 
                  (area.1.xpos - area.1.pos), area.1.pos), y = c(0.5 + 
                  ext.length[1] * (area.1.ypos - 0.5), 0.5), 
                  gp = gpar(col = label.col[1], lwd = ext.line.lwd, 
                    lty = ext.line.lty))
                grob.list <- gList(grob.list, tmp)
            }
            if ((area2 - cross.area)/area2 > ext.percent[2] & 
                (area2 - cross.area)/area1 > ext.percent[2]) {
                tmp <- textGrob(label = wrapLab(ifelse(inverted, 
                  area1, area2) - cross.area), x = area.2.pos, 
                  y = 0.5, gp = gpar(col = label.col[3], cex = cex[3], 
                    fontface = fontface[3], fontfamily = fontfamily[3]))
                grob.list <- gList(grob.list, tmp)
            }
            else {
                label.pos <- find.cat.pos(area.2.pos, 0.5, ext.pos[2], 
                  ext.dist[2], r2)
                area.2.xpos <- label.pos$x
                area.2.ypos <- label.pos$y
                tmp <- textGrob(label = wrapLab(ifelse(inverted, 
                  area1, area2) - cross.area), x = area.2.xpos, 
                  y = area.2.ypos, gp = gpar(col = label.col[3], 
                    cex = cex[3], fontface = fontface[3], fontfamily = fontfamily[3]))
                grob.list <- gList(grob.list, tmp)
                tmp <- linesGrob(x = c(area.2.pos + ext.length[1] * 
                  (area.2.xpos - area.2.pos), area.2.pos), y = c(0.5 + 
                  ext.length[1] * (area.2.ypos - 0.5), 0.5), 
                  gp = gpar(col = label.col[3], lwd = ext.line.lwd, 
                    lty = ext.line.lty))
                grob.list <- gList(grob.list, tmp)
            }
            if (cross.area/area2 > ext.percent[3] & cross.area/area1 > 
                ext.percent[3]) {
                tmp <- textGrob(label = wrapLab(cross.area), 
                  x = x.centre.1 + (d - ifelse(!inverted, r2, 
                    r1)) + (r1 + r2 - d)/2, y = 0.5, gp = gpar(col = label.col[2], 
                    cex = cex[2], fontface = fontface[2], fontfamily = fontfamily[2]))
                grob.list <- gList(grob.list, tmp)
            }
            else {
                cross.area.pos <- x.centre.1 + (d - r2) + (r1 + 
                  r2 - d)/2
                cross.pos <- find.cat.pos(cross.area.pos, 0.5, 
                  ext.pos[1], ext.dist[1], r1 + r2)
                cross.area.xpos <- cross.pos$x
                cross.area.ypos <- cross.pos$y
                tmp <- textGrob(label = wrapLab(cross.area), 
                  x = cross.area.xpos, y = cross.area.ypos, gp = gpar(col = label.col[1], 
                    cex = cex[1], fontface = fontface[1], fontfamily = fontfamily[1]))
                grob.list <- gList(grob.list, tmp)
                tmp <- linesGrob(x = c(cross.area.pos + ext.length[2] * 
                  (cross.area.xpos - cross.area.pos), cross.area.pos), 
                  y = c(0.5 + ext.length[2] * (cross.area.ypos - 
                    0.5), 0.5), gp = gpar(col = label.col[1], 
                    lwd = ext.line.lwd, lty = ext.line.lty))
                grob.list <- gList(grob.list, tmp)
            }
        }
        else {
            area.1.pos <- x.centre.1 + ifelse(!inverted, -r1 + 
                ((2 * r1 - (r1 + r2 - d))/2), -r2 + ((2 * r2 - 
                (r2 + r1 - d))/2))
            tmp <- textGrob(label = wrapLab(ifelse(!inverted, 
                area1, area2) - cross.area), x = area.1.pos, 
                y = 0.5, gp = gpar(col = label.col[1], cex = cex[1], 
                  fontface = fontface[1], fontfamily = fontfamily[1]))
            grob.list <- gList(grob.list, tmp)
            area.2.pos <- x.centre.2 + ifelse(!inverted, r2 - 
                ((2 * r2 - (r1 + r2 - d))/2), r1 - ((2 * r1 - 
                (r2 + r1 - d))/2))
            tmp <- textGrob(label = wrapLab(ifelse(inverted, 
                area1, area2) - cross.area), x = area.2.pos, 
                y = 0.5, gp = gpar(col = label.col[3], cex = cex[3], 
                  fontface = fontface[3], fontfamily = fontfamily[3]))
            grob.list <- gList(grob.list, tmp)
            tmp <- textGrob(label = wrapLab(cross.area), x = x.centre.1 + 
                (d - ifelse(!inverted, r2, r1)) + (r1 + r2 - 
                d)/2, y = 0.5, gp = gpar(col = label.col[2], 
                cex = cex[2], fontface = fontface[2], fontfamily = fontfamily[2]))
            grob.list <- gList(grob.list, tmp)
        }
        if ("outer" == cat.default.pos) {
            cat.pos.1 <- find.cat.pos(x.centre.1, 0.5, (ifelse(!inverted, 
                cat.pos[1], cat.pos[2]) + ifelse(xor(list.switch, 
                inverted), 180, 0))%%360, cat.dist[1], ifelse(!inverted, 
                r1, r2))
            cat.pos.2 <- find.cat.pos(x.centre.2, 0.5, (ifelse(!inverted, 
                cat.pos[2], cat.pos[1]) + ifelse(xor(list.switch, 
                inverted), 180, 0))%%360, cat.dist[2], ifelse(!inverted, 
                r2, r1))
        }
        else if ("text" == cat.default.pos) {
            cat.pos.1 <- find.cat.pos(area.1.pos, 0.5, cat.pos[1], 
                cat.dist[1])
            cat.pos.2 <- find.cat.pos(area.2.pos, 0.5, cat.pos[2], 
                cat.dist[2])
        }
        else {
            flog.error("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'", 
                name = "VennDiagramLogger")
            stop("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'")
        }
        tmp <- textGrob(label = category[1], x = cat.pos.1$x, 
            y = cat.pos.1$y, just = cat.just[[1]], gp = gpar(col = cat.col[1], 
                cex = cat.cex[1], fontface = cat.fontface[1], 
                fontfamily = cat.fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = category[2], x = cat.pos.2$x, 
            y = cat.pos.2$y, just = cat.just[[2]], gp = gpar(col = cat.col[2], 
                cex = cat.cex[2], fontface = cat.fontface[2], 
                fontfamily = cat.fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
    }
    if (euler.d & special.inclusion & !special.coincidental) {
        if (inverted) {
            tmp1 <- area1
            tmp2 <- area2
            area1 <- tmp2
            area2 <- tmp1
        }
        if (!scaled & !inverted) {
            r1 <- 0.4
            r2 <- 0.2
        }
        if (!scaled & inverted) {
            r1 <- 0.2
            r2 <- 0.4
        }
        tmp <- VennDiagram::ellipse(x = 0.5, y = 0.5, a = r1, 
            b = r1, gp = gpar(lty = 0, fill = fill[1], alpha = alpha[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.5 - offset * (r1 - 
            r2), y = 0.5, a = r2, b = r2, gp = gpar(lty = 0, 
            fill = fill[2], alpha = alpha[2]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.5, y = 0.5, a = r1, 
            b = r1, gp = gpar(lwd = lwd[1], lty = lty[1], col = col[1], 
                fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.5 - offset * (r1 - 
            r2), y = 0.5, a = r2, b = r2, gp = gpar(lwd = lwd[2], 
            lty = lty[2], col = col[2], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        area.2.pos <- 0.5 - offset * (r1 - r2)
        tmp <- textGrob(label = wrapLab(area2), x = area.2.pos, 
            y = 0.5, gp = gpar(col = label.col[2], cex = cex[2], 
                fontface = fontface[2], fontfamily = fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
        if (!ext.text | !scaled) {
            area.1.pos <- (1 + r1 + r2 - offset * (r1 - r2))/2
            tmp <- textGrob(label = wrapLab(area1 - area2), x = area.1.pos, 
                y = 0.5, gp = gpar(col = label.col[1], cex = cex[1], 
                  fontface = fontface[1], fontfamily = fontfamily[1]))
            grob.list <- gList(grob.list, tmp)
        }
        if (ext.text & scaled) {
            if (area2/area1 > 0.5) {
                area.1.pos <- (1 + r1 + r2 - offset * (r1 - r2))/2
                area.pos <- find.cat.pos(area.1.pos, 0.5, ext.pos[1], 
                  ext.dist[1], r1)
                area.1.xpos <- area.pos$x
                area.1.ypos <- area.pos$y
                tmp <- textGrob(label = wrapLab(area1 - area2), 
                  x = area.1.xpos, y = area.1.ypos, gp = gpar(col = label.col[1], 
                    cex = cex[1], fontface = fontface[1], fontfamily = fontfamily[1]))
                grob.list <- gList(grob.list, tmp)
                tmp <- linesGrob(x = c(area.1.pos + ext.length * 
                  (area.1.xpos - area.1.pos), area.1.pos), y = c(0.5 + 
                  ext.length * (area.1.ypos - 0.5), 0.5), gp = gpar(col = label.col[1], 
                  lwd = ext.line.lwd, lty = ext.line.lty))
                grob.list <- gList(grob.list, tmp)
            }
            else {
                area.1.pos <- (1 + r1 + r2 - offset * (r1 - r2))/2
                tmp <- textGrob(label = wrapLab(area1 - area2), 
                  x = area.1.pos, y = 0.5, gp = gpar(col = label.col[1], 
                    cex = cex[1], fontface = fontface[1], fontfamily = fontfamily[1]))
                grob.list <- gList(grob.list, tmp)
            }
        }
        if (cat.default.pos == "outer") {
            cat.pos.1 <- find.cat.pos(0.5, 0.5, cat.pos[1], cat.dist[1], 
                r1)
            cat.pos.2 <- find.cat.pos(0.5 - offset * (r1 - r2), 
                0.5, cat.pos[2], cat.dist[2], r2)
        }
        else if (cat.default.pos == "text") {
            cat.pos.1 <- find.cat.pos(area.1.pos, 0.5, cat.pos[1], 
                cat.dist[1])
            cat.pos.2 <- find.cat.pos(area.2.pos, 0.5, cat.pos[2], 
                cat.dist[2])
        }
        else {
            flog.error("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'", 
                name = "VennDiagramLogger")
            stop("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'")
        }
        tmp <- textGrob(label = category[1], x = cat.pos.1$x, 
            y = cat.pos.1$y, just = cat.just[[1]], gp = gpar(col = cat.col[1], 
                cex = cat.cex[1], fontface = cat.fontface[1], 
                fontfamily = cat.fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = category[2], x = cat.pos.2$x, 
            y = cat.pos.2$y, just = cat.just[[2]], gp = gpar(col = cat.col[2], 
                cex = cat.cex[2], fontface = cat.fontface[2], 
                fontfamily = cat.fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
    }
    if (euler.d & special.coincidental) {
        tmp <- VennDiagram::ellipse(x = 0.5, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lty = 0, fill = fill[1], 
                alpha = alpha[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.5, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lwd = lwd[1], lty = lty[1], 
                col = col[1], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        area.1.pos <- 0.46
        tmp <- textGrob(label = wrapLab(area1), x = area.1.pos, 
            y = 0.5, gp = gpar(col = label.col[2], cex = cex[2], 
                fontface = fontface[2], fontfamily = fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
        area.2.pos <- 0.54
        tmp <- textGrob(label = wrapLab(area2), x = area.2.pos, 
            y = 0.5, gp = gpar(col = label.col[2], cex = cex[2], 
                fontface = fontface[2], fontfamily = fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = "(Coincidental)", x = 0.5, y = 0.45, 
            gp = gpar(col = label.col[2], cex = cex[2], fontface = fontface[2], 
                fontfamily = fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
        if (cat.default.pos == "outer") {
            cat.pos.1 <- find.cat.pos(0.5, 0.5, cat.pos[1], cat.dist[1], 
                max.circle.size)
            cat.pos.2 <- find.cat.pos(0.5, 0.5, cat.pos[2], cat.dist[2], 
                max.circle.size)
        }
        else if (cat.default.pos == "text") {
            cat.pos.1 <- find.cat.pos(area.1.pos, 0.5, cat.pos[1], 
                cat.dist[1])
            cat.pos.2 <- find.cat.pos(area.2.pos, 0.5, cat.pos[2], 
                cat.dist[2])
        }
        else {
            flog.error("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'", 
                name = "VennDiagramLogger")
            stop("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'")
        }
        tmp <- textGrob(label = category[1], x = cat.pos.1$x, 
            y = cat.pos.1$y, just = cat.just[[1]], gp = gpar(col = cat.col[1], 
                cex = cat.cex[1], fontface = cat.fontface[1], 
                fontfamily = cat.fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = category[2], x = cat.pos.2$x, 
            y = cat.pos.2$y, just = cat.just[[2]], gp = gpar(col = cat.col[2], 
                cex = cat.cex[2], fontface = cat.fontface[2], 
                fontfamily = cat.fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
    }
    if (euler.d & special.exclusion) {
        if (!scaled) {
            r1 <- 0.2
            r2 <- 0.2
        }
        x.centre.1 <- (1 - 2 * (r1 + r2))/2 + r1 - sep.dist/2
        tmp <- VennDiagram::ellipse(x = x.centre.1, y = 0.5, 
            a = r1, b = r1, gp = gpar(lty = 0, fill = fill[1], 
                alpha = alpha[1]))
        grob.list <- gList(grob.list, tmp)
        x.centre.2 <- 1 - (1 - 2 * (r1 + r2))/2 - r2 + sep.dist/2
        tmp <- VennDiagram::ellipse(x = x.centre.2, y = 0.5, 
            a = r2, b = r2, gp = gpar(lty = 0, fill = fill[2], 
                alpha = alpha[2]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = x.centre.1, y = 0.5, 
            a = r1, b = r1, gp = gpar(lwd = lwd[1], lty = lty[1], 
                col = col[1], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = x.centre.2, y = 0.5, 
            a = r2, b = r2, gp = gpar(lwd = lwd[2], lty = lty[2], 
                col = col[2], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        area.1.pos <- x.centre.1
        tmp <- textGrob(label = wrapLab(area1), x = area.1.pos, 
            y = 0.5, gp = gpar(col = label.col[1], cex = cex[1], 
                fontface = fontface[1], fontfamily = fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        area.2.pos <- x.centre.2
        tmp <- textGrob(label = wrapLab(area2), x = area.2.pos, 
            y = 0.5, gp = gpar(col = label.col[3], cex = cex[3], 
                fontface = fontface[3], fontfamily = fontfamily[3]))
        grob.list <- gList(grob.list, tmp)
        if (cat.default.pos == "outer") {
            cat.pos.1 <- find.cat.pos(x.centre.1, 0.5, cat.pos[1], 
                cat.dist[1], r1)
            cat.pos.2 <- find.cat.pos(x.centre.2, 0.5, cat.pos[2], 
                cat.dist[2], r2)
        }
        else if (cat.default.pos == "text") {
            cat.pos.1 <- find.cat.pos(area.1.pos, 0.5, cat.pos[1], 
                cat.dist[1])
            cat.pos.2 <- find.cat.pos(area.2.pos, 0.5, cat.pos[2], 
                cat.dist[2])
        }
        else {
            flog.error("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'", 
                name = "VennDiagramLogger")
            stop("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'")
        }
        tmp <- textGrob(label = category[1], x = cat.pos.1$x, 
            y = cat.pos.1$y, just = cat.just[[1]], gp = gpar(col = cat.col[1], 
                cex = cat.cex[1], fontface = cat.fontface[1], 
                fontfamily = cat.fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = category[2], x = cat.pos.2$x, 
            y = cat.pos.2$y, just = cat.just[[2]], gp = gpar(col = cat.col[2], 
                cex = cat.cex[2], fontface = cat.fontface[2], 
                fontfamily = cat.fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
    }
    if ((!scaled & !euler.d) | (!scaled & euler.d & !special.inclusion & 
        !special.exclusion & !special.coincidental)) {
        tmp <- VennDiagram::ellipse(x = 0.4, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lty = 0, fill = fill[1], 
                alpha = alpha[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.6, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lty = 0, fill = fill[2], 
                alpha = alpha[2]))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.4, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lwd = lwd[1], lty = lty[1], 
                col = col[1], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        tmp <- VennDiagram::ellipse(x = 0.6, y = 0.5, a = max.circle.size, 
            b = max.circle.size, gp = gpar(lwd = lwd[2], lty = lty[2], 
                col = col[2], fill = "transparent"))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = wrapLab(area1 - cross.area), 
            x = 0.3, y = 0.5, gp = gpar(col = label.col[1], cex = cex[1], 
                fontface = fontface[1], fontfamily = fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = wrapLab(area2 - cross.area), 
            x = 0.7, y = 0.5, gp = gpar(col = label.col[3], cex = cex[3], 
                fontface = fontface[3], fontfamily = fontfamily[3]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = wrapLab(cross.area), x = 0.5, 
            y = 0.5, gp = gpar(col = label.col[2], cex = cex[2], 
                fontface = fontface[2], fontfamily = fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
        if (cat.default.pos == "outer") {
            cat.pos.1 <- find.cat.pos(0.4, 0.5, cat.pos[1], cat.dist[1], 
                max.circle.size)
            cat.pos.2 <- find.cat.pos(0.6, 0.5, cat.pos[2], cat.dist[2], 
                max.circle.size)
        }
        else if (cat.default.pos == "text") {
            cat.pos.1 <- find.cat.pos(0.3, 0.5, cat.pos[1], cat.dist[1])
            cat.pos.2 <- find.cat.pos(0.7, 0.5, cat.pos[2], cat.dist[2])
        }
        else {
            flog.error("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'", 
                name = "VennDiagramLogger")
            stop("Invalid value for 'cat.default.pos', should be either 'outer' or 'text'")
        }
        tmp <- textGrob(label = category[1], x = cat.pos.1$x, 
            y = cat.pos.1$y, just = cat.just[[1]], gp = gpar(col = cat.col[1], 
                cex = cat.cex[1], fontface = cat.fontface[1], 
                fontfamily = cat.fontfamily[1]))
        grob.list <- gList(grob.list, tmp)
        tmp <- textGrob(label = category[2], x = cat.pos.2$x, 
            y = cat.pos.2$y, just = cat.just[[2]], gp = gpar(col = cat.col[2], 
                cex = cat.cex[2], fontface = cat.fontface[2], 
                fontfamily = cat.fontfamily[2]))
        grob.list <- gList(grob.list, tmp)
    }
    grob.list <- adjust.venn(rotate.venn.degrees(grob.list, rotation.degree, 
        rotation.centre[1], rotation.centre[2]), ...)
    if (ind) {
        grid.draw(grob.list)
    }
    return(grob.list)
}

assignInNamespace("draw.pairwise.venn",draw.pairwise.venn, ns="VennDiagram")

######################### three set #########################

draw.triple.venn <- function (area1, area2, area3, n12, n23, n13, n123, category = rep("", 
    3), rotation = 1, reverse = FALSE, euler.d = TRUE, scaled = TRUE, 
    lwd = rep(2, 3), lty = rep("solid", 3), col = rep("black", 
        3), fill = NULL, alpha = rep(0.5, 3), label.col = rep("black", 
        7), cex = rep(1, 7), fontface = rep("plain", 7), fontfamily = rep("serif", 
        7), cat.pos = c(-40, 40, 180), cat.dist = c(0.05, 0.05, 
        0.025), cat.col = rep("black", 3), cat.cex = rep(1, 3), 
    cat.fontface = rep("plain", 3), cat.fontfamily = rep("serif", 
        3), cat.just = list(c(0.5, 1), c(0.5, 1), c(0.5, 0)), 
    cat.default.pos = "outer", cat.prompts = FALSE, rotation.degree = 0, 
    rotation.centre = c(0.5, 0.5), ind = TRUE, sep.dist = 0.05, 
    offset = 0, cex.prop = NULL, print.mode = "raw", sigdigs = 3, 
    direct.area = FALSE, area.vector = 0, ...) 
{
    if (length(category) == 1) {
        cat <- rep(category, 3)
    }
    else if (length(category) != 3) {
        flog.error("Unexpected parameter length for 'category'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'category'")
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, 3)
    }
    else if (length(lwd) != 3) {
        flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lwd'")
    }
    if (length(lty) == 1) {
        lty <- rep(lty, 3)
    }
    else if (length(lty) != 3) {
        flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lty'")
    }
    if (length(col) == 1) {
        col <- rep(col, 3)
    }
    else if (length(col) != 3) {
        flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'col'")
    }
    if (length(label.col) == 1) {
        label.col <- rep(label.col, 7)
    }
    else if (length(label.col) != 7) {
        flog.error("Unexpected parameter length for 'label.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'label.col'")
    }
    if (length(cex) == 1) {
        cex <- rep(cex, 7)
    }
    else if (length(cex) != 7) {
        flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cex'")
    }
    if (length(fontface) == 1) {
        fontface <- rep(fontface, 7)
    }
    else if (length(fontface) != 7) {
        flog.error("Unexpected parameter length for 'fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontface'")
    }
    if (length(fontfamily) == 1) {
        fontfamily <- rep(fontfamily, 7)
    }
    else if (length(fontfamily) != 7) {
        flog.error("Unexpected parameter length for 'fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontfamily'")
    }
    if (length(fill) == 1) {
        fill <- rep(fill, 3)
    }
    else if (length(fill) != 3 & length(fill) != 0) {
        flog.error("Unexpected parameter length for 'fill'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fill'")
    }
    if (length(alpha) == 1) {
        alpha <- rep(alpha, 3)
    }
    else if (length(alpha) != 3 & length(alpha) != 0) {
        flog.error("Unexpected parameter length for 'alpha'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'alpha'")
    }
    if (length(cat.pos) == 1) {
        cat.pos <- rep(cat.pos, 3)
    }
    else if (length(cat.pos) != 3) {
        flog.error("Unexpected parameter length for 'cat.pos'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.pos'")
    }
    if (length(cat.dist) == 1) {
        cat.dist <- rep(cat.dist, 3)
    }
    else if (length(cat.dist) != 3) {
        flog.error("Unexpected parameter length for 'cat.dist'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.dist'")
    }
    if (length(cat.col) == 1) {
        cat.col <- rep(cat.col, 3)
    }
    else if (length(cat.col) != 3) {
        flog.error("Unexpected parameter length for 'cat.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.col'")
    }
    if (length(cat.cex) == 1) {
        cat.cex <- rep(cat.cex, 3)
    }
    else if (length(cat.cex) != 3) {
        flog.error("Unexpected parameter length for 'cat.cex'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.cex'")
    }
    if (length(cat.fontface) == 1) {
        cat.fontface <- rep(cat.fontface, 3)
    }
    else if (length(cat.fontface) != 3) {
        flog.error("Unexpected parameter length for 'cat.fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontface'")
    }
    if (length(cat.fontfamily) == 1) {
        cat.fontfamily <- rep(cat.fontfamily, 3)
    }
    else if (length(cat.fontfamily) != 3) {
        flog.error("Unexpected parameter length for 'cat.fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontfamily'")
    }
    if (!(class(cat.just) == "list" & length(cat.just) == 3)) {
        flog.error("Unexpected parameter format for 'cat.just'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter format for 'cat.just'")
    }
    else if (!(length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2 & length(cat.just[[3]]) == 2)) {
        flog.error("Unexpected parameter format for 'cat.just'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter format for 'cat.just'")
    }
    if (euler.d == FALSE & scaled == TRUE) {
        flog.error("Uninterpretable parameter combination\nPlease set both euler.d = FALSE and scaled = FALSE to force Venn diagrams.", 
            name = "VennDiagramLogger")
        stop("Uninterpretable parameter combination\nPlease set both euler.d = FALSE and scaled = FALSE to force Venn diagrams.")
    }
    if (offset > 1 | offset < 0) {
        flog.error("'Offset' must be between 0 and 1.  Try using 'rotation.degree = 180' to achieve offsets in the opposite direction.", 
            name = "VennDiagramLogger")
        stop("'Offset' must be between 0 and 1.  Try using 'rotation.degree = 180' to achieve offsets in the opposite direction.")
    }
    cat.pos <- cat.pos + rotation.degree
    if (direct.area) {
        areas <- area.vector
        for (i in 1:7) {
            assign(paste("a", i, sep = ""), area.vector[i])
        }
    }
    else {
        a1 <- area1 - n12 - n13 + n123
        a2 <- n12 - n123
        a3 <- area2 - n12 - n23 + n123
        a4 <- n13 - n123
        a5 <- n123
        a6 <- n23 - n123
        a7 <- area3 - n13 - n23 + n123
        areas <- c(a1, a2, a3, a4, a5, a6, a7)
    }
    if (euler.d) {
        special.code <- VennDiagram::decide.special.case(areas)
        if (special.code %in% c("121AO", "100", "033", "011A", 
            "021AA", "022AAOO", "011O", "112AA", "122AAOO", "010", 
            "110", "130", "001", "012AA", "120", "022AAAO", "032", 
            "111A", "023")) {
            if (special.code %in% c("022AAAO", "022AAOO", "023", 
                "032", "120", "121AO", "122AAOO", "130")) {
                f1 <- VennDiagram::draw.sp.case.scaled
            }
            else {
                f1 <- VennDiagram::draw.sp.case.preprocess
            }
            rst <- f1(sp.case.name = special.code, a1 = areas[1], 
                a2 = areas[2], a3 = areas[3], a4 = areas[4], 
                a5 = areas[5], a6 = areas[6], a7 = areas[7], 
                category = category, reverse = reverse, cat.default.pos = cat.default.pos, 
                lwd = lwd, lty = lty, col = col, label.col = label.col, 
                cex = cex, fontface = fontface, fontfamily = fontfamily, 
                cat.pos = cat.pos, cat.dist = cat.dist, cat.col = cat.col, 
                cat.cex = cat.cex, cat.fontface = cat.fontface, 
                cat.fontfamily = cat.fontfamily, cat.just = cat.just, 
                cat.prompts = cat.prompts, fill = fill, alpha = alpha, 
                print.mode = print.mode, sigdigs = sigdigs, ...)
            rst <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(gList1 = rst, 
                angle = rotation.degree, x.centre = rotation.centre[1], 
                y.centre = rotation.centre[2]), ...)
            if (ind) {
                grid.draw(rst)
            }
            return(rst)
        }
    }
    rotated <- VennDiagram::rotate(areas, category, lwd, lty, 
        col, label.col, cex, fontface, fontfamily, cat.col, cat.cex, 
        cat.fontface, cat.fontfamily, alpha, rotation, reverse, 
        fill)
    for (i in 1:length(areas)) {
        areas[i] <- rotated[[1]][i]
    }
    category <- rotated[[2]]
    lwd <- rotated$lwd
    lty <- rotated$lty
    col <- rotated$col
    label.col <- rotated$label.col
    cex <- rotated$cex
    fontface <- rotated$fontface
    fontfamily <- rotated$fontfamily
    cat.col <- rotated$cat.col
    cat.cex <- rotated$cat.cex
    cat.fontface <- rotated$cat.fontface
    cat.fontfamily <- rotated$cat.fontfamily
    fill <- rotated$fill
    alpha <- rotated$alpha
    areas.error <- c("a1 <- area1 - n12 - n13 + n123", "a2 <- n12 - n123", 
        "a3 <- area2 - n12 - n23 + n123", "a4 <- n13 - n123", 
        "a5 <- n123", "a6 <- n23 - n123", "a7 <- area3 - n13 - n23 + n123")
    for (i in 1:length(areas)) {
        if (areas[i] < 0) {
            flog.error(paste("Impossible:", areas.error[i], "produces negative area"), 
                name = "VennDiagramLogger")
            stop(paste("Impossible:", areas.error[i], "produces negative area"))
        }
    }
    for (i in 1:length(areas)) {
        if (areas[i]) {
            scaled <- FALSE
        }
    }
    is.defaults <- TRUE
    if (is.expression(category)) {
        is.defaults <- FALSE
    }
    if (all(cat.default.pos != "outer", cat.default.pos != "text", 
        !is.defaults, cat.prompts)) {
        flog.info("No default location recognized.  Automatically changing to 'outer'", 
            name = "VennDiagramLogger")
        cat.default.pos <- "outer"
    }
    if (all(cat.default.pos == "outer", !is.defaults, cat.prompts)) {
        flog.info("Placing category labels at default outer locations.  Use 'cat.pos' and 'cat.dist' to modify location.", 
            name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.pos':", cat.pos[1], "degrees,", 
            cat.pos[2], "degrees"), name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.dist':", cat.dist[1], ",", 
            cat.dist[2]), name = "VennDiagramLogger")
    }
    if (all(cat.default.pos == "text", !is.defaults, cat.prompts)) {
        flog.info("Placing category labels at default text locations.  Use 'cat.pos' and 'cat.dist' to modify location.", 
            name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.pos':", cat.pos[1], "degrees,", 
            cat.pos[2], "degrees"), name = "VennDiagramLogger")
        flog.info(paste("Current 'cat.dist':", cat.dist[1], ",", 
            cat.dist[2]), name = "VennDiagramLogger")
    }
    grob.list <- gList()
    if (!exists("overrideTriple")) {
        r1 <- sqrt(100/pi)
        r2 <- r1
        r3 <- r1
    }
    else {
        r1 <- sqrt(area1/pi)
        r2 <- sqrt(area2/pi)
        r3 <- sqrt(area3/pi)
    }
    max.circle.size = 0.2
    shrink.factor <- max.circle.size/r1
    r1 <- r1 * shrink.factor
    r2 <- r2 * shrink.factor
    r3 <- r3 * shrink.factor
    if (!exists("overrideTriple")) {
        a <- find.dist(100, 100, 40) * shrink.factor
        b <- a
        c <- a
    }
    else {
        a <- find.dist(area1, area2, n12) * shrink.factor
        b <- find.dist(area2, area3, n23) * shrink.factor
        c <- find.dist(area1, area3, n13) * shrink.factor
    }
    x.centres <- vector(mode = "numeric", length = 3)
    y.centres <- vector(mode = "numeric", length = 3)
    beta <- (a^2 + c^2 - b^2)/(2 * a * c)
    gamma <- sqrt(1 - beta^2)
    x.centres[1] <- (r1 - r2 - a + 1)/2
    x.centres[3] <- x.centres[1] + c * beta
    y.centres[3] <- (r3 - r1 + 1 - c * gamma)/2
    y.centres[1] <- y.centres[3] + c * gamma
    x.centres[2] <- x.centres[1] + a
    y.centres[2] <- y.centres[1]
    radii <- c(r1, r2, r3)
    for (i in 1:3) {
        grob.list <- gList(grob.list, VennDiagram::ellipse(x = x.centres[i], 
            y = y.centres[i], a = radii[i], b = radii[i], gp = gpar(lty = 0, 
                fill = fill[i], alpha = alpha[i])))
    }
    for (i in 1:3) {
        grob.list <- gList(grob.list, VennDiagram::ellipse(x = x.centres[i], 
            y = y.centres[i], a = radii[i], b = radii[i], gp = gpar(lwd = lwd[i], 
                lty = lty[i], col = col[i], fill = "transparent")))
    }
    new.x.centres <- vector(mode = "numeric", length = 3)
    new.y.centres <- vector(mode = "numeric", length = 3)
    cell.labels <- areas
    cell.x <- vector(mode = "numeric", length = 7)
    cell.y <- vector(mode = "numeric", length = 7)
    x.cept.12 <- (r1^2 - r2^2 - x.centres[1]^2 + x.centres[2]^2)/(2 * 
        (x.centres[2] - x.centres[1]))
    y.cept.12.1 <- sqrt(r1^2 - (x.cept.12 - x.centres[1])^2) + 
        y.centres[1]
    y.cept.12.2 <- -sqrt(r1^2 - (x.cept.12 - x.centres[1])^2) + 
        y.centres[1]
    theta <- acos((a^2 + c^2 - b^2)/(2 * a * c))
    new.x.centres[3] <- x.centres[1] + c
    l.x.cept.13 <- (r1^2 - r3^2 - x.centres[1]^2 + new.x.centres[3]^2)/(2 * 
        (new.x.centres[3] - x.centres[1]))
    l.y.cept.13.1 <- sqrt(r1^2 - (l.x.cept.13 - x.centres[1])^2) + 
        y.centres[1]
    l.y.cept.13.2 <- -sqrt(r1^2 - (l.x.cept.13 - x.centres[1])^2) + 
        y.centres[1]
    rot <- sqrt(2 * r1^2 - 2 * r1^2 * cos(theta))
    x.cept.13.1 <- l.x.cept.13 + rot * cos(pi/2 - atan((l.y.cept.13.1 - 
        y.centres[1])/(l.x.cept.13 - x.centres[1])) + theta/2)
    x.cept.13.2 <- l.x.cept.13 + rot * cos(pi/2 - atan((l.y.cept.13.2 - 
        y.centres[1])/(l.x.cept.13 - x.centres[1])) + theta/2)
    y.cept.13.1 <- l.y.cept.13.1 - rot * sin(pi/2 - atan((l.y.cept.13.1 - 
        y.centres[1])/(l.x.cept.13 - x.centres[1])) + theta/2)
    y.cept.13.2 <- l.y.cept.13.2 - rot * sin(pi/2 - atan((l.y.cept.13.2 - 
        y.centres[1])/(l.x.cept.13 - x.centres[1])) + theta/2)
    theta <- -acos((a^2 + b^2 - c^2)/(2 * a * b))
    new.x.centres[3] <- x.centres[2] - b
    l.x.cept.23 <- (r2^2 - r3^2 - x.centres[2]^2 + new.x.centres[3]^2)/(2 * 
        (new.x.centres[3] - x.centres[2]))
    l.y.cept.23.1 <- sqrt(r2^2 - (l.x.cept.23 - x.centres[2])^2) + 
        y.centres[2]
    l.y.cept.23.2 <- -sqrt(r2^2 - (l.x.cept.23 - x.centres[2])^2) + 
        y.centres[2]
    rot <- sqrt(2 * r2^2 - 2 * r2^2 * cos(theta))
    x.cept.23.1 <- l.x.cept.23 + rot * cos(pi/2 - atan((y.centres[2] - 
        l.y.cept.23.1)/(x.centres[2] - l.x.cept.23)) + theta/2)
    x.cept.23.2 <- l.x.cept.23 + rot * cos(pi/2 - atan((y.centres[2] - 
        l.y.cept.23.2)/(x.centres[2] - l.x.cept.23)) + theta/2)
    y.cept.23.1 <- l.y.cept.23.1 - rot * sin(pi/2 - atan((y.centres[2] - 
        l.y.cept.23.1)/(x.centres[2] - l.x.cept.23)) + theta/2)
    y.cept.23.2 <- l.y.cept.23.2 - rot * sin(pi/2 - atan((y.centres[2] - 
        l.y.cept.23.2)/(x.centres[2] - l.x.cept.23)) + theta/2)
    m <- (y.cept.23.2 - y.cept.23.1)/(x.cept.23.2 - x.cept.23.1)
    y.sect <- m * (x.cept.12 - x.cept.23.1) + y.cept.23.1
    cell.x[5] <- x.cept.12
    cell.y[5] <- y.sect
    m <- (y.cept.13.2 - y.cept.13.1)/(x.cept.13.2 - x.cept.13.1)
    y0 <- y.centres[2]
    x0 <- x.centres[2]
    b <- y.cept.13.1 - m * x.cept.13.1
    x.sect <- (m * y0 + x0 - m * b)/(m^2 + 1) + sqrt(r2^2 - ((y0 - 
        m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    y.sect <- (m^2 * y0 + m * x0 + b)/(m^2 + 1) + m * sqrt(r2^2 - 
        ((y0 - m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    cell.x[3] <- (x.cept.13.1 + x.sect)/2
    cell.y[3] <- (y.cept.13.1 + y.sect)/2
    m <- (y.cept.23.2 - y.cept.23.1)/(x.cept.23.2 - x.cept.23.1)
    y0 <- y.centres[1]
    x0 <- x.centres[1]
    b <- y.cept.23.1 - m * x.cept.23.1
    x.sect <- (m * y0 + x0 - m * b)/(m^2 + 1) - sqrt(r1^2 - ((y0 - 
        m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    y.sect <- (m^2 * y0 + m * x0 + b)/(m^2 + 1) - m * sqrt(r1^2 - 
        ((y0 - m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    cell.x[1] <- (x.cept.23.1 + x.sect)/2
    cell.y[1] <- (y.cept.23.1 + y.sect)/2
    y.sect <- -sqrt(r3^2 - (x.cept.12 - x.centres[3])^2) + y.centres[3]
    cell.x[7] <- x.cept.12
    cell.y[7] <- (y.cept.12.2 + y.sect)/2
    m <- (y.cept.23.2 - y.cept.23.1)/(x.cept.23.2 - x.cept.23.1)
    y0 <- y.centres[1]
    x0 <- x.centres[1]
    b <- y.cept.23.1 - m * x.cept.23.1
    x.sect <- (m * y0 + x0 - m * b)/(m^2 + 1) + sqrt(r1^2 - ((y0 - 
        m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    y.sect <- (m^2 * y0 + m * x0 + b)/(m^2 + 1) + m * sqrt(r1^2 - 
        ((y0 - m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    cell.x[6] <- (x.cept.23.2 + x.sect)/2
    cell.y[6] <- (y.cept.23.2 + y.sect)/2
    m <- (y.cept.13.2 - y.cept.13.1)/(x.cept.13.2 - x.cept.13.1)
    y0 <- y.centres[2]
    x0 <- x.centres[2]
    b <- y.cept.13.1 - m * x.cept.13.1
    x.sect <- (m * y0 + x0 - m * b)/(m^2 + 1) - sqrt(r2^2 - ((y0 - 
        m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    y.sect <- (m^2 * y0 + m * x0 + b)/(m^2 + 1) - m * sqrt(r2^2 - 
        ((y0 - m * x0 - b)/sqrt(1 + m^2))^2)/sqrt(1 + m^2)
    cell.x[4] <- (x.cept.13.2 + x.sect)/2
    cell.y[4] <- (y.cept.13.2 + y.sect)/2
    y.sect <- sqrt(r3^2 - (x.cept.12 - x.centres[3])^2) + y.centres[3]
    cell.x[2] <- x.cept.12
    cell.y[2] <- (y.cept.12.1 + y.sect)/2

    ### BEGIN WWK
    if(length(cex.prop) == 1){
        maxArea = max(areas)
        if(cex.prop == "lin"){
            for(i in 1:length(areas)){
                cex[i] = cex[i] * areas[i] / maxArea
            }
        }
        else if(cex.prop == "log10"){
            for(i in 1:length(areas)){
                if(areas[i] != 0){
                    cex[i] = cex[i] * log10(areas[i]) / log10(maxArea)
                }
                else{
                    warn(paste("Error in log10 rescaling of areas: area ",i," is zero", sep=""))
                }
            }        
        }
        else {
            stop(paste("Unknown value passed to cex.prop:", cex.prop))
        }
    }
    ### END WWK

    processedLabels <- rep("", length(cell.labels))
    if (print.mode[1] == "percent") {
        processedLabels <- paste(signif(cell.labels/sum(cell.labels) * 
            100, digits = sigdigs), "%", sep = "")
        if (isTRUE(print.mode[2] == "raw")) {
            processedLabels <- paste(processedLabels, "\n(", 
                cell.labels, ")", sep = "")
        }
    }
    if (print.mode[1] == "raw") {
        processedLabels <- cell.labels
        if (isTRUE(print.mode[2] == "percent")) {
            processedLabels <- paste(processedLabels, "\n(", 
                paste(signif(cell.labels/sum(cell.labels) * 100, 
                  digits = sigdigs), "%)", sep = ""), sep = "")
        }
    }
    for (i in 1:7) {
        grob.list <- gList(grob.list, textGrob(label = processedLabels[i], 
            x = cell.x[i], y = cell.y[i], gp = gpar(col = label.col[i], 
                cex = cex[i], fontface = fontface[i], fontfamily = fontfamily[i])))
    }
    text.location.mapping <- c(1, 3, 7)
    for (i in 1:3) {
        if ("outer" == cat.default.pos) {
            this.cat.pos <- find.cat.pos(x = x.centres[i], y = y.centres[i], 
                pos = cat.pos[i], dist = cat.dist[i], r = radii[i])
        }
        else if ("text" == cat.default.pos) {
            this.cat.pos <- find.cat.pos(x = cell.x[text.location.mapping[i]], 
                y = cell.y[text.location.mapping[i]], pos = cat.pos[i], 
                dist = cat.dist[i])
        }
        else {
            flog.error("Invalid setting of cat.default.pos", 
                name = "VennDiagramLogger")
            stop("Invalid setting of cat.default.pos")
        }
        grob.list <- gList(grob.list, textGrob(label = category[i], 
            x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
            gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                fontfamily = cat.fontfamily[i])))
    }
    grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(gList1 = grob.list, 
        angle = rotation.degree, x.centre = rotation.centre[1], 
        y.centre = rotation.centre[2]), ...)
    if (ind) {
        grid.draw(grob.list)
    }
    return(grob.list)
}
assignInNamespace("draw.triple.venn", draw.triple.venn, ns="VennDiagram")

######################### four set #########################
draw.quad.venn <- function (area1, area2, area3, area4, n12, n13, n14, n23, n24, 
    n34, n123, n124, n134, n234, n1234, category = rep("", 4), 
    lwd = rep(2, 4), lty = rep("solid", 4), col = rep("black", 
        4), fill = NULL, alpha = rep(0.5, 4), label.col = rep("black", 
        15), cex = rep(1, 15), fontface = rep("plain", 15), fontfamily = rep("serif", 
        15), cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22, 0.22, 
        0.11, 0.11), cat.col = rep("black", 4), cat.cex = rep(1, 
        4), cat.fontface = rep("plain", 4), cat.fontfamily = rep("serif", 
        4), cat.just = rep(list(c(0.5, 0.5)), 4), rotation.degree = 0, 
    rotation.centre = c(0.5, 0.5), ind = TRUE, 

    ### BEGIN WWK
    cex.prop = NULL, 
    ### END WWK

    print.mode = "raw", sigdigs = 3, direct.area = FALSE, area.vector = 0, 
    ...) 
{
    if (length(category) == 1) {
        cat <- rep(category, 4)
    }
    else if (length(category) != 4) {
        flog.error("Unexpected parameter length for 'category'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'category'")
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, 4)
    }
    else if (length(lwd) != 4) {
        flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lwd'")
    }
    if (length(lty) == 1) {
        lty <- rep(lty, 4)
    }
    else if (length(lty) != 4) {
        flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lty'")
    }
    if (length(col) == 1) {
        col <- rep(col, 4)
    }
    else if (length(col) != 4) {
        flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'col'")
    }
    if (length(label.col) == 1) {
        label.col <- rep(label.col, 15)
    }
    else if (length(label.col) != 15) {
        flog.error("Unexpected parameter length for 'label.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'label.col'")
    }
    if (length(cex) == 1) {
        cex <- rep(cex, 15)
    }
    else if (length(cex) != 15) {
        flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cex'")
    }
    if (length(fontface) == 1) {
        fontface <- rep(fontface, 15)
    }
    else if (length(fontface) != 15) {
        flog.error("Unexpected parameter length for 'fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontface'")
    }
    if (length(fontfamily) == 1) {
        fontfamily <- rep(fontfamily, 15)
    }
    else if (length(fontfamily) != 15) {
        flog.error("Unexpected parameter length for 'fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontfamily'")
    }
    if (length(fill) == 1) {
        fill <- rep(fill, 4)
    }
    else if (length(fill) != 4 & length(fill) != 0) {
        flog.error("Unexpected parameter length for 'fill'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fill'")
    }
    if (length(alpha) == 1) {
        alpha <- rep(alpha, 4)
    }
    else if (length(alpha) != 4 & length(alpha) != 0) {
        flog.error("Unexpected parameter length for 'alpha'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'alpha'")
    }
    if (length(cat.pos) == 1) {
        cat.pos <- rep(cat.pos, 4)
    }
    else if (length(cat.pos) != 4) {
        flog.error("Unexpected parameter length for 'cat.pos'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.pos'")
    }
    if (length(cat.dist) == 1) {
        cat.dist <- rep(cat.dist, 4)
    }
    else if (length(cat.dist) != 4) {
        flog.error("Unexpected parameter length for 'cat.dist'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.dist'")
    }
    if (length(cat.col) == 1) {
        cat.col <- rep(cat.col, 4)
    }
    else if (length(cat.col) != 4) {
        flog.error("Unexpected parameter length for 'cat.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.col'")
    }
    if (length(cat.cex) == 1) {
        cat.cex <- rep(cat.cex, 4)
    }
    else if (length(cat.cex) != 4) {
        flog.error("Unexpected parameter length for 'cat.cex'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.cex'")
    }
    if (length(cat.fontface) == 1) {
        cat.fontface <- rep(cat.fontface, 4)
    }
    else if (length(cat.fontface) != 4) {
        flog.error("Unexpected parameter length for 'cat.fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontface'")
    }
    if (length(cat.fontfamily) == 1) {
        cat.fontfamily <- rep(cat.fontfamily, 4)
    }
    else if (length(cat.fontfamily) != 4) {
        flog.error("Unexpected parameter length for 'cat.fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontfamily'")
    }
    if (!(class(cat.just) == "list" & length(cat.just) == 4 & 
        length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) == 
        2)) {
        flog.error("Unexpected parameter format for 'cat.just'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter format for 'cat.just'")
    }
    cat.pos <- cat.pos + rotation.degree
    if (direct.area) {
        areas <- area.vector
        for (i in 1:15) {
            assign(paste("a", i, sep = ""), area.vector[i])
        }
    }
    else {
        a6 <- n1234
        a12 <- n123 - a6
        a11 <- n124 - a6
        a5 <- n134 - a6
        a7 <- n234 - a6
        a15 <- n12 - a6 - a11 - a12
        a4 <- n13 - a6 - a5 - a12
        a10 <- n14 - a6 - a5 - a11
        a13 <- n23 - a6 - a7 - a12
        a8 <- n24 - a6 - a7 - a11
        a2 <- n34 - a6 - a5 - a7
        a9 <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15
        a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15
        a1 <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13
        a3 <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11
        areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, 
            a12, a13, a14, a15)
    }
    areas.error <- c("a1  <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13", 
        "a2  <- n34 - a6 - a5 - a7", "a3  <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11", 
        "a4  <- n13 - a6 - a5 - a12", "a5  <- n134 - a6", "a6  <- n1234", 
        "a7  <- n234 - a6", "a8  <- n24 - a6 - a7 - a11", "a9  <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15", 
        "a10 <- n14 - a6 - a5 - a11", "a11 <- n124 - a6", "a12 <- n123 - a6", 
        "a15 <- n12 - a6 - a11 - a12", "a13 <- n23 - a6 - a7 - a12", 
        "a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15")
    for (i in 1:length(areas)) {
        if (areas[i] < 0) {
            flog.error(paste("Impossible:", areas.error[i], "produces negative area"), 
                name = "VennDiagramLogger")
            stop(paste("Impossible:", areas.error[i], "produces negative area"))
        }
    }

    ### BEGIN WWK
    if(length(cex.prop) == 1){
        maxArea = max(areas)
        if(cex.prop == "lin"){
            for(i in 1:length(areas)){
                cex[i] = cex[i] * areas[i] / maxArea
            }
        }
        else if(cex.prop == "log10"){
            for(i in 1:length(areas)){
                if(areas[i] != 0){
                    cex[i] = cex[i] * log10(areas[i]) / log10(maxArea)
                }
                else{
                    warn(paste("Error in log10 rescaling of areas: area ",i," is zero", sep=""))
                }
            }        
        }
        else {
            stop(paste("Unknown value passed to cex.prop:", cex.prop))
        }
    }
    ### END WWK

    grob.list <- gList()
    ellipse.positions <- matrix(nrow = 4, ncol = 7)
    colnames(ellipse.positions) <- c("x", "y", "a", "b", "rotation", 
        "fill.mapping", "line.mapping")
    ellipse.positions[1, ] <- c(0.65, 0.47, 0.35, 0.2, 45, 2, 
        4)
    ellipse.positions[2, ] <- c(0.35, 0.47, 0.35, 0.2, 135, 1, 
        1)
    ellipse.positions[3, ] <- c(0.5, 0.57, 0.33, 0.15, 45, 4, 
        3)
    ellipse.positions[4, ] <- c(0.5, 0.57, 0.35, 0.15, 135, 3, 
        2)
    for (i in 1:4) {
        grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
            "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i, 
            "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i, 
            "rotation"], gp = gpar(lty = 0, fill = fill[ellipse.positions[i, 
            "fill.mapping"]], alpha = alpha[ellipse.positions[i, 
            "fill.mapping"]])))
    }
    for (i in 1:4) {
        grob.list <- gList(grob.list, ellipse(x = ellipse.positions[i, 
            "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i, 
            "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i, 
            "rotation"], gp = gpar(lwd = lwd[ellipse.positions[i, 
            "line.mapping"]], lty = lty[ellipse.positions[i, 
            "line.mapping"]], col = col[ellipse.positions[i, 
            "line.mapping"]], fill = "transparent")))
    }
    label.matrix <- matrix(nrow = 15, ncol = 3)
    colnames(label.matrix) <- c("label", "x", "y")
    label.matrix[1, ] <- c(a1, 0.35, 0.77)
    label.matrix[2, ] <- c(a2, 0.5, 0.69)
    label.matrix[3, ] <- c(a3, 0.65, 0.77)
    label.matrix[4, ] <- c(a4, 0.31, 0.67)
    label.matrix[5, ] <- c(a5, 0.4, 0.58)
    label.matrix[6, ] <- c(a6, 0.5, 0.47)
    label.matrix[7, ] <- c(a7, 0.6, 0.58)
    label.matrix[8, ] <- c(a8, 0.69, 0.67)
    label.matrix[9, ] <- c(a9, 0.18, 0.58)
    label.matrix[10, ] <- c(a10, 0.32, 0.42)
    label.matrix[11, ] <- c(a11, 0.425, 0.38)
    label.matrix[12, ] <- c(a12, 0.575, 0.38)
    label.matrix[13, ] <- c(a13, 0.68, 0.42)
    label.matrix[14, ] <- c(a14, 0.82, 0.58)
    label.matrix[15, ] <- c(a15, 0.5, 0.28)
    processedLabels <- rep("", length(label.matrix[, "label"]))
    if (print.mode[1] == "percent") {
        processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
            "label"]) * 100, digits = sigdigs), "%", sep = "")
        if (isTRUE(print.mode[2] == "raw")) {
            processedLabels <- paste(processedLabels, "\n(", 
                label.matrix[, "label"], ")", sep = "")
        }
    }
    if (print.mode[1] == "raw") {
        processedLabels <- label.matrix[, "label"]
        if (isTRUE(print.mode[2] == "percent")) {
            processedLabels <- paste(processedLabels, "\n(", 
                paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                  "label"]) * 100, digits = sigdigs), "%)", sep = ""), 
                sep = "")
        }
    }
    for (i in 1:nrow(label.matrix)) {
        grob.list <- gList(grob.list, textGrob(label = processedLabels[i], 
            x = label.matrix[i, "x"], y = label.matrix[i, "y"], 
            gp = gpar(col = label.col[i], cex = cex[i], fontface = fontface[i], 
                fontfamily = fontfamily[i])))
    }
    cat.pos.x <- c(0.18, 0.82, 0.35, 0.65)
    cat.pos.y <- c(0.58, 0.58, 0.77, 0.77)
    for (i in 1:4) {
        this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i], 
            pos = cat.pos[i], dist = cat.dist[i])
        grob.list <- gList(grob.list, textGrob(label = category[i], 
            x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
            gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                fontfamily = cat.fontfamily[i])))
    }
    grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, 
        rotation.degree, rotation.centre[1], rotation.centre[2]), 
        ...)
    if (ind) {
        grid.draw(grob.list)
    }
    return(grob.list)
}

assignInNamespace("draw.quad.venn",draw.quad.venn, ns="VennDiagram")

######################### five set #########################
draw.quintuple.venn <- function (area1, area2, area3, area4, area5, n12, n13, n14, n15, 
    n23, n24, n25, n34, n35, n45, n123, n124, n125, n134, n135, 
    n145, n234, n235, n245, n345, n1234, n1235, n1245, n1345, 
    n2345, n12345, category = rep("", 5), lwd = rep(2, 5), lty = rep("solid", 
        5), col = rep("black", 5), fill = NULL, alpha = rep(0.5, 
        5), label.col = rep("black", 31), cex = rep(1, 31), fontface = rep("plain", 
        31), fontfamily = rep("serif", 31), cat.pos = c(0, 287.5, 
        215, 145, 70), cat.dist = rep(0.2, 5), cat.col = rep("black", 
        5), cat.cex = rep(1, 5), cat.fontface = rep("plain", 
        5), cat.fontfamily = rep("serif", 5), cat.just = rep(list(c(0.5, 
        0.5)), 5), rotation.degree = 0, rotation.centre = c(0.5, 
        0.5), ind = TRUE, cex.prop = NULL, print.mode = "raw", 
    sigdigs = 3, direct.area = FALSE, area.vector = 0, ...) 
{
    if (length(category) == 1) {
        cat <- rep(category, 5)
    }
    else if (length(category) != 5) {
        flog.error("Unexpected parameter length for 'category'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'category'")
    }
    if (length(lwd) == 1) {
        lwd <- rep(lwd, 5)
    }
    else if (length(lwd) != 5) {
        flog.error("Unexpected parameter length for 'lwd'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lwd'")
    }
    if (length(lty) == 1) {
        lty <- rep(lty, 5)
    }
    else if (length(lty) != 5) {
        flog.error("Unexpected parameter length for 'lty'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'lty'")
    }
    if (length(col) == 1) {
        col <- rep(col, 5)
    }
    else if (length(col) != 5) {
        flog.error("Unexpected parameter length for 'col'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'col'")
    }
    if (length(label.col) == 1) {
        label.col <- rep(label.col, 31)
    }
    else if (length(label.col) != 31) {
        flog.error("Unexpected parameter length for 'label.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'label.col'")
    }
    if (length(cex) == 1) {
        cex <- rep(cex, 31)
    }
    else if (length(cex) != 31) {
        flog.error("Unexpected parameter length for 'cex'", name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cex'")
    }
    if (length(fontface) == 1) {
        fontface <- rep(fontface, 31)
    }
    else if (length(fontface) != 31) {
        flog.error("Unexpected parameter length for 'fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontface'")
    }
    if (length(fontfamily) == 1) {
        fontfamily <- rep(fontfamily, 31)
    }
    else if (length(fontfamily) != 31) {
        flog.error("Unexpected parameter length for 'fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fontfamily'")
    }
    if (length(fill) == 1) {
        fill <- rep(fill, 5)
    }
    else if (length(fill) != 5 & length(fill) != 0) {
        flog.error("Unexpected parameter length for 'fill'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'fill'")
    }
    if (length(alpha) == 1) {
        alpha <- rep(alpha, 5)
    }
    else if (length(alpha) != 5 & length(alpha) != 0) {
        flog.error("Unexpected parameter length for 'alpha'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'alpha'")
    }
    if (length(cat.pos) == 1) {
        cat.pos <- rep(cat.pos, 5)
    }
    else if (length(cat.pos) != 5) {
        flog.error("Unexpected parameter length for 'cat.pos'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.pos'")
    }
    if (length(cat.dist) == 1) {
        cat.dist <- rep(cat.dist, 5)
    }
    else if (length(cat.dist) != 5) {
        flog.error("Unexpected parameter length for 'cat.dist'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.dist'")
    }
    if (length(cat.col) == 1) {
        cat.col <- rep(cat.col, 5)
    }
    else if (length(cat.col) != 5) {
        flog.error("Unexpected parameter length for 'cat.col'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.col'")
    }
    if (length(cat.cex) == 1) {
        cat.cex <- rep(cat.cex, 5)
    }
    else if (length(cat.cex) != 5) {
        flog.error("Unexpected parameter length for 'cat.cex'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.cex'")
    }
    if (length(cat.fontface) == 1) {
        cat.fontface <- rep(cat.fontface, 5)
    }
    else if (length(cat.fontface) != 5) {
        flog.error("Unexpected parameter length for 'cat.fontface'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontface'")
    }
    if (length(cat.fontfamily) == 1) {
        cat.fontfamily <- rep(cat.fontfamily, 5)
    }
    else if (length(cat.fontfamily) != 5) {
        flog.error("Unexpected parameter length for 'cat.fontfamily'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter length for 'cat.fontfamily'")
    }
    if (!(class(cat.just) == "list" & length(cat.just) == 5 & 
        length(cat.just[[1]]) == 2 & length(cat.just[[2]]) == 
        2 & length(cat.just[[3]]) == 2 & length(cat.just[[4]]) == 
        2 & length(cat.just[[5]]) == 2)) {
        flog.error("Unexpected parameter format for 'cat.just'", 
            name = "VennDiagramLogger")
        stop("Unexpected parameter format for 'cat.just'")
    }
    cat.pos <- cat.pos + rotation.degree
    if (direct.area) {
        areas <- area.vector
        for (i in 1:31) {
            assign(paste("a", i, sep = ""), area.vector[i])
        }
    }
    else {
        a31 <- n12345
        a30 <- n1234 - a31
        a29 <- n1235 - a31
        a28 <- n1245 - a31
        a27 <- n1345 - a31
        a26 <- n2345 - a31
        a25 <- n245 - a26 - a28 - a31
        a24 <- n234 - a26 - a30 - a31
        a23 <- n134 - a27 - a30 - a31
        a22 <- n123 - a29 - a30 - a31
        a21 <- n235 - a26 - a29 - a31
        a20 <- n125 - a28 - a29 - a31
        a19 <- n124 - a28 - a30 - a31
        a18 <- n145 - a27 - a28 - a31
        a17 <- n135 - a27 - a29 - a31
        a16 <- n345 - a26 - a27 - a31
        a15 <- n45 - a18 - a25 - a16 - a28 - a27 - a26 - a31
        a14 <- n24 - a19 - a24 - a25 - a30 - a28 - a26 - a31
        a13 <- n34 - a16 - a23 - a24 - a26 - a27 - a30 - a31
        a12 <- n13 - a17 - a22 - a23 - a27 - a29 - a30 - a31
        a11 <- n23 - a21 - a22 - a24 - a26 - a29 - a30 - a31
        a10 <- n25 - a20 - a21 - a25 - a26 - a28 - a29 - a31
        a9 <- n12 - a19 - a20 - a22 - a28 - a29 - a30 - a31
        a8 <- n14 - a18 - a19 - a23 - a27 - a28 - a30 - a31
        a7 <- n15 - a17 - a18 - a20 - a27 - a28 - a29 - a31
        a6 <- n35 - a16 - a17 - a21 - a26 - a27 - a29 - a31
        a5 <- area5 - a6 - a7 - a15 - a16 - a17 - a18 - a25 - 
            a26 - a27 - a28 - a31 - a20 - a29 - a21 - a10
        a4 <- area4 - a13 - a14 - a15 - a16 - a23 - a24 - a25 - 
            a26 - a27 - a28 - a31 - a18 - a19 - a8 - a30
        a3 <- area3 - a21 - a11 - a12 - a13 - a29 - a22 - a23 - 
            a24 - a30 - a31 - a26 - a27 - a16 - a6 - a17
        a2 <- area2 - a9 - a10 - a19 - a20 - a21 - a11 - a28 - 
            a29 - a31 - a22 - a30 - a26 - a25 - a24 - a14
        a1 <- area1 - a7 - a8 - a18 - a17 - a19 - a9 - a27 - 
            a28 - a31 - a20 - a30 - a29 - a22 - a23 - a12
        areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, 
            a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, 
            a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)
    }
    areas.error <- c("a1 <- area1 - a7 - a8 - a18 - a17 - a19 - a9 - a27 - a28 - a31 - a20 - a30 - a29 - a22 - a23 - a12", 
        "a2 <- area2 - a9 - a10 - a19 - a20 - a21 - a11 - a28 - a29 - a31 - a22 - a30 - a26 - a25 - a24 - a14", 
        "a3 <- area3 - a21 - a11 - a12 - a13 - a29 - a22 - a23 - a24 - a30 - a31 - a26 - a27 - a16 - a6 - a17", 
        "a4 <- area4 - a13 - a14 - a15 - a16 - a23 - a24 - a25 - a26 - a27 - a28 - a31 - a18 - a19 - a8 - a30", 
        "a5 <- area5 - a6 - a7 - a15 - a16 - a17 - a18 - a25 - a26 - a27 - a28 - a31 - a20 - a29 - a21 - a10", 
        "a6 <- n35 - a16 - a17 - a21 - a26 - a27 - a29 - a31", 
        "a7 <- n15 - a17 - a18 - a20 - a27 - a28 - a29 - a31", 
        "a8 <- n14 - a18 - a19 - a23 - a27 - a28 - a30 - a31", 
        "a9 <- n12 - a19 - a20 - a22 - a28 - a29 - a30 - a31", 
        "a10 <- n25 - a20 - a21 - a25 - a26 - a28 - a29 - a31", 
        "a11 <- n23 - a21 - a22 - a24 - a26 - a29 - a30 - a31", 
        "a12 <- n13 - a17 - a22 - a23 - a27 - a29 - a30 - a31", 
        "a13 <- n34 - a16 - a23 - a24 - a26 - a27 - a30 - a31", 
        "a14 <- n24 - a19 - a24 - a25 - a30 - a28 - a26 - a31", 
        "a15 <- n45 - a18 - a25 - a16 - a28 - a27 - a26 - a31", 
        "a16 <- n345 - a26 - a27 - a31", "a17 <- n135 - a27 - a29 - a31", 
        "a18 <- n145 - a27 - a28 - a31", "a19 <- n124 - a28 - a30 - a31", 
        "a20 <- n125 - a28 - a29 - a31", "a21 <- n235 - a26 - a29 - a31", 
        "a22 <- n123 - a29 - a30 - a31", "a23 <- n134 - a27 - a30 - a31", 
        "a24 <- n234 - a26 - a30 - a31", "a25 <- n245 - a26 - a28 - a31", 
        "a26 <- n2345 - a31", "a27 <- n1345 - a31", "a28 <- n1245 - a31", 
        "a29 <- n1235 - a31", "a30 <- n1234 - a31", "a31 <- n12345")
    for (i in 1:length(areas)) {
        if (areas[i] < 0) {
            flog.error(paste("Impossible:", areas.error[i], "produces negative area"), 
                name = "VennDiagramLogger")
            stop(paste("Impossible:", areas.error[i], "produces negative area"))
        }
    }

    ### BEGIN WWK
    if(length(cex.prop) == 1){
        maxArea = max(areas)
        if(cex.prop == "lin"){
            for(i in 1:length(areas)){
                cex[i] = cex[i] * areas[i] / maxArea
            }
        }
        else if(cex.prop == "log10"){
            for(i in 1:length(areas)){
                if(areas[i] != 0){
                    cex[i] = cex[i] * log10(areas[i]) / log10(maxArea)
                }
                else{
                    warn(paste("Error in log10 rescaling of areas: area ",i," is zero", sep=""))
                }
            }        
        }
        else {
            stop(paste("Unknown value passed to cex.prop:", cex.prop))
        }
    }
    ### END WWK

    grob.list <- gList()
    dist <- 0.13
    a <- 0.24
    b <- 0.46
    init.angle <- -20
    ellipse.positions <- matrix(nrow = 5, ncol = 3)
    colnames(ellipse.positions) <- c("x", "y", "rotation")
    ellipse.positions[1, ] <- c(0.5 + dist * sin(init.angle * 
        pi/180), 0.5 + dist * cos(init.angle * pi/180), 0)
    ellipse.positions[2, ] <- c(0.5 - dist * cos((288 + init.angle - 
        270) * pi/180), 0.5 + dist * sin((288 + init.angle - 
        270) * pi/180), -110)
    ellipse.positions[3, ] <- c(0.5 - dist * sin((216 + init.angle - 
        180) * pi/180), 0.5 - dist * cos((216 + init.angle - 
        180) * pi/180), 145)
    ellipse.positions[4, ] <- c(0.5 + dist * sin((180 - 144 - 
        init.angle) * pi/180), 0.5 - dist * cos((180 - 144 - 
        init.angle) * pi/180), 35)
    ellipse.positions[5, ] <- c(0.5 + dist * cos((init.angle + 
        72 - 90) * pi/180), 0.5 - dist * sin((init.angle + 72 - 
        90) * pi/180), -72.5)
    for (i in 1:5) {
        grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
            "x"], y = ellipse.positions[i, "y"], a = a, b = b, 
            rotation = ellipse.positions[i, "rotation"], gp = gpar(lty = 0, 
                fill = fill[i], alpha = alpha[i])))
    }
    for (i in 1:5) {
        grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
            "x"], y = ellipse.positions[i, "y"], a = a, b = b, 
            rotation = ellipse.positions[i, "rotation"], gp = gpar(lwd = lwd[i], 
                lty = lty[i], col = col[i], fill = "transparent")))
    }
    label.matrix <- matrix(nrow = 31, ncol = 3)
    colnames(label.matrix) <- c("label", "x", "y")
    label.matrix[1, ] <- c(a1, 0.4555, 0.9322)
    label.matrix[2, ] <- c(a2, 0.08, 0.6)
    label.matrix[3, ] <- c(a3, 0.3, 0.1)
    label.matrix[4, ] <- c(a4, 0.79, 0.17)
    label.matrix[5, ] <- c(a5, 0.9, 0.68)
    label.matrix[6, ] <- c(a6, 0.74, 0.695)
    label.matrix[7, ] <- c(a7, 0.63, 0.805)
    label.matrix[8, ] <- c(a8, 0.4, 0.795)
    label.matrix[9, ] <- c(a9, 0.255, 0.715)
    label.matrix[10, ] <- c(a10, 0.193, 0.48)
    label.matrix[11, ] <- c(a11, 0.225, 0.333)
    label.matrix[12, ] <- c(a12, 0.42, 0.205)
    label.matrix[13, ] <- c(a13, 0.572, 0.18)
    label.matrix[14, ] <- c(a14, 0.753, 0.32)
    label.matrix[15, ] <- c(a15, 0.823, 0.47)
    label.matrix[16, ] <- c(a16, 0.747, 0.582)
    label.matrix[17, ] <- c(a17, 0.662, 0.75)
    label.matrix[18, ] <- c(a18, 0.488, 0.761)
    label.matrix[19, ] <- c(a19, 0.323, 0.737)
    label.matrix[20, ] <- c(a20, 0.253, 0.573)
    label.matrix[21, ] <- c(a21, 0.225, 0.395)
    label.matrix[22, ] <- c(a22, 0.355, 0.29)
    label.matrix[23, ] <- c(a23, 0.515, 0.205)
    label.matrix[24, ] <- c(a24, 0.655, 0.29)
    label.matrix[25, ] <- c(a25, 0.783, 0.42)
    label.matrix[26, ] <- c(a26, 0.72, 0.445)
    label.matrix[27, ] <- c(a27, 0.605, 0.701)
    label.matrix[28, ] <- c(a28, 0.342, 0.668)
    label.matrix[29, ] <- c(a29, 0.294, 0.41)
    label.matrix[30, ] <- c(a30, 0.522, 0.273)
    label.matrix[31, ] <- c(a31, 0.5, 0.5)
    processedLabels <- rep("", length(label.matrix[, "label"]))
    if (print.mode[1] == "percent") {
        processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
            "label"]) * 100, digits = sigdigs), "%", sep = "")
        if (isTRUE(print.mode[2] == "raw")) {
            processedLabels <- paste(processedLabels, "\n(", 
                label.matrix[, "label"], ")", sep = "")
        }
    }
    if (print.mode[1] == "raw") {
        processedLabels <- label.matrix[, "label"]
        if (isTRUE(print.mode[2] == "percent")) {
            processedLabels <- paste(processedLabels, "\n(", 
                paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                  "label"]) * 100, digits = sigdigs), "%)", sep = ""), 
                sep = "")
        }
    }
    for (i in 1:nrow(label.matrix)) {
        tmp <- textGrob(label = processedLabels[i], x = label.matrix[i, 
            "x"], y = label.matrix[i, "y"], gp = gpar(col = label.col[i], 
            cex = cex[i], fontface = fontface[i], fontfamily = fontfamily[i]))
        grob.list <- gList(grob.list, tmp)
    }
    cat.pos.x <- c(0.4555, 0.08, 0.3, 0.79, 0.9)
    cat.pos.y <- c(0.9322, 0.6, 0.1, 0.17, 0.68)
    for (i in 1:5) {
        this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i], 
            pos = cat.pos[i], dist = cat.dist[i])
        grob.list <- gList(grob.list, textGrob(label = category[i], 
            x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
            gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                fontfamily = cat.fontfamily[i])))
    }
    grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, 
        rotation.degree, rotation.centre[1], rotation.centre[2]), 
        ...)
    if (ind) {
        grid.draw(grob.list)
    }
    return(grob.list)
}

assignInNamespace("draw.quintuple.venn",draw.quintuple.venn, ns="VennDiagram")
