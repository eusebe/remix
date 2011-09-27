## ##' Plot a summarize object (from summarize.data.frame)
## ##'
## ##' @param x a summarize object
## ##' @param ... currently not used
## ##' @author David Hajage
## ##' @keywords internal
## plot.summarize <- function(x, ...) {
##   mdf <- suppressMessages(melt(attr(x, "df")))
##   box <- ggplot(mdf, aes(1, value)) +
##     geom_boxplot() +
##       facet_grid(. ~ variable) +
##         theme_bw() +
##           opts(axis.text.x = theme_blank(), axis.ticks = theme_blank()) +
##             xlab(NULL)
##   dens <- ggplot(mdf, aes(value, ..density..)) +
##     geom_histogram() +
##       geom_density() +
##         facet_grid(. ~ variable) +
##           theme_bw() +
##             opts(axis.ticks = theme_blank()) +
##               xlab(NULL)
##   if (ncol(attr(x, "df")) == 1) {
##     box <- box + opts(title = names(attr(x, "df")))
##     dens <- dens + opts(title = names(attr(x, "df")))
##   }
##   # plutot utiliser gridExtra ici : http://code.google.com/p/gridextra/wiki/arrange
##   suppressMessages(arrange(box, dens, ncol = 1))
## }

## ##' Plot a summarize.by object (from summarize.data.frame.by)
## ##'
## ##' @param x a summarize.by object
## ##' @param ... currently not used
## ##' @author David Hajage
## ##' @keywords internal
## plot.summarize.by <- function(x, ...) {
##   df <- attr(x, "df")
##   by <- attr(x, "by")
##   box <- NULL
##   dens <- NULL
##   hist <- NULL
##   for (i in 1:ncol(by)) {
##     dfby <- data.frame(df, by = by[, i])
##     mdfby <- suppressMessages(melt(dfby, id = "by"))
##     box <- c(box, list(ggplot(mdfby, aes("variable", value)) +
##                        geom_boxplot(aes(fill = by)) +
##                        facet_grid(~ variable) +
##                        theme_bw() +
##                        opts(axis.text.x = theme_blank(), axis.ticks = theme_blank()) +
##                        scale_fill_discrete(names(by)[i]) +
##                        xlab(NULL)))
##     dens <- c(dens, list(ggplot(mdfby, aes(value, ..density..)) +
##                          geom_density(aes(fill = by), position = "dodge", alpha = 0.5) +
##                          facet_grid(. ~ variable) +
##                          theme_bw() +
##                          opts(axis.ticks = theme_blank()) +
##                          scale_fill_discrete(names(by)[i]) +
##                          xlab(NULL)))
##     hist <- c(hist, list(ggplot(mdfby, aes(value, ..density..)) +
##                          geom_histogram(aes(fill = by), position = "dodge") +
##                          facet_grid(by ~ variable) +
##                          theme_bw() +
##                          opts(axis.ticks = theme_blank()) +
##                          scale_fill_discrete(names(by)[i]) +
##                          xlab(NULL)))
##   }

##   if (ncol(df) == 1) {
##     box <- lapply(box, function(x) x + opts(title = names(df)))
##     dens <- lapply(dens, function(x) x + opts(title = names(df)))
##     hist <- lapply(hist, function(x) x + opts(title = names(df)))
##   }
  
##   suppressMessages(do.call(arrange, c(box, dens, hist, ncol = ncol(by))))
## }

## ##' Plot a correlation object (from correlation.data.frame)
## ##'
## ##' @param x a correlation object
## ##' @param ... currently not used
## ##' @author David Hajage
## ##' @keywords internal
## plot.correlation <- function(x, ...) {
##   dfx <- attr(x, "dfx")
##   dfy <- attr(x, "dfy")

##   xy <- unlist(lapply(dfx, function(x) lapply(dfy, function(y) cbind(x, y))), recursive = FALSE)
##   cnames <- rev(expand.grid(names(dfy), names(dfx), stringsAsFactors = FALSE))
##   xyp <- NULL
##   for (i in 1:length(xy)) {
##     df <- data.frame(xy[[i]])
##     xyp <- c(xyp, list(ggplot(df, aes(x, y)) +
##              geom_point() +
##              stat_smooth(method = lm, se = FALSE) +
##              theme_bw() +
##              xlab(cnames[i, 1]) +
##              ylab(cnames[i, 2])))
##   }
  
##   do.call(arrange, c(xyp, nrow = ncol(dfx), ncol = ncol(dfy)))
## }

## ##' Plot a freq object (from freq.data.frame)
## ##'
## ##' @param x a freq object
## ##' @param ... currently not used
## ##' @author David Hajage
## ##' @keywords internal
## plot.freq <- function(x, ...) {
##   df <- attr(x, "df")
##   mdf <- suppressMessages(melt(df, measure = names(df)))
##   bar <- ggplot(mdf, aes(value)) +
##     geom_bar(position = "dodge") +
##       facet_grid(~ variable, scale = "free_x") +
##         theme_bw() +
##           xlab(NULL)
##   print(bar)
## }

## ##' Plot a tabular object (from tabular.data.frame)
## ##'
## ##' @param x a tabular object
## ##' @param ... currently not used
## ##' @author David Hajage
## ##' @keywords internal
## plot.tabular <- function(x, ...) {
##   dfx <- attr(x, "dfx")
##   dfy <- attr(x, "dfy")

##   mdfx <- suppressMessages(melt(dfx, measure = names(dfx)))
##   mdfy <- suppressMessages(melt(dfy, measure = names(dfy)))
##   mdf <- rbind(mdfx, mdfy)
##   dfxy <- data.frame(dfx, dfy)
##   mdfxy <- melt(dfxy, measure = names(dfy))
  
##   bar <- ggplot(mdf, aes(value)) +
##     geom_bar(position = "dodge") +
##       facet_grid(~ variable, scale = "free_x") +
##         theme_bw() +
##           xlab(NULL)

##   sum <- NULL
##   for (i in 1:ncol(dfx)) {
##     sum <- c(sum, list(ggplot(mdfxy, aes(get(names(dfx)[i]), value)) +
##                        stat_sum(aes(group = 1)) +
##                        facet_grid(~ variable, scale = "free_y") +
##                        theme_bw() +
##                        xlab(names(dfx)[i]) +
##                        ylab("")))
##   }

##   sum <- ggplot(mdfxy, aes(get("agegp"), value)) + stat_sum(aes(group = 1)) + facet_grid(~ variable)
## }

## ## ggplot(esoph, aes(agegp, alcgp)) + stat_sum(aes(group = 1))
## ## ggplot(esoph, aes(agegp, alcgp, colour = agegp)) + stat_sum(aes(group = agegp))
## ## ggplot(esoph, aes(agegp, alcgp, colour = alcgp)) + stat_sum(aes(group = alcgp))
