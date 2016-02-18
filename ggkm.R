#' Create a Kaplan-Meier plot using ggplot2
#'
#' @param sfit: a survfit object
#' @param table: logical: Create a table graphic below the K-M plot, indicating at-risk numbers?
#' @param returns logical: if TRUE, return an arrangeGrob object
#' @param xlabs: x-axis label
#' @param ylabs: y-axis label
#' @param ystratalabs: The strata labels. Default = levels(summary(sfit)$strata)
#' @param ystrataname: The legend name. Default = "Strata"
#' @param timeby numeric: control the granularity along the time-axis
#' @param main plot title
#' @param pval logical: add the pvalue to the plot?
#' @param marks logical: should censoring marks be added?
#' @param shape: what shape should the censoring marks be, default is a vertical line
#' @param legend logical: should a legend be added to the plot?
#'
#' @return a ggplot is made. if return=TRUE, then an arrangeGlob object
#' is returned
#' @author Abhijit Dasgupta with contributions by Gil Tomas & Michael Way
#' \url{http://statbandit.wordpress.com/2011/03/08/an-enhanced-kaplan-meier-plot/}
#' slight adjustment to cope with none strata calls (e.g. Surv(time,event)~1),
#' option to remove the legend and also draw marks at censoring locations by Nadieh Bremer
#'
#' @examples
#'  library(survival)
#'  data(colon)
#'  fit <- survfit(Surv(time,status)~rx, data=colon)
#'  ggkm(fit, timeby=500)

ggkm <- function(sfit,
                 table = TRUE,
                 returns = FALSE,
                 xlabs = "Time-to-event",
                 ylabs = "Survival (%)",
                 xlims = c(0,max(sfit$time)),
                 ylims = c(0,1),
                 ystratalabs = c("CC", "CG", "GG"),
                 ystrataname = "Genotype",
                 timeby = 100,
                 main = "",
                 pval = FALSE,
                 marks = TRUE,
                 shape = 3,
                 legend = TRUE,
                 legendposition=c(0.85,0.8),
                 subs = NULL,
                 linecols=c("red","blue","green"),
                 linetype=c("solid", "dashed", "twodash"),
                 ...) {

  #############
  # libraries #
  #############

  #Check if the following packages have been installed. If not, install them
  if (!"ggplot2" %in% installed.packages()) install.packages("ggplot2")
  if (!"survival" %in% installed.packages()) install.packages("survival")
  if (!"gridExtra" %in% installed.packages()) install.packages("gridExtra")
  if (!"reshape" %in% installed.packages()) install.packages("reshape")

  require(ggplot2)
  require(survival)
  require(gridExtra)
  require(reshape) #rbind.fill function is in this package.
  require(plyr) #additional dependency.
  require(grid)

  suppressPackageStartupMessages(library(ggplot2, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(survival, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(gridExtra, warn.conflicts=FALSE))
  suppressPackageStartupMessages(library(reshape, warn.conflicts=FALSE))

  #################################
  # sorting the use of subsetting #
  #################################

  times <- seq(0, max(sfit$time), by = timeby)

  if(is.null(subs)){
    if(length(levels(summary(sfit)$strata)) == 0) {
      subs1 <- 1
      subs2 <- 1:length(summary(sfit,censored=T)$time)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$time)
    } else {
      subs1 <- 1:length(levels(summary(sfit)$strata))
      subs2 <- 1:length(summary(sfit,censored=T)$strata)
      subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$strata)
    }
  } else{
    for(i in 1:length(subs)){
      if(i==1){
        ssvar <- paste("(?=.*\\b=",subs[i],sep="")
      }
      if(i==length(subs)){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],"\\b)",sep="")
      }
      if(!i %in% c(1, length(subs))){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],sep="")
      }
      if(i==1 & i==length(subs)){
        ssvar <- paste("(?=.*\\b=",subs[i],"\\b)",sep="")
      }
    }
    subs1 <- which(regexpr(ssvar,levels(summary(sfit)$strata), perl=T)!=-1)
    subs2 <- which(regexpr(ssvar,summary(sfit,censored=T)$strata, perl=T)!=-1)
    subs3 <- which(regexpr(ssvar,summary(sfit,times = times,extend = TRUE)$strata, perl=T)!=-1)
  }

  if(!is.null(subs)) pval <- FALSE

  ##################################
  # data manipulation pre-plotting #
  ##################################

  if(length(levels(summary(sfit)$strata)) == 0) {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","","All"))
  } else {
    #[subs1]
    if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(sfit$strata)))
  }

  if(is.null(ystrataname)) ystrataname <- "Strata"
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(sfit$time), by = timeby)

  if(length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs2)))
  } else {
    Factor <- factor(summary(sfit, censored = T)$strata[subs2])
  }

  #Data to be used in the survival plot
  df <- data.frame(
    time = sfit$time[subs2],
    n.risk = sfit$n.risk[subs2],
    n.event = sfit$n.event[subs2],
    n.censor = sfit$n.censor[subs2],
    surv = sfit$surv[subs2],
    strata = Factor,
    upper = sfit$upper[subs2],
    lower = sfit$lower[subs2]
  )

  #Final changes to data for survival plot
  levels(df$strata) <- ystratalabs
  zeros <- data.frame(time = 0, surv = 1,
                      strata = factor(ystratalabs, levels=levels(df$strata)),
                      upper = 1, lower = 1)
  df <- rbind.fill(zeros, df)
  d <- length(levels(df$strata))

  ###################################
  # specifying axis parameteres etc #
  ###################################


  # Line Options (164 in code):
  #
  # For broken lines:
  #    geom_step(aes(linetype=strata), size = 0.75) +

  # For coloured lines:
  #    geom_step(aes(colour=strata), size = 0.75) +

  # To manually choose the colours add in:
  # scale_colour_manual(values=c("red","blue","green")) +


  p <- ggplot( df, aes(x=time, y=surv, colour=strata, linetype=strata)) +
    geom_step(size = 0.75) +
    scale_linetype_manual(name = ystrataname, values=linetype)+
    scale_colour_manual(name = ystrataname, values=linecols) +


    theme_bw() +
    theme(axis.title.x = element_text(vjust = 0.7)) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims) +
    scale_y_continuous(ylabs, limits = ylims) +
    theme(panel.grid.minor = element_blank()) +
    theme(axis.line = element_line(size =0.1, colour = "black")) +
    # MOVE LEGEND HERE BELOW [first is x dim, second is y dim]
    theme(legend.position = legendposition) +
    theme(legend.background = element_rect(fill = NULL)) +
    theme(legend.key = element_rect(colour = "NA")) +
    theme(panel.border = element_blank()) +
    labs(linetype = ystrataname) +
    theme(plot.margin = unit(c(0, 1, .5,ifelse(m < 10, 1.5, 2.5)),"lines")) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    ggtitle(main)+


    #Removes the legend:
    if(legend == FALSE)
      p <- p + theme(legend.position="none")

  #Add censoring marks to the line:
  if(marks == TRUE)
    p <- p + geom_point(data = subset(df, n.censor >= 1), aes(x = time, y = surv), shape = shape, colour = "black")

  ## Create a blank plot for place-holding
  blank.pic <- ggplot(df, aes(time, surv)) +
    geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),panel.border = element_blank())

  #####################
  # p-value placement #
  #####################a

  if(length(levels(summary(sfit)$strata)) == 0) pval <- FALSE

  if(pval == TRUE) {
    sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
    pvalue <- pchisq(sdiff$chisq,length(sdiff$n) - 1,lower.tail = FALSE)
    pvaltxt <- ifelse(pvalue < 0.0001,"p < 0.0001",paste("p =", signif(pvalue, 3)))
    # MOVE P-VALUE LEGEND HERE BELOW [set x and y]
    p <- p + annotate("text",x = (as.integer(max(sfit$time)/5)), y = 0.1,label = pvaltxt)
  }#if

  ###################################################
  # Create table graphic to include at-risk numbers #
  ###################################################

  if(length(levels(summary(sfit)$strata)) == 0) {
    Factor <- factor(rep("All",length(subs3)))
  } else {
    Factor <- factor(summary(sfit,times = times,extend = TRUE)$strata[subs3])
  }

  if(table) {
    risk.data <- data.frame(
      strata = Factor,
      time = summary(sfit,times = times,extend = TRUE)$time[subs3],
      n.risk = summary(sfit,times = times,extend = TRUE)$n.risk[subs3]
    )
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))

    data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3.5) + theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                       labels = rev(ystratalabs)) +
      scale_x_continuous("Numbers at risk", limits = xlims) +
      theme(axis.title.x = element_text(size = 10, vjust = 1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),axis.text.x = element_blank(),
            axis.ticks = element_blank(),axis.text.y = element_text(face = "bold",hjust = 1))

    data.table <- data.table +
      theme(legend.position = "none") + xlab(NULL) + ylab(NULL)

    # ADJUST POSITION OF TABLE FOR AT RISK
    data.table <- data.table +
      theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.15 * m), "lines"))

    #######################
    # Plotting the graphs #
    #######################


    grid.arrange(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                 ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))

    if(returns) {
      a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                       ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
      return(a)
    }#ifp = ggkm(fit, timeby=24, return=T)
    ggsave("Survival Analysis - Kaplan Meier plot.png", p)
  } else {
    if(returns) return(p)
  }#else
}

##################################################################################################
##################################################################################################
##################################################################################################
