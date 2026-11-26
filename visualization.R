# STA304 Group Project: Data Visualization
# Based on the original EDA report (34 responses).
#
# Requires content.R (for prep_data() and var_labels):
#   source("content.R")
#   source("visualization.R")
#   df <- prep_data(readxl::read_excel("cleaned_data1.xlsx"))
#
# For Shiny, call each function inside renderPlot() with the filtered data,
# and pair it with its interpretation from content.R:
#   plot_usage()      <->  interp_hours() + interp_purpose()
#   plot_rating()     <->  interp_likert(filtered(), input$var)
#   plot_efficiency() <->  interp_relationship()


# Shown instead of a plot when the filter leaves no students
empty_plot <- function(msg = "No students match the current filter") {
  plot.new()
  text(0.5, 0.5, msg, cex = 1.2, col = "grey40")
  invisible(NULL)
}


# Figure 1: Weekly AI usage by main purpose
# `limits` = range of the hours axis, e.g. c(0, 15)
plot_usage <- function(df, colour = "lightblue", limits = NULL) {
  df <- df[!is.na(df$q2_weekly_hours) & !is.na(df$main_purpose), ]
  if (nrow(df) == 0) return(empty_plot())

  # drop purposes with no students under the current filter
  df$main_purpose <- droplevels(df$main_purpose)

  old <- par(mar = c(5, 13, 4, 2))
  on.exit(par(old))

  # For a horizontal boxplot, ylim controls the numeric (hours) axis
  boxplot(q2_weekly_hours ~ main_purpose, data = df,
          main = "Weekly AI Usage by Main Purpose",
          xlab = var_labels[["q2_weekly_hours"]], ylab = "",
          col = colour, horizontal = TRUE, las = 1,
          cex.axis = 0.8, ylim = limits)
}


# Figure 2: Distribution of one 1-7 rating question chosen by the user
# `var` = a column name, e.g. input$var from selectInput(choices = rating_choices)
plot_rating <- function(df, var = "q5_understand_concepts",
                        colour = "lightgreen", limits = NULL) {
  x <- df[[var]]
  x <- x[!is.na(x)]
  if (length(x) == 0) return(empty_plot())

  counts <- table(factor(x, levels = 1:7))
  if (is.null(limits)) limits <- c(0, max(counts) + 2)

  old <- par(mar = c(5, 5, 4, 2))
  on.exit(par(old))

  barplot(counts, col = colour, ylim = limits,
          main = var_labels[[var]],
          xlab = "Score (1 = lowest, 7 = highest)",
          ylab = "Number of Students")
}


# Figure 3: Weekly AI usage and perceived efficiency
plot_efficiency <- function(df, colour = "steelblue", xlimits = NULL,
                            ylimits = c(1, 7), line_colour = "red") {
  df <- df[!is.na(df$q2_weekly_hours) & !is.na(df$q7_efficiency), ]
  if (nrow(df) == 0) return(empty_plot())

  old <- par(mar = c(5, 5, 4, 2))
  on.exit(par(old))

  # Small random jitter so students with identical answers do not overlap
  set.seed(304)
  plot(jitter(df$q2_weekly_hours, amount = 0.2),
       jitter(df$q7_efficiency, amount = 0.15),
       main = "Weekly AI Usage and Perceived Efficiency",
       xlab = var_labels[["q2_weekly_hours"]],
       ylab = var_labels[["q7_efficiency"]],
       pch = 19, col = adjustcolor(colour, alpha.f = 0.7),
       xlim = xlimits, ylim = ylimits)

  # Fit the line on the original (un-jittered) values
  if (nrow(df) >= 3 && length(unique(df$q2_weekly_hours)) > 1) {
    model <- lm(q7_efficiency ~ q2_weekly_hours, data = df)
    abline(model, col = line_colour, lwd = 2)
  }
}


# Run these lines in RStudio to preview the plots:
# source("content.R")
# df <- prep_data(readxl::read_excel("cleaned_data1.xlsx"))
# plot_usage(df)
# plot_rating(df, "q7_efficiency")
# plot_efficiency(df)
