# =====================================================================
# content.R  --  Step 3: readable labels + plain-language interpretations
#
# How to use in app.R:
#   source("content.R")
#   df <- prep_data(readxl::read_excel("cleaned_data1.xlsx"))   # or read.csv(...)
#
#   # inside server, with `filtered()` being the reactive filtered data:
#   output$interp_plot1 <- renderUI({ HTML(interp_likert(filtered(), input$var)) })
#
# Every interp_*() function takes the CURRENTLY FILTERED data, so the text
# updates automatically with the user's filter.
# Only base R is used here (no extra packages needed).
# =====================================================================


# ---------------------------------------------------------------------
# 1. Readable names (use these on axes, legends and drop-down menus)
# ---------------------------------------------------------------------

var_labels <- c(
  q1_used_genai           = "Has Used Generative AI",
  q2_weekly_hours         = "Weekly AI Use (Hours)",
  q3_main_purpose         = "Main Purpose of AI Use",
  q4_risk_concern         = "Concern About Risks for Own Purpose (1-7)",
  q5_understand_concepts  = "AI Helps Understand Concepts (1-7)",
  q6_reduces_independence = "AI Reduces Independent Learning (1-7)",
  q7_efficiency           = "AI Makes Work More Efficient (1-7)",
  q8_incorrect_info       = "AI Can Give Incorrect Information (1-7)",
  q9_primary_benefit      = "Primary Benefit of AI",
  usage_group             = "Weekly Usage Level"
)

# The 1-7 rating questions, for a "choose a variable" drop-down.
# Names = what the user sees, values = column names.
rating_choices <- setNames(
  c("q4_risk_concern", "q5_understand_concepts", "q6_reduces_independence",
    "q7_efficiency", "q8_incorrect_info"),
  var_labels[c("q4_risk_concern", "q5_understand_concepts",
                "q6_reduces_independence", "q7_efficiency",
                "q8_incorrect_info")]
)

# What the ends of each 1-7 scale mean (used in the interpretations)
scale_words <- list(
  q4_risk_concern         = c(low = "not concerned",   high = "concerned"),
  q5_understand_concepts  = c(low = "disagree",        high = "agree"),
  q6_reduces_independence = c(low = "disagree",        high = "agree"),
  q7_efficiency           = c(low = "disagree",        high = "agree"),
  q8_incorrect_info       = c(low = "disagree",        high = "agree")
)

# Plain-English description of each rating question
question_intro <- c(
  q4_risk_concern = paste0(
    "Students rated how concerned they are about the risks of using AI ",
    "for their own main purpose, from 1 (not at all concerned) to 7 ",
    "(extremely concerned)."),
  q5_understand_concepts = paste0(
    "Students rated whether AI helps them understand difficult concepts, ",
    "from 1 (strongly disagree) to 7 (strongly agree)."),
  q6_reduces_independence = paste0(
    "Students rated whether relying too much on AI can reduce their ability ",
    "to work independently, from 1 (strongly disagree) to 7 (strongly agree)."),
  q7_efficiency = paste0(
    "Students rated whether AI helps them complete academic tasks more ",
    "efficiently, from 1 (strongly disagree) to 7 (strongly agree)."),
  q8_incorrect_info = paste0(
    "Students rated whether AI can give incorrect or misleading information, ",
    "from 1 (strongly disagree) to 7 (strongly agree).")
)


# How each rating is described in the relationship text
relationship_text <- c(
  q4_risk_concern         = "concern about the risks of AI",
  q5_understand_concepts  = "agreement that AI helps them understand concepts",
  q6_reduces_independence = "agreement that AI reduces independent learning",
  q7_efficiency           = "agreement that AI makes their work more efficient",
  q8_incorrect_info       = "agreement that AI can give incorrect information"
)


# ---------------------------------------------------------------------
# 2. Prepare the data: add readable labels and a filter variable
# ---------------------------------------------------------------------

prep_data <- function(df) {
  df <- as.data.frame(df)

  # Main purpose: Coding (1) and Creative (5) have one respondent each,
  # so they are merged into "Other" to avoid one-person groups.
  purpose <- c("1" = "Other (Coding / Creative)",
               "2" = "Writing Tasks",
               "3" = "Learning & Understanding",
               "4" = "Exam Preparation",
               "5" = "Other (Coding / Creative)")
  df$main_purpose <- factor(
    purpose[as.character(df$q3_main_purpose)],
    levels = c("Exam Preparation", "Learning & Understanding",
               "Writing Tasks", "Other (Coding / Creative)")
  )

  benefit <- c("1" = "Understanding Concepts",
               "2" = "Saving Time & Efficiency",
               "3" = "Improving Work Quality",
               "4" = "Brainstorming")
  df$primary_benefit <- factor(benefit[as.character(df$q9_primary_benefit)],
                               levels = benefit)

  # Filter variable (the survey has no demographic questions).
  # Cut points give three groups of about equal size (11 / 11 / 12).
  df$usage_group <- cut(
    df$q2_weekly_hours,
    breaks = c(-Inf, 3, 7, Inf),
    labels = c("Light (3 hours or less)",
               "Moderate (4-6 hours)",
               "Heavy (8+ hours)")
  )

  df
}


# ---------------------------------------------------------------------
# 3. Small helpers
# ---------------------------------------------------------------------

pct <- function(x) paste0(round(100 * x), "%")

# Warning shown when the filtered group is very small
small_n_note <- function(n) {
  if (n == 0) return("<p><em>No students match the current filter.</em></p>")
  if (n < 10) {
    return(paste0(
      "<p><em>Note: only ", n, " student", ifelse(n == 1, "", "s"),
      " match the current filter, so these results can change a lot with ",
      "just one or two answers. Treat them as a rough picture only.</em></p>"))
  }
  ""
}

# One line saying how many students the current view is based on
interp_sample <- function(df, n_total = 34) {
  n <- nrow(df)
  if (n == n_total) {
    paste0("<p>Showing all <strong>", n, "</strong> survey responses.</p>")
  } else {
    paste0("<p>Showing <strong>", n, "</strong> of ", n_total,
           " survey responses (", pct(n / n_total), ").</p>")
  }
}


# ---------------------------------------------------------------------
# 4. Reactive interpretations (pass in the filtered data)
# ---------------------------------------------------------------------

# Any 1-7 rating question (q4-q8). `var` is a column name.
interp_likert <- function(df, var) {
  x <- df[[var]]
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(small_n_note(0))

  high <- mean(x >= 5)   # 5, 6, 7 = leaning towards agree / concerned
  low  <- mean(x <= 3)   # 1, 2, 3 = leaning towards disagree / not concerned
  w    <- scale_words[[var]]

  # "are concerned" for the concern question, plain "agree" for the others
  verb <- if (var == "q4_risk_concern") "are " else ""
  overall <- if (high >= 0.6) {
    paste0("Most students in this group ", verb, w["high"], ".")
  } else if (low >= 0.6) {
    paste0("Most students in this group ", verb, w["low"], ".")
  } else {
    "Opinions in this group are mixed."
  }

  paste0(
    "<p>", question_intro[var], "</p>",
    "<p><strong>", pct(high), "</strong> answered 5 or higher (", w["high"],
    "), and <strong>", pct(low), "</strong> answered 3 or lower (", w["low"],
    "). The typical (middle) answer is <strong>", median(x),
    "</strong> out of 7. ", overall, "</p>",
    small_n_note(n)
  )
}

# Weekly hours of AI use
interp_hours <- function(df) {
  x <- df$q2_weekly_hours
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(small_n_note(0))

  paste0(
    "<p>The typical student in this group uses generative AI for about ",
    "<strong>", median(x), " hours per week</strong> (the middle value). ",
    "Answers range from ", min(x), " to ", max(x), " hours. ",
    "The average is ", round(mean(x), 1), " hours",
    if (mean(x) > median(x) + 0.3)
      ", which is pulled up by a few students who use AI a lot."
    else ".",
    "</p>",
    small_n_note(n)
  )
}

# Main purpose (categorical)
interp_purpose <- function(df) {
  n <- nrow(df)
  if (n == 0) return(small_n_note(0))
  tab <- sort(table(df$main_purpose), decreasing = TRUE)
  tab <- tab[tab > 0]
  top <- names(tab)[1]
  paste0(
    "<p>The most common main use of AI in this group is <strong>", top,
    "</strong> (", tab[1], " of ", n, " students, ", pct(tab[1] / n), ")",
    if (length(tab) > 1)
      paste0(", followed by ", names(tab)[2], " (", pct(tab[2] / n), ")")
    else "",
    ".",
    if (top %in% c("Exam Preparation", "Learning & Understanding"))
      " In other words, students in this group mainly use AI to study and prepare."
    else "",
    "</p>",
    small_n_note(n)
  )
}

# Primary benefit (categorical)
interp_benefit <- function(df) {
  n <- nrow(df)
  if (n == 0) return(small_n_note(0))
  tab <- sort(table(df$primary_benefit), decreasing = TRUE)
  tab <- tab[tab > 0]
  top2 <- sum(head(tab, 2))
  paste0(
    "<p>The benefit students value most is <strong>", names(tab)[1],
    "</strong> (", pct(tab[1] / n), ")",
    if (length(tab) > 1)
      paste0(". Together with ", names(tab)[2], ", these two answers cover ",
             pct(top2 / n), " of students in this group")
    else "",
    ".</p>",
    small_n_note(n)
  )
}

# Relationship between weekly hours and a 1-7 rating (default: efficiency)
interp_relationship <- function(df, y = "q7_efficiency") {
  d <- df[!is.na(df$q2_weekly_hours) & !is.na(df[[y]]), ]
  n <- nrow(d)
  if (n < 5 || sd(d$q2_weekly_hours) == 0 || sd(d[[y]]) == 0) {
    return(paste0(
      "<p>There are too few students (or too little variation) in this ",
      "group to describe a relationship.</p>", small_n_note(n)))
  }

  r   <- cor(d$q2_weekly_hours, d[[y]])
  fit <- lm(d[[y]] ~ d$q2_weekly_hours)
  b   <- coef(fit)[2]
  p   <- summary(fit)$coefficients[2, 4]

  strength <- if (abs(r) < 0.1) "almost no" else
              if (abs(r) < 0.4) "a weak" else
              if (abs(r) < 0.7) "a moderate" else "a strong"
  direction <- if (r > 0) "positive" else "negative"

  paste0(
    "<p>Each dot is one student. There is <strong>", strength, " ",
    if (abs(r) >= 0.1) direction else "", "</strong> relationship between ",
    "how many hours students use AI each week and their ",
    relationship_text[[y]], ". ",
    if (abs(r) >= 0.1)
      paste0("On average, each extra hour of AI use per week goes with a ",
             abs(round(b, 2)), "-point ", ifelse(b > 0, "higher", "lower"),
             " rating. ")
    else "",
    "</p><p>",
    if (p < 0.05)
      "This pattern is strong enough that it is unlikely to be due to chance alone. "
    else
      "However, this pattern is not strong enough to rule out chance: with this many students, a pattern like this could appear even if there were no real link. ",
    "Even where a pattern exists, it does <strong>not</strong> show that using ",
    "AI more <em>causes</em> higher ratings. Students who already find AI ",
    "useful may simply choose to use it more.</p>",
    small_n_note(n)
  )
}


# ---------------------------------------------------------------------
# 5. Static text: key findings for the home / summary page
#    (based on all 34 responses)
# ---------------------------------------------------------------------

key_findings <- paste0(
  "<h4>Key findings</h4><ul>",
  "<li><strong>AI use is nearly universal.</strong> All 33 students who ",
  "answered said they had used generative AI for coursework. The typical ",
  "student uses it about 5 hours a week.</li>",
  "<li><strong>It is mainly a study tool.</strong> Almost half of students ",
  "(47%) use AI mainly for exam preparation, and another 29% use it to learn ",
  "and understand material.</li>",
  "<li><strong>Students see clear benefits.</strong> 91% agree that AI helps ",
  "them understand difficult concepts, and 88% agree it helps them work more ",
  "efficiently.</li>",
  "<li><strong>They also see the risks...</strong> 85% agree that AI can give ",
  "incorrect information, and 85% agree that relying on it can reduce their ",
  "ability to work independently.</li>",
  "<li><strong>...but are not very worried for themselves.</strong> When asked ",
  "about the risks of using AI for their <em>own</em> main purpose, only 24% ",
  "said they were concerned, and 56% were at most slightly concerned.</li>",
  "<li><strong>More use goes with slightly higher efficiency ratings,</strong> ",
  "but this link is weak and could be due to chance (it is not statistically ",
  "significant).</li>",
  "</ul>"
)
