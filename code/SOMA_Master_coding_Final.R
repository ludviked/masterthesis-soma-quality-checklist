# ============================================================
# SOMA QUALITY ASSESSMENT - DESCRIPTIVES AND ANALYTICS
# ============================================================
# The script performs descriptive and analytic work on the
# SOMA checklist dataset (n = 79 SOMAs, 25 items).
# I included the following analytics: heatmap, descriptives, tetrachoric
# correlations, EFA, and LCA.
# ============================================================


# 0. Loading librarys ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(psych)
library(corrplot)
library(poLCA)


# 1. IMPORT DATA ---------------------------------------------------------

# Import data, treating "NA" strings as true NA values
soma_data <- readxl::read_excel("./data/SOMA-Data_Descriptives_Import.xlsx", na = "NA")

str(soma_data)


# 2. HEATMAP -------------------------------------------------------------

# Reshapeing from wide to long format so each row is one SOMA x one item combination
# This gives us 79 * 25 = 1975 rows
soma_long <- soma_data %>%
  tidyr::pivot_longer(cols = Q01_RQ:Q25_Preregistration,
                      names_to = "item",
                      values_to = "score") %>%
  # Create a categorical score variable since ggplot cannot map NA to a colour directly
  dplyr::mutate(score_cat = dplyr::case_when(
    is.na(score) ~ "NA",
    score == 1   ~ "1",
    score == 0   ~ "0"
  ),
  score_cat = factor(score_cat, levels = c("1", "0", "NA")),
  # Extract just the Q number for cleaner x axis labels (might not need to do this)
  item_short = stringr::str_extract(item, "Q[0-9]+"))

# Ploting a "heatmap" of checklist scores across all SOMAs and items
ggplot(soma_long, aes(
  x = factor(item_short, levels = paste0("Q", sprintf("%02d", 1:25))),
  y = forcats::fct_rev(factor(author_year)),
  fill = score_cat)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_manual(values = c("1" = "#2166ac", "0" = "#d73027", "NA" = "#ff7f00"),
                    name = "Score") +
  labs(title = "SOMA Checklist Scores", x = "Item", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 6))


# 3. DESCRIPTIVES --------------------------------------------------------

# Calculating counts and proportions of 0s, 1s and NAs per item
item_summary <- soma_long %>%
  dplyr::group_by(item_short) %>%
  dplyr::summarise(
    n_1     = sum(score == 1, na.rm = TRUE),
    n_0     = sum(score == 0, na.rm = TRUE),
    n_na    = sum(is.na(score)),
    n_valid = n_1 + n_0,
    prop_1  = round(n_1 / n_valid, 2)
  )

print(item_summary, n = 25)

# Ploting the proportion of 1s per item as a bar chart
ggplot(item_summary, aes(
  x = factor(item_short, levels = paste0("Q", sprintf("%02d", 1:25))),
  y = prop_1)) +
  geom_col(fill = "#2166ac") +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "red") +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "Proportion of SOMAs scoring 1 per checklist item",
       x = "Item", y = "Proportion scored 1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# Figure 2 - Item proportion bar chart
ggsave("Figure2_ItemProportions.png", width = 8, height = 6, dpi = 300)

# Calculating total score per SOMA (sum of all 25 items, NAs treated as 0)
# NOTE: is the total score needed for the final analysis? --------------------------------------------------------
soma_data <- soma_data %>%
  dplyr::mutate(total_score = rowSums(dplyr::select(., Q01_RQ:Q25_Preregistration),
                                      na.rm = TRUE))

# Ploting distribution of total scores across all SOMAs
ggplot(soma_data, aes(x = total_score)) +
  geom_histogram(binwidth = 1, fill = "#2166ac", colour = "white") +
  scale_x_continuous(breaks = seq(0, 25, by = 1)) +
  labs(title = "Distribution of total checklist scores across SOMAs",
       x = "Total score", y = "Count") +
  theme_minimal()

# Figure 3 - Total score histogram
ggsave("Figure3_TotalScores.png", width = 8, height = 6, dpi = 300)

# Checking which SOMAs have the lowest total scores
soma_data %>%
  dplyr::select(author_year, total_score) %>%
  dplyr::arrange(total_score) %>%
  print(n = 10)


# 4. TETRACHORIC CORRELATION MATRIX --------------------------------------

# Extracting only the 25 item columns for correlation analysis
soma_items <- soma_data %>%
  dplyr::select(Q01_RQ:Q25_Preregistration)

# Q04 and Q11 are excluded from all subsequent analyses since they have
# no variance (every SOMA scored 1 on both items).
# Rather than letting psych and poLCA drop them automatically with warnings,
# we exclude them explicitly here for cleaner and more transparent analyses.
soma_items_reduced <- soma_items %>%
  dplyr::select(-Q04_EffectSizeDef, -Q11_MultipleES)

# Computing tetrachoric correlations for binary items
tetra_cor <- psych::tetrachoric(soma_items_reduced)

# Ploting tetrachoric correlation matrix as a heatmap
corrplot::corrplot(tetra_cor$rho,
                   method = "color",
                   type = "upper",
                   tl.cex = 0.7,
                   tl.col = "black",
                   col = colorRampPalette(c("#d73027", "white", "#2166ac"))(200),
                   title = "Tetrachoric correlations between checklist items",
                   mar = c(0, 0, 1, 0))

# Figure 4 - Correlation matrix
png("Figure4_CorrelationMatrix.png", width = 800, height = 800, res = 150)
corrplot::corrplot(tetra_cor$rho,
                   method = "color",
                   type = "upper",
                   tl.cex = 0.7,
                   tl.col = "black",
                   col = colorRampPalette(c("#d73027", "white", "#2166ac"))(200),
                   title = "Tetrachoric correlations between checklist items",
                   mar = c(0, 0, 1, 0))
dev.off()

# 5. EFA -----------------------------------------------------------------

# 5a. FACTOR ENUMERATION ------------------------------------------------

# Setting seed before parallel analysis since it uses random simulated data internally (is this needed?)
set.seed(802)
psych::fa.parallel(tetra_cor$rho,
                   n.obs = 79,
                   fa = "fa",
                   main = "Parallel Analysis Scree Plot")
# Parallel analysis (seed 802) suggests 5 factors

# Kaiser-Guttman Criterion (KGC): retain factors with eigenvalues > 1
# Eigenvalues are extracted from the tetrachoric correlation matrix
kgc_eigenvalues <- eigen(tetra_cor$rho)$values
print(round(kgc_eigenvalues, 3))
sum(kgc_eigenvalues > 1)
# KGC suggests 7 factors

# Very Simple Structure (VSS) and MAP test
# VSS examines how well each factor solution reproduces the correlation matrix
# MAP (Minimum Average Partial) retains factors as long as they reduce
# the average partial correlation among items
psych::VSS(tetra_cor$rho,
           n = 8,
           n.obs = 79,
           fm = "ml",
           plot = TRUE)
# VSS complexity 1 suggests 1 factor, complexity 2 suggests 2 factors
# MAP suggests 1 factor

# Summary of factor enumeration results:
# Parallel analysis: 5 - KGC: 7 - VSS (c1): 1 - VSS (c2): 2 - MAP: 1
# The criteria disagree substantially, ranging from 1 to 7.
# The lower-end criteria (MAP and VSS) converge on 1-2 factors.
# I used a 2-factor solution as the primary model since it aligns with
# the majority of criteria. I also kept a 4-factor solution as a secondary
# comparison. I think all solutions should be treated as exploratory given the
# disagreement and poor fit.


# 5b. EFA MODEL ----------------------------------------------------------

# PRIMARY: 2-factor solution (consistent with MAP and VSS)
# Using oblimin rotation since checklist domains are likely related to each other
efa_2 <- psych::fa(tetra_cor$rho,
                   nfactors = 2,
                   n.obs = 79,
                   rotate = "oblimin",
                   fm = "ml")

# Printing factor loadings, hiding loadings below 0.3 for clarity
print(efa_2$loadings, cutoff = 0.3)

# SECONDARY: 4-factor solution (sits between the lower (1-2) and higher (5-7)
# suggestions and was kept for comparison)
efa_4 <- psych::fa(tetra_cor$rho,
                   nfactors = 4,
                   n.obs = 79,
                   rotate = "oblimin",
                   fm = "ml")

print(efa_4$loadings, cutoff = 0.3)


# 6. EFA FIT STATISTICS --------------------------------------------------

# Fit statistics for both solutions side by side
fit_stats <- data.frame(
  Statistic = c("RMSEA", "TLI", "BIC", "Chi-square", "df", "p-value"),
  EFA_2     = c(round(efa_2$RMSEA[1], 3),
                round(efa_2$TLI, 3),
                round(efa_2$BIC, 3),
                round(efa_2$STATISTIC, 3),
                efa_2$dof,
                round(efa_2$PVAL, 3)),
  EFA_4     = c(round(efa_4$RMSEA[1], 3),
                round(efa_4$TLI, 3),
                round(efa_4$BIC, 3),
                round(efa_4$STATISTIC, 3),
                efa_4$dof,
                round(efa_4$PVAL, 3))
)

print(fit_stats)

# NOTE: Both solutions are expected to show poor fit given the small sample
# (n = 79), low variance in several items, and the smoothed tetrachoric matrix.
# Combined with the disagreement between enumeration criteria, this supports the
# interpretation that the checklist items do not exhibit a stable latent factor
# structure. This is a finding in itself, and is substantively meaningful and motivates
# the person-centered LCA approach in section 7.

# 7. LCA -----------------------------------------------------------------

# poLCA requires items coded as 1 and 2 rather than 0 and 1
# NAs are kept as NA
soma_lca <- soma_items_reduced %>%
  dplyr::select(Q01_RQ:Q25_Preregistration) %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ . + 1))

# Defining the formula with all 25 items for poLCA
# cbind() combines all items into a matrix as required by poLCA
# ~ 1 specifies an unconditional LCA with no covariates
lca_formula <- cbind(Q01_RQ, Q02_Population, Q03_ConstructOutcome,
                     Q05_InclExclCriteria, Q06_JustifySelection,
                     Q07_ListIncludedMA, Q08_OverlapMention, Q09_OverlapReported,
                     Q10_OverlapHandled, Q12_StatModel,
                     Q13_ESHandling, Q14_ExtractionProcess, Q15_ReplicationSearch,
                     Q16_DoubleScreening, Q17_DoubleCoding, Q18_HetMethods,
                     Q19_BiasAssess, Q20_MAQualityAssess, Q21_DataAccess,
                     Q22_CodeAccess, Q23_SuppMaterialAccess, Q24_StatReporting,
                     Q25_Preregistration) ~ 1

# Fit LCA models with 1 to 6 classes and store results
# nrep = 10 means each model is run 10 times with different starting values
# to avoid local optima
# set.seed() to ensure reproducibility
set.seed(802)
lca_results <- list()

for (k in 1:6) {
  lca_results[[k]] <- poLCA::poLCA(lca_formula,
                                   data = soma_lca,
                                   nclass = k,
                                   nrep = 10,
                                   verbose = FALSE)
}

# Note: poLCA automatically drops Q04 and Q11 since they have no variance

# Entropy function (computed as 1 minus normalized classification uncertainty
# Values closer to 1 indicate cleaner separation between classes (higher = better))
entropy <- function(x) {
  p <- x$posterior
  1 - (-sum(p * log(p + 1e-10), na.rm = TRUE) / (nrow(p) * log(ncol(p))))
}

# Extract fit indices and entropy for each model to compare solutions
fit_table <- data.frame(
  Classes = 1:6,
  AIC     = sapply(lca_results, function(x) x$aic),
  BIC     = sapply(lca_results, function(x) x$bic),
  logLik  = sapply(lca_results, function(x) x$llik),
  df      = sapply(lca_results, function(x) x$resid.df),
  Entropy = c(NA, sapply(lca_results[2:6], entropy))
)

print(fit_table)

# BIC is lowest at 2 classesthe 
# AIC keeps decreasing but degrees of freedom go negative from 4 classes onwards,
# meaning those models are overparameterised relative to the sample size
# Entropy is high across all solutions, indicating clean classification in each


# 7a. PRIMARY SOLUTION: 2-CLASS MODEL ------------------------------------

# The 2-class solution has the best BIC, balanced class sizes, and is consistent
# with the given parsimony recommendation. The 3-class solution was kept
# below as an exploratory comparison.

lca_2 <- lca_results[[2]]

# Class sizes
print(round(lca_2$P, 3))

# Item response probabilities per class
lca_2$probs

# Extracting item response probabilities into a plottable dataframe
lca2_plot_data <- do.call(rbind, lapply(names(lca_2$probs), function(item) {
  data.frame(
    item    = item,
    class   = paste0("Class ", 1:2),
    prob_1  = lca_2$probs[[item]][, 2]
  )
}))

# Profile plot of class means for the 2-class solution
ggplot(lca2_plot_data, aes(
  x = item,
  y = prob_1,
  group = class,
  colour = class
)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "LCA 2-class solution: class means",
       x = "Item",
       y = "Mean probability of scoring 1",
       colour = "Class") +
  scale_color_manual(values = c("#d73027", "#2166ac")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Figure 6 - LCA 2-class profile plot
ggsave("Figure6_LCAProfiles.png", width = 8, height = 6, dpi = 300)

# Heatmap of item response probabilities per class
# Blue = high probability of scoring 1, Red = low probability
ggplot(lca2_plot_data, aes(x = item, y = class, fill = prob_1)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#2166ac",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "P(score = 1)") +
  labs(title = "LCA 2-class solution: item response probabilities",
       x = "Item", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# Class 2 (~74%) shows consistently high endorsement across most items (higher rigor)
# Class 1 (~26%) is notably weaker on certain domains


# 7b. EXPLORATORY COMPARISON: 3-CLASS MODEL ------------------------------

# Kept for comparison. The third class is very small (~5%, ~4 SOMAs),
# degrees of freedom are barely positive, and BIC is higher than the 2-class model.

lca_3 <- lca_results[[3]]

# Class sizes
print(round(lca_3$P, 3))

# Item response probabilities per class
lca_3$probs

# Extracting item response probabilities into a plottable dataframe
lca3_plot_data <- do.call(rbind, lapply(names(lca_3$probs), function(item) {
  data.frame(
    item    = item,
    class   = paste0("Class ", 1:3),
    prob_1  = lca_3$probs[[item]][, 2]
  )
}))

# Heatmap of item response probabilities per class
ggplot(lca3_plot_data, aes(x = item, y = class, fill = prob_1)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#2166ac",
                       midpoint = 0.5, limits = c(0, 1),
                       name = "P(score = 1)") +
  labs(title = "LCA 3-class solution: item response probabilities (exploratory)",
       x = "Item", y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# NOTE: extreme probabilities in the output (e.g. 2.239306e-107) are a sign of
# boundary separation. The model is pushing some cells to 0 or 1.
# Combined with the very small Class 1, this supports treating the 3-class
# solution as exploratory rather than the primary finding.

# (Notes for myself)
# cmPk and Bayes factor for model selection
# cmPk is the probability that model k is the best model given the data
# It is computed from BIC values following Masyn (2013)
# Bayes factor compares each model against the 2-class solution (the primary model)
# BF > 1 favours the comparison model, BF < 1 favours the 2-class model

# cmPk
bic_values <- fit_table$BIC
delta_bic   <- bic_values - min(bic_values)
cmPk        <- exp(-0.5 * delta_bic) / sum(exp(-0.5 * delta_bic))

fit_table$cmPk <- round(cmPk, 4)

# Bayes factor relative to 2-class solution
# BF(k vs 2) = exp(-0.5 * (BIC_k - BIC_2))
bic_2class       <- fit_table$BIC[fit_table$Classes == 2]
fit_table$BF_vs2 <- round(exp(-0.5 * (fit_table$BIC - bic_2class)), 4)

print(fit_table)

# Potentially easier code to view? I mean they both show the same results
# but im not sure which one is easier to follow..
fit_table %>%
  dplyr::mutate(
    Entropy = round(Entropy, 3),
    cmPk = signif(cmPk, 3),
    BF_vs2 = signif(BF_vs2, 3)
  ) %>%
  print()

# 8. NETWORK MODEL (ISING) -----------------------------------------------

library(IsingFit)
library(psychonetrics)
library(qgraph)

# IsingFit did not support missing data
# NAs in Q10 and Q13 are replaced with 0 for the network analysis
soma_network <- soma_items_reduced %>%
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(., 0)))

ising_fit <- IsingFit(soma_network, family = "binomial", plot = TRUE)

# Fit Ising network model using regularized logistic regression
# IsingFit handles larger networks (>20 nodes) unlike psychonetrics



# I was not satisfied with the visual outcome so as an exploratory option i
# removed low variance items (Q01. Q03, and Q07), for the network analysis
# as they caused instability in the logistic regression
soma_network_reduced <- soma_network %>%
  dplyr::select(-Q01_RQ, -Q03_ConstructOutcome, -Q07_ListIncludedMA)

ising_fit_reduced <- IsingFit(soma_network_reduced, 
                              family = "binomial", 
                              plot = FALSE)

# Names were so small, so I tried to make them bigger, but didn't change much..
qgraph(ising_fit_reduced$weiadj,
       labels = colnames(soma_network_reduced),
       label.cex = 1.5,
       layout = "spring",
       theme = "colorblind",
       title = "Ising network model (exploratory)")

# Figure 5 - Ising network
png("Figure5_IsingNetwork.png", width = 800, height = 800, res = 150)
qgraph(ising_fit_reduced$weiadj,
       labels = colnames(soma_network_reduced),
       label.cex = 1.5,
       layout = "spring",
       theme = "colorblind",
       title = "Ising network model (exploratory)")
dev.off()


getwd()

citation("tidyverse")
citation("readxl")
citation("psych")
citation("corrplot")
citation("poLCA")
citation("IsingFit")
citation("psychonetrics")
citation("qgraph")
citation()  # Base R citation
