# Anthropometric Status and Arsenic Methylation among Pregnant Women in Rural Northern Bangladesh

This repository contains the R code for an analysis of anthropometric status and arsenic methylation among pregnant women in rural northern Bangladesh. The analysis was conducted using data from the enrollment visit of the Pregnancy, Arsenic, and Immune Response (PAIR) Study. You can learn more about the PAIR Study [here](https://doi.org/10.1111/ppe.12949).

## 00 Make File

* [Make File (00_make.R)](00_make.R)

## 01 Setup

The first set of scripts loads functions, plot themes and labels, and data.

* [Setup: Functions (01A_setup_functions.R)](01A_setup_functions.R)
* [Setup: Plot Themes and Labels (01B_setup_themes.R)](01B_setup_themes.R)
* [Setup: Data (01C_setup_data.R)](01C_setup_data.R)

## 02 Models

The second set of scripts fits linear, beta, and Dirichlet regression models.

* [Models: Linear Regression (02A_models_linear.R)](02A_models_linear.R)
* [Models: Beta Regression (02B_models_beta.R)](02B_models_beta.R)
* [Models: Dirichlet Regression (02C_models_dirichlet.R)](02C_models_dirichlet.R)

## 03 Tables

The third set of scripts makes Tables 1-3. Tables 1-2 report summary statistics for the study sample. Table 3 reports pairwise Spearman's correlation coefficients for several continuous measures.

* [Table 1 (03A_tbl1.R)](03A_tbl1.R)
* [Table 2 (03B_tbl2.R)](03B_tbl2.R)
* [Table 3 (03C_tbl3.R)](03C_tbl3.R)

## 04 Figures

The fourth set of scripts makes Figures 1-2, which report coefficients and confidence intervals from the linear regression models.

* [Figures 1-2 (04A_fig1_fig2.R)](04A_fig1_fig2.R)

## 05 Supplemental Tables

The fifth set of scripts makes Tables S1-S3, which report coefficients and confidence intervals from the linear, beta, and Dirichlet regression models.

* [Table S1 (05A_tblS1.R)](05A_tblS1.R)
* [Table S2 (05B_tblS2.R)](05B_tblS2.R)
* [Table S3 (05C_tblS3.R)](05C_tblS3.R)

## 06 Supplemental Figures

The sixth set of scripts makes Figures S1-S7. Figure S1 compares dependent variable distributions on linear and natural log scales. Figures S2-S5 are scatter plots of arsenic methylation by anthropometric measures used to assess linearity. Figures S6-S6 report coefficients and confidence intervals from the beta and Dirichlet regression models.

* [Figure S1 (06A_figS1.R)](06A_figS1.R)
* [Figure S2 (06B_figS2_figS3_figS4_figS5.R)](06B_figS2_figS3_figS4_figS5.R)
* [Figure S6 (06C_figS6.R)](06C_figS6.R)
* [Figure S7 (06D_figS7.R)](06D_figS7.R)



