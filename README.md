# Anthropometric Measures and Arsenic Methylation among Pregnant Women in Rural Northern Bangladesh

This repository contains the R code for an analysis of anthropometric measures and arsenic methylation among pregnant women in rural northern Bangladesh. The analysis has been published in *Environmental Research*, available [here](https://doi.org/10.1016/j.envres.2023.116453).

The analysis was conducted using data from the enrollment visit of the Pregnancy, Arsenic, and Immune Response (PAIR) Study. The cohort profile for the PAIR Study has been published in *Paediatric and Perinatal Epidemiology*, available [here](https://doi.org/10.1111/ppe.12949).

## 00 Make File

* [Make File](00_make.R)

## 01 Setup

The first set of scripts loads functions, plot themes and labels, and data.

* [Setup: Functions](01A_setup_functions.R)
* [Setup: Plot Themes and Labels](01B_setup_themes.R)
* [Setup: Data](01C_setup_data.R)

## 02 Models

The second set of scripts fits linear, beta, and Dirichlet regression models.

* [Models: Linear Regression](02A_models_linear.R)
* [Models: Beta Regression](02B_models_beta.R)
* [Models: Dirichlet Regression](02C_models_dirichlet.R)

## 03 Tables

The third set of scripts makes Tables 1-3. Tables 1-2 report summary statistics for the study sample. Table 3 reports pairwise Spearman's correlation coefficients for several continuous measures.

* [Table 1: Participant Characteristics](03A_tbl1.R)
* [Table 2: Anthropometric and Arsenic Methylation Measures](03B_tbl2.R)
* [Table 3: Spearman's Correlations](03C_tbl3.R)

## 04 Figures

The fourth set of scripts makes Figures 1-2, which report coefficients and confidence intervals from the linear regression models.

* [Figures 1-2: Linear Regression](04A_fig1_fig2.R)

## 05 Supplemental Tables

The fifth set of scripts makes Table S1, which reports AIC values for linear and quadratic model terms, as well as Tables S2-S4, which report coefficients and confidence intervals from the linear, beta, and Dirichlet regression models.

* [Table S1: AIC](05A_tblS1.R)
* [Table S2: Linear Regression](05B_tblS2.R)
* [Table S3: Beta Regression](05C_tblS3.R)
* [Table S4: Dirichlet Regression](05D_tblS4.R)
* [Tables 2A, 3A, 4A: MUAFA and MUAMA Models](05E_tblS2A-tblS3A-tblS4A.R)

## 06 Supplemental Figures

The sixth set of scripts makes Figures S1-S9. Figure S1 compares dependent variable distributions on linear and natural log scales. Figures S2-S7 are scatter plots of arsenic methylation by anthropometric measures used to assess linearity. Figures S8-S9 report coefficients and confidence intervals from the beta and Dirichlet regression models.

* [Figure S1: Histograms of Arsenic Methylation](06A_figS1.R)
* [Figure S2-S7: Scatter Plots](06B_figS2-figS7.R)
* [Figure S8: Beta Regression](06C_figS8.R)
* [Figure S9: Dirichlet Regression](06D_figS9.R)



