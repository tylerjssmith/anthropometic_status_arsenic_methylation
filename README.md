# Anthropometric Status and Arsenic Methylation among Pregnant Women in Rural Northern Bangladesh

This repository contains the R code for an analysis of anthropometric status and arsenic methylation among pregnant women in rural northern Bangladesh. The analysis was conducted using data from the enrollment visit of the Pregnancy, Arsenic, and Immune Response (PAIR) Study. You can learn more about the PAIR Study [here](https://doi.org/10.1111/ppe.12949).

## Make File
* [Make File (00_make.R)](00_make.R)

## Setup

The first set of scripts loads functions, plot themes and labels, and data.

* [Setup: Functions (01A_setup_functions.R)](01A_setup_functions.R)
* [Setup: Plot Themes and Labels (01B_setup_themes.R)](01B_setup_themes.R)
* [Setup: Data (01C_setup_data.R)](01C_setup_data.R)

## Models

The second set of scripts fits linear, beta, and Dirichlet regression models. Linear models are reported by Figures 1-2 and Table S1. Beta models are reported by Table S2 and Figure S6. Dirichlet models are reported by Table S3 and Figure S7.

* [Models: Linear Regression (02A_models_linear.R)](02A_models_linear.R)
* [Models: Beta Regression (02B_models_beta.R)](02B_models_beta.R)
* [Models: Dirichlet Regression (02C_models_dirichlet.R)](02C_models_dirichlet)

## Tables

The third set of scripts makes Tables 1-3.

* [Table 1 (03A_tbl1.R)](03A_tbl1.R)
* [Table 2 (03B_tbl2.R)](03B_tbl2.R)
* [Table 3 (03C_tbl3.R)](03C_tbl3.R)

## Figures

The fourth set of scripts makes Figures 1-2.

## Supplemental Tables

The fifth set of scripts makes Tables S1-S3.

## Supplemental Figures

The sixth set of scripts makes Figures S1-S7.
