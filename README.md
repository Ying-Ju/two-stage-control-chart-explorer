# Two-Stage Control Charts with a Bivariate Skew-Normal Framework

This repository contains the full implementation of methods, simulations, and numerical experiments for the paper:

> **Two-Stage Control Charts for Monitoring Correlated Quality Variables with a Bivariate Skew-Normal Framework**  
> Nan-Cheng Su, Ying-Ju (Tessa) Chen, Wei-Heng Huang

---

## 🔍 Overview

This project develops a **two-stage control chart framework** for monitoring correlated quality variables under **skew-normal distributions**, extending classical economic design approaches.

Key contributions include:

- A **bivariate skew-normal modeling framework**
- Economic design of **two-stage control charts**
- Analysis of **cross-chart correlation under skewness**
- Comparison with **two-stage normal charts***[^costa]
* Costa, A. F., & De Magalhães, M. S. (2005). Economics, 95(1), 9-20.


---

## 📂 Repository Structure

```text
├── R/
│   ├── 00_all_functions.R        # Core functions
│   ├── 01_two_stage_sn.R
│   ├── 02_two_stage_normal.R
|   ├── 03_skewness_impact.R
│   └── 04_benchmark_comparison.R
│
│
├── app/                      
│   ├── app.R
│   └── www/
│
├── figures/
│   
│
└── README.md

[^costa]: Costa, A. F. B., & De Magalhães, M. S. (2005). *Economics*, 95(1), 9–20.
