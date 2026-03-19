# Two-Stage Control Chart Explorer

An interactive Shiny application for exploring **economically designed two-stage control charts** under both skew-normal and normal modeling frameworks.

This app accompanies the research paper:

> **Two-Stage Control Charts for Monitoring Correlated Quality Variables with a Bivariate Skew-Normal Framework**  
> Nan-Cheng Su, Ying-Ju (Tessa) Chen, Wei-Heng Huang

---

## 🔍 Overview

This application allows users to:

- Compare **two-stage skew-normal** and **two-stage normal**[^costa] control charts
- Explore **economic design optimization**
- Investigate how **skewness parameters affect cross-chart correlation**
- Visualize performance metrics and optimal chart parameters

The app is designed for:
- researchers in statistical quality control
- students learning control chart design
- practitioners exploring economic chart optimization

Key contributions include:

- A **bivariate skew-normal modeling framework**
- Economic design of **two-stage control charts**
- Analysis of **cross-chart correlation under skewness**
- Comparison with **two-stage normal charts**[^costa]

[^costa]: Costa, A. F. B., & De Magalhães, M. S. (2005). *Economic design of control charts: A review*. *International Journal of Production Economics*, 95(1), 9–20.
---

## 🚀 Live Demo

👉 *TBD*

---

## ⚙️ Features

### 1. Design Explorer
- Compare:
  - Two-stage skew-normal chart
  - Two-stage normal chart (Costa)
- Outputs:
  - Expected net income (E[A])
  - Average time to signal (ATS)
  - Expected false alarms (EFA)
- Displays optimal design parameters:
  - sample sizes, control limits, warning limits

---

### 2. Skewness–Correlation Explorer
- Study how correlation changes as a function of:
  - α<sub>ζ</sub>, α<sub>δ</sub>, α<sub>ε</sub>, or β₁
- Flexible parameter scanning
- Supports both **positive and negative skewness**

---

### 3. Learn Page
- Explains:
  - model components
  - economic design parameters
  - interpretation of outputs

---

## 🧮 Key Model Inputs

| Parameter | Description |
|----------|------------|
| i₁ | Income per unit time (in-control) |
| i₂ | Income per unit time (out-of-control) |
| η₁ | Cost of removing assignable cause |
| η₂ | Cost of false alarm investigation |
| η₃,X / η₄,X | Fixed / variable sampling cost for X |
| η₃,Y / η₄,Y | Fixed / variable sampling cost for Y |
| b₁ | Time to eliminate assignable cause |
| b₂ | Time lost due to false alarms |
| b₃,X / b₃,Y | Sampling and inspection time |
| θ | Rate parameter (1/θ = in-control duration) |
| β₁ | Link between surrogate and primary variables |
| α parameters | Skewness parameters |

---

