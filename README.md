# 🧬 Projections of Colorectal Cancer Incidence and Recurrence from 2025 to 2050

> An interactive R Shiny application for exploring the future burden of colorectal cancer and recurrence in the context of demographic change.

---

## 🎯 About the application

This application allows users to explore projected numbers of patients with colorectal cancer (CRC) and subsequent disease recurrence from **2025 to 2050**.

Users can define a hypothetical CRC population according to:

- 👥 **Population size**
- 🎂 **Age distribution**
- 🧬 **UICC stage distribution**

Based on demographic projections from **Eurostat**, the application estimates colorectal cancer incidence rates for 2025 and projects the number of patients with CRC through 2050.

The application subsequently estimates the expected number of patients experiencing colorectal cancer recurrence based on the specified population and stage distribution.

The purpose of the application is to illustrate how **demographic changes and an ageing population may influence the future burden of colorectal cancer and cancer recurrence**.

---

## 🔬 Background

Colorectal cancer is predominantly a disease of older adults, and demographic changes are therefore expected to have a substantial impact on the future number of patients diagnosed with colorectal cancer.

Even if age-specific cancer incidence rates remain relatively stable, changes in the size and age composition of the population may lead to substantial changes in the absolute number of patients diagnosed with CRC.

This application was developed to explore these demographic effects and estimate the potential future burden of colorectal cancer and recurrence from **2025 to 2050**.

---

## 📊 Outputs

The application provides projections of:

- 📈 The number of patients diagnosed with colorectal cancer
- 🔄 The number of patients experiencing colorectal cancer recurrence
- 📅 Changes in the projected disease burden from 2025 to 2050

The results can be explored interactively by changing the characteristics of the simulated CRC population.

---

## 🚀 Getting started

### Requirements

The application is written in **R** and uses the **Shiny** framework.

You will need:

- [R](https://cran.r-project.org/)
- [RStudio](https://posit.co/download/rstudio/)

### Launch the application

The application can be launched directly from RStudio.

Copy and paste the following code into the RStudio console:

```r
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny")
}

shiny::runGitHub("future_rec_estimator", "jbergnors")
```

## 🖥️ How to use the application

### 1. Define the population

Specify the number of colorectal cancer patients included in the population of interest.

### 2. Define the age distribution

Define the age distribution of the CRC population.

The age distribution determines how the population interacts with age-specific incidence rates and future demographic projections.

### 3. Define the UICC stage distribution

Specify the distribution of patients across UICC stages.

The stage distribution is subsequently used when estimating the number of patients expected to experience recurrence.

### 4. Explore the projections

The application generates projections of colorectal cancer incidence and recurrence through 2050.

The results can be used to explore how different assumptions regarding population size, age distribution, and disease stage influence the projected future burden of CRC.

---

## 📚 Data sources

### Population projections

The demographic projections used by the application are based on **Eurostat population projections**.

Further information about the Eurostat population projection methodology is available here:

[Eurostat – Population projections](https://ec.europa.eu/eurostat/cache/metadata/en/proj_25n_esms.htm)

### Colorectal cancer incidence

The application uses age-specific colorectal cancer incidence rates to estimate the number of new CRC patients in the projected population.

### Recurrence

Projected recurrence is estimated using recurrence patterns according to UICC stage and the projected number of patients diagnosed with colorectal cancer.

---

## 👥 Development team

The application was developed by:

**Jesper Nors, MD, PhD**  
[ORCID: 0000-0002-9104-7263](https://orcid.org/0000-0002-9104-7263)

**Kåre Andersson Gotschalck, MD, PhD**  
[ORCID: 0000-0001-5119-2231](https://orcid.org/0000-0001-5119-2231)

---

## 📖 Related publication

The application was developed in connection with research investigating the future burden of colorectal cancer incidence and recurrence in the context of demographic change.

A publication describing the underlying methodology and findings will be referenced here.

> **Please cite the associated publication when using this application in academic work.**

---

## 📜 License

This project is licensed under the **PolyForm Noncommercial License 1.0.0**.

The software is made available for **non-commercial use**.

Please see the [`LICENSE`](LICENSE) file for the complete license terms and conditions.

---

## 📧 Contact

For questions regarding the application, methodology, or potential collaborations, please contact:

**Jesper Nors, MD, PhD**

[ORCID: 0000-0002-9104-7263](https://orcid.org/0000-0002-9104-7263)

---

> ⚠️ **Disclaimer**
>
> This application is intended for research and exploratory purposes. Projected numbers are dependent on the assumptions, input parameters, and data sources underlying the model.
