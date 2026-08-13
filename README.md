# 🔭 Projections of Colorectal Cancer Incidence and Recurrence from 2025 to 2050

> An interactive R Shiny application for exploring the future burden of colorectal cancer and recurrence in the context of demographic change.

---

## 🎯 About the application

This application allows users to explore projected numbers of patients with colorectal cancer (CRC) and subsequent disease recurrence from **2025 to 2050** across all 27 European Union (EU) Member States and three European Free Trade Association (EFTA) countries (Iceland, Norway, and Switzerland).

Users can define a hypothetical CRC population according to:

- 👥 **Population size**
- 🎂 **Age distribution**
- 📊 **UICC stage distribution**

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

## ⚙️ How to use the application

### 1. 👥 Define the population
- Specify the **number of CRC patients**
- Define the **median age**
- Define the **age range**
> The age distribution is used to calculate age-specific incidence rates based on the 2025 population demographics.

### 2. 🧬 Define the UICC stage distribution
- Specify the proportion of patients with **UICC stage I, II, and III CRC**
> The stage distribution is used to estimate future recurrence

### 3. 🌍 Select a country
- Select one of the **30 available countries**

### 4. 📈 Explore CRC incidence projections
- Explore projected numbers of **new CRC patients from 2025 to 2050**
- Examine how population size and age distribution influence future CRC incidence

### 5. 🔄 Define recurrence rates
- Specify the **5-year recurrence rate** for UICC stage I, II, and III CRC
> Default recurrence rates are based on previous Danish data  
  [DOI: 10.1001/jamaoncol.2023.5098](https://doi.org/10.1001/jamaoncol.2023.5098)
> Only recurrences occurring within **5 years after surgery** are included
- Choose the annual change in recurrence rates from **−2% to +2%**

> The default assumption is stable recurrence rates from 2025 to 2050.

### 6. 📊 Explore CRC recurrence projections
- Explore projected numbers of **CRC recurrences from 2025 to 2050**
- Assess the impact of different recurrence-rate assumptions

---

## 📚 Data sources

### Population projections

The demographic projections used by the application are based on **Eurostat population projections**.

Further information about the Eurostat population projection methodology is available here:

[Eurostat – Population projections](https://ec.europa.eu/eurostat/cache/metadata/en/proj_25n_esms.htm)


### Recurrence

The methodology for colorectal cancer recurrence used in this application is based on our nationwide Danish cohort study:

> Nors J, Iversen LH, Erichsen R, Gotschalck KA, Andersen CL.  
> [Incidence of Recurrence and Time to Recurrence in Stage I to III Colorectal Cancer: A Nationwide Danish Cohort Study](https://jamanetwork.com/journals/jamaoncology/fullarticle/2812113).  
> *JAMA Oncology.* 2024;10(1):54–62.  
> doi: [10.1001/jamaoncol.2023.5098](https://doi.org/10.1001/jamaoncol.2023.5098)

**Please cite this publication when using the recurrence estimates implemented in this application.**

---

## 👥 Development team

This application was developed by:

### 👨‍⚕️ Jesper Nors, MD, PhD

**Resident Physician**  
Department of Surgery, Randers Regional Hospital

**Researcher**  
University Research Clinic for Coloproctology and Endoscopy (U-COPE), Aarhus University

**Role:** Conceptualisation · Methodology · Software development · Statistical analysis

[![ORCID](https://img.shields.io/badge/ORCID-0000--0002--9104--7263-A6CE39?logo=orcid&logoColor=white)](https://orcid.org/0000-0002-9104-7263)
[![Scopus](https://img.shields.io/badge/Scopus-Author%20ID%2057200193503-E9711C?logo=scopus&logoColor=white)](https://www.scopus.com/authid/detail.uri?authorId=57200193503)

### 👨‍⚕️ Kåre Andersson Gotschalck, MD, PhD

**Consultant Surgeon and Clinical Associate Professor**  
Department of Surgery, Horsens Regional Hospital  
Department of Clinical Medicine, Aarhus University

**Role:** Conceptualisation · Clinical expertise · Methodology · Supervision

[![ORCID](https://img.shields.io/badge/ORCID-0000--0001--5119--2231-A6CE39?logo=orcid&logoColor=white)](https://orcid.org/0000-0001-5119-2231)
[![Scopus](https://img.shields.io/badge/Scopus-Author%20ID%2057213590742-E9711C?logo=scopus&logoColor=white)](https://www.scopus.com/authid/detail.uri?authorId=57213590742)

---

## 📖 Related publication

The application was developed in connection with research investigating the future burden of colorectal cancer incidence and recurrence in the context of demographic change.

A publication describing the underlying methodology and findings will be referenced here.

> **Please cite the associated publication when using this application in academic work.**

---

## 📜 License

This project is licensed under the **MIT License**.

Please see the [`LICENSE`](LICENSE) file for the complete license terms and conditions.

---

## 📧 Contact

For questions regarding the application, methodology, or potential collaborations, please contact:

**Jesper Nors, MD, PhD**

📧 Email: [jenors@rm.dk](mailto:jenors@rm.dk)

[jespernors.dk](https://www.jespernors.dk)

---

> ⚠️ **Disclaimer**
>
> This application is intended for research and exploratory purposes. Projected numbers are dependent on the assumptions, input parameters, and data sources underlying the model.
