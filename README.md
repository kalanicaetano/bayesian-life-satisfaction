# Bayesian Hierarchical Analysis of Life Satisfaction Across 66 Countries

What makes people happy? Does the answer change depending on where you live? This project uses Bayesian hierarchical modeling to examine the individual and cross-national determinants of subjective life satisfaction, drawing on nationally representative survey data from 66 countries.

The analysis finds that self-rated health and income are the strongest individual predictors of life satisfaction, while national context independently accounts for around 11.5% of the total variation in reported wellbeing, even after controlling for all individual characteristics. Notably, the relationship between income and happiness is not uniform: it is far stronger in countries marked by material deprivation than in wealthier nations. Money matters most where basic needs are least secure.

---

## Data

**Source:** World Values Survey (WVS) Wave 7 (2017–2022)  
**Coverage:** 90,940 respondents across 66 countries
**Citation:** Haerpfer, C., et al. (2022). *World Values Survey Wave 7 Cross-National Data-Set* (v6.0). World Values Survey Association. https://doi.org/10.14281/18241.20

The WVS data cannot be redistributed but is freely available for academic use. Download the `.rds` file from [worldvaluessurvey.org](https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp) and place it in the project root before running the script.

---

## What's in this repo

| File | Description |
|---|---|
| `bayesian_analysis.R` | Full analysis script: data cleaning, EDA, model fitting, figures, tables |
| `bayesian_report.tex` | LaTeX source for the written report |
| `plots/` | All 10 output figures (PDF) |
| `README.md` | This file |

---

## How to reproduce

Install the required R packages:

```r
install.packages(c("tidyverse", "haven", "rstanarm", "bayesplot",
                   "loo", "patchwork", "countrycode", "knitr"))
```

Stan installs automatically with `rstanarm`. Then run `bayesian_analysis.R` from top to bottom. To compile the report to PDF:

```r
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::pdflatex("bayesian_report.tex")
```

---

## Methods at a glance

Three Bayesian hierarchical linear models were fit using `rstanarm` (Hamiltonian Monte Carlo via Stan), progressing from complete pooling to random intercepts to random intercepts with random slopes for income. Model comparison used leave-one-out cross-validation (LOO-CV). All predictors were standardized. A stratified subsample of 150 respondents per country was used for computational efficiency.

---

## License

Code is available under the MIT License. WVS data is subject to its own terms of use. See [worldvaluessurvey.org](https://www.worldvaluessurvey.org) for details.
