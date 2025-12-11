# Fourth & Go: Predicting NFL 4th Down Success
In my project titled "Fourth & Go," I conducted a comprehensive analysis of NFL play-by-play data from the 2024 season to determine the most significant predictors of fourth-down conversion success. Utilizing R to source and clean data from nflfastr and SPSS to perform a hierarchical logistic regression, I modeled the probability of success based on two distinct blocks of variables: situational controls (such as yards-to-go, score differential, and Net DVOA) and active coaching decisions (such as play type, formation, and tempo). The analysis quantified the impact of these variables—highlighting "yards-to-go" as a critical determinant—and translated the statistical findings into interpretable visualizations and an actionable framework to assist coaching staffs in making data-driven "go-or-no-go" decisions in high-leverage situations.

Key Findings
* **The "No-Man's Land":** Conversion probability drops significantly (from 72% to 33%) once the distance exceeds 2 yards.
* **Model Accuracy:** The final logistic regression model successfully classified conversion outcomes based on field position, defensive efficiency (DVOA), and score differential.
* **Deliverable:** Created a "Go/No-Go" decision framework for coaching staffs.

### 📂 Repository Structure
* **`data/`**: Contains the cleaned NFL play-by-play dataset used for analysis.
* **`scripts/`**: R code used to fetch data via `nflfastr`, clean variables, and generate visualizations.
* **`results/`**: 
  * 📄 [View Project Poster (PDF)](results/Your_Poster_Filename.pdf)
  * 📑 [View Full SPSS Statistical Output (PDF)](results/Your_SPSS_Filename.pdf)

### 🛠️ Tech Stack
* **R (nflfastr, ggplot2):** Data extraction, cleaning, and visualization.
* **SPSS:** Hierarchical Logistic Regression.
* **Excel:** Preliminary data review.

---
*Author: Chris Coker | Texas A&M University*
