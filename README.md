# hospital-readmission-analysis-r
## Hospital Readmission Analysis for Diabetic Patients (R)

### Business Problem
Hospital readmissions within 30 days are costly and often preventable. Healthcare providers and payers want to understand which patient characteristics are associated with higher readmission risk in order to improve care planning and reduce costs.

This project analyzes hospital encounter data for diabetic patients to identify patterns in readmissions and to validate reported findings from an existing healthcare study.

---

### Objective
- Analyze patient demographics, diagnoses, and clinical factors related to hospital readmissions
- Quantify 30-day readmission rates across key patient groups
- Validate summary statistics reported in a published study using real-world data

---

### Tools & Technologies
- **R**
- **dplyr** – data cleaning and transformation
- **ggplot2** – exploratory visualization
- **kableExtra** – business-ready summary tables

---

### Data Overview
- 100,000+ hospital encounters of diabetic patients
- One encounter per patient
- Variables include age group, gender, race, diagnosis, HbA1c test results, length of stay, and readmission status

---

### Analysis Approach
1. Filtered data to match real-world business rules:
   - First encounter per patient
   - Excluded hospice discharges and deaths
2. Aggregated patient data to calculate:
   - Population distributions
   - 30-day readmission rates by category
3. Reproduced a comprehensive summary table of patient characteristics
4. Analyzed length of hospital stay using descriptive statistics
5. Explored readmission patterns by HbA1c testing results using visualization

---

### Key Insights
- Readmission rates vary across patient demographics and clinical characteristics
- Certain diagnostic groups and age categories show higher readmission proportions
- HbA1c testing categories exhibit differences in readmission patterns, suggesting areas for further investigation
- Most reported statistics from the reference study were reproducible using the raw dataset

---

### Business Value
- Helps healthcare stakeholders identify patient segments associated with higher readmission risk
- Supports data-driven decisions for care management and quality improvement initiatives
- Demonstrates how analytical validation can be used to confirm published insights before operational use

---

### Repository Structure
- `hospital-readmission-r.R` – data cleaning, aggregation, and visualization
- `data/` – dataset or data description
- `visuals/` – generated tables and charts

---

### Notes
This project focuses on descriptive analytics and insight generation rather than predictive modeling.
