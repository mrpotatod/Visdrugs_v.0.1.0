# VisDrugs: An Interactive Shiny R Package for Drug Safety Analysis
**Use online**: [VisDrugsD Web App](http://sctdb.cn/shiny-server/Visdrugs_v.0.1.0/)  
**Or run locally** as follows:  

## Overview  
This shinyR app integrates **adverse reaction data** in ASCII format from the **FAERS** database, covering **2014Q3 to 2024Q3**. The dataset specifically filters reports submitted by four types of healthcare professionals:  
- **HP** (Health Professional)  
- **MD** (Physician)  
- **OT** (Other Health Professional)  
- **PH** (Pharmacist)  

### **Dataset Highlights**
- **Over 2,700,000** independent reports from FAERS, each involving only **one drug**, avoiding adverse reaction interference from multiple drugs.  
- Covers **~4,000 drug active ingredients** and provides **6,000,000+ adverse reaction entries**.  
- Enables precise drug safety analysis with an interactive interface.  

Users can **enter the active ingredient of a drug** to analyze and compare potential adverse reactions.  

---

## **Understanding Adverse Reactions in FAERS**
The adverse reactions referenced in this tool correspond to the **Preferred Term (PT)** in the FAERS database, based on the **Medical Dictionary for Regulatory Activities (MedDRA)**.  
When reporting data to FAERS, professionals may include:  
✔ **Adverse reactions caused by diseases**  
✔ **Reactions related to the drug's indications**  
✔ **Reactions directly caused by the drug itself**  

For **accurate analysis of drug-induced adverse reactions**, special care should be taken to **exclude indications-related adverse reactions**.  

### **How to Ensure Accuracy**
1. If the **top 15 adverse reactions** in a drug’s pie chart are all **related to indications**, download the raw data.  
2. Identify the most **frequent adverse reactions unrelated to indications**.  
3. Use the **"Reaction Comparison Between Drugs"** mode for in-depth analysis.  

---

## **Getting Started**

1. **Download Required Data File**
To use this Shiny app, download the required dataset and place it in the **`WWW\data\`** directory:

📂 **File Name**: `6keydata_prof_v2.RData`  
📥 **Download Link**: [Baidu Cloud](https://pan.baidu.com/s/19H7oEVbFy5Wb7_Z-j1v_SQ)  
🔑 **Access Code**: `fb73`  


2. **Install required dependencies**:
   ```r
   library(shiny)
   library(bslib)
   library(stringr)
   library(stringi)
   library(shinycssloaders)
   library(shinyjs)
   library(plotly)
   library(echarts4r)
   library(DT)

3. **Run app.R**

---

## ✍️ **Authors**
Renjun Yang

Research Center for Eco-Environmental Sciences, Chinese Academy of Sciences

---

If you use VisDrugs in published research, please cite the most appropriate paper(s) from this list:
Renjun Yang, Nuoya Yin, Ying Zhao, Dandan Li, Xuanling Zhang, Xingang Li, Yang Zhang, Francesco Faiola. Adverse Events During Pregnancy Associated With Entecavir and Adefovir: New Insights From a Real-World Analysis of Cases Reported to FDA Adverse Event Reporting System. Front Pharmacol. 2022 Jan. 3:12:772768. DOI: 10.3389/fphar.2021.772768.

