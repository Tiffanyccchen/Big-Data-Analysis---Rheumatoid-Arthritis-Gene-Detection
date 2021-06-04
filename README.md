# Big-Data-Analysis---Rheumatoid-Arthritis-Gene-Detection-2018
Final Project for the Big Data Analysis Course (ran on cluster and both local machine) - Data Cleaning, EDA, Modeling, and Inferencing using Random Forest

## Introduction
The whole analysis process is in the team3GWAS.pdf.

* Data:
   * The data is private (provided by the lecturer). Also, it's too big to be uploaded.
     We use two fields - whether the person has Rheumatoid-Arthritis or not and [SNPs](https://en.wikipedia.org/wiki/Single-nucleotide_polymorphism) values.
 
## Structure
  1. First Analyzation:   
    Sample a small section of data to conduct tests
    
  2. Second Analyzation:     
    Split data into numerous sections and perform Random Forest.     
    Select top 100 important SNPs each and combine them. Do Random Forest again to find out important SNPs.  
      
  3. Third Analyzation:  
    Ran on Cluster (Server provided by the department).   
    Recode SNPs into numbers (Additive, Recessive, Dominant).  
    The results improve.
      
  4. Final Analyzation:  
    Recode SNPs + Run on Cluster -> achieve > 80% accuracy.  
    Important SNPs conform to other literatures. Find possible interactions between Genes using Logistic Regression Analysis.  
  
## Notice
The project is written when I was a junior, therefore codes are rather messy.     
However, it still showcase my ability to conduct big data analysis project from preprocessing to modeling and prediction,    
and the ability to make a report of findings.  
