# Exploration & Data Analysis
Pier Lorenzo Paracchini  
11/27/2016  



Using the `tidyverse` package for performing exploration and data analysis.


```r
require(data.table)
require(tidyverse)
```

## Information about the data

__Data fields__

__fecha_dato__, The table is partitioned for this column  
__ncodpers__, Customer code  
__ind_empleado__, Employee index: A active, B ex employed, F filial, N not employee, P pasive
pais_residencia	Customer's Country residence  
__sexo__, Customer's sex  
__age__, Age  
__fecha_alta__, The date in which the customer became as the first holder of a contract in the bank  
__ind_nuevo__, New customer Index. 1 if the customer registered in the last 6 months.  
__antiguedad__, Customer seniority (in months)
__indrel__	1 (First/Primary), 99 (Primary customer during the month but not at the end of the month)  
__ult_fec_cli_1t__, Last date as primary customer (if he isn't at the end of the month)  
__indrel_1mes__, Customer type at the beginning of the month ,1 (First/Primary customer), 2 (co-owner ),P (Potential),3 (former primary), 4(former co-owner)  
__tiprel_1mes__, Customer relation type at the beginning of the month, A (active), I (inactive), P (former customer),R (Potential)  

__indresi__, Residence index (S (Yes) or N (No) if the residence country is the same than the bank country)  
__indext__, Foreigner index (S (Yes) or N (No) if the customer's birth country is different than the bank country)  
__conyuemp__, Spouse index. 1 if the customer is spouse of an employee  
__canal_entrada__, channel used by the customer to join  
__indfall__, Deceased index. N/S  
__tipodom__, Addres type. 1, primary address  
__cod_prov__, Province code (customer's address)  
__nomprov__, Province name  
__ind_actividad_cliente__,Activity index (1, active customer; 0, inactive customer)  
__renta__, Gross income of the household  
__segmento__, segmentation: 01 - VIP, 02 - Individuals 03 - college graduated  

__ind_ahor_fin_ult1__, Saving Account  
__ind_aval_fin_ult1__, Guarantees  
__ind_cco_fin_ult1__, Current Accounts  
__ind_cder_fin_ult1__, Derivada Account  
__ind_cno_fin_ult1__, Payroll Account  
__ind_ctju_fin_ult1__, Junior Account  
__ind_ctma_fin_ult1__, Más particular Account  
__ind_ctop_fin_ult1__, particular Account  
__ind_ctpp_fin_ult1__, particular Plus Account  
__ind_deco_fin_ult1__, Short-term deposits  
__ind_deme_fin_ult1__, Medium-term deposits  
__ind_dela_fin_ult1__, Long-term deposits  
__ind_ecue_fin_ult1__, e-account  
__ind_fond_fin_ult1__, Funds  
__ind_hip_fin_ult1__, Mortgage  
__ind_plan_fin_ult1__, Pensions  
__ind_pres_fin_ult1__, Loans  
__ind_reca_fin_ult1__, Taxes  
__ind_tjcr_fin_ult1__, Credit Card  
__ind_valo_fin_ult1__, Securities  
__ind_viv_fin_ult1__, Home Account  
__ind_nomina_ult1__, Payroll  
__ind_nom_pens_ult1__, Pensions  
__ind_recibo_ult1__, Direct Debit  

## Getting the data


```r
input_file <- "./../data/raw/train_ver2.csv"
df <- fread(input = input_file, header = T, stringsAsFactors = F, nrows = 10000)
```

