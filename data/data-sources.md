# Data Sources

1. Business Creation Data from the Ecuadorian Companies' Superintendency

Directorio de Compañías (Companies' Directory dataset)

https://mercadodevalores.supercias.gob.ec/reportes/directorioCompanias.jsf

2. Indicadores de Seguridad Ciudadana (Ministerio de Gobierno)

- Citizen safety indicators denote thefts and homicides, as reported by MinGob.

http://cifras.ministeriodegobierno.gob.ec/comisioncifras/

- Normally this doesn't work outside Ecuador.

3. DPA 2022 (defines the political/administrative division of Ecuador.)

https://www.ecuadorencifras.gob.ec/documentos/web-inec/Geografia_Estadistica/Micrositio_geoportal/index.html#clasificador-geog-dpa

- This is important to identify provinces accurately.

- DPA 2022 (found in the INEC website) uses tildes for provinces. SCVS uses without tildes but with the ñ in Cañar. I correct for this in the data preparation script.

4. Formal Job Contracts (SUT)

https://www.datosabiertos.gob.ec/dataset/contratos-en-el-sistema-unico-de-trabajo

- Holds all formal jobs data registered in the Unified Jobs System (SUT from its initials in Spanish) from 2020 to 2023. 

- Can only be observed at the monthly level, and has significant limitations. Used as a robustness check.

5. Social Security Employment Registry

https://www.ecuadorencifras.gob.ec/registro-empleo-seguridad-social/

- The Social Security Institute's (IESS, for its initials in Spanish) registry of labour contracts. All formal job contracts pass through here, since there is mandatory contribution to Social Security. 

6. IRS Active Contributors

6. IRS Sales

7. Remote Workers

8. Transit Accidents

9. Covid Cases

10. Layoffs

