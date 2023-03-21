# Data Sources

1. Directorio de Compañías

https://mercadodevalores.supercias.gob.ec/reportes/directorioCompanias.jsf

2. Indicadores de Seguridad Ciudadana (Ministerio de Gobierno)

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

5. Layoffs

https://www.datosabiertos.gob.ec/dataset/actas-de-finiquito-en-el-sistema-unico-de-trabajo

- Formally, total number of layoff notices (or job contract termination) registered in the SUT, the registration repository for all labour contracts in the country.

6. Social Security Employment Registry

https://www.ecuadorencifras.gob.ec/registro-empleo-seguridad-social/

- The Social Security Institute's (IESS, for its initials in Spanish) registry of labour contracts. All formal job contracts pass through here, since there is mandatory contribution to Social Security. 





