# Portal BDOSS — Database of Social Health Organizations (Brazil)

**Author:** Henrique Bracarense  
**Institutional partnership:** Instituto Brasileiro de Organizações Sociais de Saúde (IBROSS)  
**Project:** Censitary Survey on Social Health Organizations in Brazil  
**Live app:** https://geesc.shinyapps.io/portal_bdoss/  
**Type:** R Shiny (shinydashboard)  
**Scope:** Mapping and analysis of Social Health Organizations (OSS) in the Brazilian public health system

---

## Overview

**Portal BDOSS** is an interactive R Shiny application designed to explore and visualize the **Base das Organizações Sociais de Saúde (BDOSS)** — a comprehensive census-style database describing contracts, geographic distribution, and institutional characteristics of **Social Health Organizations (OSS)** in Brazil.

The system was developed by **Henrique Bracarense** in partnership with the **Instituto Brasileiro de Organizações Sociais de Saúde (IBROSS)**.  
It provides a national overview of OSS participation in the **Unified Health System (SUS)**, enabling users to query, map, and analyze key features of service contracts, health facilities, and managing entities.

---

## Data and Components

- **`base_final.rds`** — core dataset containing all standardized OSS contracts and establishment information.  
- **Cartographic data (shapefiles)** for all Brazilian states (`ac.*`, `al.*`, `am.*`, …, `sp.*`, `df.*`, etc.), used for mapping via **Leaflet**.  
- **Static assets** in `/www/`:
  - Illustrations and header images (`Imagem1.jpeg`, `Imagem2.png`, …).  
  - `nota_metodologica.pdf` — methodological note describing data construction and classification rules.

---

## Functionalities

The application offers an integrated dashboard with the following main modules:

1. **Search Interface**
   - Dynamic **form inputs** (via `selectizeInput`) allow filtering by:
     - **Year** of contract start (`year_input`)
     - **State (UF)** and **Municipality**
     - **OSS name** (at state or municipal level)
     - **Facility type**
   - Supports incremental refinement (filters cascade as selections are made).
   - Implements **Google reCAPTCHA** verification for form submission security (custom `recaptchaUI` and `recaptcha` modules).

2. **Interactive Maps**
   - **Leaflet** maps render OSS-managed establishments by state and municipality.
   - Layers highlight the number and distribution of active OSS contracts across the country.
   - Shapefiles stored in `/www/` allow nationwide visualization without external dependencies.

3. **Contract Analytics**
   - Aggregates data by OSS, state, or municipality.
   - Displays metrics such as:
     - Number of active contracts
     - Type of health facility (e.g., hospital, UPA, CAPS)
     - Start year of management
   - Outputs can be viewed in tables or charts.

4. **Data Export**
   - Users can **download results** (via `xlsx` export) for offline analysis.

5. **Security and Validation**
   - `mailR` and `httr` used for form submission and backend communication.
   - reCAPTCHA validation handled directly through the Google API (`siteverify` endpoint).

---

## Key Packages

| Purpose | Packages |
|----------|-----------|
| Web framework | `shiny`, `shinydashboard`, `shinyjs`, `shinyalert`, `shinycssloaders` |
| Forms and validation | `shinyWidgets`, `mailR`, `httr`, `jsonlite` |
| Data processing | `dplyr`, `tidyverse`, `DT`, `xlsx` |
| Mapping | `leaflet`, `tmap`, `cartography`, `spdep`, `maptools`, `rgdal` |
| Visualization | `plotly`, `RColorBrewer` |

---

## File Structure

```
portal_bdoss_zip/
├─ app.R                 # Main Shiny application (UI + server)
├─ base_final.rds        # Core database of OSS contracts and facilities
└─ www/                  # Static and geospatial assets
   ├─ Imagem1.jpeg, Imagem2.png, ...
   ├─ nota_metodologica.pdf
   ├─ <UF>.{shp,shx,dbf,prj} (state shapefiles)
   └─ br.{shp,shx,dbf,prj} (national boundary)
```

---

## How to Run Locally

1. Install **R ≥ 4.1** and the following packages:

```r
install.packages(c(
  "shiny", "shinydashboard", "shinyjs", "shinyalert", "shinycssloaders",
  "shinyWidgets", "dplyr", "tidyverse", "DT", "xlsx",
  "leaflet", "tmap", "cartography", "spdep", "maptools", "rgdal",
  "plotly", "RColorBrewer", "mailR", "httr", "jsonlite"
))
```

2. Place all files in a single directory and ensure that the `www/` folder contains the shapefiles and images.

3. Launch from RStudio or an R console:

```r
shiny::runApp('app.R')
```

4. (Optional) Set Google reCAPTCHA keys in your environment:

```r
Sys.setenv(recaptcha_sitekey = "your_site_key", recaptcha_secret = "your_secret_key")
```

---

## Notes

- The application is **bilingual (Portuguese labels)** for the public version.  
- The dataset `base_final.rds` should remain in the app’s root directory for correct loading.  
- For performance, shapefiles are preloaded and stored locally within the package.

---

## Citation

If you use this app or reproduce its data, please cite:

> **Bracarense, Henrique** (2025). *Portal BDOSS — Database of Social Health Organizations in Brazil.*  
> R Shiny application developed in partnership with the Instituto Brasileiro de Organizações Sociais de Saúde (IBROSS).  
> URL: https://geesc.shinyapps.io/portal_bdoss/

---

## License

Unless otherwise specified by IBROSS, this code is released under an **MIT-style license** for academic and non-commercial use.  
The `nota_metodologica.pdf` document should be cited when referencing the methodological framework of the BDOSS.
