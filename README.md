Time Series Portfolio

**Natalie Chidikwindi**  
STA4010W · University of Cape Town · 2026

This repository contains my time series learning portfolio, built as a Quarto website. It brings together four substantial projects, worked tutorials, a learning journal and a reflective commentary.

The portfolio explores how statistical theory, computation and interpretation contribute to modelling and forecasting time series.

## Projects

| Project | Focus | Materials |
|---|---|---|
| **1. Forecasting Influenza Cases in Australia** | Comparing SARIMA and exponential smoothing forecasts using Australian influenza notification data. | [Summary](projects/project-01/index.qmd) · [Analysis and R code](projects/project-01/code.qmd) |
| **2. GARCH Modelling of Financial Data** | Modelling FTSE/JSE Top 40 volatility through first-principles implementation, model comparison and simulation. | [Summary](projects/project-02/index.qmd) · [Analysis and R code](<projects/project-02/Project 2.qmd>) |
| **3. Crime and Economic Indicators in South Africa** | Comparing ARIMA and ARIMAX models to investigate associations and forecast recorded crime. | [Summary](projects/project-03/index.qmd) · [Analysis and R code](<projects/project-03/Project 3.qmd>) |
| **4. State Estimation Under Uncertainty** | Implementing the Kalman filter and investigating uncertainty, structural change and missing observations through simulation. | [Summary](projects/project-04/index.qmd) · [Analysis and R code](<projects/project-04/Project 4.qmd>) |

## Other portfolio components

- [**Section 1**](section-1/section-1.qmd): time series exploration, forecasting, ARIMA models, ARCH/GARCH, spectral analysis and structural breaks.
- [**Section 2**](section-2/section-2.qmd): exponential smoothing, time series regression, state space models, Kalman filtering and Hidden Markov models.
- [**Learning journal**](journal/index.qmd): a chronological record of my learning, challenges and next steps.
- [**Reflective commentary**](reflective-commentary.qmd): reflection on my development across the course.
- [**Code and data appendix**](appendix/code-and-data.qmd): supporting materials and data access information.
- [**AI use log**](ai-use-log.qmd): documentation of substantive AI assistance and checks.

These links open the source documents in the repository. The rendered website presents the formatted pages, figures and results.

## Repository structure

```text
Time-Series-Portfolio/
├── _quarto.yml
├── index.qmd
├── styles.css
├── journal/
├── projects/
│   ├── project-01/
│   ├── project-02/
│   ├── project-03/
│   └── project-04/
├── section-1/
├── section-2/
├── appendix/
├── reflective-commentary.qmd
├── ai-use-log.qmd
└── _site/                     # Local rendered website
```

## Run the portfolio locally

You will need **R**, **Quarto**, and the R packages used in the analyses. RStudio is optional.

1. Clone or download the repository.
2. Open the project in RStudio, or open a terminal in the repository folder.
3. Install the packages listed in the analyses' setup chunks.
4. Keep the supplied data and supporting files in their existing folders.
5. Render the complete website:

   ```bash
   quarto render
   ```

The rendered output is saved in `_site/`. Open `_site/index.html` to view the built website.

For a local preview, run:

```bash
quarto preview
```

Rendering executes the R code, including model fitting and simulation studies, so some pages may take time to complete. Analyses that download external data require an internet connection.

## Publish to GitHub Pages

From the project folder, run:

```bash
quarto publish gh-pages
```

This publishes the rendered site to the `gh-pages` branch while retaining `_site` as the local output folder.

In the repository's **Settings → Pages**, confirm:

- **Source:** Deploy from a branch
- **Branch:** `gh-pages`
- **Folder:** `/ (root)`

The published website address is shown in the Pages settings after deployment. Repeat the publish command when updating the website.

## Data and reproducibility

The projects use NNDSS influenza notifications, financial index data, South African crime and economic indicators, and simulated state space data.

Data sources, preparation steps, modelling assumptions and limitations are described within the analyses and the [appendix](appendix/code-and-data.qmd). External datasets may change after download; simulation results depend on the specified seeds and settings.

## Academic context

This portfolio was prepared as individual coursework for STA4010W. Sources and AI assistance are acknowledged within the portfolio. The analyses are presented for academic learning and assessment.

