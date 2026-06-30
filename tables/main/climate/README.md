# Figure 7 climate source

`fig_discussion_climate_ngrip_d18o_50_29ka.csv` is the 50-29 ka subset used for the NGRIP/GICC05 climate panel.

The source dataset is:

NGRIP dating group (2008), *Greenland Ice Core Chronology 2005 (GICC05), 60,000 year, 20 year resolution*, IGBP PAGES/World Data Center for Paleoclimatology Data Contribution Series 2008-034, NOAA/NCDC Paleoclimatology Program.

Dataset page and DOI:

- https://www.ncei.noaa.gov/access/paleo-search/study/6086
- https://doi.org/10.25921/pkqb-2w42

Greenland Stadial and Interstadial boundaries are encoded in `R/04_reproduce_discussion_figure.R` from Rasmussen et al. (2014, Table 2). Ages in ka b2k are converted to ka BP by subtracting 0.05 ka. The HS5, HS4 and HS3 bands use the Greenland stadial envelopes GS-13, GS-9 and GS-5.1, respectively; they are not independent estimates of Heinrich-event duration.
