# mathesis-depop

This repository contains all of the code, text and files needed to reproduce my master's thesis in its entirety.
It also contains parts of the data needed for the calculation of the econometric estimates.
More precisely, the data delivered with this repository are the historical population data linked to shape files, allowing for its spatial interpretation.
It is stored in the Excel file `LAU2_REFERENCE_DATES_POPL.xlsx` the folder `data`, as it cannot be easily attained via an API and was still small enough to be stored with Git.
The remainder of the data (the shape files and the weather grids) were way too big in size to be handled by Git, but fortunately are easy to attain via publicly accessible APIs of the data providers.
Access to these APIs and the correct format and scope of the data are hard-coded into the data preparation files `prep-pop-data.R` and `prep-weather-data.R`.
The latter data can only be downloaded with a user account

## Prerequisites for reproduction of the thesis on another machine

I am an advocate for reproducibility, not only but also in scientific research.
Therefore, I have put in some effort trying to ensure that the calculations run and their output renders successfully on any other machine.
But this is only my hope and reproducibility is also a community issue -- one might never be able to account fully for the vast variety of machines and configurations.
Hence, if you ever try reproducing my work and it fails, please feel free to [contact me](mailto:jakob@jakobarendt.com). -I will try to help making it work and, this way, I can also learn about new computing environments and improve the reproducibility of my work.

### Checklist before running the computations on your machine

Please ensure all of the conditions are met before running these thesis calculations on your machine:

-   [ ] Account on CDS

!! If you want to render this Quarto book project yourself, render the book in its entirety and **not** individual chapters.
There are dependencies in the R code among the different chapters, and it might not calculate correctly if executed in the wrong order.
