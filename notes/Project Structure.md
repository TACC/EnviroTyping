### Overview for the current project structure

The project tree:

```
├── Premium    # This is a git submodule directory. It natively links to Silvias PReMiuM-R-package repo.
│   ├── PReMiuM
│   │   ├── ChangeLog
│   │   ├── DESCRIPTION
│   │   ├── NAMESPACE
│   │   ├── R
│   │   ├── inst
│   │   ├── man
│   │   ├── src
│   │   └── tests
│   │       ├── testthat
│   ├── PReMiuM-Ex.pdf
│   ├── PReMiuM-Ex.timings
│   ├── PReMiuM.Rcheck
│   │   ├── PReMiuM-Ex.R
│   │   ├── PReMiuM-Ex.Rout
│   │   ├── PReMiuM-manual.log
│   │   ├── PReMiuM-manual.pdf
│   │   ├── Rdlatex.log
│   │   └── tests
│   ├── PReMiuM_3.1.6.tar.gz
│   ├── PReMiuM_3.1.7.tar.gz
│   ├── manual
│   ├── tests
│   └── to-do-before-CRAN-submission
├── README.md   #   Currently borrowed from EnviroTyping abstract
├── data    #   any data and descripts
│   ├── external    #   outside sources such as the G2F data
│   │   └── G2F\ from\ Cyverse\ DataStore
│   │       ├── _g2f_2015_hybrid_data_description.txt
│   │       ├── _g2f_2015_inbred_data_description.txt
│   │       ├── _g2f_2015_weather_data_description.txt
│   │       ├── _readme.txt
│   │       ├── g2f_2015_hybrid_data_no_outliers.csv
│   │       ├── g2f_2015_inbred_raw_data.csv
│   │       └── g2f_2015_weather_clean.csv
│   ├── interim #   data that we have generated in our analysis
│   │   ├── G2F_Hybrid
│   │   │   └── hybridXmonth.csv
│   │   ├── G2F_Inbred
│   │   │   └── inbredXmonth.csv
│   │   └── G2F_Weather
│   │       ├── g2f_2014_weather_clean.csv
│   │       ├── monthly_weather.json
│   │       ├── monthly_weather_summary.csv
│   │       │   ├── _SUCCESS
│   │       │   └── part-00000-8a6a06f9-8194-499f-9c36-42d5ddaeb204.csv
│   │       └── weather.scala
│   └── processed   #   this would be final output ready to present
├── docs    #   This file structure was created by ReadtheDocs for anyone not familiar
│   ├── Makefile
│   ├── _build
│   ├── _static
│   ├── _templates
│   ├── conf.py
│   ├── index.rst
│   └── make.bat
├── notes   #   General notes and questions that otherwise do not fit another category
│   ├── Project\ Structure.md
│   └── dataWrangling.md
├── references  #   List of any supportive documents (ie technical or scholary articles)
│   ├── 2017\ USDA\ Project\ Directors\ meeting\ abstract.docx
│   ├── Carter\ et\ al.\ -\ 2016\ -\ Separating\ heat\ stress\ from\ moisture\ stress\ analy.pdf
│   ├── Chenu\ et\ al.\ -\ 2017\ -\ Contribution\ of\ Crop\ Models\ to\ Adaptation\ in\ Wheat.pdf
│   ├── Chenu_2015_Chapter\ 13\ -\ Characterizing\ the\ crop\ environment\ �\200\223\ nature,\ significance\ and.pdf
│   ├── Jiang\ et\ al_2016_Characterizing\ Predictability\ of\ Precipitation\ Means\ and\ Extremes\ over\ the.pdf
│   ├── Liverani\ and\ Smith\ -\ 2016\ -\ Bayesian\ selection\ of\ graphical\ regulatory\ models.pdf
│   ├── Liverani\ et\ al.\ -\ Modelling\ collinear\ and\ spatially\ correlated\ data.pdf
│   ├── PReMiuM\ article\ in\ Journal\ of\ Statistical\ Software.pdf
│   ├── Papathomas\ and\ Richardson\ -\ 2016\ -\ Exploring\ dependence\ between\ categorical\ variables.pdf
│   ├── Remes\ et\ al.\ -\ 2017\ -\ Latent\ Correlation\ Gaussian\ Processes.pdf
│   ├── Rincent\ et\ al_2017_Optimization\ of\ multi-environment\ trials\ for\ genomic\ selection\ based\ on\ crop.pdf
│   ├── Stapleton\ envirotyping\ talk\ UNCWFall2016.pdf
│   ├── Statistical\ Weather-Impact\ Models.pdf
│   ├── USDA\ NAPB\ 2017\ poster.pptx
│   ├── Wang\ et\ al.\ -\ 2015\ -\ Complete\ Effect-Profile\ Assessment\ in\ Association\ .pdf
│   ├── bayes\ and\ mixture\ models.pdf
│   └── bayesian\ prof\ regression.pdf
├── scripts #   All the code used to create interim and processed data
│   ├── Rfiles
│   │   └── G2F_Monthly_subset_min:max
│   │       ├── hybridXmonth.R
│   │       ├── inbredXmonth.R
│   │       └── weatherXmonth.R
│   └── shellFiles
└── tools   #   Tools used for optimization; could be a good location for all the TACC testing
    └── Austins\ Benchmark\ Tool.R
```
### Questions?

*   Do we need a TravisCI?
    *   Does this only belong in the Premium directory?
*   Are we interested in using git flow?
    *   This will allow us to automate the process of merging anything from the developing branch into the master
*   How should we track our data?
    *   https://github.com/End-to-end-provenance/git End-to-end-provenance.github.io
    *   https://github.com/End-to-end-provenance/RDataTracker/wiki/About-Data-Provenance 
