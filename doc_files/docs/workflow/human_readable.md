# Making Human Readable Visualizations

We want to ensure both researchers and practioners alike will understand the results of our post-hoc analysis. Because the output of the analysis is rather extensive, visualizing results past those seen previously can be a massive undertaking. Consequently, it is a challenge to make code for the figures easy to follow and to choose the proper results to visualize. Our goal for this walkthrough is to extend our work in the post-hoc analysis page by highlighting various results through different types of human-readable figures.

## Shiny Apps

R has the ability to run dynamic web-based apps, called Shiny apps. Shiny apps may be hosted on a server, or they may be simply ran on a local machine. The apps are great for allowing viewers to choose their own different options or formats to visualize the data you want to present. Obviously, the primary downside of Shiny apps is the issue of hosting. Nonetheless, we have a couple apps hosted through RStudio that serve to visualize different aspects of the post-hoc analysis.

The first Shiny app is one which allows users to choose several different hybrids and plot their Yield distributions via violin plots. There is also an option to split the hybrids into their respective groups in order to see how hybrids that are associated with more than one weather profile differ across post-hoc groups. We won't provide all the code to produce the app in this walkthrough because it is extensive and may be found at this [link](https://github.com/TACC/EnviroTyping/blob/Tutorial_Additions/sandbox/posthoc_group_analysis/2016/violin_app/app.R). If you desire to run the app on your local machine, you must download the whole folder entitled `violin_app` and ensure any directory references match those in your own R workspace. __*However, you may also see the app hosted online [here](https://bno5761.shinyapps.io/violin_app/)*__. 

The second Shiny app is one which provides a more in-depth look at the weather profiles of the respective groups. Users may choose any group and see how any number of the significant weather variables determine its weather profile. Further, users may select different scales for the *y*-axis and treat NAs as 0s in order to identify trends more clearly. The code for this app is much more extensive than the violins, so may find the code to run the app locally [here](https://github.com/TACC/EnviroTyping/blob/Tutorial_Additions/sandbox/posthoc_group_analysis/2016/profiles_app/app.R). Like with the violin plots app, the app to visualize the weather profiles is also online. __*It may be utilized at this [page](https://bno5761.shinyapps.io/profiles_app/)*__.

Of course, many more apps could be produced; and you are more than welcome to help in our effort.

## Maps

Recall that one of our main goals of EnviroTyping is to identify which weather variables contribute to the differences in GxE. Simply looking at results from an analysis of the post-hoc weather profiles (as in the aforementioned app) will go only so far because there is still room for human inference to decide the climate each profile reflects. We bridge the gap between the post-hoc results and human inference by plotting on a map the locations of the hybrids and looking for trends in clusters of hybrids that are associated with certain groups.

Describe getting lat and lon data (including the IA exps)
Show code for each step
Examples
Code

![Corn Experiments](../../img/BanksPlots/Figures/Corn_Experiments.png)
