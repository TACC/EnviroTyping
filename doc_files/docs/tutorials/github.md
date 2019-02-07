## Intro to GitHub

Because the files necessary to EnviroTyping are located in a repository, it will be increasingly helpful over time if you create your own account on [GitHub](https://github.com). This allows you to easily access not only the EnviroTyping datasets available at the time of this writing, but also any changes made over time. If you have used GitHub previously, you can skim (or simply skip) this tutorial and jump to the information you need. However, if you are new to the whole experience, following the below outline will provide you with a strong foundation to understand the tools GitHub provides.

## Creating the Account and Forking

This first step is straight-forward. Once you arrive on [GitHub](https://github.com), simply create an account by inputting a few identifying details, such as your email address. After your account is created, you will be taken to your personal page. Here you can see your repositories, other projects you follow, and personal details. Feel free to customize it or add a picture because this account is yours, and becoming active on GitHub is a great networking opportunity to become connected to the global research community. 

Now, direct your attention to the search bar on the top, left-hand side of the portal. In order to find the EnviroTyping repository, search for "TACC/EnviroTyping". There should be only one result, so click on its name. A quick glance at the directory may be overwhelming at first, but do not worry about navigating the GitHub just yet. Instead, look near the top of the portal for an icon that says "Fork". Click on it to have control over your own copy of the EnviroTyping repository. Verify the repository was forked by clicking on your profile icon on the top right-hand side of the portal, going to "Your Repositories", and checking that EnviroTyping is an option in the list.

This step is important because "forking" allows you to read and write the files in the master repository without fear of losing the original data. It also grants you the opportunity to contribute to the project by submitting "Pull" requests when you create (or edit) a file that helps further the project's goals. 

## Navigating Directories

<center>

|Directory|Description|Commonly Used?|
|---------|:---------:|:------------:|
|.ipynb_checkpoints|Holds one file|No|
|Premium @ 6750f8a|Collection of files for PReMiuM package|No|
|Simulated-data-for-performance-testing|Collection of files for simulated datasets|No|
|data|Central hub for data pertaining to each year's hybrids|Yes|
|doc_files|Collection of Markdown files for ReadtheDocs|Yes|
|Notes|Various notes|No|
|References|Collection of various articles for research|Yes|
|sandbox|Collection of data and files in current use|Yes|
|tools|Some benchmark funcitons|No|

</center>
## Setting EnviroTyping Project in RStudio

Because most of the code for EnviroTyping is written in R, you should consider creating a "Project" in RStudio that you utilize when working with EnviroTyping files. Projects allow you to consistently work in a specific working directory on your local machine, easily connect to GitHub repositories, and streamline your work. If you do not already have RStudio downloaded on your computer, go to RStudio's [website](https://www.rstudio.com/products/rstudio/download) and select the installer for your OS.

Creating the Project is easy to do, but you will first need the URL of your forked EnviroTyping repository. In GitHub, go to your EnviroTyping repository. Click on the green button that reads "Clone or Download" on the right-hand side of the page, then press the "Copy" button beside the URL that appears.

Next, in RStudio go to "Projects" on the top, right-hand side of the screen. Select "New Project", then "Version Control", then "Git". From here, paste the URL of your repository into the "Repository URL" field and type "EnviroTyping" as the project directory name. You may choose where you place the subdirectory; but because this process will download the GitHub repository to your local machine, you are encouraged to choose a parent directory where you may easily find the EnviroTyping files. Finally, you may also select whether or not to have RStudio open a new session when you work in the Project (most opt for the new session). 

Now, the Project setup in RStudio is complete! When you need to use R for EnviroTyping tasks, simply open RStudio, go to "Projects", and select "EnviroTyping". From there, you can choose to navigate the EnviroTyping files in the directory on the bottom, left-hand side of the screen (depending on your selected layout of RStudio); create new files; or do whatever else you need.

## Cloning Repository into Stampede

Ensure you are connected to secure Wi-Fi.
c
