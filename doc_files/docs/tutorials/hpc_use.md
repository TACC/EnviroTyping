## The Batch Script and Jobs

This section of the tutorial will assume some basic knowledge of how to log into the HPC via `ssh` and to navigate directories via Command Line or Terminal. Feel free to go to the last section of the GitHub tutorial for a brief refresher of the `ssh` log-in process. Likewise, we will briefly discuss how to navigate directories in this tutorial, but more information is given in the "Reminders and Useful Commands" tutorial. 

### Batch Script

Unlike a personal computer, the HPC will not simply run an R or Python script. The high performance computer must be "told" what to do. This is because endusers have the ability to request how much of and what type of resources are utilized when a job is submitted through the system. For example, you will have one option to choose how many cores the HPC uses to process your request; and yet another option is to have the system email you when a job is submitted or completed. The batch script is what communicates your requests to the HPC.

`
#!/bin/bash <br/>
#<br/>
#<br/>
#SBATCH -J min163k <br/>
#SBATCH -N 1 <br/>
#SBATCH -n 1 <br/>
#SBATCH --mail-user=<email@host.com> <br/>
#SBATCH --mail-type=all <br/>
#SBATCH -p skx-normal <br/>
#SBATCH -t 15:00:00 <br/>
#SBATCH -A Envriotype <br/>
#SBATCH -o job_%j_%N.out <br/>
#------------------------------------------------------ <br/>
mkdir -p output2 <br/>
Rscript --verbose ./prem_workflow2.R > ./output2.Rout <br/>
`

### Job Submittal Process

## Stampede Rules

## RStudio Interface on HPC
