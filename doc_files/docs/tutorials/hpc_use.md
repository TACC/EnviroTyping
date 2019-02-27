## The Batch Script and Jobs

This section of the tutorial will assume some basic knowledge of how to log into the HPC via `ssh` and to navigate directories via Command Line or Terminal. Feel free to go to the last section of the GitHub tutorial for a brief refresher of the `ssh` log-in process. Likewise, we will briefly discuss how to navigate directories in this tutorial, but more information is given in the "Reminders and Useful Commands" tutorial. 

### Batch Script

Unlike a personal computer, the HPC will not simply run an R or Python script. The high performance computer must be "told" what to do. This is because endusers have the ability to request how much and what type of resources are utilized when a job is submitted through the system. For example, you will have one option to choose how many cores the HPC uses to process your request; and yet another option is to have the system email you when a job is submitted or completed. The batch script is what communicates your requests to the HPC. You may find an example batch script below:


`#!/bin/bash` (Never change this line) <br/>
`#` <br/>
`#` <br/>
`#SBATCH -J job_name` <br/>
`#SBATCH -o job_name.%j.out` <br/>
`#SBATCH -N number_of_nodes` <br/>
`#SBATCH -n total_number_of_MPI_tasks` <br/>
`#SBATCH -p process_queues` <br/>
`#SBATCH -t max_time` <br/>
`#SBATCH -A Envriotype` <br/>
`#SBATCH --mail-user=<email@host.com>` <br/>
`#SBATCH --mail-type=all` <br/>
`#------------------------------------------------------` <br/>
`mkdir -p output` <br/>
`Rscript --verbose ./script_to_be_run.R > ./output.Rout` <br/>

Notice, however, you must change a few generic idenfiers with your own specifics. For example, "job_name" should be replaced with a name which is less than 8 characters long that will help you identify it in the queue. Likewise, the expressions "number_of_nodes" (N) and "total_number_of_MPI_tasks" (n) must both be replaced with at least 1. The value "max_time" must be in the format hh:mm:ss, where hh is the number of hours, mm is the number of minutes, and ss is the number of seconds you think are needed for your job to complete. The expression "<email@host.com>" should be replaced with your preferred email address (without the arrows on the ends), and "script_to_be_run.R" should be replaced with the name of a file in the same folder as the batch script you wish the HPC to run. The only line which requires much thought is the one where you choose how to replace "process_queues". The typical selection is to replace it with "normal"; but you may also choose to select queues like "short", "long", or "development". There are several other options in addition to these, but know that the process queue you choose will determine when your job begins, how long it may run before being automatically terminated, and the number of compute resources available to you. 

If you wish to know more about batch scripts, there are plenty of other tutorials online. One great tutorial is provided by Cornell University's Center for Advanced Computing. This [tutorial](https://www.cac.cornell.edu/education/training/StampedeJan2017/Envi.pdf) provides a good introduction to how Stampede2 operates and gives several great examples of batch scripts and other useful SLURM codes.

### Job Submittal Process

## Stampede Rules

## RStudio Interface on HPC
