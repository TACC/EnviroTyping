# Use Singularity on Stampede2 to Launch IDOLS for a PReMiuM Demo

Singularity is a free, cross-platform and open-source computer program that performs operating-system-level virtualization also known as containerization. One of the main uses of Singularity is to bring containers and reproducibility to scientific computing and the high-performance computing world

This tutorial is on pulling a docker image with all dependencies for PReMiuM using Singulary and seting up the IDOLS web application for the demo workflow using a synthetic dataset.

## Purpose
Installing the dependencies for PReMiuM R package on HPC is complex, so a pre-built docker image can reduce the burden and  resolve the compiling issues on HPC for the R package.  


## Instructions

On Stampede2 at TACC

``` 
$ module load singularity
$ cdw
$ git clone https://github.com/ruizhuhuang/premium_demo
$ git clone https://github.com/ruizhuhuang/idols
$ singularity run  docker://ruizhuhuang/r_premium:latest
$ .  /home/tacc/.bashrc
$ cd $WORK/idols
$ sbt run
```

Do port forwarding from compute node to login node
```
$ ssh -f -g -N -R 58889:127.0.0.1:9000 login1
```

Open new terminal
```
$ ssh -L 8889:localhost:58889 login1.stampede2.tacc.utexas.edu
```

Open web browser, type
```
localhost:8889
```

