# Green Lab (X_418158) - Evergreen Group
This is a part of project group work of the course Green Lab X_418158 at Vrije University Amsterdam in 2022. This repository contains the  replication package of group experiment including the material for replicating our experiment and data analysis.

## Topic
The cost of ads and analytics in mobile web apps (performance and energy consumption) 


## Team members
### Name, student ID, email
1. Arini Nur Rohmah, 2770542, a.n.rohmah@student.vu.nl
2. Mochamad Rizal Hidayat, 2689966, mochamadrizalhidayat@student.vu.nl
3. Rajitha Manellanga, 2770528, r.a.manellanga@student.vu.nl
4. Rakkan Tiasakul, 2769627, r.tiasakul@student.vu.nl
5. Soha Ali, 2770581, s.ali4@student.vu.nl

## Preparation

Downloading the source code of web apps locally is a prerequisite of our experiment. After selecting web apps to be the subjects in our experiment, we use Save All Resources Extension https://github.com/up209d/ResourcesSaverExt to download web apps’ resources and packages which contain ads and analytics related codes. Then we verify their index.html file to run on Live Server on Chrome successfully.

## Set Up

For setting up the experiment, we connect the Android device with a **Raspberry Pi** via a USB Type-C cable for controlling the device. The Raspberry Pi is also connected to the local network by an Ethernet cable. The device is mainly controlled through the **Android Runner**, which acts as a wrapper around Android SDK and Android Debug Bridge (adb)[Ivano]. We use Python scripts in the installed **Batterystats** plugin to execute and measure the energy consumption. 

In addition, we embed a snippet of Javascript code (https://github.com/S2-group/android-runner/blob/master/docs/run_stop_condition_http_post_tips.md) from the Android Runner package into each web app’s index.html file. It sends an HTTP POST request  to calculate the web ‘s Page Load Time (PLT) when the web page has fully loaded. With added PLT calculation snippet code, it allows us to measure the Page Load Time and energy consumption at the same time.

## The repository

The repository is forked from https://github.com/S2-group/android-runner and its original structure has been customized. We removed some folders which are not related to our experiment. Thus, this repository contains:
* web-apps resources in both with and without ads and analytic related code (direct to our [Google Drive](https://drive.google.com/drive/folders/1bfCNJWytrUwlmJFu_hQp_pRtOy_rnZdO?usp=sharing) because uploading more than 25MB to github is not allowed.) 
* source code of the configuration scripts for building the dataset
* raw data resulting from the execution of the experiment
* R scripts for data analysis

## How to run the experiment on performance and energy consumption measurement

All parameters required for measuring the performance and energy consumption are specified in the config_web.json file.  At the beginning of each run, the local Live Server is started. 

steps to run the experiment in raspberry Pi:

1. Make sure the android device is plugged into raspberry Pi by running the command,
     adb devices
     The output should return the device ID
3. Run the commands to invoke batterystats plugin:
      python3 android-runner android-runner/examples/batterystats/config_web.json 

## R Script
3rdTrial.csv
