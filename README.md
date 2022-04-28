# Mak_h_ro_0 repository - Readme #

This repository contains all models and SESSL-experiments from the paper:
CITATION

Software requirements:

* Java Runtime Environment 8 or newer (e.g., from https://adoptopenjdk.net/).
* R, with the packages "dplyr", "readr" , "colorspace", "gridExtra", "lubridate", "ggloop", "rlang" and "ggplot2".
* You do not need to install any additional software, as all required artifacts are downloaded automatically.

The repository contains the `Mak_h_ro_0.ml3` model in ML3, the corresponding experiment (`Mak_h_ro_0.scala`) [1], and the related supplement. The supplement presents the model parameters and main variables, the link between the model description [1] and ML3/SESSL code, and information about the validation replication.
The supplied experiment can be executed with
* `./mvnw scala:run` or the `run.sh` script on Unix
* `mvnw.cmd scala:run` or the `run.bat` script on Windows
By default, the package is set up to execute `Mak_h_ro_0.scala`. To execute another experiment, you need to change line 53 in `pom.xml`.

To customize the experiment, you can edit three files:
* Change the SESSL version to use in the pom.xml
* Change the log level (i.e., the verbosity of the console output) in the pom.xml
* Edit the simulation model in the *.ml3 file or create a new one
* Edit the simulation experiment Specification in the *.scala file or create a new one

You can rename and replace the *.ml3 and *.scala files. However, make sure that
* the experiment object in the *.scala file is correctly referenced (including the package path) in the pom.xml
* the *.ml3 model file is correctly referenced in the *.scala file

The experiment will produce an output folder results-timestamp. It contains a folder "config-0" (for the one parameter configuration in the experiment), which contains a file "config.csv" with the parameter values and one csv file with the observed data for each run.

To plot the simulation outcome you can use the R-script `plot_single_run.ml3`. 
To plot several simulation outcomes (e.g., several replication or parameter scans) you can use the R-script `plot_all_folders.ml3`. Depending how many parameters should be considered, lines 38 ff. and line 56 have to be adapted.
Make sure to adapt the working directory and the path to the simulation outcome.

# Further Reading about ML3

The Modeling Language for Linked Lives (ML3) is a domain-specific modeling language for agent based models in the social sciences. An informal description of the language can be found in [2]. A formal definition of the language and its semantics can be found in [3]. The models provided with [4] may be of use as further examples. Please note that there have been slight changes to the syntax over the years, so please compare with most recent version in this repository or in [3] if there are any issues.

# Further Reading and Documentation about SESSL

Simulation Experiment Specification via a Scala Layer (SESSL) is a domain-specific language for the specification of simulation experiments [5]. The SESSL-ML3 binding allows for the use of SESSL together with ML3 [4]. For documentation (including a User Guide and a Developer Guide), see [sessl.org](http://sessl.org/).

# References
[1] CITATION

[2] Tom Warnke, Oliver Reinhardt, Anna Klabunde, Frans Willekens, Adelinde M. Uhrmacher. "Modelling and simulating decision processes of linked lives: An approach based on concurrent processes and stochastic race" In: Population Studies, 71 (sup1) 69-83, 2017.

[3] Oliver Reinhardt, Tom Warnke, Adelinde M. Uhrmacher. "A Language for Agent-Based Discrete-Event Modeling and Simulation of Linked Lives" In: ACM Transactions on Modeling and Computer Simulation, Volume 32, Issue 1, January 2022, Article No.: 6, pp 1–26, https://doi.org/10.1145/3486634

[4] Oliver Reinhardt, Jason Hilton, Tom Warnke, Jabuk Bijak, Adelinde M. Uhrmacher: "Streamlining Simulation Experiments with Agent-Based Models in Demography" In: Journal of Artificial Societies and Social Simulation, 21 (3) 9, 2018.

[5] Roland Ewald and Adelinde M. Uhrmacher. "SESSL: A Domain-Specific Language for Simulation Experiments". In: ACM Transactions on Modeling and Computer Simulation 24(2), 2014.
