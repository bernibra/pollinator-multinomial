# pollinator-multinomial [![](https://badgen.net/badge/DOI/10.1038%2Fs41467-020-17894-y/red)](https://doi.org/10.1038/s41467-020-17894-y)

Multinomial logistic regression for the analysis of plant-pollinator interaction dynamics.

## How to cite
[Bramon Mora, B., Shin, E., CaraDonna, P.J. et al. Untangling the seasonal dynamics of plant-pollinator communities. Nat Commun 11, 4086 (2020)](https://www.nature.com/articles/s41467-020-17894-y)

## Structure
There are two folders inside `./code`: 'empirical' and 'test'. As the names indicate, the former contains the models used in the paper while the latter contains a test for the approach (simplified versions that I used to check the overall model structure used). The code is not particularly clean and documented, but I don't think it's impossible to follow (except for the terrible parameter naming choice... what were you thinking Bernat?). The models are slow to run, and there are probably faster implementations (e.g. `brms` implementation of the multinomial regression is pretty sweet).

## Data
I added the processed data as an rds file. It is worth pointing out that this data is only designed for the model runs and do not provide any context or data-processing steps. For example, in the paper we say: "We also treated species phenologies as uninterrupted; therefore, we considered any observation of a species transition from any state $y_i$ (i.e. species in the network) to $y_{pre}$ (i.e. species not yet in the network) and back to any state $y_j$ during its activity period to be the result of a likely sampling error. Note that we ignored any of such observations". This is important because you might observe gaps in the time series for some species. Those gaps are those "phenology interruptions" that we considered as sampling error. 
