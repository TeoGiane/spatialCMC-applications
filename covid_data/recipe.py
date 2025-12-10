import os
import sys

from cook import create_task, Task
from cook.contexts import create_group

# Add parent directory to path to import shared functions
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from utils import create_output_path
from utils import download_and_extract

# Unique name labelling for current subfolder
def name(root_name: str) -> str:
     return f"cd:{root_name}"

# Get directory of the current file
workdir = os.path.dirname(os.path.abspath(__file__))

# Specify download URLs
urls = ["https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/Limiti01012020.zip",
        "https://www.istat.it/wp-content/uploads/2020/03/Dataset-decessi-comunali-giornalieri-e-tracciato-record_5marzo.zip",
        "https://demo.istat.it/data/posas/POSAS_2021_it_Comuni.zip",
        "https://www1.finanze.gov.it/finanze/analisi_stat/public/v_4_0_0/contenuti/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.zip"]

# Define download_data task
def download_data(_ : Task) -> None:
    for url in urls:
        download_and_extract(url, os.path.join(workdir, "raw"))
download_data_targets = [os.path.join(workdir, "raw", 'comuni_giornaliero_31dicembre.csv'),
                         os.path.join(workdir, "raw", 'POSAS_2021_it_Comuni.csv'),
                         os.path.join(workdir, "raw", 'Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv'),
                         os.path.join(workdir, "raw", 'Limiti01012020', 'Reg01012020', 'Reg01012020_WGS84.shp'),
                         os.path.join(workdir, "raw", 'Limiti01012020', 'ProvCM01012020', 'ProvCM01012020_WGS84.shp'),
                         os.path.join(workdir, "raw", 'Limiti01012020', 'Com01012020', 'Com01012020_WGS84.shp')]
create_task(name("download_data"), action = download_data, targets = download_data_targets)

# Define generate data task
generate_shapefile_action = ['Rscript', os.path.join(workdir, 'src/generate_shapefiles.R')] + \
    ['--raw-data-dir', 'raw'] + \
    ['--dest-dir', 'input']
generate_shapefile_task_deps = [name("download_data")]
generate_shapefile_targets = [os.path.join(workdir, 'input', 'covid_data_clean', 'covid_data_clean.shp')]
create_task(name("generate_shapefiles"), action = generate_shapefile_action,
            task_dependencies = generate_shapefile_task_deps, targets = generate_shapefile_targets)

# Define create_run_sampler_task function
def create_run_sampler_task(algo_type: str, hier_prior: str, mix_prior: str) -> Task:
    filename = "mcmc_chain" if algo_type == "MCMC" else "cmc_chain"
    output_path = create_output_path(hier_prior, mix_prior)
    output_file = f"output/{output_path}/{filename}.dat"
    run_sampler_action = ["Rscript", os.path.join(workdir,"src/run_sampler.R")] + \
        ["--input-file", f"input/covid_data_clean/covid_data_clean.shp"] + \
        ["--hier-prior", hier_prior] + \
        ["--mix-prior", mix_prior] + \
        ["--algo-params", algo_params] + \
        ["--output-file", output_file]
    run_sampler_task_dependencies = [name("generate_shapefiles")]
    run_sampler_targets = [os.path.join(workdir, output_file)]
    if algo_type == "MCMC":
        run_sampler_action += ["--use-mcmc"]
    return create_task(f"_{name(f"run-{output_path}-{filename}")}", action=run_sampler_action,
                       task_dependencies=run_sampler_task_dependencies, targets=run_sampler_targets)

# Define run_sampler task group
algo_params = 'algo_id: "Neal2" rng_seed: 10092022 iterations: 40 burnin: 10 init_num_clusters: 5'
algo_types = ["MCMC", "CMC"]
hier_priors = ["fixed_values { shape: 250 rate: 50 }"]
mix_priors = ["fixed_value { totalmass: 1.0 lambda: 0.35 }"]
# mix_priors = ["fixed_value { totalmass: 0.001 lambda: 0.35 }", "fixed_value { totalmass: 0.01 lambda: 0.35 }",
#               "fixed_value { totalmass: 0.1 lambda: 0.35 }", "fixed_value { totalmass: 1.0 lambda: 0.35 }"]
with create_group(name("run")):
    for algo_type in algo_types:
        for hier_prior in hier_priors:
            for mix_prior in mix_priors:
                    create_run_sampler_task(algo_type, hier_prior, mix_prior)


# Define create_generate_plots_task function
def create_generate_plots_task(algo_type: str, hier_prior: str, mix_prior: str) -> Task:
    filename = "mcmc_chain" if algo_type == "MCMC" else "cmc_chain"
    sim_path = create_output_path(hier_prior, mix_prior)
    sim_file = f"output/{sim_path}/{filename}.dat"
    generate_plots_action = ["Rscript", os.path.join(workdir,"src/generate_plots.R")] + \
        ["--data-file", 'input/covid_data_clean/covid_data_clean.shp'] + \
        ["--shard-geom-file", 'input/regions/regions.shp'] + \
        ["--sim-file", sim_file] + \
        ["--output-dir", f"plots/{sim_path}/{algo_type.lower()}"]
    generate_plots_task_dependencies = [f"_{name(f"run-{sim_path}-{filename}")}"]
    return create_task(f"_{name(f"generate-plots-{sim_path}-{filename}")}", action=generate_plots_action,
                       task_dependencies=generate_plots_task_dependencies)

# Define generate_plots task group
with create_group(name("generate_plots")):
    for algo_type in algo_types:
        for hier_prior in hier_priors:
            for mix_prior in mix_priors:
                create_generate_plots_task(algo_type, hier_prior, mix_prior)