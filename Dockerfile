# Reference image
FROM python:3.12

# Update packages
RUN apt-get update && apt-get upgrade -y

# Install required libraries
RUN apt-get update && apt-get install -y \
    build-essential \
    cargo \
    cmake \
    git \
    protobuf-compiler \
    libprotobuf-dev \
    libprotoc-dev \
    libgdal-dev \
    libudunits2-dev \
    # libgsl-dev \
    # libfontconfig1-dev \
    # libharfbuzz-dev \
    # libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R and R-development packages
RUN apt-get update && apt-get install -y \
    r-base \
    r-base-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY requirements.R .
RUN Rscript requirements.R

# Install Python packages
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

# Download and install (R)spatialCMC
RUN git clone https://github.com/TeoGiane/spatialCMC.git
RUN cd spatialCMC/R && ./install_rspatialcmc.sh