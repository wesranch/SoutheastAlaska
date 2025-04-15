#!/bin/bash

# private ssh in (you may need to cp your .pub key onto JS2 web terminal)
export SSH_KEYS_DIR="$HOME/.ssh/id_rsa"

read -p "Enter ACCESS username: " access_id
read -p "Enter Jetstream VM Public IP: " pub_ip

# secure copy sh scripts to vm // replace w full paths
scp -i "$SSH_KEYS_DIR" setup-jetstream.sh "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" setup-container.sh "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" s3-upload.sh "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" climate.sh "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" gdown.sh "$access_id@$pub_ip:~/"


# python scripts
#scp -i "$SSH_KEYS_DIR" ../scripts/run-landis.py "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" ../scripts/climate-data-processing.Rmd "$access_id@$pub_ip:~/"
scp -i "$SSH_KEYS_DIR" ../scripts/mosaic_rasters.R "$access_id@$pub_ip:~/"

#gdrive account export wesley.rancher.2023@owu.edu
#scp -i "$SSH_KEYS_DIR" gdrive_export-wesley_rancher_2023_owu_edu.tar "$access_id@$pub_ip:~/"

#single cell
#scp -i "$SSH_KEYS_DIR" -r /Users/wancher/documents/thesis/data/biomass-data "$access_id@$pub_ip:~/"

#dockerfile
#scp -i "$SSH_KEYS_DIR" -r /Users/wancher/documents/thesis/docker "$access_id@$pub_ip:~/"

#connect and make scripts executable
ssh -i "$SSH_KEYS_DIR" "$access_id@$pub_ip"
chmod +x setup-jetstream.sh
chmod +x setup-container.sh
chmod +x s3-upload.sh
chmod +x climate.sh
chmod +x gdown.sh
