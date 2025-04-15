#!/bin/bash

# run a container for the docker image
#docker run --platform linux/amd64 -it --entrypoint /bin/bash -p 7681:7681 -v /Users/wancher/Documents/thesis/data/landis_input:/full_input wesranch/landis_v8:latest
#sudo docker run --platform linux/amd64 -it --entrypoint /bin/bash -p 7681:7681 -v /home/$USER/landis/input:/full_input wesranch/landis_v8:latest
docker run --platform linux/amd64 --mount type=bind,src=/Users/wancher/Documents/thesis/data/landis_input/,dst=/full_input wesranch/landis_v8:latest /bin/sh -c "cd /full_input && ./historic-ncar.sh" 
# output container name
#container_name=$(sudo docker container ls --format '{{.Names}}' | tail -n 1)
#echo "Container started: $container_name"
#access username
#read -p "access ID/username?:" access_id
#export access_id

#install python boto3 for the python script
#pip3 install boto3
#python3 ~landis/input/run-landis.py \
#    --container_name "$container_name" \
#	--access_id "$access_id"