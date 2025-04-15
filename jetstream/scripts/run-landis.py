#libraries
import os
import subprocess
from concurrent.futures import ThreadPoolExecutor
import argparse

# function to run landis batch files in the docker container
def run_batch_file_in_container(batch_file, mount_path, container_name, local_output_folder):
    try:
        #run (this needs changed)
        command = f'sudo docker exec -it -v {local_output_folder}:/output {container_name} bash -c "{mount_path}/{batch_file}"'
        subprocess.run(command, shell=True, check=True)
        print(f"Completed {batch_file}.")
        return True
    except subprocess.CalledProcessError as e:
        print(f"Error: {e}")
        return False

# function to process a replicate and control output
def process_replicate(rep_id, batch_files, mount_path, output_folder, container_name):
    local_output_folder = os.path.join(output_folder, f"rep{rep_id}")
    os.makedirs(local_output_folder, exist_ok=True)

    for scenario_name, batch_file in batch_files:
        scenario = os.path.splitext(batch_file)[0]
         
        local_batch_file = os.path.join(local_output_folder, scenario)
        os.makedirs(local_batch_file, exist_ok=True)  #create folder if needed
        print(f"Local output path: /home/wrancher/landis/output/rep{rep_id}/{scenario}")


        if not os.path.exists(local_batch_file):
            print(f"Missing {batch_file} for replicate {rep_id}.")
            continue
        
        if not run_batch_file_in_container(batch_file, mount_path, container_name):
            print(f"Failed {batch_file} for replicate {rep_id}.")
            return

        print(f"Scenario {scenario_name}, replicate {rep_id} done.")

#main processing function
def main(reps, batch_files, mount_path, output_folder, container_name):
    with ThreadPoolExecutor() as executor:
        executor.map(lambda rep_id: process_replicate(rep_id, batch_files, mount_path, output_folder, container_name), reps)

#using arg parser to take specified environment variables from the cli and use as vars in this script
if __name__ == '__main__':
    #batch files
    batch_files = ["historic-ncar.sh", "future-ncar.sh", "future-gfdl.sh"]
    reps = range(1,3)

    # command line arg parsing
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("--container_name", type=str, required=True, help="Docker container name")
    arg_parser.add_argument("--access_id", type=str, required=True, help="User access ID")

    #parse
    args = arg_parser.parse_args()

    # get environment vars
    access_id = args.access_id
    container_name = args.container_name
    print(f"Access ID: {access_id}")
    print(f"Container name: {container_name}")

    # set mount path and output folder
    mount_path = "/home/user"
    output_folder = f'/home/{access_id}/landis/output/'
    print(f"Output folder: {output_folder}")

    # run the main function
    main(reps, batch_files, mount_path, output_folder, container_name)
