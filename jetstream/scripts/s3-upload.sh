#!/bin/bash
set -euo pipefail

trap 'echo "Error occurred at line $LINENO"; exit 1;' ERR

export S3_ENDPOINT_URL=${S3_ENDPOINT_URL:-"https://usgs2.osn.mghpcc.org"}

#params for aws
read -p "s3 bucket name: " bucket_name
read -p "what do you want the folder(s) or file(s) to be in named in "$bucket_name" bucket? Comma-seperated if many: " s3_upload_location
read -p "what files or folders do you want to upload to "$bucket_name" bucket? Comma-seperated if many: " files_to_upload

# check if multiple files to upload
if [[ "$s3_upload_location" == *","* ]] && [[ "$files_to_upload" == *","* ]]; then
  echo "Uploading multiple files/folders to S3..."
  IFS=',' read -ra LOCATIONS <<< "$s3_upload_location"
  IFS=',' read -ra FILES <<< "$files_to_upload"

  # ensure the number of locations matches the number of files
  if [ "${#LOCATIONS[@]}" -ne "${#FILES[@]}" ]; then
    echo "Error: Number of files and S3 locations specified must match."
    exit 1
  fi

  # iterate and upload each file/folder to its corresponding location
  for i in "${!FILES[@]}"; do
    file_path=$(realpath "${FILES[$i]}")  # resolves the absolute path
    if [ -d "$file_path" ]; then
      echo "Syncing directory: $file_path to s3://$bucket_name/${LOCATIONS[$i]}"
      aws s3 sync "$file_path" s3://"$bucket_name"/"${LOCATIONS[$i]}" --endpoint-url "$S3_ENDPOINT_URL"
    elif [ -f "$file_path" ]; then
      echo "Copying file: $file_path to s3://$bucket_name/${LOCATIONS[$i]}"
      aws s3 cp "$file_path" s3://"$bucket_name"/"${LOCATIONS[$i]}" --endpoint-url "$S3_ENDPOINT_URL"
    else
      echo "Error: File or directory '$file_path' does not exist."
      exit 1
    fi
  done
  
else
  # handle single file/directory upload
  file_path=$(realpath "$files_to_upload")
  if [ -d "$file_path" ]; then
    echo "Syncing directory: $file_path to s3://$bucket_name/$s3_upload_location"
    aws s3 sync "$file_path" s3://"$bucket_name"/"$s3_upload_location" --endpoint-url "$S3_ENDPOINT_URL"
  elif [ -f "$file_path" ]; then
    echo "Copying file: $file_path to s3://$bucket_name/$s3_upload_location"
    aws s3 cp "$file_path" s3://"$bucket_name"/"$s3_upload_location" --endpoint-url "$S3_ENDPOINT_URL"
  else
    echo "Error: File or directory '$file_path' does not exist."
    exit 1
  fi
fi

echo "Upload complete!"
