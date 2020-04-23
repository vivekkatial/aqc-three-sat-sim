module load awscli/1.16.67-GCC-6.2.0

# Set env vars
source bin/init/set-environment-variables.sh

# Set the directory in which experiments parameter files stored
export EXPERIMENT_FILE_DIR=$S3_BUCKET/$EXPERIMENT_NAME/ready/
echo "$EXPERIMENT_FILE_DIR"

# Remove all scripts
read -r -p "Are you sure you wish to delete $EXPERIMENT_FILE_DIR? [Y/n]" response
response=${response,,} # tolower
if [[ $response =~ ^(yes|y| ) ]] || [[ -z $response ]]; then
    aws s3 rm $EXPERIMENT_FILE_DIR --recursive --exclude "" --endpoint-url=$MLFLOW_S3_ENDPOINT_URL    
fi