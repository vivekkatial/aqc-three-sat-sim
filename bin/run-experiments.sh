# List ready experiments

export EXPERIMENT_FILE_DIR=$S3_BUCKET/$EXPERIMENT_NAME/ready/
# aws s3 ls $EXPERIMENT_FILE_DIR --endpoint-url=$MLFLOW_S3_ENDPOINT_URL

# for file in "$EXPERIMENT_FILE_DIR"*; do
#   echo "Running Experiment on $file"
#   # Run Script
#   # Rscript main.R $file
# done

for file in $(aws s3 ls $EXPERIMENT_FILE_DIR --endpoint-url=$MLFLOW_S3_ENDPOINT_URL | awk '{print $4}'); do
    echo "Running experiment on $file"
    # Pull file into local directory
    aws s3 
done