#!/bin/bash

export REMOTE="AWS"

QUEUE_URL="https://sqs.us-west-2.amazonaws.com/975050180415/water-tracker-Q"

### stop on an error
set -e

# capture cl arguments
bid_name=$1
input_bucket=$2
auction_shapefile=$3
output_bucket=$4

send_sqs_message() {
    local queue_url="$1"
    local message="$2"
    local bid_name="$3"

    local message_attributes="{
        \"bid_name\": {
            \"DataType\": \"String\",
            \"StringValue\": \"$bid_name\"
        }
    }"

    aws sqs send-message \
        --queue-url "$queue_url" \
        --message-body "$message" \
        --message-attributes "$message_attributes"
}



send_sqs_message "$QUEUE_URL" "<execute.sh> - Starting up model run..." "$bid_name"

send_sqs_message "$QUEUE_URL" "<execute.sh> - Running 02_analyze_bids.R" "$bid_name"
Rscript --no-save scripts/02_analyze_bids.R $auction_id $auction_shapefile $input_bucket
send_sqs_message "$QUEUE_URL" "<execute.sh> - Running 02_analyze_bids.R... DONE" "$bid_name"

