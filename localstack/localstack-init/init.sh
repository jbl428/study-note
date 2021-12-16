#!/bin/sh
echo "Init localstack"
awslocal s3 mb s3://test-bucket
awslocal ses verify-email-identity --email-address test@email.com
