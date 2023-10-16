#!/bin/bash
# remove old credentials

# -i "" - is specific to MAC os
sed -i "" '1,/\[inbo-alien\]/!d' ~/.aws/credentials

if ! grep -Fxq "[inbo-alien]"  ~/.aws/credentials
then
  echo '[inbo-alien]' >> ~/.aws/credentials
fi
# extract credentials
OUT=$(aws sts assume-role --role-arn  arn:aws:iam::625469168093:role/inbo-exotenportaal-openanalytics-role --role-session-name exoten --serial-number arn:aws:iam::196229073436:mfa/oppo --token-code $1);\

echo "aws_access_key_id = $(echo $OUT | jq -r '.Credentials''.AccessKeyId')" >> ~/.aws/credentials
echo "aws_secret_access_key = $(echo $OUT | jq -r '.Credentials''.SecretAccessKey')" >> ~/.aws/credentials
echo "aws_session_token = $(echo $OUT | jq -r '.Credentials''.SessionToken')" >> ~/.aws/credentials

