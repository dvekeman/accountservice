#!/bin/bash

bin/deploy_local.sh
localresult=$?

bin/deploy_remote.sh
remoteresult=$?

echo "================================================================================================================="

if [[ $localresult = 0  ]]; then
  echo "Local deployment succeeded"
else 
  echo "Local deployment failed"
fi

if [[ $remoteresult = 0  ]]; then
  echo "Remote deployment succeeded"
else 
  echo "Remote deployment failed"
fi

echo "================================================================================================================="
