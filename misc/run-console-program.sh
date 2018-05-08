#!/bin/bash

$*

exit_code=$?
if [ $exit_code -eq 0 ]; then
    echo -e "\nProces returned with exit code: \e[0;32m"$exit_code"\e[0m."
else
    echo -e "\nProces returned with exit code: \e[0;31m"$exit_code"\e[0m."
fi
read junk
