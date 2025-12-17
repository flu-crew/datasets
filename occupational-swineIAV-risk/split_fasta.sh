#!/bin/bash

# Define input and output file paths
inputFile="input.fasta"
outputH1="input_H1.fasta"
outputH3="input_H3.fasta"

# Clear any existing content in output files
> "$outputH1"
> "$outputH3"

# Initialize a variable to hold the next line
nextLine=""

# Loop through each line in the input file
while IFS= read -r line || [ -n "$line" ]; do
    # Check for H1 sequences (including H1N, H1NX, etc.)
    if [[ "$line" =~ ^\>.*H1(N[[:alnum:]]*)? ]]; then
        echo "$line" >> "$outputH1"
        
        # Read the next line (sequence) for H1 output
        read -r nextLine || break
        if [[ -n "$nextLine" ]]; then
            echo "$nextLine" >> "$outputH1"
        fi
    # Check for H3 sequences (including H3N, H3N2, etc.)
    elif [[ "$line" =~ ^\>.*H3(N[[:alnum:]]*)? ]]; then
        echo "$line" >> "$outputH3"
        
        # Read the next line (sequence) for H3 output
        read -r nextLine || break
        if [[ -n "$nextLine" ]]; then
            echo "$nextLine" >> "$outputH3"
        fi
    fi
done < "$inputFile"