#!/bin/bash

# Check if the input PDF file is provided
if [ -z "$1" ]; then
    echo "Usage: $0 input.pdf"
    exit 1
fi

# Set the output file name
output="${1%.pdf}.tiff"

# Convert PDF to TIFF using ImageMagick 7
magick -density 600 "$1" -depth 8 -compress lzw "$output"

echo "Converted $1 to $output"
