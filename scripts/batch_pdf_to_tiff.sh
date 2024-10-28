#!/bin/bash

# Directory containing PDF files
input_dir="figs"

# Check if the directory exists
if [ ! -d "$input_dir" ]; then
    echo "Directory $input_dir does not exist."
    exit 1
fi

# Loop through all PDF files in the directory
for pdf_file in "$input_dir"/*.pdf; do
    # Set output file names
    output_tiff="${pdf_file%.pdf}.tiff"
    output_png="${pdf_file%.pdf}.png"

    # Convert PDF to TIFF
    magick -density 600 "$pdf_file" -depth 8 -compress lzw "$output_tiff"
    echo "Converted $pdf_file to $output_tiff"

    # Convert PDF to PNG
    magick -density 600 "$pdf_file" -depth 8 "$output_png"
    echo "Converted $pdf_file to $output_png"
done
