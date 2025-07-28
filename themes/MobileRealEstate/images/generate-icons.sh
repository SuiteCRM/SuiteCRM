#!/bin/bash

# Script to generate PWA icons from SVG
# Requires ImageMagick or rsvg-convert

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SVG_FILE="$SCRIPT_DIR/icon-base.svg"

# Array of sizes needed for PWA
SIZES=(72 96 128 144 152 192 384 512)

echo "Generating PWA icons..."

# Check if ImageMagick is installed
if command -v convert &> /dev/null; then
    echo "Using ImageMagick to generate icons..."
    for size in "${SIZES[@]}"; do
        convert -background none -resize ${size}x${size} "$SVG_FILE" "$SCRIPT_DIR/icon-${size}.png"
        echo "Generated icon-${size}.png"
    done
elif command -v rsvg-convert &> /dev/null; then
    echo "Using rsvg-convert to generate icons..."
    for size in "${SIZES[@]}"; do
        rsvg-convert -w $size -h $size "$SVG_FILE" -o "$SCRIPT_DIR/icon-${size}.png"
        echo "Generated icon-${size}.png"
    done
else
    echo "Neither ImageMagick nor rsvg-convert found. Creating placeholder PNGs..."
    # Create simple placeholder PNGs using base64 encoded 1x1 blue pixel
    for size in "${SIZES[@]}"; do
        # Create a simple blue PNG as placeholder
        echo "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==" | base64 -d > "$SCRIPT_DIR/icon-${size}.png"
        echo "Created placeholder icon-${size}.png (Please replace with actual icons)"
    done
fi

# Generate badge icon
if [ -f "$SCRIPT_DIR/icon-72.png" ]; then
    cp "$SCRIPT_DIR/icon-72.png" "$SCRIPT_DIR/badge-72.png"
    echo "Created badge-72.png"
fi

# Generate shortcut icons
if [ -f "$SCRIPT_DIR/icon-96.png" ]; then
    cp "$SCRIPT_DIR/icon-96.png" "$SCRIPT_DIR/add-property.png"
    cp "$SCRIPT_DIR/icon-96.png" "$SCRIPT_DIR/properties.png"
    echo "Created shortcut icons"
fi

echo "Icon generation complete!"
echo ""
echo "Note: If you see placeholder icons, please install ImageMagick or rsvg-convert:"
echo "  macOS: brew install imagemagick"
echo "  Ubuntu/Debian: sudo apt-get install imagemagick"
echo "  or: sudo apt-get install librsvg2-bin"