#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")

../../tildy -p osm-standard \
            -z 0 \
            -Z 3 \
            -as areas.ini \
            -o $script_dir/{p}/{z}/{x}/{y} \
            -skip-missing