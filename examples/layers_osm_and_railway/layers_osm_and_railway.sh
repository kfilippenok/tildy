#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")

../../tildy -layers layers.ini \
            -out $script_dir/tiles/{p}/{z}/{x}/{y} \
            -min-zoom 0 \
            -max-zoom 2 \
            -skip-missing