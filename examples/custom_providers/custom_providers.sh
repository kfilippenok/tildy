#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")

../../tilesdownloader -providers $script_dir/cconfig.ini \
                      -layers config.ini \
                      -out $script_dir/tiles/{p}/{z}/{x}/{y} \
                      -min-zoom 0 \
                      -max-zoom 2 \
                      -skip-missing