#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")

../../tildy -ls local_osm_and_railway.ini \
            -ps local_osm_and_railway.ini \
            -z 0 \
            -Z 10 \
            -skmono \
            -ms monochromes.ini \
            -as areas.ini \
            -o $script_dir/tiles/{p}/{z}_{x}_{y} \
            -skip-missing \
            -skip-existing