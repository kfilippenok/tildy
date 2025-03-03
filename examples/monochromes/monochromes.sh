#!/bin/bash

script_dir=$(dirname "$(realpath "$0")")

../../tildy -ls local_osm_and_railway.ini \
            -ps local_osm_and_railway.ini \
            -z 0 \
            -Z 14 \
            -skmono \
            -ms monochromes.ini \
            -as areas.ini \
            -o $script_dir/{p}/{z}/{x}/{y} \
            -skip-missing