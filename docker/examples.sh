#!/bin/bash

docker image build -t ghcr.io/nathansam/libdr:latest .

docker container run \
  --mount type=bind,source="/Volumes/igmm/cvallejo-predicct/libdr/",target="/analysis/data" \
  --mount type=bind,source="$(pwd)/docs",target="/analysis/docs" \
  --mount type=bind,source="$(pwd)",target="/analysis/src" \
  --rm \
  ghcr.io/nathansam/libdr
