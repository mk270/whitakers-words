#!/bin/bash

sudo apt-get update -qq
sudo apt-get install -qq gnat-4.8 gprbuild

make
make test
