#!/bin/bash

sudo apt-get update -qq
sudo apt-get install -qq gnat-4.6 gprbuild

make
make test
