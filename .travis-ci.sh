#!/bin/bash

sudo apt-get update -qq
sudo apt-get install -qq gnat gprbuild

make
make test
