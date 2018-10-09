#!/bin/bash

gprconfig -v --target=all
make
make test
