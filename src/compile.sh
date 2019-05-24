#!/bin/bash
chmod +x  my_script.sh //add execute permission
scalac .\scala\util\*.sc .\scala\scene\*.sc .\scala\geometry\*.sc
scala main.sc