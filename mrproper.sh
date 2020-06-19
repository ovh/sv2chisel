#!/bin/bash
# Clean ALL compiled files in the project

# Little trick to preserve target/.history
# s* => scala-<version>/ ; streams/
rm -rf target/s*
rm -rf project/target
rm -rf project/project/target