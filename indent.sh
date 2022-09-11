#!/bin/bash

find "src" -type f -name "*.hs" | xargs -L1 hindent
