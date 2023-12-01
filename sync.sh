#!/bin/bash

rsync -r --info=progress2 jrcoyle@redvelvet:~/pasc-sim/Results .
rsync -r --info=progress2 jrcoyle@redvelvet:~/pasc-sim/Logs .
