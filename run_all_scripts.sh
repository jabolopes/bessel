#!/bin/bash

for i in Test/*Main.bsl; do echo -e :load $(echo $i | sed -e "s,.bsl,,g" -e "s,/,.,g") "\nmain" | ./bsl; done 2>&1 | grep -v -e Staging -e "\[./.\]"
