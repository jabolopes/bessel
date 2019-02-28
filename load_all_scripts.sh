#!/bin/bash

for i in Test/*.bsl; do echo -e :load $(echo $i | sed -e "s,.bsl,,g" -e "s,/,.,g") | ./bsl; done 2>&1 | grep -v -e Staging -e "\["
