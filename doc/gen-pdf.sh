#!/bin/sh

FILES=*.ps

for PS in ${FILES}; do
        NAME=$(echo ${PS} | cut -d. -f1)
        PDF=${NAME}.pdf
        echo "Converting ${PS} to ${PDF}"
        ps2pdf ${PS} ${PDF}
done
