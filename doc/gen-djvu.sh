#!/bin/sh

FILES=*.pdf

for PDF in ${FILES}; do
        NAME=$(echo ${PDF} | cut -d. -f1)
        DJ=${NAME}.djv
        pdf2djvu -o ${DJ} ${PDF}
done
