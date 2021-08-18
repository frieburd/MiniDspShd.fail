#!/bin/sh
set -x

VERSION=`git describe --tags --abbrev=0`.`git rev-list $(git describe --tags --abbrev=0)..HEAD --count`

sed "s/{{ env\['VERSION'\] }}/$VERSION/g" install.template.xml > install.xml
zip -r MiniDspShd-$VERSION.zip . -x \*.zip \*.sh \*.git\* \*README\* \*webauth\* \*.sublime\* \*.DS_Store\* \*.editorconfig \*.template.xml
rm install.xml
