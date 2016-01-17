#! /bin/bash

set -e
echo "Building QuickLift..."
stack build
strip `stack exec -- which quicklift-exe`
echo "Building assets..."
cd ql-ui
deploy
cd ..
echo "Creating bundle..."
cp `stack exec -- which quicklift-exe` quicklift-exe
tar -czvf quicklift.keter quicklift-exe config ql-ui/assets
rm quicklift-exe
scp ./quicklift.keter root@104.236.4.9:/opt/keter/incoming/quicklift.keter
rm quicklift.keter
