if [ ! -f ".pacmelm-tools-dir" ]; then
    echo "You must execute this script in tools/";
    exit;
fi

cd ..
cp -Rv build/result pacmelm-$1
7z a -tzip pacmelm-$1.zip pacmelm-$1
rm -rf pacmelm-$1
