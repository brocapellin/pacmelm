if [ ! -f ".pacmelm-tools-dir" ]; then
    echo "You must execute this script in tools/";
    exit;
fi

cd ..
cp -Rv build/result /tmp/pacmelm-release
git checkout gh-pages
cp -Rv /tmp/pacmelm-release/* .
rm -rf /tmp/pacmelm-release
mv Main.html index.html
