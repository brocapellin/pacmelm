if [ ! -f ".pacmelm-tools-dir" ]; then
    echo "You must execute this script in tools/";
    exit;
fi

cd ..
rm -rfv build/release
mkdir -pv build/release/build
cd src
elm --make --build-dir=../build/release/build --cache-dir=../build/release/cache --set-runtime=resources/elm-runtime-0.12.3.js Main.elm
cd ..
cp -Rv resources build/release/build
rm -rfv build/release/build/resources/dev
