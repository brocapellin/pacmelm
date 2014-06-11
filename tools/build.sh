if [ ! -f ".pacmelm-tools-dir" ]; then
    echo "You must execute this script in tools/";
    exit;
fi

cd ../src
rm -rfv ../build
elm --make --build-dir=../build --cache-dir=../cache --set-runtime=resources/elm-runtime-0.12.3.js Main.elm
cp -Rv resources ../build
