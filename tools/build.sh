if [ ! -f ".pacmelm-tools-dir" ]; then
    echo "You must execute this script in tools/";
    exit;
fi

cd ../src
rm -rf ../build/result
elm --cache-dir=../build/cache --build-dir=../build/result --set-runtime=resources/elm-runtime-0.12.3.js Main.elm
cp -Rv resources ../build/result
