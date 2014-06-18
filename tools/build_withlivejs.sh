if [ ! -f "Main.elm" ]; then
    echo "You must execute this script in src/";
    exit;
fi

mkdir -p ../build/new
elm --make --build-dir=../build/new --cache-dir=../build/cache --set-runtime=resources/elm-runtime-0.12.3.js --scripts=resources/dev/live.js Main.elm > /dev/null
cd ..
cp -R resources build/new
ln -sfn `pwd`/build/new build/build
rm -rf build/current
cp -R build/new build/current
ln -sfn `pwd`/build/current build/build
rm -rf build/new
