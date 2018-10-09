#!/bin/bash

set -e

appname=space
sdl2_path=/usr/local/opt/sdl2/lib
sdl2_dylib=libSDL2-2.0.0.dylib
sdl2_ttf_path=/usr/local/opt/sdl2_ttf/lib
sdl2_ttf_dylib=libSDL2_ttf-2.0.0.dylib

mkdir -p "$appname".app/Contents/{Frameworks,MacOS,Resources}

cp $(stack exec -- which "$appname") "$appname".app/Contents/MacOS

install_name_tool \
  -change "$sdl2_path/$sdl2_dylib" "@executable_path/../Frameworks/$sdl2_dylib" \
  -change "$sdl2_ttf_path/$sdl2_ttf_dylib" "@executable_path/../Frameworks/$sdl2_ttf_dylib" \
  "$appname".app/Contents/MacOS/"$appname"

cp "$sdl2_path/$sdl2_dylib" "$sdl2_ttf_path/$sdl2_ttf_dylib" \
  "$appname".app/Contents/Frameworks

install_name_tool \
  -change "$sdl2_path/$sdl2_dylib" "@executable_path/lib/$sdl2_dylib" \
  "$appname/lib/$sdl2_ttf_dylib"

cp -r data "$appname".app/Contents/MacOS/
cp LICENSE "$appname".app/

cat << 'EOF' > "$appname".app/Contents/MacOS/launcher.sh
#!/bin/bash
cd "${0%/*}"
./space
EOF
chmod +x "$appname".app/Contents/MacOS/launcher.sh

cat << EOF > "$appname".app/Contents/Info.plist
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleExecutable</key>
    <string>launcher.sh</string>
    <key>CFBundleIdentifier</key>
    <string>$appname</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>$appname</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
</dict>
</plist>
EOF

zip -q -r "$appname-osx.zip" "$appname.app"
