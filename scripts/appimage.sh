#!/bin/bash

set -e

APPIMAGEDIR=.appimage
mkdir -p "$APPIMAGEDIR"

APPIMAGE_TOOL_URL="https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-x86_64.AppImage"
APPIMAGE_TOOL_PATH="$APPIMAGEDIR/appimagetool-x86_64.AppImage"

if [ ! -f "$APPIMAGE_TOOL_PATH" ]; then
    echo "Downloading appimagetool..."
    wget -c "$APPIMAGE_TOOL_URL" -O "$APPIMAGE_TOOL_PATH"
    chmod +x "$APPIMAGE_TOOL_PATH"
else
    echo "appimagetool already exists, skipping download."
fi

APPDIR="$APPIMAGEDIR/lisp-chat.AppDir"

# Create the AppDir structure
mkdir -p "$APPDIR/usr/bin/"

# Create the .desktop file
cat > "$APPDIR/lisp-chat.desktop" <<EOL
[Desktop Entry]
Name=lisp-chat
Exec=AppRun
Icon=computer-symbolic
Terminal=true
Type=Application
Categories=Utility;
EOL

# Create AppRun
cat > "$APPDIR/AppRun" <<EOL
#!/bin/sh
HERE=\$(dirname "\$(readlink -f "\${0}")")
"\${HERE}/usr/bin/lisp-chat" "\$@"
EOL
chmod +x "$APPDIR/AppRun"

# Copy the binary and icon
cp -v ./roswell/lisp-chat "$APPDIR/usr/bin/"
cp -v /usr/share/icons/Adwaita/scalable/devices/computer-symbolic.svg "$APPDIR/"

# Run appimagetool
"$APPIMAGE_TOOL_PATH" "$APPDIR"
