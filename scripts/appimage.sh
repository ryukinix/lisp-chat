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

# Handle Icon
ICON_NAME="lisp-chat"
cp -v "logo/logo.png" "$APPDIR/${ICON_NAME}.png"

# Create the .desktop file
cat > "$APPDIR/lisp-chat.desktop" <<EOL
[Desktop Entry]
Name=lisp-chat
Exec=AppRun
Icon=${ICON_NAME}
Terminal=true
Type=Application
Categories=Utility;
EOL

# Copy the binary
cp -v ./roswell/lisp-chat "$APPDIR/usr/bin/"

# Copy shared libraries
mkdir -p "$APPDIR/usr/lib/"
for lib_base in "libncurses.so" "libtinfo.so" "libcrypto.so" "libreadline.so"; do
    FOUND=0
    for version in "6" "5"; do
        LIB_PATTERN="${lib_base}.${version}"
        # Extract path: "    libname.so.x (libc6,x86-64) => /path/to/lib"
        # We want the part after "=> "
        LIB_PATH=$(ldconfig -p | grep "$LIB_PATTERN" | head -n 1 | sed 's/.*=> //')

        if [ -n "$LIB_PATH" ] && [ -f "$LIB_PATH" ]; then
            echo "Bundling $LIB_PATTERN from $LIB_PATH"
            cp -v "$LIB_PATH" "$APPDIR/usr/lib/"
            # Copy real file if symlink
            if [ -L "$LIB_PATH" ]; then
                REAL_PATH=$(readlink -f "$LIB_PATH")
                if [ -f "$REAL_PATH" ]; then
                     cp -v "$REAL_PATH" "$APPDIR/usr/lib/"
                fi
            fi
            FOUND=1
            break # Found a version, move to next library
        fi
    done

    if [ $FOUND -eq 0 ]; then
        echo "Warning: $lib_base (version 5 or 6) not found via ldconfig"
    fi
done

# Create AppRun
cat > "$APPDIR/AppRun" <<EOL
#!/bin/sh
HERE=\$(dirname "\$(readlink -f "\${0}")")
export LD_LIBRARY_PATH="\${HERE}/usr/lib:\$LD_LIBRARY_PATH"
exec "\${HERE}/usr/bin/lisp-chat" "\$@"
EOL
chmod +x "$APPDIR/AppRun"

# Run appimagetool
"$APPIMAGE_TOOL_PATH" "$APPDIR"
