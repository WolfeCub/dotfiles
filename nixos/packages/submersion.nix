{inputs, ...}: {
  # Prebuilt Flutter release, runnable as `.#submersion`.
  perSystem = {
    pkgs,
    lib,
    ...
  }: let
    versionInfo = lib.importJSON "${inputs.submersion}/data/flutter_assets/version.json";
  in {
    packages.submersion = pkgs.stdenv.mkDerivation {
      pname = "submersion";
      version = "${versionInfo.version}.${versionInfo.build_number}";
      src = inputs.submersion;

      nativeBuildInputs = with pkgs; [autoPatchelfHook makeWrapper copyDesktopItems];

      buildInputs = with pkgs; [
        gtk3
        glib
        pango
        cairo
        atk
        harfbuzz
        gdk-pixbuf
        libepoxy
        fontconfig
        libsecret
        libsoup_3
        webkitgtk_4_1
        zlib
        stdenv.cc.cc.lib
      ];

      # Flutter dlopens GL at runtime
      runtimeDependencies = with pkgs; [libGL];

      # Dart FFI dlopens bundled libs (e.g. libsqlcipher.so) by bare name
      appendRunpaths = ["${placeholder "out"}/opt/submersion/lib"];

      # The jni plugin is bundled but never loaded on Linux
      autoPatchelfIgnoreMissingDeps = ["libjvm.so"];

      installPhase = ''
        runHook preInstall

        mkdir -p $out/opt/submersion $out/bin
        cp -r submersion lib data $out/opt/submersion/
        # path_provider shells out to xdg-user-dir for the documents directory.
        # Native Wayland gets stuck in a resize loop pinning the GPU.
        makeWrapper $out/opt/submersion/submersion $out/bin/submersion \
          --prefix PATH : ${lib.makeBinPath [pkgs.xdg-user-dirs]} \
          --set-default GDK_BACKEND x11

        install -Dm644 data/flutter_assets/assets/icon/icon.png \
          $out/share/icons/hicolor/256x256/apps/app.submersion.png
        install -Dm644 60-submersion-divecomputers.rules \
          $out/lib/udev/rules.d/60-submersion-divecomputers.rules

        runHook postInstall
      '';

      desktopItems = [
        (pkgs.makeDesktopItem {
          name = "app.submersion";
          desktopName = "Submersion";
          genericName = "Dive Log";
          comment = "An open-source dive logging application for scuba divers.";
          exec = "submersion %U";
          icon = "app.submersion";
          categories = ["Science" "Education" "Utility"];
          keywords = ["dive" "diving" "scuba" "divelog" "logbook"];
          startupWMClass = "submersion";
        })
      ];

      meta = {
        description = "Submersion dive log";
        homepage = "https://submersion.app";
        license = lib.licenses.gpl3Only;
        platforms = ["x86_64-linux"];
        mainProgram = "submersion";
      };
    };
  };
}
