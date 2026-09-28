_: {
  flake.nixosModules.udev-rules = {...}: {
    services.udev.extraRules = ''
      # liris60 Via raw HID (cargo bootsel)
      KERNEL=="hidraw*", ATTRS{idVendor}=="4c4b", ATTRS{idProduct}=="4643", OWNER="wolfe", MODE="0600"
      # RP2040 BOOTSEL (picotool)
      SUBSYSTEM=="usb", ATTRS{idVendor}=="2e8a", ATTRS{idProduct}=="0003", OWNER="wolfe", MODE="0600"
    '';
  };
}
