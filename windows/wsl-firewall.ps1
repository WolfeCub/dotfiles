if (!([Security.Principal.WindowsPrincipal][Security.Principal.WindowsIdentity]::GetCurrent()).IsInRole([Security.Principal.WindowsBuiltInRole] "Administrator")) { Start-Process powershell.exe "-NoProfile -ExecutionPolicy Bypass -File `"$PSCommandPath`"" -Verb RunAs; exit }


$remoteip = wsl.exe /bin/bash -c "ip addr show eth0 | grep -oP '(?<=inet\s)\d+(\.\d+){3}'"
$found = $remoteip -match '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}';

if( !$found ){
  echo "The Script Exited, the ip address of WSL 2 cannot be found";
  exit;
}

#Remove Firewall Exception Rules
iex "Remove-NetFireWallRule -DisplayName 'WSL 2 Firewall Unlock' ";

iex "New-NetFireWallRule -DisplayName 'WSL 2 Firewall Unlock' -Direction Inbound -RemoteAddress $remoteip -Action Allow -Protocol TCP";
