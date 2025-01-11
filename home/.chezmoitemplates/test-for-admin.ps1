# A template to test for privilege

$wid = [System.Security.Principal.WindowsIdentity]::GetCurrent()
$prp = New-Object System.Security.Principal.WindowsPrincipal($wid)
$adm = [System.Security.Principal.WindowsBuiltInRole]::Administrator

# Test if administrator
if (-not ($prp.IsInRole($adm))) {
    throw "You need to run this as administrator."
}
