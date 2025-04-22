# Show top n processes regarding cpu usage or total memory
# See example from Microsoft:
#  https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-process?view=powershell-7.5#example-5-add-a-property-to-the-default-get-process-output-display
Function Get-TopProcess {
    param(
        [ValidateSet("Process", "Memory")]
        [string]$Option = "Process"
    )
    switch ($option) {
        "Memory" { $sort = "TotalMemory (M)" }
        default { $sort = "CPU" }
    }
    while ($true) {
        Clear-Host
        Get-Process |
            Select-Object -Property @{Name="SI"; Expression = {$_.SessionId}},
            @{Name="PID"; Expression = {$_.Id}},
            @{name = "CPU"; expression = { ($_.CPU).ToString("#") -as [int] } },
            @{name = "TotalMemory (M)"; expression = { ($_.VM + $_.PM) / 1MB -as [int] }},
            @{Name="Handles"; Expression = {[int] $_.Handles}},
            @{Name = "Threads"; Expression = { $_.Threads.Count}}, ProcessName |
            Sort-Object -Property $sort -Descending -Top 20 |
            Format-Table -Auto
        Start-Sleep 5
    }
}
