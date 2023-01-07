# show top n processes regarding cpu usage or total memory
Function Get-TopProcess {
    param(
        [ValidateSet("Process","Memory")]
        [string]$Option = "Process"
    )
    switch ($option) {
        "Memory"  {$sort = "TotalMemory (M)"}
        default  {$sort = "CPU"}
    }
    while ($true){
        Clear-Host
        Get-Process|
          Select-Object -Property Id, ProcessName,
        @{name="CPU";expression={($_.CPU).ToString("#") -as [int]}},
        @{name="TotalMemory (M)";expression={($_.VM + $_.PM) / 1MB -as [int]}},
        Handles| Sort-Object -Property $sort -Descending -Top 20|Format-Table -Auto
        Start-Sleep 5
    }
}
