# Outputs the time diference in ms
Function Get-RunningTime {
    param(
        $time
    )
    [int]$CurrentTime = ((Get-Date) - $time).TotalMilliseconds
    $CurrentTime
}
