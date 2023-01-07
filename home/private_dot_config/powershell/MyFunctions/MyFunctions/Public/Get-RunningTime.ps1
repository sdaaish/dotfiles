# Outputs the time diference in ms
Function Get-RunningTime {
    param(
        $time
    )
    $currenttime = ((Get-Date) - $time).TotalMilliseconds
    $currenttime
}
