# Search an alias or display all of them
Function Search-Alias {
    param (
        [string]$alias
    )

    if ($alias){
        Get-Alias| Where DisplayName -Match $alias
    }
    else {
        Get-Alias
    }
}
