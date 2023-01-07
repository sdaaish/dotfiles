# My local files in a bare git repo
Function Invoke-DotGit {
    if  ($isLinux){
    }
    else {
        $gitdir = Join-Path ${env:USERPROFILE} ".dotgit"
        $workdir = ${env:USERPROFILE}
        $cmd = Get-Command git.exe

        $options = @(
            "--git-dir=${gitdir}"
            "--work-tree=${workdir}"
        )
        Write-Verbose "$cmd @options $args"
        & $cmd @options $args
    }
}
