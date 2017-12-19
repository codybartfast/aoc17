function Global:aoc(){
    $repo = Split-Path -Parent $PSCommandPath
    $dll = Join-Path $repo "bin\Debug\netcoreapp2.0\aoc17.dll"
    dotnet $dll $args
}