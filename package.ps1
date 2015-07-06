mkdir dist -Force | Out-Null
mkdir build\lib\net45\ -Force | Out-Null
cp .\MongoDB.FSharp\bin\Release\MongoDB.FSharp.* .\build\lib\net45\
.\.nuget\NuGet.exe pack -OutputDirectory .\dist\ -BasePath .\build\
