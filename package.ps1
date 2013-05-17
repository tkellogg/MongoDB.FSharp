mkdir dist -Force | Out-Null
mkdir build\lib\net40\ -Force | Out-Null
cp .\MongoDB.FSharp\bin\Release\MongoDB.FSharp.* .\build\lib\net40\
.\.nuget\NuGet.exe pack -OutputDirectory .\dist\ -BasePath .\build\
