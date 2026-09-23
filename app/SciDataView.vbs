Dim fso, strDir, WshShell, rscript, runAppScript, cmd
Set fso = CreateObject("Scripting.FileSystemObject")
strDir = fso.GetParentFolderName(WScript.ScriptFullName)

rscript = strDir & "\R-Portable\bin\x64\Rscript.exe"
If Not fso.FileExists(rscript) Then
    rscript = fso.GetParentFolderName(strDir) & "\R-Portable\bin\x64\Rscript.exe"
End If

If Not fso.FileExists(rscript) Then
    MsgBox "R-Portable runtime not found at: " & vbCrLf & rscript, vbCritical, "SciDataView Error"
    WScript.Quit 1
End If

runAppScript = strDir & "\run_app.R"
If Not fso.FileExists(runAppScript) Then
    runAppScript = strDir & "\app\run_app.R"
End If

Set WshShell = CreateObject("WScript.Shell")
WshShell.CurrentDirectory = strDir
cmd = """" & rscript & """ """ & runAppScript & """"
WshShell.Run cmd, 0, False
Set WshShell = Nothing
Set fso = Nothing
