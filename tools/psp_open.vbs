' Opens images in Paint Shop Pro 8 elevated (PSP 8 needs admin to write HKLM\Software\Classes).
' Used by the HKCU override of PaintShopPro8.Image\shell\Open\command. Undo: delete that HKCU shell key.
Dim exe, arg
exe = "C:\Program Files (x86)\Jasc Software Inc\Paint Shop Pro 8\Paint Shop Pro.exe"
arg = ""
If WScript.Arguments.Count > 0 Then arg = """" & WScript.Arguments(0) & """"
CreateObject("Shell.Application").ShellExecute exe, arg, "", "runas", 1
