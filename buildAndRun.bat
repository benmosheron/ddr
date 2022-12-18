echo off
cls
if not exist out md out
call scalac -cp . -d out ben/*.scala
scala -cp out ben.App %*