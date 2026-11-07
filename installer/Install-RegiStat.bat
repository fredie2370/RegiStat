@echo off
chcp 65001 >nul
title Install RegiStat
set RSCRIPT=
where Rscript >nul 2>nul && set RSCRIPT=Rscript
if not defined RSCRIPT for /f "delims=" %%i in ('dir /b /s /o-n "%ProgramFiles%\R\Rscript.exe" 2^>nul') do if not defined RSCRIPT set RSCRIPT=%%i
if not defined RSCRIPT (
  echo R was not found / No se encontro R. Install it first: https://cran.r-project.org/
  pause
  exit /b 1
)
"%RSCRIPT%" "%~dp0Install-RegiStat.R"
pause
