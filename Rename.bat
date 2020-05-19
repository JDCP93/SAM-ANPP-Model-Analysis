@echo off
Setlocal enabledelayedexpansion

Set "Pattern=prior"
Set "Replace=Obs_pri"

For %%# in ("C:\Users\jono_\Documents\PHD\SAM-ANPP-Model-Analysis\*.Rdata") Do (
    Set "File=%%~nx#"
    Ren "%%#" "!File:%Pattern%=%Replace%!"
)

Pause&Exit