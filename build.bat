@echo off
rem build.bat -- Byte-compile all Elisp packages in the dot-emacs repo.
rem
rem Usage:
rem   build.bat              compile everything
rem   build.bat --debug-init pass extra flags to Emacs
rem
rem Requires Emacs to be on PATH.  Set EMACS= to override the binary.

if "%EMACS%"=="" set EMACS=emacs

"%EMACS%" --batch --no-site-file --no-site-lisp -l "%~dp0build.el" %*
