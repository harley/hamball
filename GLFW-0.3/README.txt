This is a Haskell module for GLFW OpenGL framework
(http://glfw.sourceforge.net). It provides an alternative
to GLUT for OpenGL based Haskell programs.

SOE (http://www.haskell.org/soe) now depends on this package.

The website for this Haskell module is at Haskell Wiki site:
http://haskell.org/haskellwiki/GLFW

=========
ChangeLog
=========

- Tue Jan 15 EST 2008

  Version 0.3 adds Haddock documentation and the missing Joystick
  APIs.
 
- Thu Dec 20 EST 2007

  Version 0.2 is repackaged to work with Cabal 1.2 or later.
  Now it builds and installs out-of-box on most platforms.


============
Installation
============

The package comes together with a (partial) source distribution 
of GLFW v2.6, which is compiled and installed together with
the Haskell package.

It follows the standard Cabal package installation steps:

1. To configure the module, type

       runhaskell Setup.hs configure
   or

       runhaskell Setup.hs configure --user --prefix=DIR

   if you want to install the package to your user's directory
   instead of the system one (replace DIR with your own directory
   choice).

2. To build the module, type 

       runhaskell Setup.hs build

3. To install, type 

       runhaskell Setup.hs install   

In the process it builds all GLFW C library source code. You may
use "runhaskell Setup.hs build --verbose" to see the actual 
compilation steps.

4. Optionally to build its Haddock documentation, type

       runhaskell Setup.hs haddock

====
NOTE
====

For Windows users, you'll have to include GHC's gcc-lib directory 
in your PATH environment, e.g., c:\ghc\ghc-6.6.1\gcc-lib, before 
configuring the GLFW module, otherwise it'll complain about missing
program for ld.

GHC 6.8.x is already equipped with Cabal 1.2, so everything should
work as expected. But for GHC 6.6.x users, you'll have to first 
install the newest version of Cabal 1.2 before installing GLFW. 
Cabal 1.2 also requires the module FilePath, which doesn't come
with GHC 6.6.x.


=============
Package Usage
=============

The package is tested with GHC 6.6.1 (with Cabal 1.2.2) on all
three platforms (Linux, Win32/MinGW, and Mac OS X). Though it may
work with older versions of GHC or even Hugs, they are not tested.

It installs a new Haskell package called "GLFW" and the actual
module to import is "Graphics.UI.GLFW". You'll need to pass 
"-package GLFW" to GHC if you want to compile it.

GLFW itself is well documented (see GLFW website), and the
Haskell module API is documented via Haddock. 

Not all functions are fully tested, and there are still a 
few GLFW C functions missing from the Haskell module, namely 
the image loading functions. They are excluded because image
handling is a separate issue, and low level buffer manipulation
would obscure their use further. Texture loading from TGA
format is supported both from file and from memory (via a
string buffer). 

The Haskell module also provides basic text rendering while
GLFW doesn't. It comes from a free 8x16 font which is made
into a TGA texture, stored as a Haskell string in the file 
GLFW.hs (also the reason for its big size). Text rendering
is only possible with Alpha enabled. Again, see SOE.hs from
the SOE package for sample usage.

GLFW doesn't work well with GHC threads, forkIO or threadDelay. 
So avoid them if you can.


===================
Contact Information
===================

You may send your bug report and feature request to the package 
maintainer: Paul H. Liu <paul@thev.net>.

--
Last Updated: Tue Jan 15 EST 2008
