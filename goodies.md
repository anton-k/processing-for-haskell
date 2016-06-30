instruction to instal ftgl:

https://noamlewis.wordpress.com/2012/12/16/cabal-install-ftgl-on-windows-and-getting-exes-that-use-it-to-work/

How to install FTGL. I'm not using it right now but who knows

### For Linux

We need to install the dev libraries for ftgl. It should be libftgl-dev or similar.

~~~
> sudo apt-get install libftgl-dev
~~~

### For Windows

For Windows it's somewhat more complicated. We need to download
  the DLLs for FreeType and FTGL, copy them to system32 or system64 directory
  and write out the coresponding include and lib dirs in the cabal install command.
  Here is how to do it:

* Get 32-bit windows binaries for FreeType and FTGL. I downloaded them 
    from: http://www.opencascade.org/getocc/download/3rdparty/, but you might 
    as well compile them from the official sources.

* Copy the FTGL.dll and FreeType.dll to:

    * 64-bit version of Windows: copy to c:\windows\syswow64

    * 32-bit version of Windows: copy to c:\windows\system32

* Install the [Visual C++ 2010 redistributable, 32-bit version](https://www.microsoft.com/en-us/download/details.aspx?id=5555&tduid=(78d8e7036ed52f69c7cec950d42fe15d)(256380)(2459594)(TnL5HPStwNw-tlEstIGjsicPt6X8U_TS0Q)())

* Assuming you’ve unpackged the FTGL binaries in some directory “<blabla>\ftgl-2.1.3-vc10-32”, 
    run the following:

    ~~~
    cabal install ftgl --extra-include-dirs=<blabla>\ftgl-2.1.3-vc10-32\include --extra-lib-dirs=<blabla>\ftgl-2.1.3-vc10-32\lib --reinstall --force-reinstalls
    ~~~

* cabal build / install the processing-for-haskell library.

The original guide on installing FTGL can be found [here](https://noamlewis.wordpress.com/2012/12/16/cabal-install-ftgl-on-windows-and-getting-exes-that-use-it-to-work/).
