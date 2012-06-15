oj.lisp
====

Online Judge Submit Script written by Common Lisp.
Study for Common Lisp and ContextL

Only tested on Clozure CL 64bit on Mac OS 10.7.4

Requirements
----

Following Common Lisp libraries are required:

- [CL-PPCRE](http://weitz.de/cl-ppcre/)
- [S-HTTP-CLIENT](http://homepage.mac.com/svc/s-http-client/)
- [DRAKMA](http://weitz.de/drakma/)
- [ContextL](http://common-lisp.net/project/closer/contextl.html)

You can easily install these libraries by using [Quicklisp](http://www.quicklisp.org/).
[Download Quicklisp](http://beta.quicklisp.org/quicklisp.lisp) and eval following codes in your Common Lisp system.

    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (ql:add-to-init-file)
    (ql:quickload :cl-ppcre)
    (ql:quickload :s-http-client)
    (ql:quickload :drakma)
    (ql:quickload :contextl)

Usage
----

First, you should create "setting.lisp" file.
This file contains Online Judge site's configuration.

    (defvar setting
        '((poj . ((user . "username")
                  (pass . "password")
                  (ext-lang-map . (("cpp" . "G++") ("cc" . "G++") ("c" . "GCC") ("java" . "Java")))))
          (aoj . ((user . "username")
                  (pass . "password")
                  (ext-lang-map . (("cpp" . "C++11") ("cc" . "C++11") ("c" . "C") ("java" . "JAVA")))))
          ))

<code>ext-lang-map</code> specifies which extension corresponds to "language" field element.

Then you can run this script by following command.

    $ ccl64 --load oj.lisp -- poj submit 1000

Command is defined formally by

    $ ccl64 --load oj.lisp -- <judge> <command> <number>

where arguments are below:

### &lt;judge&gt;

Online Judge System. Current supporting systems are:

- poj: [PKU Judge Online](http://poj.org/)
- aoj: [AIZU ONLINE JUDGE](http://judge.u-aizu.ac.jp/onlinejudge/)

### &lt;command&gt;

- get: Get sample input and output.
- submit: Submit programs.

### &lt;number&gt;

Problem number.


Source Code
----

This repository contains both ordinary OOP (CLOS) version and COP (ContextL) version.
OOP version is under master branch, while COP version is under contextl branch.
You can switch these versions by

    $ git checkout contextl

or

    $ git checkout master
