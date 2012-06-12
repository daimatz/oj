oj.lisp
====

Online Judge Submit Script written by Common Lisp.
Study for Common Lisp and ContextL

Usage
----

Command line options differs what system you use.
Some examples are below:

### CLISP

    $ clisp oj.lisp <judge> <command> <number>

### SBCL

    $ sbcl --script oj.lisp <judge> <command> <number>

### ECL

    $ ecl -shell oj.lisp -- <judge> <command> <number>

### Clozure CL

    $ ccl --load oj.lisp -- <judge> <command> <number>

Where arguments are below:

### &lt;judge&gt;

Online Judge System. Current supporting systems are:

- poj: [PKU Judge Online](http://poj.org/)

### &lt;command&gt;

- get: Get sample input and output.
- submit: Submit programs.

### &lt;number&gt;

Problem number.

Requirements
----

Following Common Lisp libraries are required:

- [CL-PPCRE](http://weitz.de/cl-ppcre/)
- [DRAKMA](http://weitz.de/drakma/)
- [ContextL](http://common-lisp.net/project/closer/contextl.html)

You can easily install these libraries using [Quicklisp](http://www.quicklisp.org/).
[Download Quicklisp](http://beta.quicklisp.org/quicklisp.lisp) and eval following codes in your Common Lisp system.

    (load "quicklisp.lisp")
    (quicklisp-quickstart:install)
    (ql:add-to-init-file)
    (ql:quickload :cl-ppcre)
    (ql:quickload :drakma)
    (ql:quickload :contextl)
