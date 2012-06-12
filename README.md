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

    $ sbcl oj.lisp <judge> <command> <number>

### ECL

    $ ecl -shell oj.lisp -- <judge> <command> <number>

### Clozure CL

    $ ccl --load oj.lisp -- <judge> <command> <number>

Where arguments are below:

### &lt;judge&gt;

Online Judge System. Current supporting systems are:

- [PKU Judge Online](http://poj.org/)

### &lt;command&gt;

- get: Get sample input and output.
- submit: Submit programs.

### &lt;number&gt;

Problem number.
