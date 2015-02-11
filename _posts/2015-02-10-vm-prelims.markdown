---
title: Getting started with Quelea VM
layout: post
permalink: prelims.html
---

#### Booting the VM

Once you have downloaded the VM tarball, and untar'ed it, you will see
a virtual machine hard disk file named `UbuntuQuelea.vmdk`. The file
can be opened either with Virtual Box
([instructions](http://techathlon.com/how-to-run-a-vmdk-file-in-oracle-virtualbox/))
or VMWare player
([instruction](http://kb.vmware.com/selfservice/microsites/search.do?language=en_US&cmd=displayKC&externalId=2010196)). 
We suggest that you allocate atleast 2GB of memory, and atleast 2
processors to the VM. 

Once VM boots, you will see a login screen:

![loginscreen]({{ site.baseurl }}/assets/loginscreen.png)

Select the user with display name _Quelea_, and enter _queleapldi_ for
password. Please ignore any warnings about system errors that Ubuntu
throws.

#### Starting Cassandra

Quelea uses Cassandra as its data store. VM comes with Cassandra
installed. The first thing to do after successfully booting the VM is
to start the cassandra service:

+ Open a terminal (Press right _command_ or _windows_ key, and type
  _terminal_).
+ Run `cassandra > /dev/null`

Cassandra service should now run in the background listening on port
9042 for new connections. We can verify this by opening a Cassandra
Query Languagae (CQL) shell:

![cqlsh]({{ site.baseurl }}/assets/cqlsh.png)

#### Getting familiar with Quelea source repository

VM maintains a local git repository of Quelea at `~/git/quelea`. The
sub-directory `src` contains Quelea's Haskell source files, along with
a cabal build file named `Codeec.cabal`. The cabal package of Quelea
is called Codeec (for historic reasons), and is already installed in
the VM. However, should you chose to do a `git pull` (not required),
you can build and install latest version of Codeec by simply running a
`cabal install` under the `src` directory. 

Tests (benchmarks) are located in the sub-directory called `tests`:

![queleadirs]({{ site.baseurl }}/assets/queleadirs.png)

A short description of benchmarks has been provided on p.9 of our
[draft paper](http://gowthamk.github.io/docs/quelea.pdf)

### Building and running benchmarks

Contract classification is a static component of Quelea;
classification of operation contracts (as `Strongly Consistent`,
`Causally Consistent` etc.), and transaction contracts (as `Read
Committed`, `Monotonic Atomic View` etc.) happens when a Quelea
application is compiled. In order to evaluate the performance of
contract classification, it is therefore imperative to build the
benchmarks and observe the output. Performance of the run-time
component can then be evaluated by running the binaries generated.

For each benchmark, instructions on how to build and execute the
benchmark, and guidelines on how to interpret the data generated are
provided in the following pages:

+ [BankAccount]({{ site.baseurl }}/bank-account-test.html) benchmark
+ [LWW Transactions]({{ site.baseurl }}/lww-txn-test.html) benchmark
