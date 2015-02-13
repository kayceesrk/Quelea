---
title: Getting started with Quelea VM
layout: post
permalink: prelims.html
---

#### Booting the VM

The VM is packaged as a `.ova` file, which can be opened either with VirtualBox
([instructions](http://www.virtualbox.org/manual/ch01.html#ovf)) or VMWare
player
([instructions](https://pubs.vmware.com/fusion-5/topic/com.vmware.fusion.help.doc/GUID-275EF202-CF74-43BF-A9E9-351488E16030.html)).
We suggest that you allocate atleast 2GB of memory, and atleast 2 processors to
the VM. The username and password for the VM is `quelea`.

#### Starting Cassandra

Quelea uses Cassandra as its data store. VM comes with Cassandra installed. The
first thing to do after successfully booting the VM is to start the cassandra
service:

+ Open a terminal (Press right _command_ or _windows_ key, and type
  _terminal_).
+ Run `sudo ~/dsc-cassandra-2.0.12/bin/cassandra -f > /dev/null`

Cassandra service should now run in the foreground, listening on port
9160 for new connections. We can verify this by opening a Cassandra
Query Languagae (CQL) shell:

![cqlsh]({{ site.baseurl }}/assets/cqlsh.png)

#### Getting familiar with Quelea source repository

VM maintains a local git repository of Quelea at `~/git/quelea`. The
sub-directory `src` contains Quelea's Haskell source files, along with
a cabal build file named `Codeec.cabal`. The cabal package of Quelea
is called Codeec (for historic reasons), and is already installed in
the VM. However, it is advisable to do a `git pull origin master`, and
install the latest version of Codeec by running a `cabal install`
under the `src` directory.

Tests (benchmarks) are located in the sub-directory called `tests`:

![queleadirs]({{ site.baseurl }}/assets/queleadirs.png)

A short description of benchmarks has been provided on p.9 of our
[draft paper](http://gowthamk.github.io/docs/quelea.pdf)

### Building and running benchmarks

Contract classification is a static component of Quelea; classification of
operation contracts (as `Strongly Consistent`, `Causally Consistent` etc.), and
transaction contracts (as `Read Committed`, `Monotonic Atomic View` etc.)
happens when a Quelea application is <u>compiled</u>. In order to evaluate the
performance of contract classification, it is therefore imperative to build the
benchmarks and observe the output. Performance of the run-time component can
then be evaluated by running the binaries generated.

For each benchmark, instructions on how to build and execute, and guidelines on
how to interpret the data generated are provided in the following pages:

+ [BankAccount]({{ site.baseurl }}/bank-account-test.html) benchmark
+ [LWW Transactions]({{ site.baseurl }}/lww-txn-test.html) benchmark
+ [RUBiS]({{ site.baseurl }}/rubis-test.html) benchmark
+ [LWW GC]({{ site.baseurl }}/lww-gc-test.html) benchmark

### Running experiments in cloud

Please note that the experimental results reported in the paper were conducted
on a 5 node Quelea cluster, where each node was a c3.4xlarge machine with 16
vCPUs and 30GiB of main memory. As a result, the scale of the results are much
smaller in the VM. Nevertheless, you should be able to observe similar patterns
as reported in the paper, at a smaller scale in the VM. If you are interested
in reconstructing experiments in a cloud setting, instructions are
[here]({{ site.baseurl }}/ec2-instructions.html).

### Troubleshooting

We have tried to ensure that benchmarks can be run as smoothly as possible.
However, you might still encounter some issues due to some concurrency bugs
lurking in our software. We have documented some of the known issues, and quick
fixes [here]({{ site.baseurl }}/troubleshooting.html).
