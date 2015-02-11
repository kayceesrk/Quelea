---
title: Bank Account Benchmark
layout: post
permalink: bank-account-test.html
---
### Building the Benchmark

Navigate to `~/git/quelea/tests/BankAccount`, and run `make`. On the
stdout, along with GHC's build information, you should find something
similar to the following getting printed:

![ba-make]({{ site.baseurl }}/assets/ba-make.png)

The above lines indicate the consistency class for each Bank Account
operation (i.e., `Deposit`, `Withdraw` and `GetBalance`), along with
the time taken to classify the operation, and also the total time
consumed by the contract classification procedure. Similar information
is also displayed for Bank Account transactions (namely, `saveTxn`,
and `totalBalanceTxn`). If things went well, you should be able to
observe that compile-time overhead of classifying each operation is
very low, and the total overhead of contract classification is roughly
the sum of overheads incurred in each case.

Succesfully building the benchmark results in four binaries -
`BankAccount_EC`, `BankAccount_CC`, `BankAccount_SC`, and
`BankAccount_Q`, corresponding to curves named _EC_, _CC_, _SC_ and
_Q_ in the graph shown in Fig. 9(a) of our [paper]({{ site.pldiurl }}).

### Running the Benchmark

First, we run a basic experiment to measure latency and throughput,
when a single client sends 1000 successive requests (with an
inter-request delay of 1ms (1000 μs)). We take the measurements for
each of the _EC_, _CC_, _SC_, and _Q_ cases. The commands to run, and
relevant parts of the output generated in our sample runs are shown
below:

##### EC
    ./BankAccount_EC --kind Daemon --measureLatency --delayReq 1000 --numThreads 1 --numRounds 1000

![ba-ec1]({{ site.baseurl }}/assets/ba-ec1.png)

##### CC

    ./BankAccount_CC --kind Daemon --measureLatency --delayReq 1000 --numThreads 1 --numRounds 1000

![ba-cc1]({{ site.baseurl }}/assets/ba-cc1.png)


##### SC
    ./BankAccount_SC --kind Daemon --measureLatency --delayReq 1000 --numThreads 1 --numRounds 1000

![ba-sc1]({{ site.baseurl }}/assets/ba-sc1.png)

##### Q

    ./BankAccount_Q --kind Daemon --measureLatency --delayReq 1000 --numThreads 1 --numRounds 1000

![ba-q1]({{ site.baseurl }}/assets/ba-q1.png)

<!-- 
The commandline arguments are explained [here]({{ site.baseurl }}/print-usage.html). By default, latency is measured every 100
requests. A sample run is shown below ( commandline arguments):
-->


