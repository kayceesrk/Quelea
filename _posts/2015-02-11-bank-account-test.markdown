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
and `totalBalanceTxn`). Observe that compile-time overhead of
classifying each operation is very low, and the total overhead of
contract classification is roughly the sum of overheads incurred in
each case.

Succesfully building the benchmark results in four binaries -
`BankAccount_EC`, `BankAccount_CC`, `BankAccount_SC`, and
`BankAccount_Q`, that correspond to curves named _EC_, _CC_, _SC_ and
_Q_ in the graph shown in Fig. 9(a) of our
[paper]({{ site.pldiurl }}).

### Running the Benchmark

First, we run a basic experiment to measure latency and throughput,
when a single client sends 1000 successive requests (with an
inter-request delay of 1ms (1000 μs)). We take the measurements for
each of the _EC_, _CC_, _SC_, and _Q_ cases. Client requests are
roughly 25% `getBalance`s, 25% `deposit`s, and 50% `withdraw`s. _EC_,
_CC_ and _SC_ binaries run all operations under eventual, causal and
strong consistency levels, respectively. On the other hand, the _Q_
testcase runs operations at appropriate consistency levels as
determined by the contract classification procedure. The commands to
execute, and output generated in our sample runs are shown below:

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


The experiment can now be repeated varying the number of client
threads (`--numThreads`) from 2 to 8. Screenshots capturing
measurements for sample runs when `--numThreads` is 4 are shown below:

##### EC

![ba-ec2]({{ site.baseurl }}/assets/ba-ec2.png)

##### CC

![ba-cc2]({{ site.baseurl }}/assets/ba-cc2.png)

##### SC

![ba-sc2]({{ site.baseurl }}/assets/ba-sc2.png)

##### Q

![ba-q2]({{ site.baseurl }}/assets/ba-q2.png)


As shown in the sample runs, you should be able to observe that:

+ Throughput is highest when all operations are executed under
  eventual consistency (_EC_). Causal consistency (_CC_) throughput is
  comparable to that of _EC_. However, in both cases, `withdraw`
  operation can exhibit incorrect behaviour.
+ Throughput is lowest when all operations are executed under Strong
  Consistency (_SC_). 
+ Testcase `BankAccount_Q`, which executes operations at appropriate
  consistency levels, delivers performance which is between the above
  two extremes.

