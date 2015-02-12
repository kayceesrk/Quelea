---
title: RUBiS Benchmark
layout: post
permalink: rubis-test.html
---

RUBiS is an eBay-like auction that allows users to browse for items,
bid for items on sale, and pay for items from a wallet modelled after
a bank account. To support the rich functionality, RUBiS implements a
large number of operations and transactions requiring varying levels
of consistency and isolation, respectively. The primary purpose of
this benchmark is to study the impact of using Quelea in such a
real-world setting (Fig. 9(c) of the [paper]({{ site.pldiurl}})); We
would like to demonstrate significant performance gains obtained by
annotating operations/transactions with Quelea contracts, which would
otherwise need to be executed with strong consistency/isolation to
guarantee correctness. Another purpose of this benchmark is to
demonstrate the scalability of static contract classification, as
shown in the following section.

### Building the benchmark

Navigate to `~/git/quelea/tests/Rubis`, and run `make`. On the stdout,
along with GHC's build information, you should find something similar
to the following getting printed:

![rubis-make2]({{ site.baseurl }}/assets/rubis-make2.png)

The above lines indicate the consistency class for each RUBiS
operation, along with the time taken to classify the operation, and
also the total time consumed by the contract classification procedure.
Similar information is also displayed for RUBiS transactions: 

![rubis-make1]({{ site.baseurl }}/assets/rubis-make1.png)

As sample runs demonstrate, compile-time overhead of classifying each
operation/transaction is very low, and the total overhead of contract
classification is roughly the sum of overheads incurred in each case.

### Running the Benchmark
