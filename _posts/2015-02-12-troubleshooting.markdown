---
title: Troubleshooting
layout: post
permalink: troubleshooting.html
---

### ZMQError - Address already in use

Here is how the error looks:

![ZMQ-error-1]({{ site.baseurl }}/assets/zmq-error-1.png)

This error occurs when a previous run of the experiment did not
perform proper cleanup before terminating, resulting on zombie
processes:

![ZMQ-error-2]({{ site.baseurl }}/assets/zmq-error-2.png)

The fix is to `kill` such processes.


### Cannot add already existing column family

Here is how the error looks like:

![table-delete-error-1]({{ site.baseurl }}/assets/table-delete-error-1.png)

This error occurs when a previous run of the experiment failed to
delete all Cassandra column families that were created during the
run. Straightforward fix is to run the same binary with `--kind Drop`
option:

![table-delete-error-2]({{ site.baseurl }}/assets/table-delete-error-2.png)

Another way is to use `cqlsh` to manually drop tables:

    $ cqlsh
    cqlsh> USE Codeec;
    cqlsh> DROP TABLE bankaccount;
    cqlsh> DROP TABLE bankaccount_lock;
    cqlsh> DROP TABLE bankaccount_gc;

### Unconfigured column family error

Here is how the error looks like:

![unconfigured-error-1]({{ site.baseurl }}/assets/unconfigured-error-1.png)

This error occurs when a table being used by the current run of the
experiment was dropped in a concurrent run. The fix is to simply rerun
the experiment.

### One of the client threads hangs

For example, consider the following sample run:

![thread-hangs-error-1]({{ site.baseurl }}/assets/thread-hangs-error-1.png)

Even though `numThreads` option is set to 4, only three threads ever
make progress. The other thread simply hangs. This error occurs
frequently on a VM that is severely resource constrained. The exact
cause of the bug is not yet known. Increasing the resources allocated
to the VM will prevent this error from occuring. If this is not
possible, we suggest that you rerun the experiment until you generate
an execution where all threads make progress.
