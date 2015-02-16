---
title: Instructions to run experiments on EC2
layout: post
permalink: ec2-instructions.html
---

In this setup, we will create a 2 node Quelea cluster with a single client node
to run the experiments. 

### Part 1: Booting up the instances

+ Log into Amazon EC2 console: [http://aws.amazon.com/ec2/](http://aws.amazon.com/ec2/)
+ Change the region to US West (Oregon):

![ec2-1]({{ site.baseurl }}/assets/ec2-1.png)

+ Click on EC2 Console (the first option in the top left of the control panel).
+ Lets launch the instances now. Click on Launch instance:

![ec2-2]({{ site.baseurl }}/assets/ec2-2.png)

+ Click on “Community AMI”, and search for "ami-f7c79cc7”. You should see an AMI named quelea_pldi15_050220151653. Select this AMI.

![ec2-3]({{ site.baseurl }}/assets/ec2-3.png)

+ Choose any instance type. The free-tier eligible instance will not have sufficient memory to compile the programs. For this set up, choose m3.large instance. Be warned that this costs $0.14 an hour at the time of writing. Click "Next: Configure instance details.”
+ Enter 3 for the number of instances. 2 for the server and 1 for the client. Click “Next: Add storage”. -> “Next: Tag Instance” -> “Next: Configure Security Group”.
+ Ensure “Create new security group” is selected. And for Type, choose “All TCP” from the drop down menu. 

![ec2-4]({{ site.baseurl }}/assets/ec2-4.png)

+ Click “Launch”.
+ If you don’t already have a key pair, create a new key pair, and download it to a secure location (say, your `~/.ssh` directory). Change the permissions of the pem file to “400”. Finally, launch Instances. Click “View Instances” to get back to the console and wait for the instances to boot and initialise.

### Part 2: Setting up the cluster

+ Open three terminals, and log into all three instances using their public IPs
(listed on EC2 dashboard, against the instance id), and your pem file. For
example, if your pem file is saved as `~/.ssh/name-key-pair-uswestoregon.pem`,
to log on to an instance wiht public IP of `54.149.150.102`, you should the
following:

      ssh -i ~/.ssh/name-key-pair-uswestoregon.pem ubuntu@54.149.150.102

![ec2-5]({{ site.baseurl }}/assets/ec2-5.png)

+ On every instance, run:

      cd ~Quelea/src
      git pull origin master
      cabal install

  To build and install latest sources.
+ Of the three instances (say A, B & C) pick two to be servers (say A & B). 
+ Get the private IP of machine A. Type “hostname” on A. If the output is “ip-10-36-2-14”, then the private ip is “10.36.2.14”.
+ On A and B:

       cd ~/Quelea/Ec2Files
       sudo sh ec2snitch_init.sh <private_ip_of_A>

  This sets up the cassandra configuration files appropriately. Please note that
  you should use the private IP of A on **both** A and B.
+ On A and B:

        cd ~/dsc-cassandra-2.0.12
        sudo ./bin/cassandra -f

  You should see that the cassandra replicas have seen each other. There should be something along the lines of

      INFO 15:08:07,492 Handshaking version with /10.41.30.134
      INFO 15:08:09,090 Node /10.41.30.134 is now part of the cluster
      INFO 15:08:09,097 Handshaking version with /10.41.30.134
      INFO 15:08:09,104 InetAddress /10.41.30.134 is now UP

indicating that the hand-shaking is complete.

+ Ctrl-C the both cassandra instances to kill them. Restart them with their outputs redirected to /dev/null. It might be good to start a screen or a tmux session on the instances since we will be needing multiple terminal windows.

      sudo ./bin/cassandra -f > /dev/null

+ On A, open a new terminal window. Type `~/dsc-cassandra-2.0.12/bin/cqlsh`
to start the cassandra command line client. Then, create a new keyspace with
the following command: 

      create keyspace Codeec with replication = { 'class' : 'NetworkTopologyStrategy', 'us-west-2' : 2 };
      quit
      
  Here ‘us-west-2’ is the name of the datacenter and the argument ‘2’ is the replication factor. Since we use a fully replicated cluster for Quelea, this is equal to the number of server replicas.

### Part 3: Running BankAccount_Q

Lets run the `BankAccount_Q` benchmark. The instructions for other benchmarks
are the same. At this point, the 2 node cassandra cluster on A and B are
running.

+ On all 3 instances, 

      cd ~/Quelea/tests/BankAccount
      make BROKER=-DLBB BankAccount_Q

  The "BROKER=-DLBB” is required so that LoadBalancingBroker is used in a multi-replica deployment.

+ On A:

       ./BankAccount_Q --kind Create --brokerAddr <private_ip_of_A>
       ./BankAccount_Q --kind Broker --brokerAddr <private_ip_of_A>

  to start the broker.

+ Open a new terminal window on A.

       ./BankAccount_Q --kind Server --rtsArgs “-N2” --brokerAddr <private_ip_of_A>

+ On B:

       ./BankAccount_Q --kind Server --rtsArgs “-N2” --brokerAddr <private_ip_of_A>

+ On C:
        ./BankAccount_Q --kind Client --rtsArgs “-N2” --numThreads 16 --brokerAddr <private_ip_of_A>

You should see the results of the run as usual.

+ Finally, kill all the processes including the broker and the server. Delete the tables create invoking the following command on A.

       ./BankAccount_Q --kind Drop

The instructions for running other experiments are similar.

### Expanding the cluster

More server nodes can be added to the cluster by following the same instructions as above. The only change is that the keyspace creation. For a 5, node cluster, the command would be

     create keyspace Codeec with replication = { 'class' : 'NetworkTopologyStrategy', 'us-west-2' : 5 };
