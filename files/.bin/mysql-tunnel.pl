#!/usr/bin/perl

# MySQL Tunnel Tool for MacOS X and Linux
# Copyright (c) 2009 Linode, LLC
# Author: Philip C. Paradis <pparadis@linode.com>
# Usage: mysql-tunnel.pl [start|stop]
# Access a MySQL database server via an SSH tunnel.

$local_ip    = "127.0.0.1";
$local_port  = "3306";
$remote_ip   = "127.0.0.1";
$remote_port = "3306";
$remote_user = "username";
$remote_host = "hostname.yourdomain.com";

$a = shift;
$a =~ s/^\s+//;
$a =~ s/\s+$//;

$pid=`ps ax|grep ssh|grep $local_port|grep $remote_port`;
$pid =~ s/^\s+//;
@pids = split(/\n/,$pid);
foreach $pid (@pids)
{
 if ($pid =~ /ps ax/) { next; }
 split(/ /,$pid);
}

if (lc($a) eq "start")
{
 if ($_[0]) { print "Tunnel already running.\n"; exit 1; }
 else
 {
  system "ssh -f -L $local_ip:$local_port:$remote_ip:$remote_port $remote_user\@$remote_host -N";
  exit 0;
 }
}
elsif (lc($a) eq "stop")
{
 if ($_[0]) { kill 9,$_[0]; exit 0; }
 else { exit 1; }
}
else
{
 print "Usage: mysql-tunnel.pl [start|stop]\n";
 exit 1;
}

