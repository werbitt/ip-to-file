IP To File
==========

IP to File is a utility to write you IP addresss to a file.
-----------------------------------------------------------

My intention with this tool is to set up a chron job to write my
public IP to a pseudo hosts file on a shared drive, as an
alternative to using a dynamic IP service

Usage
-------
ip-to-file -f OUTPUT-FILE -n HOSTNAME -u IP-SERVICE-URL

Example
-------
ip-to-file -f ~/Dropbox/hosts -n example.global -u http://ipecho.net/plain
