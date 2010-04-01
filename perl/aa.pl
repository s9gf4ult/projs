#!/usr/bin/perl

@aa=`find /home/razor/projs -iname "*.cpp"`;
foreach $aa(@aa)
{
    print($aa);
}
print("hello world\n");
foreach $aaa ("wfsfs", "wefwfe", "wefwef")
{
    print("select * from sfasd where wefwef = ${aaa}\n");
}
