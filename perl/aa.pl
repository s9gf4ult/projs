#!/usr/bin/perl

@aa=`find ./ -iname "*.cpp"`;
for ($ii=@aa;$ii>0;$ii--)
{
    print(@aa[$ii]);
}
print("hello world");
