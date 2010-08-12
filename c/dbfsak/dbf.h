#ifndef DBF_H
#define DBF_H

#include <stdio.h>

#ifndef uchar
#define uchar unsigned char
#endif

#ifndef ushort
#define ushort unsigned short
#endif

#ifndef ulong
#define ulong unsigned long
#endif

#define MAXPATH 127

/*
	Classes for using xBase files.
*/

class dbf_fspec
{
   friend class dbf_table;
	
   // field descriptor

   char fbuf[32];
   
   char name[11];
   uchar type;
   uchar length;
   uchar decimals;

   unsigned long offset;

   dbf_fspec();
   dbf_fspec(char[32], ulong);
   dbf_fspec(char *, uchar, uchar, uchar, ulong);
   ~dbf_fspec();
   void init(char[32], ulong);
   void init(char *, uchar, uchar, uchar, ulong);
   
   void dump();

};

class dbf_memo 
{
	char memofilename[MAXPATH];
	FILE *memofile;
	unsigned short blocksize;
	unsigned int nextfree;
	
	int blocksig;
	unsigned int memolen;
	char *mbuf;

	public:

	dbf_memo(char *);
	~dbf_memo();
	char *get_memo(long);	

};


class dbf_table
{
   // main dbf header from file

   uchar version_id;
   char last_update[3];
   ulong rcount;
   ushort hlength;
   ushort rlength;

   unsigned int fcount;
   char filename[MAXPATH];
   FILE *datafile;
   char *rbuf;
   char *hbuf;

   dbf_memo *memo_file;

   dbf_fspec *fields;

public:
   
   dbf_table();
   dbf_table(char *);
   int init(char *);
   ~dbf_table();
   int read(ulong);
   unsigned what_rlength() { return (unsigned)rlength; }
   ulong what_rcount() { return rcount; }
   unsigned what_fcount() { return fcount; }
   char what_type(unsigned);
   unsigned what_length(unsigned);
   unsigned what_decimals(unsigned);
   char *what_rbuf();
   bool is_deleted();
   char *what_filename() { return filename; }
   void rdump(char, int);
   void dump(char, int);
   void dump_header();
   void dump_fspecs();
   void dump_nosql(char *);
   void header2sql();
   void data2sql();
   void rec2sql();
   void rec2nosql();
   void from_rdb(char *, char *);
   void rdb_append(char *);

};

#endif
