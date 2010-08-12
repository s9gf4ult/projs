#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "dbf.h"

static char rcsid[] = "$Id: dbfsak.cpp,v 1.1.1.1 2001/12/10 00:36:36 paulf Exp $";
static char version[] = VERSION;

void usage() {
	printf("\ndbfsak -- xBase Swiss Army Knife\n");
	printf("Usage: dbfsak OPTIONS filename\n");
	printf("\tOPTIONS are:\n");
    printf("\t-a <filename>: append from rdb/nosql file\n");
	printf("\t-b: show records\n");
	printf("\t-d: set field delimiter (default is ':')\n");
	printf("\t-e: show field descriptions\n");
	printf("\t-h: help!\n");
	printf("\t-i: display header info\n");
    printf("\t-n <filename>: dump content to nosql/rdb file (on STDOUT)\n");
    printf("\t\tand make <filename> structure file\n");
    printf("\t-r <filename>: make dbf from <filename> rdb/nosql file\n");
	printf("\t-s: dump in PostreSQL SQL script format\n");
	printf("\t-t: trim browse fields\n");
	printf("\t-v: display version\n");
	printf("NOTE: -s parameter without -b, -e, or -i does nothing\n\n");
}

void show_version() {
	printf("dbfsak version %s\n", version);
}

/*
dbview options:

-b browse
-d set delimiter
-e show field descriptions
-h help
-i display db info
-o omit db records
-r reserve fieldnames from being translated
-t trim browse fields
-v display version
*/

#define F_BROWSE 0x01
#define F_FDESC  0x02
#define F_HDR    0x04
#define F_SQL    0x08
#define F_TRIM   0x10
#define F_RDB    0x20
#define F_NOSQL  0x40
#define F_APPEND 0x80

#define F_SQLERR 0x07

// externs for getopt()
extern char *optarg;
extern int optind, opterr, optopt;

int main(int argc, char *argv[])
{
	unsigned long end;
	char *fname, *afname;
	int ret;
	int flags = 0;
	char delim = ':';
	char *user;
    dbf_table *mydbf;

	opterr = 1;
	while ((ret = getopt(argc, argv, "a:bd:ehin:r:stv")) != -1) {
		switch (ret) {
		    case 'a':
		        // rdb_append
		        flags |= F_APPEND;
		        afname = new char[strlen(optarg) + 1];
		        strcpy(afname, optarg);
		        break;
			case 'b':
				// browse
				flags |= F_BROWSE;
				break;
			case 'd':
				// set delimiter
				delim = *optarg;
				break;
			case 'e':
				// show field descriptions
				flags |= F_FDESC;
				break;
			case 'h':
				// help
				usage();
				return(0);
			case 'i':
				// display header info
				flags |= F_HDR;
				break;
		 case 'n':
		    // dump table to nosql database
		    flags |= F_NOSQL;
		    afname = new char[strlen(optarg) + 1];
		    strcpy(afname, optarg);
		    break;
		    
		 case 'r':
		    // make table from rdb file
		    flags |= F_RDB;
		    afname = new char[strlen(optarg) + 1];
		    strcpy(afname, optarg);
		    break;
		    
			case 's':
				// SQL output
				flags |= F_SQL;
				break;
			case 't':
				// trim browse fields
				flags |= F_TRIM;
				break;
			case 'v':
				// version
				show_version();
				return(0);
			default:
				usage();
				return(0);
		} // switch
	} // while

	if (flags == 0) {
		usage();
		return(0);
	}

	fname = new char[strlen(argv[optind]) + 1];
	strcpy(fname, argv[optind]);

    if (flags & F_RDB) {
	mydbf = new dbf_table();
	mydbf->from_rdb(afname, fname);
	return 0;
    }
    
    if (flags & F_NOSQL) {
	mydbf = new dbf_table(fname);
	mydbf->dump_nosql(afname);
	return 0;
    }
    
    if (flags & F_APPEND) {
	mydbf = new dbf_table(fname);
	mydbf->rdb_append(afname);
	return 0;
    }
    
    mydbf = new dbf_table(fname);
    end = mydbf->what_rcount();

    if (flags & F_SQL) {

	if (!(flags & F_SQLERR)) {
		
	    usage();
	    return(0);
	}

	user = getlogin();
	
	printf("\\connect - %s\n", user);

	if (flags & F_HDR || flags & F_FDESC)
	    mydbf->header2sql();
	if (flags & F_BROWSE)
	    mydbf->data2sql();
	}
    else {
	// non-SQL
	
	if (flags & F_HDR)
	    mydbf->dump_header();
	if (flags & F_FDESC)
	    mydbf->dump_fspecs();
	if (flags & F_BROWSE)
	    mydbf->dump(delim, flags & F_TRIM);

    }

    return 0;

}

