#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <vector>
#include "dbf.h"

// File type for Microsoft's Visual FoxPro files

#define VISUALTOILET 48

// Amount of excrement Microsoft puts _after_ the field specs in the header of
// Visual FoxPro files, part of Microsoft's "embrace and extend" strategy.

#define CRAP 263

void rtrim(char *text)
{
	char *p;
	int len = strlen(text);
	int i;

	p = text;	
	for (i = len - 1; i >= 0; i--) {
		if (*(p + i) == ' ')
			*(p + i) = '\0';
		else
			break;
	}
        // return text;
}

void ltrim(char *text)
{
	char *p, *q;

	p = q = text;
	while (*q == ' ')
		q++;
	
  	while (*q != '\0') {
		*p = *q;
		p++;
		q++;
	}
	*p = '\0';

}

short fox2short(char *fox)
{
	char c[2];

	memcpy(c, fox + 1, 1);
	memcpy(c + 1, fox, 1);

	return (short)(*c);
}

bool is_null(char *fld)
{
	char *p = fld;
	int i = strlen(fld);
	int n;
	bool ret = true;

	for (n = 0; n < i; n++) {
		if (*p != ' ') {
			ret = false;
			break;
		}
		p++;
	}
	return ret;
}


dbf_fspec::dbf_fspec()
{
	name[0] = '\0';
	length = 0;
	type = ' ';
	decimals = 0;
	offset = 0;
}

dbf_fspec::dbf_fspec(char b[32], ulong l)
{
    init(b, l);
}

dbf_fspec::dbf_fspec(char *n, uchar t, uchar l, uchar d, ulong o)
{
    init(n, t, l, d, o);
}

void dbf_fspec::init(char b[32], ulong l)
{
    char *p;
    
    memcpy(fbuf, b, 32);
    p = fbuf;
    memcpy(name, p, 11);
    p = fbuf + 11;
    memcpy(&type, p, 1);
    p = fbuf + 16;
    memcpy(&length, p, 1);
    p = fbuf + 17;
    memcpy(&decimals, p, 1);    
    offset = l;
    return;
}

void dbf_fspec::init(char *n, uchar t, uchar l, uchar d, ulong o)
{
    memcpy(name, n, 11);
    name[10] = '\0';
    type = t;
    length = l;
    decimals = d;
    offset = o;
   
    memset(fbuf, '\0', 32);
    memcpy(fbuf, name, 11);
    memcpy(fbuf + 11, &type, 1);
    memcpy(fbuf + 16, &length, 1);
    memcpy(fbuf + 17, &decimals, 1);
    return;
}



dbf_fspec::~dbf_fspec()
{
}

void dbf_fspec::dump() {
	printf("%10s %c %6hu %3hu %6lu\n", name, type,
			length, decimals, offset);
}

// this ctor is designed to be used in conjunction with from_rdb()

dbf_table::dbf_table()
{
	version_id = 3;
	memset(last_update, 0, 3);
	rcount = 0;
	hlength = 0;
	rlength = 0;
	fcount = 0;
	memset(filename, 0, MAXPATH);
	datafile = NULL;
	rbuf = NULL;
	hbuf = NULL;
	memo_file = NULL;
	fields = NULL;
}

/* =====================================================================
 * This routine requires a nosql file (not a full rdb file) in the form:
 * fieldname fieldtype fieldlen fielddec
 * --------- --------- -------- --------
 * The field names don't matter, but they must be in that order.
 * This routine reads in the file and constructs the appropriate header
 * and field objects. Then it writes those back out to the dbf file.
 * N.B.: If an existing dbf file by <fname> already exists, it is
 * truncated and overwritten.
 * ================================================================== */ 

void dbf_table::from_rdb(char *afname, char *fname)
{
    char buf[MAXPATH + 1];
    char rdb_n[11];
    char rdb_t;
    uchar rdb_l;
    uchar rdb_d;
    ulong rdb_o;
    char rdb_f[MAXPATH];
    char *g[4];
    char *p;
    dbf_fspec *f;
    std::vector <dbf_fspec *> fs;
    unsigned int i;
    FILE *rdb_fp;

//    printf("in from_rdb\n");
    
    if (strlen(afname) < MAXPATH)
        strcpy(rdb_f, afname);
    else {
        memcpy(rdb_f, afname, MAXPATH - 1);
	rdb_f[MAXPATH] = '\0';
    }

    if (strlen(fname) < MAXPATH)
        strcpy(filename, fname);
    else {
        memcpy(filename, fname, MAXPATH - 1);
	filename[MAXPATH] = '\0';
    }
    
    
    rdb_fp = fopen(rdb_f, "r");

    if (rdb_fp) {

//	printf("datafile is opened\n");
	
	// first two lines are irrelevant
	fgets(buf, MAXPATH, rdb_fp);
	fgets(buf, MAXPATH, rdb_fp);

	rdb_o = 1;
	fcount = 0;
	
	while (fgets(buf, MAXPATH, rdb_fp)) {
//	    printf("<%s>", buf);
	    p = buf;
	    while (*p == ' ') p++;
	    if (*p == '\n' || *p == '\0')
	        continue;
	    
	    g[0] = strtok(p, "\t");
	    g[1] = strtok(NULL, "\t");
	    g[2] = strtok(NULL, "\t");
	    g[3] = strtok(NULL, "\t");
	    
	    if (strlen(p) <= 10)
	        strcpy(rdb_n, p);
	    else {
	        memcpy(rdb_n, p, 10);
		rdb_n[10] = '\0';
	    }
	    for (i = 0; i < strlen(rdb_n); i++)
	        rdb_n[i] = toupper(rdb_n[i]);
	    rdb_t = toupper(*g[1]);
	    rdb_l = (uchar)atoi(g[2]);
	    rdb_d = (uchar)atoi(g[3]);

//	    printf("name = %s, type = %c, length = %d, dec = %d\n", rdb_n, rdb_t, rdb_l, rdb_d);
	    
	    f = new dbf_fspec(rdb_n, rdb_t, rdb_l, rdb_d, rdb_o);
	    
//	    printf("made new dbf_fspec\n");
	    
	    rdb_o += rdb_l;
	    
	    fs.push_back(f);
	    fcount++;
	    
	}
	
//	printf("done with loop\n");
	
        rlength = rdb_o;
        hlength = 32 + fcount * 32 + 1;
	version_id = 3;
        rcount = 0;
	memset(&last_update, '\0', 3);
	rbuf = new char[rlength];
	hbuf = new char[hlength];

	// set header values

	p = buf;
	memcpy(p, &version_id, sizeof(uchar));
	p = buf + 1;
	memcpy(p, &last_update, 3);
	p = buf + 4;
	memcpy(p, &rcount, sizeof(ulong));
	p = buf + 8;
	memcpy(p, &hlength, sizeof(ushort));
	p = buf + 10;
	memcpy(p, &rlength, sizeof(ushort));
	
	// filename (ugh!)
//	p = rdb_f;
//	for (i = 0; i < strlen(rdb_f); i++)
//	    rdb_f[i] = tolower(rdb_f[i]);
//	if (p = strstr(rdb_f, ".rdb"))
//	    strcpy(p, ".dbf");
//	strcpy(filename, rdb_f);

        fclose(rdb_fp);
  
	datafile = fopen(filename, "w+");

	f = new dbf_fspec[fcount];
	for (i = 0; i < fcount; i++)
	    f[i] = *(fs[i]);
	fields = f;
	
	// write out header info
	
	fwrite(buf, 1, 32, datafile);
	for (i = 0; i < fcount; i++)
	    fwrite(fs[i]->fbuf, 1, 32, datafile);
	
	fputc(0x0d, datafile);
	
    } // if

    return;
}

dbf_table::dbf_table(char *fname)
{
    if (init(fname)) {
	printf("Can't open datafile!\n");
    }
}

int dbf_table::init(char *fname)
{
    char *p, *fbuf;
    ulong running_offset;
    unsigned int i;
    dbf_fspec *f = NULL;
    bool has_memo = false;
    char mfilen[MAXPATH + 1];
    char ext[5];
    uchar len = 0;

    strcpy(filename, fname);
    datafile = fopen(filename, "r");
    
    if (datafile) {
	
	hbuf = new char[32];
	
	fread(hbuf, 1, 32, datafile);

	p = hbuf;

	// set header values

	memcpy(&version_id, p, sizeof(uchar));
	p = hbuf + 1;
	memcpy(&last_update, p, 3);
	p = hbuf + 4;
	memcpy(&rcount, p, sizeof(ulong));
	p = hbuf + 8;
	memcpy(&hlength, p, sizeof(ushort));
	p = hbuf + 10;
	memcpy(&rlength, p, sizeof(ushort));
	
	// create a record buffer

	rbuf = new char[rlength + 1];
			
	// how many fields?

	if (version_id == VISUALTOILET)
	  fcount = ((hlength - CRAP) / 32) - 1;
	else
	  fcount = (hlength / 32) - 1;
	
	// build array of field specs

	f = new dbf_fspec[fcount];

	fbuf = new char[32];

	// now field descriptors

	running_offset = 1;
		
	for (i = 0; i < fcount; i++) {

	    // read in a field spec
	    fread(fbuf, 1, 32, datafile);

	    f[i].init(fbuf, running_offset);
	    memcpy(&len, fbuf + 16, 1);
	    running_offset += len;
		    
	} // for

	// memo file stuff (if applicable)

	switch (version_id) {
	 case 0x03:
	    has_memo = false;
	    break;
	 case 0x83:
	    strcpy(ext, ".dbt");
	    has_memo = true;
	    break;
	 case 0xf5:
	    strcpy(ext, ".fpt");
	    has_memo = true;
	    break;
	}

	if (has_memo) {

	    strcpy(mfilen, filename);
	    p = strstr(mfilen, ".dbf");
	    if (p)
	      strcpy(p, ext);

	    memo_file = new dbf_memo(mfilen);
	    
	}

    } // if
    else {
	// couldn't open datafile
	return 1;
    }
    
    fields = f;

    return 0;
}

dbf_table::~dbf_table()
{
	fclose(datafile);
	delete rbuf;
	delete hbuf;
	delete memo_file;
	delete fields;
}

char dbf_table::what_type(unsigned u)
{
	dbf_fspec *f = fields;

	return f[u].type;
}

unsigned dbf_table::what_length(unsigned u)
{
	dbf_fspec *f = fields;
	return f[u].length;
}

unsigned dbf_table::what_decimals(unsigned u)
{
	dbf_fspec *f = fields;
	return f[u].decimals;
}

int dbf_table::read(ulong recnum)
{
	int failed, numread, ret;

	failed = fseek(datafile, hlength + ((recnum - 1) * rlength), SEEK_SET);
	if (!failed) {
		numread = fread(rbuf, 1, rlength, datafile);		
		if (numread != 0)
			ret = 0;
		else
			ret = -1;
	}
	else
		ret = -1;

	return ret;
}

bool dbf_table::is_deleted()
{
	if (rbuf)
		return (rbuf[0] == '*' ? true : false);
	else
		return true;
}

void dbf_table::rdump(char rsep, int squash)
{
	unsigned int i;
	int len;
	char *s;
	char *tbuf;
	dbf_fspec *f;
	char *nbuf;

	len = rlength + fcount + sizeof('\n') + 1;
	tbuf = new char[len];
	memset(tbuf, ' ', len);

	f = fields;

#if 0
		// old, but useful code
		for (i = 0; i < fcount; i++) {
			*s = rsep;
			s++;
			memcpy(s, rbuf + f[i].offset, f[i].length);
			s += f[i].length;
		}
#endif

	s = tbuf;
	*s = *rbuf;
	s++;
	*s = '\0';
	nbuf = NULL;
	len = 0;

	for (i = 0; i < fcount; i++) {

		if (f[i].length > len) {
			if (nbuf != NULL)
				delete nbuf;
			len = f[i].length;
			nbuf = new char[len + 2];
		}			

		nbuf[0] = rsep;
		nbuf[1] = '\0';

		memcpy(nbuf + 1, rbuf + f[i].offset, f[i].length);
		nbuf[f[i].length + 1] = '\0';

		if (squash) {
			s = nbuf + 1;
			ltrim(s);
			rtrim(s);
		}
			
		strcat(tbuf, nbuf);

	}

	printf("%s\n", tbuf);
	delete nbuf;
	delete tbuf;

}

// dump records in text/delimited format

void dbf_table::dump(char rsep, int squash) {

	unsigned n, end = rcount;

	for (n = 1; n <= end; n++) {
		read(n);
		rdump(rsep, squash);
	}

}

char *dbf_table::what_rbuf()
{
	return rbuf;
}

void dbf_table::dump_header()
{
	dbf_fspec *f;
	f = fields;	

	printf("\n***** xBase File Specs *****\n\n");

	printf("Filename:      %s\n", filename);
	printf("Version ID:    %x\n", (unsigned)version_id);
	printf("Record count:  %lu\n", rcount);
	printf("Header length: %hu\n", hlength);
	printf("Record length: %hu\n", rlength);
	printf("Field count:   %u\n\n", fcount);

}

void dbf_table::dump_fspecs() {
	dbf_fspec *f;
	f = fields;	
	unsigned int i;

	printf("*** Field Specs ***\n\n");
	printf("   Name    T Length Dec Offset\n");
	printf("---------- - ------ --- ------\n");

	for (i = 0; i < fcount; i++) {
		f->dump();
		f++;
	}

	printf("\n");

}
		
void dbf_table::header2sql()
{
	ulong i;
	dbf_fspec *f;
	bool first = true;
	char fldname[11];
	char *fname;
	char *p;

	f = fields;

	fname = new char[strlen(filename) + 1];
	strcpy(fname, filename);
	// sloppily remove .dbf from filename ;-}
	p = strchr(fname, '.');
	if (p)
		*p = '\0';

	printf("create table \"%s\" (\n", fname);
	for (i = 0; i < fcount; i++) {
		if (!first)
			printf(",\n");
		else
			first = false;

		// convert name string to lowercase
		strcpy(fldname, f->name);
		p = fldname;
		while (*p)
			*p++ = tolower(*p);

		switch (f->type) {
			case 'C':
				printf("\t%s char(%hu)", fldname, f->length);
				break;
			case 'M':
				printf("\t%s varchar", fldname);
				break;
			case 'D':
				printf("\t%s date", fldname);
				break;
			case 'N':
				if (f->decimals == 0)
					printf("\t%s int", fldname);
				else
					printf("\t%s decimal(%hu, %hu)", fldname, f->length, f->decimals);
				break;
			case 'L':
				printf("\t%s bool", fldname);
				break;
		} // switch
		f++;
	} // for
	printf(" );\n");
}

void dbf_table::data2sql() {

	char *p, *fname;
	unsigned int n, end;

	fname = new char[strlen(filename) + 1];
	strcpy(fname, filename);
	// sloppily remove .dbf from filename ;-}
	p = strchr(fname, '.');
	if (p)
		*p = '\0';

	printf("copy \"%s\" from stdin;\n", fname);
	end = rcount;
	for (n = 1; n <= end; n++) {
		read(n);
		if (!is_deleted())
			rec2sql();
	}
	printf("\\.\n");	
}

void dbf_table::rec2sql()
{
	unsigned int i;
	char *s;
	char *tbuf = new char[rlength + fcount + sizeof('\n') + 1];
	dbf_fspec *f;
	char *fbuf = new char[rlength + 1];
	long memonum;
	char *memo;

	f = fields;

	s = tbuf + 1;

	// delete flag
	memcpy(tbuf, rbuf, 1);

	// # printf("insert into %s values ( ", fname);
	// # printf("copy \"%s\" from stdin;\n", fname);

	for (i = 0; i < fcount; i++) {

		memcpy(fbuf, rbuf + f[i].offset, f[i].length);
		memset(fbuf + f[i].length, '\0', 1);
		rtrim(fbuf);

		if (i != 0)
			printf("\t");

		switch (f[i].type) {
			case 'C':
				printf("%s", fbuf);
				break;
			case 'N':
				// if (fbuf[f[i].length - 1] == ' ')
				if (is_null(fbuf))
					printf("\\N");
				else
					printf("%s", fbuf);
				break;
			case 'L':
				if (*fbuf == 'Y' || *fbuf == 'y' || *fbuf == 'T' || *fbuf == 't')
					printf("t");
				else
					printf("f");
				break;
			case 'D':
				// test for blank date
				if (!is_null(fbuf))
					printf("%c%c-%c%c-%c%c%c%c", fbuf[4], fbuf[5], fbuf[6], fbuf[7], fbuf[0], fbuf[1], fbuf[2], fbuf[3]);
				else
					printf("\\N");
				break;
			case 'M':
				memonum = atol(fbuf);
				if (memonum) {
					memo = memo_file->get_memo(memonum);
					if (memo)
						printf("%s", memo);
					else
						printf("\\N");
					delete memo;
				}
				else
					printf("\\N");
				break;
		}


	}

	printf("\n");

}

void dbf_table::rec2nosql()
{
	unsigned int i;
	char *s;
	char *tbuf = new char[rlength + fcount + sizeof('\n') + 1];
	dbf_fspec *f;
	char *fbuf = new char[rlength + 1];

	f = fields;

	s = tbuf + 1;

	// delete flag
	memcpy(tbuf, rbuf, 1);

	for (i = 0; i < fcount; i++) {

		memcpy(fbuf, rbuf + f[i].offset, f[i].length);
		memset(fbuf + f[i].length, '\0', 1);
		rtrim(fbuf);

		if (i != 0)
			putchar('\t');

		switch (f[i].type) {
			case 'C':
				printf("%s", fbuf);
				break;
			case 'N':
				if (!is_null(fbuf))
					printf("%s", fbuf);
				break;
			case 'L':
				if (*fbuf == 'Y' || *fbuf == 'y' || *fbuf == 'T' || *fbuf == 't')
					printf("T");
				else
					printf("F");
				break;
			case 'D':
				// test for blank date
				if (!is_null(fbuf))
		    printf("%s", fbuf);
				break;
			case 'M':
		    printf("%s", fbuf);
				break;
		}


	}

	putchar('\n');

}

void dbf_table::dump_nosql(char *afname)
{
    dbf_fspec *f;
    f = fields;	
    unsigned int i, j;
    FILE *rsfile;

    // header first
    
    for (i = 0; i < fcount; i++) {
	if (i != 0)
	    putchar('\t');
	printf("%s", f[i].name);
    }

    putchar('\n');

    for (i = 0; i < fcount; i++) {
	if (i != 0)
	    putchar('\t');
	for (j = 0; j < f[i].length; j++)
	    putchar('-');
    }
    putchar('\n');

    // now records

    for (i = 1; i <= rcount; i++) {
	read(i);
	if (!is_deleted())
	    rec2nosql();	
    }

    // @@@ dump header and such to afname nosql file
    rsfile = fopen(afname, "w+");
    if (rsfile) {
	fprintf(rsfile, "field_name\tfield_type\tfield_len\tfield_dec\n");
	fprintf(rsfile, "----------\t----------\t---------\t---------\n");
	for (i = 0; i < fcount; i++)
	    fprintf(rsfile, "%s\t%c\t%d\t%d\n", f[i].name, f[i].type, f[i].length, f[i].decimals);
	fclose(rsfile);
    }
    
}

#define MAXREC 1024

#define MIN(a,b) (a < b ? a : b)

void dbf_table::rdb_append(char *s)
{
    char fn[MAXPATH];
    FILE *ns;
    char nb[MAXREC];
    dbf_fspec *f;
    unsigned int nfields, i;
    bool first, done, found;
    std::vector <int> fvect;
    char *a, *b, *p;
        
    if (strlen(s) < MAXPATH)
        strcpy(fn, s);
    else {
        memcpy(fn, s, MAXPATH - 1);
	fn[MAXPATH] = '\0';
    }
    
//    printf("filename is: %s\n", fn);
    
    ns = fopen(fn, "r");
    if (!ns) {
	printf("Can't open %s!\n", fn);
        return;
    }

    freopen(filename, "r+", datafile);
    
    f = fields;

    // parse first line to get field names

    nfields = 0;
    if (fgets(nb, MAXREC, ns)) {
	p = nb;
	*p = toupper(*p);
	while (*p++)
	    *p = toupper(*p);
	a = strchr(nb, '\n');
	*a = '\t';
//	printf("read line of file: %s\n", nb);
	
	a = nb;
	
	first = true;
	done = false;
        while (!done) {
	    
	    b = strchr(a, '\t');
	    if (b) {
		*b = '\0';
	    }
	    else
	        break;
	    
//	    printf("fname is %s, fields is %d\n", a, fcount);
	    if (a) {
		found = false;
		for (i = 0; i < fcount; i++) {
		    if (!strncmp(f[i].name, a, strlen(a))) {
			fvect.push_back(i);
			found = true;
			nfields++;
//			printf("field FOUND! It's field %d\n", i);
			break;
		    }
		}
		if (!found)
		    fvect.push_back(-1);
//		printf("%s %s %d\n", a, f[i].name, i);
	    }
	    else
	        done = true;
	    
	    a = ++b;
	    
	} // while
    }
    else
        return;
    
//    printf("done reading fields\n");
    
    // throw away the next line
    fgets(nb, MAXREC, ns);
    
    fseek(datafile, 0, SEEK_END);
    
    while (fgets(nb, rlength, ns)) {
	p = strchr(nb, '\n');
	*p = '\t';
	a = nb;
//	printf("line was: <%s>\n", nb);
	memset(rbuf, ' ', rlength);
	for (i = 0; i < nfields; i++) {

	    b = strchr(a, '\t');
	    if (b)
	        *b = '\0';
	    else
	        break;
	    
	    if (fvect[i] >= 0) {
//		printf("%s %s %d %d %d\n", f[i].name, a, strlen(a), MIN(f[fvect[i]].length, strlen(a)), f[fvect[i]].offset);
	        memcpy(rbuf + f[fvect[i]].offset, a, MIN(f[fvect[i]].length, strlen(a)));
	    }

	    a = ++b;
	    
	}
	// intact rbuf, so write out to file
	fwrite(rbuf, 1, rlength, datafile);
	rcount++;

    }
    fclose(ns);
    fseek(datafile, 4, SEEK_SET);
    fwrite(&rcount, 1, sizeof(ulong), datafile);
    freopen(filename, "r", datafile);
}

/* ====== .fpt file format =======
	Header is 512 bytes, most of which is unused
	All numeric values outside of actual memo fields are represented
	in left to right order in hexadecimal.
	First four bytes are a pointer to the next free block.
	Next two bytes are unused.
	Next two bytes are the size of a block.
	Memos may occupy more than one consecutive block, and start on even
	block boundaries.
	The address of the block is the number of the block multiplied by the
	block size.
	Within a memo, the first two bytes are the block signature; 0 for
	picture data and 1 for text.
	Next four bytes are the length of the memo in bytes.
	What follows is the block data. The actual data in the block is not
	terminated by anything, so the length of the block is important.
	What follows from the end of the data to the end of the block may be
	garbage.
================================== */

// convert the odd FoxPro file integer format to int
unsigned int fp2int(unsigned int n)
{
	int m = 
		((n << 24) & 0xff000000) +
		((n << 8) & 0x00ff0000) +
		((n >> 24) & 0x000000ff) +
		((n >> 8) & 0x0000ff00);
	return m;
}

dbf_memo::dbf_memo(char *fname)
{
	strcpy(memofilename, fname);
	memofile = fopen(memofilename, "r");

	if (memofile) {

		fread(&nextfree, sizeof(int), 1, memofile);
		nextfree = fp2int(nextfree);
		fread(&blocksize, sizeof(short), 1, memofile);
		fread(&blocksize, sizeof(short), 1, memofile);
		blocksize = (blocksize << 8) | (blocksize >> 8);
	}

}

dbf_memo::~dbf_memo()
{
	fclose(memofile);
	delete mbuf;
}

// have to kill newlines in memos...

char *dbf_memo::get_memo(long n)
{
	unsigned int i;
	char *p;

	fseek(memofile, n * blocksize, SEEK_SET);
	fread(&blocksig, sizeof(int), 1, memofile);
	blocksig = fp2int(blocksig);
	fread(&memolen, sizeof(int), 1, memofile);
	memolen = fp2int(memolen);
    if (mbuf != NULL)
		delete mbuf;
	mbuf = new char[memolen + 1];
	fread(mbuf, sizeof(char), memolen, memofile);
	mbuf[memolen] = '\0';
	p = mbuf;
	for (i = 0; i < strlen(mbuf); i++) {
		if (*p == '\r')
			*p = '\\';
	        p++;
	}

	return mbuf;
}
