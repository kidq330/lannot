#include "gd.h"


int my_strcmp(const char *s1, const char *s2)
{
  if (s1 == s2)
    return (0);
  while (*s1 == *s2++)
    if (*s1++ == '\0')
      return (0);
  return (*(unsigned char *)s1 - *(unsigned char *)--s2);
}


int search(char *keyName, char **base, int nmemb)
{
  int iter;
  /*for (iter = 0; iter < nmemb; iter++)
    if (my_strcmp (keyName, base[iter]) == 0)
      return 1;
  return 0;
	*/return 1;
}


/* Greatly, GREATLY simplified. There's a bunch of cruft that doesn't
 * have to do with the manipulation of "string". */
void gdImageStringFTEx (char *string, int nondetEntry) {
  int next;
  int encoding;
  int i;
  int ch;
  int len;
	int byte;


  encoding = nondetEntry;
	
  if (encoding > 2 || encoding < 0)
    return;

  next = 0;
  /* Unsafe read -- next can be out of bounds. */

	/* BAD */
  for (i=0; string[next] != EOS; i++)
    {
      /* grabbing a character and storing it in an int
       *
       * this'll fill the low-order byte, and keep more space free for
       * extra bytes for Unicode encoding, etc.
       */
      ch = string[next];

      /* carriage returns */
      if (ch == '\r')
	{
	  next++;
	  continue;
	}
      /* newlines */
      if (ch == '\n')
	{
	  next++;
	  continue;
	}


      switch (encoding)
        {
        case gdFTEX_Unicode:
          {
            //len = gdTcl_UtfToUniChar (string + next, &ch);
			char *p;
			int indiceP = 0;
			char entity_name_buf[ENTITY_NAME_LENGTH_MAX+1];
			char *key;
			int ii, n = 0;
			int res = 0;

			char *entities[] = { "AElig", "Aacute", "Acirc"};
			  
			byte = (unsigned char)(string[next]);

			if (byte == '&') { 
			
		
			byte = (unsigned char)(string[next+1]);
			if (byte == '#') {
			  byte = (unsigned char)(string[next+2]);
			  if (byte == 'x' || byte == 'X') {
				for (ii = 3; ii < 8; ii++) {
				  byte = (unsigned char)(string[next+ii]);
				  if (byte >= 'A' && byte <= 'F')
				    byte = byte - 'A' + 10;
				  else if (byte >= 'a' && byte <= 'f')
				    byte = byte - 'a' + 10;
				  else if (byte >= '0' && byte <= '9')
				    byte = byte - '0';
				  else
				    break; /* end FOR */
				  n = (n * 16) + byte;
				}
			  }
			  else {
				for (ii = 2; ii < 8; ii++) {
				  byte = (unsigned char)(string[next+ii]);
				  if (byte >= '0' && byte <= '9')
				    n = (n * 10) + (byte - '0');
				  else
				    break;
				}
			  }
			  
			  if (byte == ';') {
				len = ++ii;
				goto endFun;
			  }
			}
		   
			else {
			  key = p = entity_name_buf;
			  for (ii = 1; ii < 1 + ENTITY_NAME_LENGTH_MAX; ii++) {
				byte = (unsigned char)(string[next+ii]);
				if (byte == EOS)
				  break;
				if (byte == ';') {
				  p[indiceP] = EOS;
				  indiceP++;
				  /* This was originally bsearch(), which is a polymorphic
				   * function that uses void pointers and size arguments. I
				   * think most tools and tool users would rewrite the program
				   * as a first phase, using type information to choose the
				   * implementation of bsearch.
				   */
				  res = search(key, entities, NR_OF_ENTITIES);
				  if (res != 0) {
				    len = ++ii;
					goto endFun;
				  }
				  break;
				}
				p[indiceP] = byte;
				indiceP++;
			  } /* End FOR */
			}
		  }

			  len = 1;
			  byte = (unsigned char)(string[next]);
			  if (byte < 0xC0) {
				len = 1;
			  }
			  else if (byte < 0xE0) {
				if ((string[next+1] & 0xC0) == 0x80) {
				  len = 2;
				}
				len = 1;
			  }
			  else if (byte < 0xF0) {
				if (((string[next+1] & 0xC0) == 0x80) && ((string[next+2] & 0xC0) == 0x80)) {
				  len = 3;
				}
				len = 1;
			  }



			 endFun: 
			  
            next += len;
          }
          break;
        case gdFTEX_Shift_JIS:
          {
            unsigned char c;
            c = (unsigned char) string[next];
            if (0xA1 <= c && c <= 0xFE)
              {
                next++;
              }
            /* We're incrementing next twice, which could make us skip
             * the terminating EOS character. The read of "string"
             * could then be out of bounds. */

            // start Nikolai correction :
        if ( string[next] != EOS)
            // end Nikolai correction
            next++;
          }
          break;
        case gdFTEX_Big5:
          {
            ch = (string[next]) & 0xFF;	/* don't extend sign */
            next++;
            if (ch >= 161
                && string[next] != EOS)
              {
                next++;
              }
          }
          break;
        }
    }
}
/*
int main ()
{
  	char in[12] = 0;
	int nondetEntry;
	int i = 0;

	nondetEntry = Frama_C_interval(-50, 50);

	for(i=0; i<12; i++)
		in[i] = Frama_C_interval(-128, 127);
	
	
  in[INSZ-1] = EOS;
  gdImageStringFTEx(in, nondetEntry);
  return 0;
}*/

