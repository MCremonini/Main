//////////////////////////////////////////////////////////////////////////
//
// write a function which takes two strings and returns a string containing 
// only the characters found in both strings in the order of the first string given.
// Linear solution
//
//////////////////////////////////////////////////////////////////////////
char *MatchChars( const char a[], const char b[] ) 
{
	char ch;
	unsigned long ulMask = 0x0;
	int i = 0;

	char offset = 'a' + 1;

	while( b[i] != '\0' ) 
	{
		ch = b[i] - offset;
		ulMask |= 0x1<<ch;
		i++;
	}
	// now scan a, so common letters are saved in same order as a
	char *common = new char[sizeof(char) * i];
	i = 0; int j = 0;
	while( a[i] != '\0' ) 
	{
		ch = a[i] - offset; 
		if (ulMask & (0x1<<ch)) 
		{
			common[j++] = a[i];
		}
		i++;
	}
	common[j] = '\0';

	/* 'common' needs to be freed in main */
	return common;
}


//////////////////////////////////////////////////////////////////////////
//
// Reversing string
//
//////////////////////////////////////////////////////////////////////////
void ReverseString(char *a)
{
	int length = 0;
	while( a[length++] != '\0' ); // counts also termination character 
	length--; // dont want to count termination character
	for( int i = 0; i < length/2; i++ )
	{
		/*
		a[i] = a[i] + a[length-i-1];
		a[length-i-1] = a[i] - a[length-i-1];
		a[i] = a[i] - a[length-i-1];
		*/
		// XOR algorithm
		a[i] ^= a[length-i-1];
		a[length-i-1] ^= a[i];
		a[i] ^= a[length-i-1];
	}
	a[length] = '\0';
}


//////////////////////////////////////////////////////////////////////////
//
// count the number of bits set to 1 in a variable or register
//
//////////////////////////////////////////////////////////////////////////
int HiBitCounting( int x )
{
	int count = 0;
	for( ; x; count++ )
		x &= x-1;
	return count;
}