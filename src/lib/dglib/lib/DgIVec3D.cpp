////////////////////////////////////////////////////////////////////////////////
//
// DgIVec3D.cpp: DgIVec3D class implementation
//
// Version 6.1 - Kevin Sahr, 5/23/13
//
////////////////////////////////////////////////////////////////////////////////

#include <climits>

#include "DgBase.h"
#include "DgIVec3D.h"

////////////////////////////////////////////////////////////////////////////////

const DgIVec3D& DgIVec3D::undefDgIVec3D = DgIVec3D(INT_MAX, INT_MAX, INT_MAX);

////////////////////////////////////////////////////////////////////////////////
const char*
DgIVec3D::fromString (const char* str, char delimiter)
{
   char delimStr[2];
   delimStr[0] = delimiter;
   delimStr[1] = '\0';

   char* tmpStr = new char[strlen(str) + 1];
   strcpy(tmpStr, str);

   // Get i, j, and k
   char* tok;

   long long int	iIn(0),
	   		jIn(0),
                        kIn(0);

   try
    {
   	tok = strtok(tmpStr, delimStr);
   	iIn = dgg::util::from_string<long long int>(tok);
	
   	tok = strtok(NULL, delimStr);
   	jIn = dgg::util::from_string<long long int>(tok);

   	tok = strtok(NULL, delimStr);
   	kIn = dgg::util::from_string<long long int>(tok);
    }  
   catch(...)
    {
      ::report("DgIVec3D::fromString() invalid value in string " + string(tok), 
               DgBase::Fatal);
    }

   setI(iIn);
   setJ(jIn);
   setK(kIn);

   unsigned long long int offset = (tok - tmpStr) + strlen(tok) + 1;
   if (offset >= strlen(str)) 
    return 0;

   return &str[offset];

} // const char* DgIVec3D::fromString

////////////////////////////////////////////////////////////////////////////////
