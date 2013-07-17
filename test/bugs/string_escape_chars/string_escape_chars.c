 #include <stdio.h>
 #include <string.h>
 
 static char str[128] = "12345\t";
 
 int main(int argc, char** argv)
 {
         if(strlen(str) != 6)
                 return 1;
         else
                 return 0;
 }
