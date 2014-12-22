#include "stdlib.h"
#include "stdbool.h"
#include "stdio.h"

void helpSize(int* a, char* str)
{
	char* temp;
	int t = strtol(str,&temp,10);
	if(t && !(*temp))
		(*a) = t;
}

void main(int argc, char **argv)
{
	int a = 1024;
	int b = 4096;
	char* filename = "input.txt";
	if(argc > 1)
		filename = argv[1];
	if(argc > 2)
		helpSize(&a,argv[2]);
	if(argc > 3)
		helpSize(&b,argv[3]);


	char* array = (char*)calloc(a,sizeof(char));
	char* commands = (char*)calloc(b,sizeof(char));
	char* c = array;
	char* p = commands;
	int depth = 0;
	bool skip = false;
	bool forwards = true;

	FILE* file = fopen(filename,"r");
	if(file)
	{
		do
		{
			*p = getc(file);
			p++;
		}while(p < (commands+b) && *p != EOF);
		fclose(file);
		p = commands;

		while(*p)
		{
			switch (*p)
			{
				case '<':
					if(!skip && c > array) c--;
					break;
				case '>':
					if(!skip && c < (array+a)) c++;
					break;
				case '+':
					if(!skip) (*c)++;
					break;
				case '-':
					if(!skip) (*c)--;
					break;
				case '.':
					if(!skip) putchar(*c);
					break;
				case ',':
					if(!skip) (*c) = getchar();
					break;
				case '[':
					if(skip) {
						if(forwards) depth++;
						else if(!depth) {
							skip = false; 
							forwards = true;
						} else depth--;
					} else
					if(!(*c)) skip = true;
					break;
				case ']':
					if(skip) {
						if(forwards) {
							if(!depth) skip = false;
							else depth--;
						} else depth++;
					} else
					if((*c)) {
						skip = true; 
						forwards = false; 
					}
				default:
					break;	
			}
			if(forwards) {
				if(p < (commands+b)) p++;
				else break;
			}else {
				if(p > (commands)) p--;
				else break;
			}
		}
	}
	free(array);
	free(commands);
}