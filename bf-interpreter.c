#include "stdlib.h"
#include "stdbool.h"
#include "stdio.h"

void main()
{

char* array = (char*)calloc(1024,sizeof(char));
char* end = (array+1024);
char* commands = (char*)calloc(4096,sizeof(char));
char* c = array;
char* p = commands;
int depth = 0;
bool skip = false;
bool forwards = true;

FILE* file = fopen("input.txt","r");
if(file)
{
do
{
	*p = getc(file);
	p++;
}while(p < (commands+4096) && p != EOF);
p = commands;
while(*p)
{
switch (*p)
{
	case '<':
		if(!skip && c != array) c--;
		break;
	case '>':
		if(!skip && c != end) c++;
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
		if(skip) { if(forwards) {if(!depth) skip = false; else depth--;} else depth++;} else
		if((*c)) { skip = true; forwards = false; }
	default:
		break;	
}
if(forwards) p++; else p--;
}
}
}