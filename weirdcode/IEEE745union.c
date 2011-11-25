#include <stdio.h>

union {
	unsigned int i;
	float f;
} data;


main(){
	data.f = 1.0;
	printf("%X\n", data.i);
}
