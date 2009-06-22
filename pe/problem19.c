#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int monthCnt[12] = { 31,28,31,30,31,30,31,31,30,31,30,31, };

bool isLeapyear(int year){
	// return true if year is leapyear
	return (year%4 == 0 && year%100 != 0 || year%400 == 0);
}
int third(int year , int month){
	int i,j,counter = 0;
	for(i=0;i<=year-1900;i++){
		for(j=0;j<(i!=year-1900 ? 12 : month-1);j++){
			counter +=(( j!=1 ) ? monthCnt[j] : isLeapyear(i+1900) ? 29 : 28);
		}
	}
	return counter;
}

int main(void){
        int year,month,counter=0;
        for(year=1901;year<=2000;year++){
                for(month=1;month<=12;month++){
                        if(third(year,month)%7 == 6) counter++;  
                }
        }
        
        printf("%d",counter);


	return 0;
}
