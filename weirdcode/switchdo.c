

int f(int i) {
    switch(i){  
        case 1: do {i++; puts("1");
        case 2:     i++; puts("2"); 
        case 3:     i++; puts("3"); } while(i<10);
        case 4: puts("4");
    }
    return i;
}

main(){
    f(2);
}
