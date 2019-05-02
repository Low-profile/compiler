int add1(int i) {
    return i + 1;
}

int main( void )  {
    int a,b;
    for( a = 0 ; a < 10 ; a = a+1 ){
        b = add1(b);
    }
    return b;
}