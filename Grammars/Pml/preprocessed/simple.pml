typedef me {
    int a;
};

proctype System () {
    int x = 3;
    int y;
    me b;
    do 
    :: x < 5 -> 
        printf("@@@ %d LOG System running, ID: %d\n", _pid, b.a); 
        x = x + 1;
        b.a = x;
    :: else -> break;
    od
}

init {
    int x = 1;
    int y = 3;
    int z = x + y;
    run System();
    do 
    :: z < 10 -> printf("hello world %d\n", z); z = z + 1;
    :: else -> printf("else test\n"); break;
    od
}