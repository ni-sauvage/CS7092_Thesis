proctype System () {
    int x = 3;
    do 
    :: x < 5 -> printf("@@@ %d LOG System running...\n", x); x = x + 1;
    :: else -> break;
    od
}

init {
    int x = 1;
    int y = 3;
    int z = x + y;
    run System();
    do 
    :: z < 10 -> printf("hello world %d", z); z = z + 1;
    :: else -> printf("else test"); break;
    od
}