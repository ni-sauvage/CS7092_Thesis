typedef me {
    int a;
};

inline inl(){
    printf("\n");
}

proctype System () {
    int x = 3;
    int y;
    me b;;
    start_loop:
    printf("START LOOP\n");
    do 
    :: x < 5 -> 
        atomic{
            printf("@@@ %d LOG System running, ID: %d", _pid, b.a); 
            printf("\n");
            x = x + 1;
        }
        b.a = x;
    :: y < 3 && x >= 5 -> 
        y++;
        x = 3;
        goto start_loop; 
    :: else -> goto end_loop;
    od
    end_loop:
    printf("@@@ Exited loop\n");
}


init {
    int x = 1;
    int y = 3;
    int z = x + y;
    run System();
    do 
    :: z < 10 -> printf("hello world %d\n", z); z++;
    :: else -> printf("else test\n"); break;
    od
}