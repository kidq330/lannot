/* run.config
   STDOPT: +"-lannot=RCC -lannot-max-mutation 1 -inline-calls=check"
 */

#define PASSWORD 12345

int f1(int password){
    __cm_start();
    if(password != PASSWORD) return 1;
    if(password != PASSWORD) return 1;

    __cm_target();
    return 0;
}

int f2(int password){
    __cm_start();
    __cm_double_if();
    if(password != PASSWORD || password != PASSWORD) return 1;
    __cm_target();
    return 0;
}

int f3(int password){
    __cm_start();
    if(password == PASSWORD){
        if(password != PASSWORD) return 1;
        __cm_step();

        if(password != PASSWORD) return 1;
        __cm_target();
        return 0;
    }
    return 1;
}

int inline check(int p1, int p2){
    __lannot_start_inline();
    __cm_start();
    if(p1 != p2) return 1;
    __cm_target();
    return 0;
    __lannot_end_inline();
}

int f4(int password){
    __cm_start();
    if(check(password,PASSWORD)) return 1;
    if(check(PASSWORD,password)) return 1;
    if(check(password,PASSWORD)) return 1;

    __cm_target();
    return 0;
}
