/* run.config
   STDOPT: +"-lannot=RCC -lannot-max-mutation 1 -inline-calls=check,check_code_integrity"
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
    __cm_start();
    if(p1 != p2) return 1;
    __cm_target();
    return 0;
}

int f4(int password){
    __cm_start();
    if(check(password,PASSWORD)) return 1;
    if(check(PASSWORD,password)) return 1;
    if(check(password,PASSWORD)) return 1;

    __cm_target();
    return 0;
}
int f5(int password){
    __cm_start();
    __cm_target();
    return 0;
}

int f6(int password){
    __cm_start();
    __cm_double_if();
    if(password != PASSWORD) return 1;
    if(password != PASSWORD) return 1;

    __cm_target();
    return 0;
}

typedef enum {secfalse = 0x55aa55aa, sectrue = 0xaa55aa55} secbool;

#define SIZE 10
#define SUM 10
int integrity[SIZE];

secbool inline check_code_integrity(){
    int sum=0;
    /*@ loop pragma UNROLL SIZE+1, "completely";*/
    for(int i = 0; i < SIZE; i++)
        sum += integrity[i];
    if(sum == SUM) return sectrue;
    return secfalse;
}

int f7(){
    __cm_start();
    secbool chk1=check_code_integrity();
    if(chk1 != sectrue) return 1;
    secbool chk2=check_code_integrity();
    if(!chk2 == sectrue) return 1; // incorrect countermeasure
    __cm_target();
    return 0;
}