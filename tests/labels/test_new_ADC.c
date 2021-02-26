/* run.config
   STDOPT: +"-lannot=ADC" 
 */

int f(int wcond){
	int wres=0, wx=1,a;
	a = 12;
	if(wcond){
		a = a + 1;
		wres = a;
		wx = 0;
	}
	if(wx){
		wres += 2*a;
		wres*=a;
	}
	return wres;
}