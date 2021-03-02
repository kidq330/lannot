/* run.config
   STDOPT: +"-lannot=ADC" 
 */

int f(int wcond){
	int wres=0, wx=1,a=1;
	if(wcond){
		a = a + 1;
		wres = a;
		wx = 0;
	}
	if(wx){
		wres += 2*a;
		wres *= a;
	}
	return wres;
}


int g(int n)
{
	int i;
	for(i = 0;i<n;i = i + f(i));
	return i;
}