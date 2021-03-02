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


int g(int wa)
{
	wa = 12;
	wa = 13;
	int wi = 0;
	while (1){
		if(!(wi<10)) break;
		int tmp = wi;
		tmp++;
        wi = tmp;
	}

	return wa;
}