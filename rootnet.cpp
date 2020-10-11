#include	<algorithm>
#include	<iostream>
#include	<stdint.h>
#include	<unistd.h>
#include	"wyhash.h"
#include	<fstream>
#include	<cfloat>
#include	<vector>
#include	<cmath>
using	namespace	std;
const	uint64_t	batch=1ull<<24;
vector<float>	target,	root;
uint64_t	size,	seed=wyhash64(time(NULL),0);

bool	load_matrix(const	char	*F,	vector<float>	&M,	unsigned	&R,	unsigned	&C) {
	ifstream	fi(F);
	if(!fi) {	cerr<<"fail to open "<<F<<'\n';	return	false;	}
	string	buf;	R=C=0;
	while(getline(fi,buf))	if(buf.size()) {
		char	*p=(char*)buf.data(),	*q;
		for(;;) {	
			q=p;	float	x=strtod(p,	&p);
			if(p!=q)	M.push_back(x);	else	break;
		}
		R++;
	}
	fi.close();
	if(M.size()%R) {	cerr<<"unequal column\t"<<F<<'\n';	return	false;	}
	C=M.size()/R;	cerr<<F<<'\t'<<R<<'*'<<C<<'\n';
	return	true;
}

double	sgd(float	eta){
	double	loss=0;
	for(uint64_t	b=0;	b<batch;	b++){
		uint64_t	i=wy2u0k(wyrand(&seed),size),	j=wy2u0k(wyrand(&seed),size),	k=0;
		float	ma=-1;
		for(uint64_t	l=0;	l<size;	l++){
			float	d=root[i*size+l]*root[l*size+j];
			if(d>ma){	ma=d;	k=l;	}
		}
		loss-=target[i*size+j]*logf(fmaxf(ma,FLT_MIN))+(1-target[i*size+j])*logf(fmaxf(1-ma,FLT_MIN));
		ma=eta*(target[i*size+j]-ma);	float	dik=ma*root[k*size+j],	dkj=ma*root[i*size+k];
		root[i*size+k]+=dik*root[i*size+k]*(1-root[i*size+k]);
		root[k*size+j]+=dkj*root[k*size+j]*(1-root[k*size+j]);
	}
	return	loss/batch;
}

int	main(int	ac,	char	**av){
	if(ac!=2){	cerr<<"GraphRoot matrix.txt\n";	return	0;	}
	{
		unsigned	rn,	cn;
		if(!load_matrix(av[1],target,rn,cn))	return	0;
		if(rn!=cn){	cerr<<av[1]<<" is not square matrix!\n";	return	0;	}
		size=rn;
	}
	root.resize(size*size);
	for(size_t	i=0;	i<root.size();	i++)	root[i]=wy2u01(wyrand(&seed));
	size_t	gr=0;	double	l0=DBL_MAX;	float	eta=0.1;	cerr.precision(5);	cerr.setf(ios::fixed);
	for(size_t	it=0;	gr<5;	it++){
		double	l=sgd(eta);
		cerr<<it<<'\t'<<l<<'\t'<<eta<<'\n';
		if(l>l0){	gr++;	eta/=10;	}
		l0=l;
	}
	ofstream	fo("GraphRoot.txt");
	fo.precision(0);	fo.setf(ios::fixed);
	for(size_t	i=0;	i<size;	i++){
		for(size_t	j=0;	j<size;	j++)	fo<<floor(10*root[i*size+j])<<' ';
		fo<<'\n';
	}
	fo.close();
	return	0;
}
