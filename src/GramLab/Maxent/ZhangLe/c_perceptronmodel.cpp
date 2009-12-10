#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <list>
#include <string>
#include <utility>
#include <limits>
#include <cassert>
#include "c_perceptronmodel.h"

using namespace std;

typedef struct { 
  int labelsize ;
  int featsize ;
  double** params ;
} C_Perceptron ;

int c_eval_all(C_ZhangLeModel * mv,
	       int*    features,
	       double* featvals,
	       int     f_size,
	       int*    out_labels,
	       double* out_probs,
	       int     out_size) {
  C_Perceptron * m = (C_Perceptron *) mv;
  double* x = new double[out_size]; 
  double A = 0;
  int maxi = 0;
  double maxv = -1.0 * numeric_limits<double>::max() ;
  for(int l=0;l<out_size;l++) {
    out_labels[l] = l;
    x[l] = 0;
    for(int f=0;f<f_size;f++){
      x[l] += m->params[l][features[f]] * featvals[f];
    }
    if (x[l]>maxv) { maxv = x[l]; maxi = l;} 
        else { if ( maxv == x[l] ) { if ( l > maxi ) maxi = l ; } }
      
  }
  for (int l=0;l<out_size;l++) A += exp(x[l]-maxv);
  for (int l=0;l<out_size;l++) out_probs[l] = exp(x[l] - maxv - log(A));  
  double sum = 0 ;
  for (int l=0;l<out_size;l++) {
    assert(out_probs[l] >= 0 && out_probs[l] <= 1);
    sum += out_probs[l];
  }
  double epsi = 0.0000001;
  
  if (abs(sum-1.0) > epsi) { 
    fprintf(stderr,"Sum = %f\nA = %f\nlog(A) = %f\nmaxv = %f\n"
	    ,sum,A,log(A),maxv); 
    for (int l=0;l<out_size;l++) fprintf(stderr,"%f ",x[l]-maxv-log(A));
    fprintf(stderr,"\n");
    exit(1);
  }
  
  delete [] x;
  int index = 0;
  double value = 0;
  for (int l=0;l<out_size;l++){
    if (out_probs[l]>value){value=out_probs[l];index=out_labels[l];}
  }

  if(out_probs[maxi]!=out_probs[index] ){
    fprintf(stderr,"maxi=%d index=%d\n",maxi,index);
    for (int l=0;l<out_size;l++){
      fprintf(stderr,"label=%d x[l]=%f p=%f\n",out_labels[l],x[l],out_probs[l]);
    }
    fprintf(stderr,"\n");
    exit(1);
  }
  return out_labels[maxi];
}

C_ZhangLeModel * c_train_C_ZhangLeModel(int    iter,
					double r, // learning rate
					int*   labels,
					int*   row_sizes,
					int**  featids,
					double** featvals,
					int    colsize) {
    fprintf(stderr,"Starting perceptron training\n");
    fprintf(stderr,"Iterations: %d\n",iter);
    fprintf(stderr,"Learning rate: %f\n",r);
  int labelmax = 0;
  int maxfeatid = 0;
  for (int i=0;i<colsize;i++){
    if (labels[i]>labelmax) labelmax = labels[i];
    for (int j=0;j<row_sizes[i];j++){
      if (featids[i][j]>maxfeatid) maxfeatid = featids[i][j];
    }
  }
  C_Perceptron *m= new C_Perceptron();
  int c = 1;
  m->labelsize = labelmax+1;
  m->featsize = maxfeatid+1;
  m->params = new double*[m->labelsize];
  double ** params_a = new double*[m->labelsize];
  for (int i=0; i < m->labelsize; i++) { 
    m->params[i] = new double[m->featsize];
    params_a[i] = new double[m->featsize];
    for (int f=0;f < m->featsize;f++){
      m->params[i][f] = 0; params_a[i][f] = 0;
    }
  }
  for(int it=1;it<=iter;it++){
    for (int i=0; i<colsize;i++) {
      int* labs = new int[m->labelsize] ;
      double* probs = new double[m->labelsize];
      int y_hat = c_eval_all(m,featids[i],featvals[i],row_sizes[i],
			     labs,probs,m->labelsize);
      delete [] labs;
      delete [] probs;
      if (y_hat!=labels[i]) {
	for (int j=0;j<row_sizes[i];j++){
	  m->params[labels[i]][featids[i][j]] += r*featvals[i][j];
	  m->params[y_hat][featids[i][j]] -= r*featvals[i][j];
	  params_a[labels[i]][featids[i][j]] += ((double)c)*r*featvals[i][j];
	  params_a[y_hat][featids[i][j]] -= ((double)c)*r*featvals[i][j];
	}
      }
      c += 1;
    }
    int i;int error = 0;
    for(i=0;i<colsize;i++){
      int* labs = new int[m->labelsize] ;
      double* probs = new double[m->labelsize];
      int y_hat = c_eval_all(m,featids[i],featvals[i],row_sizes[i],
			     labs,probs,m->labelsize);
      delete [] labs;
      delete [] probs;
      if (y_hat!=labels[i]) error += 1;
    }
    fprintf(stderr,"Iteration %d: ",it);
    fprintf(stderr,"error: %2.4f\n",((double)error)/((double)i));
  }
  

      
  
  for(int l=0;l < m->labelsize;l++){ 
    for(int j=0;j < m->featsize;j++){
      m->params[l][j] = (m->params[l][j] - params_a[l][j]/((double)c));
    }
  }
  
  for(int i = 0; i < m->labelsize; i++) { 
    delete [] params_a[i];
  }
  delete [] params_a;
  return (C_ZhangLeModel *)m;
}

void c_destroy_C_ZhangLeModel(C_ZhangLeModel * m) {
  C_Perceptron * mm = (C_Perceptron *) m;
  for(int i = 0; i < mm->labelsize; i++) { 
    delete [] mm->params[i];
  }
  delete [] mm->params;
  delete mm ;
}


void c_save(C_ZhangLeModel *m,char* path) {
  C_Perceptron * mm = (C_Perceptron *) m;
  FILE * fh = fopen(path,"w");
  fprintf(stderr,"Saving labels: %d\n",mm->labelsize);
  fprintf(stderr,"Saving features: %d\n",mm->featsize);
  fprintf(fh,"%d\n",mm->labelsize);
  fprintf(fh,"%d\n",mm->featsize);
  for(int l=0;l < mm->labelsize ;l++){
    list<pair<int,double> > buffer;
    for(int f=0;f < mm->featsize;f++){
      if (mm->params[l][f] != 0)
	buffer.push_back(make_pair(f,mm->params[l][f]));
    }
    fprintf(fh,"%d",(int)buffer.size());
    for(list<pair<int,double> >::iterator i = buffer.begin();
	i != buffer.end(); i++)
      fprintf(fh," %d %lf",i->first,i->second); 
    fprintf(fh,"\n");
  }
  fclose(fh);
}

  
C_ZhangLeModel * c_load(char * path) {
  C_Perceptron *m= new C_Perceptron();
  FILE * fh = fopen(path,"r");
  int labelsize;
  int featsize;
  if(fscanf(fh,"%d",&labelsize)!=1) {fprintf(stderr,"fscanf failed\n");exit(1);}
  if(fscanf(fh,"%d",&featsize)!=1) {fprintf(stderr,"fscanf failed\n");exit(1);}
  m->labelsize = labelsize;
  m->featsize = featsize;
  m->params = new double*[m->labelsize];
  for (int line=0; line < m->labelsize; line++) { 
    m->params[line] = new double[m->featsize];
    int size;
    if(fscanf(fh,"%d",&size)!=1) {fprintf(stderr,"fscanf failed\n");exit(1);}
    for(int i=0;i<size;i++){
      int index;
      double val;
      if(fscanf(fh,"%d %lf",&index,&val)!=2) {
	fprintf(stderr,"fscanf failed\n");
	exit(1);
      }
      //      fprintf(stderr,"%d %f ",index,val);
      m->params[line][index] = val;
    }
    // fprintf(stderr,"\n");
  }
  fclose(fh);
  fprintf(stderr,"Loaded labels: %d\n",m->labelsize);
  fprintf(stderr,"Loaded features: %d\n",m->featsize);
  return (C_ZhangLeModel *)m;
}

