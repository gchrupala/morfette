#include <maxent/maxentmodel.hpp>
#include <sstream>
#include <string>
#include "c_maxentmodel.h"

using namespace maxent;
using namespace std;

std::string int_to_string(int i) {
    std::string str;
    std::stringstream ss;
    ss << i;
    ss >> str;
    return str;
}


C_ZhangLeModel * c_train_C_ZhangLeModel(int    iter,
					double sigma,
					double tolerance,
					int    verbosity,
					int*   labels,
					int*   row_sizes,
					int**  featids,
					float ** featvals,
					int    col_size) {
  MaxentModel * m = new MaxentModel();
  int old_verbosity = maxent::verbose;
  maxent::verbose = verbosity;
  m->begin_add_event();
  for (int i=0; i<col_size;i++) {
    string label = int_to_string(labels[i]);
    vector<pair<string,float> > fs;
    for(int j=0;j<row_sizes[i];j++){
      fs.push_back(make_pair(int_to_string(featids[i][j]),featvals[i][j]));
    }

    m->add_event(fs,label,1);
  }
  m->end_add_event();
  m->train(iter,"lbfgs",sigma,tolerance);
  maxent::verbose = old_verbosity;
  return (C_ZhangLeModel *)m;
}

void c_destroy_C_ZhangLeModel(C_ZhangLeModel * m) {
  MaxentModel * mm = (MaxentModel *)m;
  delete mm;
}

void c_eval_all(C_ZhangLeModel * m,
		int*    features,
		float*  realvals,
		int     f_size,
		int*    out_labels,
		double* out_probs,
		int     out_size) {
  MaxentModel * mm = (MaxentModel *)m;
  vector<pair<string,float> > fs;
  for(int i=0;i<f_size;i++){
    fs.push_back(make_pair(int_to_string(features[i]),realvals[i]));
  }
  vector<pair<string,double> > dist;
  mm->eval_all(fs,dist,true);
  assert(out_size==dist.size());
  for(unsigned int i=0;i<dist.size();i++){
    out_labels[i] = atoi(dist[i].first.c_str());
    out_probs[i]  = dist[i].second;
  }
}


void c_save(C_ZhangLeModel *m,char* path) {
    MaxentModel * mm = (MaxentModel *)m;
    mm->save(path,true);
}

C_ZhangLeModel * c_load(char * path) {
  MaxentModel * m = new MaxentModel();
  m->load(path);
  return (C_ZhangLeModel *)m;
}

