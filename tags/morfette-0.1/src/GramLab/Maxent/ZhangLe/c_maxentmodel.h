typedef void C_ZhangLeModel;

#ifdef __cplusplus
extern "C" { 
#endif
  C_ZhangLeModel * c_train_C_ZhangLeModel(int    iter,
					  double sigma,
					  double tolerance,
					  int    verbosity,
					  int*   labels,
					  int*   row_sizes,
					  int**  featids,
					  float ** featvals,
					  int    col_size);
  void c_destroy_C_ZhangLeModel(C_ZhangLeModel *);
  void c_eval_all(C_ZhangLeModel *,
		  int*    features,
		  float*  realvals,
		  int     f_size,
		  int*    out_labels,
		  double* out_probs,
		  int     out_size);
  void c_save(C_ZhangLeModel *,char* path);
  C_ZhangLeModel * c_load(char * path);
#ifdef __cplusplus  
}
#endif
