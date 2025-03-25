import shap
import pandas as pd
import lightgbm as lgbm
import numpy as np
from sklearn.metrics import auc
try:
    import cPickle as pickle
except BaseException:
    import pickle
from matplotlib import pyplot as plt
import statsmodels.api as sm



def explain_model(model, # can be any model or scoring function or Radar model, but need to test
                  df_train, # same df used for training the model SHAP does not like categorical variables so need to encode them
                  algorithmn , # use "tree" for GBM or "permutation"  for blackbox models/scoring function
                  out_path,
                  suffix , 
                  compute_shap_interaction = False):
    explainer = shap.Explainer(model, model_output = "raw",algorithmn=algorithmn)
    sv= explainer.shap_values(df_train)
    with open(f"{out_path}\\shap_values_{suffix}.pkl", "wb") as f:
      pickle.dump(sv,f)
    sv_df = pd.DataFrame(sv , columns=df_train.columns)
    ft_importance =((sv_df.abs().sum())/(sv_df.abs().sum().sum())).sort_values(ascending  = False)
    ft_importance.to_csv(f"{out_path}\\ft_importance_{suffix}.csv")
    exp_sv = np.exp(sv)
    exp_sv_df = pd.DataFrame(exp_sv , columns=df_train.columns)
    exp_sv_df['sv_prod'] =  np.multiply.reduce(exp_sv, axis=1)
    exp_sv_df.to_csv(f"{out_path}\\exp_sv_df_{suffix}.csv")
    if compute_shap_interaction:
      df_sample = df_train.sample(n = 20000 , replace = False) 
      sv_x = explainer.shap_interaction_values(df_sample)
      with open(f"{out_path}\\shap_interaction_values_{suffix}.pkl", "wb") as f:
        pickle.dump(sv_x,f)
      sv_x_importance = pd.DataFrame(sv_x[0], index = df_sample.columns, columns = df_sample.columns)
      sv_x_importance.to_csv(f"{out_path}\\sv_x_importance_{suffix}.csv")
      return sv, ft_importance ,sv_x ,  sv_x_importance , df_sample
      
    else :
      return  sv, ft_importance 
    

  
def train_lgbm_model(df_train, y, weight,objective,metric, n_estimators =500,learning_rate = 0.15 ,max_depth = 4):
  
  model = lgbm.LGBMRegressor(
                  objective = objective,
                  metric = metric,
                  n_estimators = n_estimators,
                  learning_rate = learning_rate,
                  # boosting_type = "goss",# using goss quicker as it selects subset of the data based on the gradient and another random sample.
                  max_depth = max_depth,
                  boosting_type= 'gbdt',
                  random_state = 42)
  mod = model.fit(X = df_train,
                   y = y,
                   sample_weight = weight,
                     eval_metric = metric)
  return mod

def plot_shap_x(df_sample,sv_x,f1,f2):
    ax=plt.gca()
    shap.dependence_plot((f1, f2), 
                          sv_x, 
                          df_sample, # must be the same df used to compute shap interaction
                          display_features=df_sample, 
                          axis_color='w', 
                          show=False, 
                          alpha  = 0.2,
                          ax = ax
                          )
                

    ax.grid()


