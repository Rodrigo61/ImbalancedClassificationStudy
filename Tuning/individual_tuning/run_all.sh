#!/bin/bash
cd submission_files
sleep 20 | condor_submit acc_svm_false.sub
sleep 20 | condor_submit acc_svm_true.sub
sleep 20 | condor_submit acc_rf_false.sub
sleep 20 | condor_submit acc_rf_true.sub
sleep 20 | condor_submit acc_xgboost_false.sub
sleep 20 | condor_submit acc_xgboost_true.sub
sleep 20 | condor_submit f1_svm_false.sub
sleep 20 | condor_submit f1_svm_true.sub
sleep 20 | condor_submit f1_rf_false.sub
sleep 20 | condor_submit f1_rf_true.sub
sleep 20 | condor_submit f1_xgboost_false.sub
sleep 20 | condor_submit f1_xgboost_true.sub
sleep 20 | condor_submit gmeans_svm_false.sub
sleep 20 | condor_submit gmeans_svm_true.sub
sleep 20 | condor_submit gmeans_rf_false.sub
sleep 20 | condor_submit gmeans_rf_true.sub
sleep 20 | condor_submit gmeans_xgboost_false.sub
sleep 20 | condor_submit gmeans_xgboost_true.sub
sleep 20 | condor_submit mcc_svm_false.sub
sleep 20 | condor_submit mcc_svm_true.sub
sleep 20 | condor_submit mcc_rf_false.sub
sleep 20 | condor_submit mcc_rf_true.sub
sleep 20 | condor_submit mcc_xgboost_false.sub
sleep 20 | condor_submit mcc_xgboost_true.sub
sleep 20 | condor_submit auc_svm_false.sub
sleep 20 | condor_submit auc_svm_true.sub
sleep 20 | condor_submit auc_rf_false.sub
sleep 20 | condor_submit auc_rf_true.sub
sleep 20 | condor_submit auc_xgboost_false.sub
sleep 20 | condor_submit auc_xgboost_true.sub
