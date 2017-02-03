#!/bin/bash
sleep 20 | condor_submit submission_files/acc_svm_false.sub
sleep 20 | condor_submit submission_files/acc_svm_true.sub
sleep 20 | condor_submit submission_files/acc_rf_false.sub
sleep 20 | condor_submit submission_files/acc_rf_true.sub
sleep 20 | condor_submit submission_files/f1_svm_false.sub
sleep 20 | condor_submit submission_files/f1_svm_true.sub
sleep 20 | condor_submit submission_files/f1_rf_false.sub
sleep 20 | condor_submit submission_files/f1_rf_true.sub
sleep 20 | condor_submit submission_files/gmeans_svm_false.sub
sleep 20 | condor_submit submission_files/gmeans_svm_true.sub
sleep 20 | condor_submit submission_files/gmeans_rf_false.sub
sleep 20 | condor_submit submission_files/gmeans_rf_true.sub
sleep 20 | condor_submit submission_files/mcc_svm_false.sub
sleep 20 | condor_submit submission_files/mcc_svm_true.sub
sleep 20 | condor_submit submission_files/mcc_rf_false.sub
sleep 20 | condor_submit submission_files/mcc_rf_true.sub
