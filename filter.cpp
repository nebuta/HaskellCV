//
//  mainlib.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 8/31/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//
// OpenCV Haskell interface via FFI

 #include "mainlib.h"

#include "opencv2/opencv.hpp"
#include "opencv2/highgui/highgui.hpp"

#include <map>

extern "C" {

    using namespace std;
    
    cv::Mat* f_gaussian(int kernel_w, int kernel_h, int sigma_x, int sigma_y, Mat* mat) {
        Mat* res = new Mat();
        cv::GaussianBlur(*mat, *res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
        return res;
    }
    
    cv::Mat* f_medianFilter(int ksize, Mat* mat) {
        Mat* res = new Mat();
        cv::medianBlur(*mat, *res, ksize);
        return res;
    }
    
    cv::Mat* f_boxFilter(int kernel_w, int kernel_h, Mat* mat) {
        Mat* res = new Mat();
        cv::boxFilter(*mat, *res, mat->depth(), Size(kernel_w,kernel_h));
        return res;
    }
    
    cv::Mat* f_derivFilter(int dx, int dy, int ksize, Mat* mat) {
        Ptr<FilterEngine> Fderiv = createDerivFilter(mat->type(),mat->type(),dx,dy,ksize);
        Mat* res = new Mat();
        Fderiv->apply(*mat,*res);
        return res;
    }
    
    cv::FilterEngine* f_makeGaussian(int kernel_w, int kernel_h, int sigma_x, int sigma_y) {
        //return cv::createGaussianFilter(*mat, *res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
        return NULL; //Stub!!!
    }
    
}