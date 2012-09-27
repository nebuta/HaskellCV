//
//  array.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 9/17/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//

#include "array.h"
#include "opencv2/opencv.hpp"
#include "opencv2/highgui/highgui.hpp"


#include <map>

extern "C" {
    #include "mainlib.h"
    
    using namespace cv;
    using namespace std;
    
    Mat* m_abs(Mat* mat) {
        Mat m = cv::abs(*mat);
        Mat *res = new Mat(m);
        return res;
    }
    
    Mat* m_add(Mat* a, Mat* b){
        Mat mat = (*a) + (*b);
        Mat *res = new Mat(mat);
        return res;
    }
    
    Mat* m_sub(Mat* a, Mat* b){
        Mat mat = (*a) - (*b);
        Mat *res = new Mat(mat);
        return res;
    }
    
    int m_eq(Mat* a, Mat* b){
        Mat res(a->size(),CV_8UC1);
        compare(*a,*b,res,CMP_NE);
        int count = countNonZero(res);
        return (count == 0) ? 1 : 0;
    }
    
    Mat* m_compare(Mat* a, Mat* b, int code){
        Mat *res = new Mat();
        compare(*a,*b,*res,code);
        return res;
    }
    
    Mat* addWeighted(Mat* ma, double alpha, Mat* mb, double beta, double gamma) {
        Mat res;
        cv::addWeighted(*ma, alpha, *mb, beta, gamma, res);
        Mat *res2 = new Mat(res);
        return res2;
    }
}