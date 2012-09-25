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
    
    int abs(int id) {
        Mat res = cv::abs(getMat(id));
        return registerMat(res);
    }
    int add(int idA, int idB){
        Mat& mat1 = getMat(idA);
        Mat& mat2 = getMat(idB);
        Mat res = mat1 + mat2;
        return registerMat(res);
    }
    
    int subMat(int idA, int idB){
        Mat& mat1 = getMat(idA);
        Mat& mat2 = getMat(idB);
        Mat res = mat1 - mat2;
        return registerMat(res);
    }
    
    int eqMat(int idA, int idB){
        Mat& mat1 = getMat(idA);
        Mat& mat2 = getMat(idB);
        int res = 0;    // false always: Stub!!!!!
        return res;
    }
    
    int addWeighted(int ma, double alpha, int mb, double beta, double gamma) {
        Mat mat1 = getMat(ma);
        Mat mat2 = getMat(mb);
        Mat res;
        cv::addWeighted(mat1, alpha, mat2, beta, gamma, res);
        return registerMat(res);
    }
}