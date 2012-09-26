//
//  mainlib.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 8/31/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//
// OpenCV Haskell interface via FFI

 #include "mainlib.h"


#include <map>

extern "C" {
    
    using namespace cv;
    using namespace std;
    
    static map<int,Mat> mats;
    static int new_id = 0;
    
/*    int valAt(int id,int pos){
        Mat m = mats.at(map<int,Mat>::key_type(id));
        return *(m.ptr<int>(pos));
    } */
    int length(int id){
        printf("id: %d\n",id);
        printf("%d matrices in memory\n",(int)(mats.size()));
        Mat m = mats.at(0);
        printf("(%d,%d)",m.size().width,m.size().height);
        return m.size().height * m.size().width;
    }
    
    //Stub
    int meanMat(int id){
        return 0;
    }
       
    Mat* randMat(int y,int x){
        Mat* mat = new Mat(y, x, CV_8UC1);
        cv::randu(*mat, cv::Scalar(0), cv::Scalar(256));
        return mat;
    }
    
    Mat* monoColor(int h, int w, int r, int g, int b){
        Mat_<Vec3b>* mat = new Mat_<Vec3b>(h,w,Vec3b(r,g,b));
        return mat;
    }
    
    void showMat(Mat* mat){
        imshow("showMat()",*mat);
        waitKey();
    }
    
    //ToDo: This is still a bug: mat is allocated in stack, not heap???
    Mat* readImg(char* path){
        string str(path);
        Mat mat = cv::imread(str);
        Mat *res = new Mat(mat);
        return res;
    }
    
    Mat* cvtColor(int code, Mat* mat) {
        Mat* res = new Mat();
        cv::cvtColor(*mat,*res,code);
        return res;
    }
    
    Mat* gaussian(int kernel_w, int kernel_h, int sigma_x, int sigma_y, Mat* mat) {
        Mat* res = new Mat();
        cv::GaussianBlur(*mat, *res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
        return res;
    }
    
}