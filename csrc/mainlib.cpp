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
    
/*    int valAt(int id,int pos){
        Mat m = mats.at(map<int,Mat>::key_type(id));
        return *(m.ptr<int>(pos));
    } */
    
    void matFree(Mat* mat){
        mat->release();
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
        //Returns CV_8UC3
        Mat_<Vec3b>* mat = new Mat_<Vec3b>(h,w,Vec3b(r,g,b));
        return mat;
    }
    
    void showMat(Mat* mat, int delay){
        string name = format("Mat: %p",mat);
        namedWindow(name,CV_WINDOW_NORMAL);
        imshow(name,*mat);
        waitKey(delay);
        destroyWindow(name);
    }
    
    //ToDo: Is this way of allocation okay?
    Mat* readImg(char* path){
//                  puts("readImg() start.");
        string str(path);
        Mat mat = cv::imread(str);
        Mat *res = new Mat(mat);
//                  puts("readImg() end.");
        return res;
    }
    
    Mat* cvtColor(int code, Mat* mat) {
        Mat* res = new Mat();
        cv::cvtColor(*mat,*res,code);
        return res;
    }
    
}