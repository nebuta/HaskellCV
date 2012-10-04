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
    
    int m_rows(Mat* mat){
        return mat->rows;
    }
    
    int m_cols(Mat* mat){
        return mat->cols;
    }

    int m_type(Mat* mat){
        return mat->type();
    }
    
    int m_channels(Mat* mat){
        return mat->channels();
    }
    
    //change depth with the same size and channels
    Mat* m_changeDepth(int depth, Mat* mat){
        int type = mat->type() + depth - mat->depth();
        Mat *res = new Mat();
        mat->convertTo(*res, type);
        return res;
    }
    
    int m_pixelIntAt(int y, int x, Mat* mat){
        return mat->at<int>(y,x);
    }
    
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
    
    Mat* m_mul(Mat* a, Mat* b){
        Mat mat = a->mul(*b);
        Mat *res = new Mat(mat);
        return res;
    }
    
    Mat* m_divNum(Mat* a, double denom){
        Mat ones = Mat::ones(a->size(),CV_8U);
        Mat mat = a->mul(ones,1.0/denom);
        Mat *res = new Mat(mat);
        return res;
    }
    
    Mat* m_div(Mat* a, Mat *b){
        Mat *res = new Mat();
        cv::divide(*a,*b,*res);
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
    
    
    extern "C++"{
        template <class T> int* findNonZero(Mat *mat){
            int *ret;
            vector<int> ys;
            vector<int> xs;
            for(int y=0; y < mat->rows; y++){
                for(int x=0; x < mat->rows; x++){
                    if(mat->at<T>(y,x)!=0){
                        ys.push_back(y);
                        xs.push_back(x);
                    }
                }
            }
            int len = ys.size();
            ret = new int[len*2+1];
            ret[0] = len;
            for(int i=0; i<len; i++){
                ret[i+1] = ys.at(i);
                ret[i+1+len] = xs.at(i);
            }
            return ret;
        }
    }
    
    //Only supports single channel image for now
    int* m_findNonZero(Mat *mat) {
        if (mat->channels() != 1)
            return NULL;
        else {
            switch (mat->depth()){
                case CV_8U:
                    return findNonZero<uint8_t>(mat);
                case CV_8S:
                    return findNonZero<int8_t>(mat);
                case CV_16U:
                    return findNonZero<uint16_t>(mat);
                case CV_16S:
                    return findNonZero<int16_t>(mat);
                case CV_32S:
                    return findNonZero<int32_t>(mat);
                case CV_32F:
                    return findNonZero<float>(mat);
                case CV_64F:
                    return findNonZero<double>(mat);
            }
            return NULL;
        }
    }

    
    double m_mean(Mat* mat){
        return cv::mean(*mat)[0];
    }
    
    Mat* addWeighted(Mat* ma, double alpha, Mat* mb, double beta, double gamma) {
        Mat res;
        cv::addWeighted(*ma, alpha, *mb, beta, gamma, res);
        Mat *res2 = new Mat(res);
        return res2;
    }
    
    //Continuous uchar
    uchar* m_valsUCharC(Mat* mat)
    {
        if (!mat->isContinuous() || mat->depth() != CV_8U)
            return NULL;
        else {
            return mat->ptr<uchar>(0);
        }
    }
    
    //Continuous char
    char* m_valsCharC(Mat* mat)
    {
        if (!mat->isContinuous() || mat->depth() != CV_8S)
            return NULL;
        else {
            return mat->ptr<char>(0);
        }
    }
    //(possibly noncontinuous) uchar (returns array of uchar*, this should be freed by Haskell side)
    uchar** m_valsUChar(Mat* mat)
    {
        if (mat->depth() != CV_8U)
            return NULL;
        else {
            int channels = mat->channels();
            int nRows = mat->rows * channels;        
            uchar** ps = new uchar*[nRows];
            
            for( int i = 0; i < nRows; ++i)
            {
                ps[i] = mat->ptr<uchar>(i);
            }
            return ps;
        }
    }
    
    //(possibly noncontinuous) char (returns array of uchar*, this should be freed by Haskell side)
    char** m_valsChar(Mat* mat)
    {
        if (mat->depth() != CV_8S)
            return NULL;
        else {
            int channels = mat->channels();
            int nRows = mat->rows * channels;
            char** ps = new char*[nRows];
            
            for( int i = 0; i < nRows; ++i)
            {
                ps[i] = mat->ptr<char>(i);
            }
            return ps;
        }
    }
}