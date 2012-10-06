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
    
    // Creating and destroying Mat
    
    Mat* m_newMat(int type, int rows, int cols){
        Mat *mat = new Mat(type,rows,cols);
        
        return mat;
    }
    
    extern "C++" {
        template <typename T> void fillMatRandFloat(Mat *mat, T min, T max){
            for(int y=0; y < mat->rows; y++){
                for(int x=0; x < mat->rows; x++){
                    mat->at<T>(y,x) = min+(T)rand()/((T)RAND_MAX/(max-min));
                }
            }
        }
        template <typename T> void fillMatRandInt(Mat *mat, T min, T max){
            for(int y=0; y < mat->rows; y++){
                for(int x=0; x < mat->rows; x++){
                    mat->at<T>(y,x) = min+(T)rand()%(max-min+1);
                }
            }
        }
    }
    
    Mat* m_randMat(int type, int rows, int cols){
        Mat *mat = new Mat(type,rows,cols);
        switch(mat->depth()){
            case CV_8U:
                fillMatRandInt<uchar>(mat,0,UCHAR_MAX);
                break;
            case CV_8S:
                fillMatRandInt<char>(mat,CHAR_MIN,CHAR_MAX);
                break;
            case CV_16U:
                fillMatRandInt<uint16_t>(mat,0,UINT16_MAX);
                break;
            case CV_16S:
                fillMatRandInt<int16_t>(mat,INT16_MIN,INT16_MAX);
                break;
            case CV_32S:
                fillMatRandInt<int32_t>(mat,INT32_MIN,INT32_MAX);
                break;
            case CV_32F:
                fillMatRandFloat<float>(mat,0,1);
                break;
            case CV_64F:
                fillMatRandFloat<double>(mat,0,1);
                break;
                
        }
        
        return mat;
    }
    
    Mat* m_clone(Mat* mat){
        Mat res = mat->clone();
        Mat *ret = new Mat(res);
        return ret;
    }
    
    //
    // Getting Mat info
    //
    
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
    
    //
    // Convert depth/channel/color
    //
    
    //change depth with the same size and channels
    Mat* m_changeDepth(int depth, Mat* mat){
        int type = mat->type() + depth - mat->depth();
        Mat *res = new Mat();
        mat->convertTo(*res, type);
        return res;
    }
    
    //
    // Get a single pixel
    //
    
    int m_pixelIntAt(int y, int x, Mat* mat){
        int ret;
        switch (mat->depth()) {
            case CV_8U:
                ret = mat->at<uchar>(y,x);
                break;
            case CV_16U:
                ret = mat->at<ushort>(y,x);
                break;
            case CV_16S:
                ret = mat->at<short>(y,x);
                break;
            case CV_32S:
                ret = mat->at<uint32_t>(y,x);
                break;
            default:
                break;
        }
   //     printf("#%d %d %d\n",y,x,ret);
        return ret;
    }
    
    float m_pixelFloatAt(int y, int x, Mat* mat){
        float ret;
        if(mat->depth()!=CV_32F)
            printf("Error: CV_32F required.");
        ret = mat->at<float>(y,x);
        //     printf("#%d %d %d\n",y,x,ret);
        return ret;
    }
    
    double m_pixelDoubleAt(int y, int x, Mat* mat){
        double ret;
        if(mat->depth()!=CV_64F)
            printf("Error: CV_64F required.");
        ret = mat->at<double>(y,x);
        //     printf("#%d %d %d\n",y,x,ret);
        return ret;
    }
    
    //
    // Get pixels
    //
    
    //Not used for now. But this is better performance
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
    
    extern "C++" {
        template <typename T> T** valPtr(Mat* mat){
                int channels = mat->channels();
                int nRows = mat->rows * channels;
                T** ps = new T*[nRows];
                
                for( int i = 0; i < nRows; ++i)
                {
                    ps[i] = mat->ptr<T>(i);
                }
                return ps;
        }
    }
    
    //(possibly noncontinuous) uchar (returns array of uchar*, this should be freed by Haskell side)
    uchar** m_valsU8(Mat* mat)
    {
        if (mat->depth() != CV_8U)
            return NULL;
        else
            return valPtr<uchar>(mat);
    }
    //(possibly noncontinuous) char (returns array of uchar*, this should be freed by Haskell side)
    char** m_valsS8(Mat* mat)
    {
        if (mat->depth() != CV_8S)
            return NULL;
        else
            return valPtr<char>(mat);
    }

    uint16_t** m_valsU16(Mat* mat)
    {
        if (mat->depth() != CV_16U)
            return NULL;
        else
            return valPtr<uint16_t>(mat);
    }
    
    int16_t** m_valsS16(Mat* mat)
    {
        if (mat->depth() != CV_16S)
            return NULL;
        else
            return valPtr<int16_t>(mat);
    }
    
    int32_t** m_valsS32(Mat* mat)
    {
        if (mat->depth() != CV_32S)
            return NULL;
        else
            return valPtr<int32_t>(mat);
    }
    
    float** m_valsF32(Mat* mat)
    {
        if (mat->depth() != CV_32F)
            return NULL;
        else
            return valPtr<float>(mat);
    }
    
    
    double** m_valsF64(Mat* mat)
    {
        if (mat->depth() != CV_64F)
            return NULL;
        else
            return valPtr<double>(mat);
    }
    
    //
    // Elementary arithmetic
    //
    
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
    
    Mat* addWeighted(Mat* ma, double alpha, Mat* mb, double beta, double gamma) {
        Mat res;
        cv::addWeighted(*ma, alpha, *mb, beta, gamma, res);
        Mat *res2 = new Mat(res);
        return res2;
    }
    
    //
    // Comparison and search
    //
    
    Mat* m_compare(Mat* a, Mat* b, int code){
 //                 printf("m_compare() start.\n");
        Mat *res = new Mat();
        compare(*a,*b,*res,code);
 //                  printf("m_compare() end.%p %p %p\n",a,b,res);
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
            int sum = 0;
            for(int i=0; i<len; i++){
                ret[i+1] = ys.at(i);
                ret[i+1+len] = xs.at(i);
                sum += ret[i+1] + ret[i+1+len];
            }
  //          printf("%d non-zero elems. sum=%d\n",len,sum);
            return ret;
        }
    }
    
    //Only supports single channel image for now
    int* m_findNonZero(Mat *mat) {
//          puts("m_findNonZero() start.");
        if (mat->channels() != 1)
            return NULL;
        else {
            int* ret = NULL;
            switch (mat->depth()){
                case CV_8U:
                    ret = findNonZero<uint8_t>(mat);
                    break;
                case CV_8S:
                    ret = findNonZero<int8_t>(mat);
                    break;
                case CV_16U:
                    ret = findNonZero<uint16_t>(mat);
                    break;
                case CV_16S:
                    ret = findNonZero<int16_t>(mat);
                    break;
                case CV_32S:
                    ret = findNonZero<int32_t>(mat);
                    break;
                case CV_32F:
                    ret = findNonZero<float>(mat);
                    break;
                case CV_64F:
                    ret = findNonZero<double>(mat);
                    break;
                default:
                    ret = NULL;
            }
  //          puts("m_findNonZero() end.");
            return ret;
        }
    }

    //
    // Statistics and histogram
    //
    
    double m_mean(Mat* mat){
        return cv::mean(*mat)[0];
    }

    //For now, only single channel images
    int* m_hist(int channel, int numBins, float min, float max, Mat* mat){
//        puts("m_hist() start.");
        int histSize[] = {numBins};
        float range[] = { min, max };
        const float* ranges[] = { range };
        Mat* hist = new Mat();
        // we compute the histogram from the 0-th and 1-st channels
        int channels[] = {channel};
        
        calcHist( mat, 1, channels, Mat(),
                 *hist, 1, histSize, ranges,
                 true, // the histogram is uniform
                 false );
        Mat histi;
        hist->convertTo(histi, CV_32S);
 //       puts("m_hist() end.");
        return histi.ptr<int>();
    }
    

    //Only C1 images

    int m_percentileInt(double percentile, Mat* mat){
        //check if this is okay.
        Mat linear;
        int ret;

        if(mat->isContinuous() && mat->channels() == 1 && percentile >= 0 && percentile <= 100){
            linear = mat->reshape(0,1);
            Mat sorted;
            cv::sort(linear,sorted,CV_SORT_DESCENDING);
            int pos = min<int>(sorted.cols-1,floor(percentile/100*sorted.cols));
            switch(mat->depth()){
                case CV_8U:
                    ret = sorted.at<uint8_t>(pos);
                    break;
                case CV_16U:
                    ret = sorted.at<uint16_t>(pos);
                    break;
                case CV_16S:
                    ret = sorted.at<int16_t>(pos);
                    break;
                case CV_32S:
                    ret = sorted.at<int32_t>(pos);
                    break;
            }
            return ret;
        }else{
            // error for now.
            throw Exception();
        }
    }
    
    //Only C1 images
    int m_percentileFloat(double percentile, Mat* mat){
        //check if this is okay.
        Mat linear;
        int ret;
        if(mat->isContinuous() && mat->channels() == 1 && percentile >= 0 && percentile <= 100){
            linear = mat->reshape(0,1);
            Mat sorted;
            cv::sort(linear,sorted,CV_SORT_DESCENDING);
            int pos = min<int>(sorted.cols-1,floor(percentile/100*sorted.cols));
            switch(mat->depth()){
                case CV_32F:
                    ret = sorted.at<float>(pos);
                    break;
                case CV_64F:
                    ret = sorted.at<double>(pos);
                    break;
            }
            return ret;
        }else{
            // error for now.
            throw Exception();
        }
    }

}

