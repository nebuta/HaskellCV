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
    
    //Simple filters without mask
    
    cv::Mat* f_gaussian(int kernel_w, int kernel_h, double sigma_x, double sigma_y, Mat* mat, int inplace) {
        if(inplace){
            cv::GaussianBlur(*mat, *mat, Size(kernel_w,kernel_h),sigma_x,sigma_y);
            return mat;            
        }else{
            Mat* res = new Mat();
            cv::GaussianBlur(*mat, *res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
            return res;
        }
    }
    
    cv::Mat* f_medianFilter(int ksize, Mat* mat, int inplace) {
        Mat* res = new Mat();
        cv::medianBlur(*mat, *res, ksize);
        return res;
    }
    
    
    cv::Mat* f_laplacian(int ksize, double scale, double delta, Mat* mat, int inplace) {
        Mat* res = new Mat();
        cv::Laplacian(*mat, *res, mat->depth(),ksize,scale,delta);
        return res;
    }
    
    cv::Mat* f_bilateral(int d, double sigmaColor, double sigmaSpace, Mat* mat, int inplace) {
        Mat* res = new Mat();
        cv::bilateralFilter(*mat, *res, d, sigmaColor, sigmaSpace);
        return res;
    }

    cv::Mat* f_sobel(int dx, int dy, int ksize, double scale, double delta, Mat* mat, int inplace) {
        Mat* res = new Mat();
        cv::Sobel(*mat, *res, dx, dy, ksize, scale, delta);
        return res;
    }
    
    cv::Mat* f_boxFilter(int kernel_w, int kernel_h, Mat* mat, int inplace) {
        Mat* res = new Mat();
        cv::boxFilter(*mat, *res, mat->depth(), Size(kernel_w,kernel_h));
        return res;
    }
    
    cv::Mat* f_derivFilter(int dx, int dy, int ksize, Mat* mat, int inplace) {
        Ptr<FilterEngine> Fderiv = createDerivFilter(mat->type(),mat->type(),dx,dy,ksize);
        Mat* res = new Mat();
        Fderiv->apply(*mat,*res);
        return res;
    }
    
    cv::FilterEngine* f_makeGaussian(int kernel_w, int kernel_h, int sigma_x, int sigma_y, int inplace) {
        //return cv::createGaussianFilter(*mat, *res, Size(kernel_w,kernel_h),sigma_x,sigma_y);
        return NULL; //Stub!!!
    }
    
    //Making mask
    
    cv::Mat* f_getStructuringElement(int shape, int width, int height){
        Mat mat = getStructuringElement(shape, Size(width,height));
        Mat* res = new Mat(mat);
        return res;
    }
    
    //Filters that use mask
    cv::Mat* f_dilate(Mat* strel, Mat* mat, int inplace){
        Mat* res = new Mat();
        cv::dilate(*mat,*res,*strel);
        return res;
    }
    cv::Mat* f_erode(Mat* strel, Mat* mat, int inplace){
        Mat* res = new Mat();
        cv::erode(*mat,*res,*strel);
        return res;
    }

    // Copied from https://github.com/joshdoe/opencv/blob/1d319f683f6b9a8b0c7cbe2abdc9664f0dac919f/modules/imgproc/src/imadjust.cpp
void stretchlimFromHist( const cv::MatND& hist, double* low_value,
                     double* high_value, double low_fract, double high_fract,
                     unsigned int histSum)
{
    CV_Assert( low_fract >= 0 && low_fract < 1.0 );
    CV_Assert( low_fract < high_fract && high_fract <= 1.0);

    unsigned int sum;
    unsigned int low_count = low_fract * histSum;
    sum = 0;
    for( unsigned int i = 0; i < hist.rows; i++ ) {
        if (sum >= low_count) {
            *low_value = i;
            break;
        }

        sum += ((float*)hist.data)[i];
    }

    unsigned int high_count = (1 - high_fract) * histSum;
    sum = 0;
    for( unsigned int i = hist.rows - 1; i >= 0; i-- ) {
        if (sum >= high_count) {
            *high_value = i;
            break;
        }

        sum += ((float*)hist.data)[i];
    }
}

int bitsFromDepth( int depth )
{
    if (depth == CV_8U)
        return 8;
    else if (depth == CV_16U)
        return 16;
    else
        return 0;
}

//TODO: handle RGB or force user to do a channel at a time?
void stretchlim( const InputArray _image, double* low_value,
                     double* high_value, double low_fract, double high_fract )
{
    Mat image = _image.getMat();

    CV_Assert( image.type() == CV_8UC1 || image.type() == CV_16UC1 );

    if (low_fract == 0 && high_fract == 1.0) {
        // no need to waste calculating histogram
        *low_value = 0;
        *high_value = 1;
        return;
    }

    int nPixelValues = 1 << bitsFromDepth( image.depth() );
    int channels[] = { 0 };
    MatND hist;
    int histSize[] = { nPixelValues };
    float range[] = { 0, nPixelValues };
    const float* ranges[] = { range };
    calcHist( &image, 1, channels, Mat(), hist, 1, histSize, ranges );
    
    stretchlimFromHist( hist, low_value, high_value, low_fract, high_fract, image.rows * image.cols );

    //TODO: scaling to 0..1 here, but should be in stretchlimFromHist?
    unsigned int maxVal = (1 << bitsFromDepth( _image.depth() )) - 1;
    *low_value /= maxVal;
    *high_value /= maxVal;
}


Mat* f_imadjust(double low_in, double high_in, double low_out, double high_out, Mat* mat, int inplace)
{
    CV_Assert( (low_in == 0 || high_in != low_in) && high_out != low_out );
    Mat* res = new Mat();

    //FIXME: use NaN or something else for default values?
//    if (low_in == 0 && high_in == 0)
 //       stretchlim ( *mat, &low_in, &high_in );

    double alpha = (high_out - low_out) / (high_in - low_in);
    double beta = high_out - high_in * alpha;

    int depth = mat->depth();

    //TODO: handle more than just 8U/16U
    //adjust alpha/beta to handle to/from different depths
    int max_in = (1 << bitsFromDepth( mat->depth() )) - 1;
    int max_out = (1 << bitsFromDepth( mat->depth() )) - 1;
    // y = a*x*(outmax/inmax) + b*outmax
    alpha *= max_out / max_in;
    beta *= max_out;

    mat->convertTo( *res, depth, alpha, beta );
    return res;
}
}