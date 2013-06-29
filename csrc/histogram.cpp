//
//  histogram.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 6/11/13.
//  Copyright (c) 2012-2013 Hiroyuki Kai. All rights reserved.
//
// OpenCV Haskell interface via FFI

#include <opencv2/opencv.hpp>
#include <map>

extern "C" {
    
    using namespace cv;
    using namespace std;
    
    Mat* m_equalizeHist(Mat* m){
        Mat* res = new Mat();
        equalizeHist(*m,*res);
        return res;
    }
    
}