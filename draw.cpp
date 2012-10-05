//
//  draw.cpp
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
    
    
    //ToDo: check if this is correct (*mat should not change)
    Mat* m_circle(const Mat* mat, int* pos, int num, int radius, int isColor, int* color){

    }
}