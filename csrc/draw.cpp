//
//  draw.cpp
//  HaskellCV
//
//  Created by Hiroyuki Kai on 10/5/12.
//  Copyright (c) 2012 Hiroyuki Kai. All rights reserved.
//

#include "draw.h"

#include "opencv2/opencv.hpp"
#include "opencv2/highgui/highgui.hpp"

#include <map>

extern "C" {

#include "mainlib.h"
    
    using namespace cv;
    using namespace std;

    void m_circle(Mat* mat, int y, int x, int radius, int b, int g, int r, int thickness){
        circle(*mat, Point(x,y), radius, Scalar(b,g,r),thickness,CV_AA);
    }
    void m_rectangle(Mat* mat, int y1, int x1, int y2, int x2, int b, int g, int r, int thickness){
        rectangle(*mat, Point(x1,y1), Point(x2,y2), Scalar(b,g,r),thickness,CV_AA);
    }
    void m_line(Mat* mat, int y1, int x1, int y2, int x2, int b, int g, int r, int thickness){
        line(*mat, Point(x1,y1), Point(x2,y2), Scalar(b,g,r),thickness,CV_AA);
    }
}