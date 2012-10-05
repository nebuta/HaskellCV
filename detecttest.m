function detecttest

% Implementation of Journal of Structural Biology 151 (2005) 182-195
%  by Hiro Kai on August 2012.

% Settings
DoParticleDetection = true;
DoTrack = false;
ShowImageProgress = false;

% Use 'InputFile' if it exists

tic
InputFile = '/Users/hiroyuki/Documents/OpenCV/OpenCV/HaskellCV/cell.jpg';

disp(['Analyzing: ',InputFile]);

%To use imread instead of imageIterator functions.
% According to http://blogs.mathworks.com/steve/2009/04/02/matlab-r2009a-imread-and-multipage-tiffs/
% info = imfinfo(InputFile);
% numFrames = numel(info);

%Read the first frame for info
Image = imread(InputFile);

fprintf(1,'Starting particle detection.\n');



% Parameters for particle detection
w = 3;
lambda = 0.5;
percentileThreshold = 20;  % Top (percentileThreshold) %

% Parameters for particle detection (those not discussed explicitly in the
% paper)
dilateRadius = w;

numFrames = 1;
%Cell for keeping all particles
particles = cell(numFrames,1);
particlesWithInfo = cell(numFrames,1);

%definition of filters
hMean = fspecial('average', w);
hGauss = fspecial('gaussian', w, lambda);
%Definition of "structural element"
seDilate = strel('disk',dilateRadius,0);

% Repeat.
for ii = 1:100
    imageIndex = ii;
    if mod(imageIndex,50) == 0
        disp(['Repeat ' num2str(imageIndex)]);
    end
    
    %Read (imageIndex)th slice of stack file
    Image = imread(InputFile);
    
    %Apply filters to remove noise,
    B = imfilter(Image, hGauss, 'symmetric');
    C = imfilter(B, hMean, 'symmetric');
    
    %Find local maxima
    D = imdilate(C, seDilate);
    ps = findParcileCandidate(C,D,percentileThreshold,w);
%    particles{imageIndex} = refineParticlePositions(C,ps,w);
   % particlesWithInfo{imageIndex} = getMomentumInfo(particles{imageIndex},Image,w);
    
    %Plot detected particles with image
    if ShowImageProgress
        showImage(Image,particles{imageIndex});
    end
    
end
toc
fprintf(1,'\nParticle detection was done.\n');
end

function psinfo = getMomentumInfo(ps,image,w)
numPs = size(ps,1);
psinfo = [ps zeros(numPs,2)];
for ii=1:numPs
    neighbors = getNeighbors(image,round(ps(ii,:)),w);
    psinfo(ii,3) = zeroOrderMoment(neighbors,w);
    psinfo(ii,4) = secondOrderMoment(neighbors,w);
end
end


function showImage(Image,ps)
y = ps(:,1); x = ps(:,2);
imshow(imadjust(Image)); hold on;
scatter(x,y,'Marker', '*','MarkerEdgeColor','r');
hold off;
pause(0.001);
end

function points = findParcileCandidate(original,dilated,percentileThreshold,w)
threshold = prctile(original(:),(100-percentileThreshold));
% find returns "linear indices" of elements that match the specified
% condition.
indices = find(original==dilated & original>=threshold);
% Convert to matrix indices
[y x] = ind2sub(size(original),indices);
points = removeClosePoints(original,[y x],w);
end

function filtered = removeClosePoints(original,points,w)
% Get image dimension
imgHeight = size(original,1);
imgWidth = size(original,2);

numPoints = size(points,1);
filtered = zeros(1,2);
count = 0;
% Repeat over all points, and collect only brightest points
% within the distance of w
for ii=1:numPoints    % i is predefined as sqrt(-1), so ii is used for a loop.
    % Get point coordinate
    p = points(ii,:);
    y = p(1); x = p(2);
    % Set ranges so that they don't exceed array boundary.
    rangeY = max(y-w,1):min(y+w,imgHeight);
    rangeX = max(x-w,1):min(x+w,imgWidth);
    % Get the intensity of the brightest point in (2w+1) * (2w+1) area.
    neighbors = original(rangeY,rangeX);
    maxneighbor = max(neighbors(:));
    % If the point is brightest among neighbors, add it to "filtered"
    if maxneighbor == original(y,x)
        count = count + 1;
        filtered(count,:) = [y x];
    end
end
end

%image is the image after noise reduction filtering, not original image.
function points = refineParticlePositions(image,ps,w)
numPoints = size(ps,1);
points = zeros(size(ps));
for ii=1:numPoints
    y = ps(ii,1); x = ps(ii,2);
    while true
        neighbors = double(getNeighbors(image,[y x],w));
        vec = -w:w;
        sump = sum(neighbors(:));
        deltaY = sum(vec * neighbors) / sump;
        deltaX = sum(neighbors * vec') / sump;
        if abs(deltaY) < 0.5 && abs(deltaX) < 0.5
            break
        else
            y = round(y+deltaY); x = round(x+deltaX);
        end
    end
    points(ii,:) = [y+deltaY x+deltaX];
end
end

% Get neighbor points with filling out-of-boundary pixels by symmetric
% mirror filling. This function should always return a square matrix.
function neighbors = getNeighbors(image,point,w)
% Get image dimension
imgHeight = size(image,1);
imgWidth = size(image,2);
y = point(1); x = point(2);
rangeY = max(y-w,1):min(y+w,imgHeight);
rangeX = max(x-w,1):min(x+w,imgWidth);
submat = double(image(rangeY,rangeX));
if isequal(size(submat),[2*w+1 2*w+1])
    neighbors = submat;
else
    % Not square, so fill in the missing part.
    % ToDo check if this algorithm is correct.
    f1 = flipdim(submat,1);
    f2 = flipdim(submat,2);
    f12 = flipdim(f1,2);
    mirror = [f12 f1 f12; f2 submat f2; f12 f1 f12];
    miny = size(submat,1)+1;
    minx = size(submat,2)+1;
    if y - w <= 0
        miny = y - w + size(submat,1);
    end
    if x - w <= 0
        minx = x - w + size(submat,2);
    end
    % [y miny x minx w] % for debug
    neighbors = mirror(miny:miny+2*w,minx:minx+2*w);
end

end

function second = secondOrderMoment(neighbors,w)
zeroth = zeroOrderMoment(neighbors,w);
distSq = makeQuadMat(w);
secondeach = neighbors .* getCircle(w) .* distSq;
second = sum(secondeach(:)) / zeroth;
end

function value = zeroOrderMoment(neighbors,w)
mat = getCircle(w);
intensity = neighbors .* mat;
value = sum(intensity(:));
end

function mat = getCircle(w)
if w == 1
    mat = [0 1 0;1 1 1;0 1 0];
elseif w == 2
    mat = [0 0 1 0 0;0 1 1 1 0;1 1 1 1 1;0 1 1 1 0;0 0 1 0 0];
else
    mat = zeros(2*w+1);
    for ii=1:2*w+1
        for jj=1:2*w+1
            if norm([ii-w-1 jj-w-1])<=w
                mat(ii,jj) = 1;
            else
                mat(ii,jj) = 0;
            end
        end
    end
end
end

function mat = makeQuadMat(w)
if w == 1
    mat = [2 1 2;1 0 1;2 1 2];
elseif w == 2
    mat = [8 5 4 5 8; 5 2 1 2 5; 4 1 0 1 4; 5 2 1 2 5; 8 5 4 5 8];
else
    mat = zeros(2*w+1);
    for ii=1:2*w+1
        for jj=1:2*w+1
            a = [ii-w-1 jj-w-1];
            mat(ii,jj) = sum(a .* a);
        end
    end
end
end

